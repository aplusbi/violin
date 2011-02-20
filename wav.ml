module FFT = Fftw3.D
open Bigarray

let pi = 3.1415926

let clip signal max =
    let len = Array.length signal in
    for i = 0 to len-1 do
        if signal.(i) > max then
            signal.(i) <- max
        else
            if signal.(i) < -.max then
                signal.(i) <- -.max
    done

let createsin samples rate amp freq =
    let step = 2. *. pi /. (float_of_int rate) in
    let wave i =
        let t = step *. (float_of_int i) in
        List.fold_left2 (fun a b c -> a +. b *. sin (c *. t)) 0. amp freq
    in
    Array.init samples wave

let createsinbuff samples rate amp freq buff start =
    let step = 2. *. pi /. (float_of_int rate) in
    let wave i =
        let t = step *. (float_of_int i) in
        List.fold_left2 (fun a b c -> a +. b *. sin (c *. t)) 0. amp freq
    in
    for i = 0 to samples - 1 do
        buff.(start + i) <- wave i
    done

let load_wav fname =
    let file = open_in_bin fname in
    seek_in file 40;
    let b0 = input_byte file in
    let b1 = input_byte file in
    let b2 = input_byte file in
    let b3 = input_byte file in
    let ch_size = b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24) in
    seek_in file (48 + ch_size);
    let len = in_channel_length file in
    let pcm = Array1.create float64 Bigarray.c_layout (len/2) in
    for i = 0 to len / 2 do
        try
            let a = input_byte file in
            let b = input_byte file in
            let c = a lor (b lsl 8) in
            let d =
                if c <= 32768 then c
                else -(65536 - c)
            in
            Array1.set pcm i ((float_of_int d) /. 8192.);
        with End_of_file -> ()
    done;
    pcm;;

let hann_window ?(l = -1) ba =
    ()(*
    let len =
        if l == -1 then
            Array1.dim ba
        else
            l
    in
    for i = 0 to len-1 do
        let n = float_of_int i in
        let l = float_of_int (len - 1) in
        let wn = 0.5 *. (1. -. (cos (2. *. pi *. n /. l))) in
        let x = Array1.get ba i in
        Array1.set ba i (x *. wn)
    done*)


let freq_from_bucket rate samples i =
    i * rate / samples / 2

let fftsize = 8192

let stream_processor stream sstep fftsz fstep mag =
    (* fft stuff *)
    let signal = FFT.Array1.create FFT.float Bigarray.c_layout fftsz in
    Array1.fill signal 0.;
    let dft = FFT.Array1.create FFT.complex Bigarray.c_layout (fftsz/2 + 1) in
    Array1.fill dft Complex.zero;
    let plan = FFT.Array1.r2c signal dft in

    (* portaudio stuff *)
    let data = Array.make fstep 0. in
    let buf = [|data|] in
    let curr = ref 0 in
    (fun i ->
        Portaudio.read_stream stream buf !curr sstep;
        curr := !curr + sstep;
        if !curr >= fstep then
        begin
            curr := 0;
            Utils.big_array_copy signal data 0 fstep;
            hann_window ~l:fstep signal;
            FFT.exec plan;
            Some (Utils.mag dft mag)
        end
        else
            None
    )

let stream_playback_processor stream sstep fftsz fstep mag =
    (* fft stuff *)
    let signal = FFT.Array1.create FFT.float Bigarray.c_layout fftsz in
    Array1.fill signal 0.;
    let dft = FFT.Array1.create FFT.complex Bigarray.c_layout (fftsz/2 + 1) in
    Array1.fill dft Complex.zero;
    let plan = FFT.Array1.r2c signal dft in

    (* portaudio stuff *)
    let data = Array.make fstep 0. in
    let buf = [|data|] in
    let curr = ref 0 in
    (fun i ->
        Portaudio.read_stream stream buf !curr sstep;
        Portaudio.write_stream stream buf !curr sstep;
        curr := !curr + sstep;
        if !curr >= fstep then
        begin
            curr := 0;
            Utils.big_array_copy signal data 0 fstep;
            hann_window ~l:fstep signal;
            FFT.exec plan;
            Some (Utils.mag dft mag)
        end
        else
            None
    )

let line_in_processor rate sstep fftsz fstep mag =
    let stream = Portaudio.open_default_stream 1 0 rate fstep in
    Portaudio.start_stream stream;
    stream_processor stream sstep fftsz fstep mag

let data_processor data rate step fftsz mag =
    let len = Array1.dim data in

    (* fft stuff *)
    let signal = FFT.Array1.create FFT.float Bigarray.c_layout fftsz in
    Array1.fill signal 0.;
    let dft = FFT.Array1.create FFT.complex Bigarray.c_layout (fftsz/2 + 1) in
    Array1.fill dft Complex.zero;
    let plan = FFT.Array1.r2c signal dft in

    (fun i ->
        if (i + 1) * step >= len-1 then
        begin
            raise End_of_file
        end
        else
        begin
            Array1.blit (Array1.sub data (i * step) step) (Array1.sub signal 0 step);
            hann_window ~l:step signal;
            FFT.exec plan;
            Some (Utils.mag dft mag)
        end
    )

let wav_processor fname rate step fftsz mag =
    let data = load_wav fname in
    data_processor data rate step fftsz mag
