module FFT = Fftw3.S

let createsin samples amp freq =
    let pi = 3.1415926 in
    let step = pi /. 22050. in
    let wave i =
        let t = step *. (float_of_int i) in
        List.fold_left2 (fun a b c -> a +. b *. sin (c *. t)) 0. amp freq
    in
    Array.init samples wave

let itermaxima f signal =
        let len = Array.length signal in
        for i = 1 to len-2 do
                let p = signal.(i-1) in
                let c = signal.(i) in
                let n = signal.(i+1) in
                if p <= c && n <= c then
                        f i c
  done 

let foldmaxima f acc signal =
    let a = ref acc in
    let len = Array.length signal in
    for i = 1 to len-2 do
        let p = signal.(i-1) in
        let c = signal.(i) in
        let n = signal.(i+1) in
        if p <= c && n <= c then
            a := f !a i c
    done;
    !a


let miclen = 1024
let micbufc = Array.make miclen 0.
let micbuf = [|micbufc|]

let big_copy_array ba a first last =
    let len = last - first in
    for i = 0 to len-1 do
        Bigarray.Array1.set ba i a.(first + i)
    done

let dftmag ba a =
    let len = min (Bigarray.Array1.dim ba) (Array.length a) in
    let mx = ref 0. in
    for i = 0 to len-1 do
        a.(i) <- Complex.norm (Bigarray.Array1.get ba i);
        if a.(i) > !mx then
            mx := a.(i)
    done;
    !mx


let signal = FFT.Array1.create FFT.float Bigarray.c_layout 1024
let dft = FFT.Array1.create FFT.complex Bigarray.c_layout 513
let mag = Array.make 257 0.
let plan = FFT.Array1.r2c signal dft

let init () =
    Sdl.init [`VIDEO];
    Sdlttf.init ();
    Portaudio.init ();
    at_exit Sdl.quit;
    at_exit Sdlttf.quit;
    at_exit Portaudio.terminate

let print_freqs font dest fs =
    let rec print fs x y = match fs with
    | [] -> ()
    | (i, v)::t ->
        let f = (float_of_int i) *. 44100. /. 1024. in
        let str = Printf.sprintf "%f: %f" f v in
        let src = Sdlttf.render_text_solid font str Sdlvideo.white in
        let { Sdlvideo.w=w; Sdlvideo.h=h } = Sdlvideo.surface_info src in
        let destrect = { Sdlvideo.r_x=x; Sdlvideo.r_y=y; Sdlvideo.r_w=w; Sdlvideo.r_h=h } in
        Sdlvideo.blit_surface ~src:src ~dst:dest ~dst_rect:destrect ();
        print t x (y+h)
    in
    print fs 0 0

let main () =
    init ();
    let surface = Sdlvideo.set_video_mode 300 300 [`DOUBLEBUF; `HWSURFACE; `RESIZABLE] in
    let tnr = Sdlttf.open_font "assets/Times_New_Roman.ttf" 24 in
    let stream = Portaudio.open_default_stream 1 1 44100 256 in
    Portaudio.start_stream stream;
    let loop = ref true in
    while !loop do
        Sdlvideo.fill_rect surface (Int32.of_int 0);
        Sdlevent.pump ();
        match Sdlevent.poll () with
        | Some Sdlevent.QUIT -> loop := false
        | _ -> ();
        if Sdlkey.is_key_pressed Sdlkey.KEY_ESCAPE then
            loop := false;
        Portaudio.read_stream stream micbuf 0 miclen;
        Portaudio.write_stream stream micbuf 0 miclen;
        big_copy_array signal micbufc 0 1024;
        FFT.exec plan;
        let mx = dftmag dft mag in
        let freqs = foldmaxima (fun a i v -> if v > 0.9 *. mx then (i, v)::a else a) [] mag in
        print_freqs tnr surface freqs;
        Sdlvideo.flip surface;
    done

let _ = main ()

