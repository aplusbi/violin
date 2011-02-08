module FFT = Fftw3.S

let clip signal max =
    let len = Array.length signal in
    for i = 0 to len-1 do
        if signal.(i) > max then
            signal.(i) <- max
        else
            if signal.(i) < -.max then
                signal.(i) <- -.max
    done

let createsin samples amp freq =
    let pi = 3.1415926 in
    let step = pi /. 11025. /. 2. in
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
let readlen = 256
let micbufc = Array.make miclen 0.
let micbuf = [|micbufc|]

let big_copy_array ba a first len =
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


let fftsize = 4096
let signal = FFT.Array1.create FFT.float Bigarray.c_layout fftsize
let dft = FFT.Array1.create FFT.complex Bigarray.c_layout (fftsize/2 + 1)
let mag = Array.make 257 0.
let plan = FFT.Array1.r2c signal dft

let init () =
    Sdl.init [`VIDEO];
    Sdlttf.init ();
    Portaudio.init ();
    at_exit Sdl.quit;
    at_exit Sdlttf.quit;
    at_exit Portaudio.terminate

let sdl_print_string font str dest x y =
        let src = Sdlttf.render_text_solid font str Sdlvideo.white in
        let { Sdlvideo.w=w; Sdlvideo.h=h } = Sdlvideo.surface_info src in
        let destrect = { Sdlvideo.r_x=x; Sdlvideo.r_y=y; Sdlvideo.r_w=w; Sdlvideo.r_h=h } in
        Sdlvideo.blit_surface ~src:src ~dst:dest ~dst_rect:destrect ()

let print_freqs notes font dest fs =
    let rec print fs x y = match fs with
    | [] -> ()
    | (i, v)::t ->
        try
            let f = Notes.NoteMap.find i notes in
            let str = Printf.sprintf "%s: %f" f v in
            let src = Sdlttf.render_text_solid font str Sdlvideo.white in
            let { Sdlvideo.w=w; Sdlvideo.h=h } = Sdlvideo.surface_info src in
            let destrect = { Sdlvideo.r_x=x; Sdlvideo.r_y=y; Sdlvideo.r_w=w; Sdlvideo.r_h=h } in
            Sdlvideo.blit_surface ~src:src ~dst:dest ~dst_rect:destrect ();
            print t x (y+h)
        with Not_found -> print t x y
    in
    print fs 0 0

let main () =
    let width, height = 640, 480 in
    Bigarray.Array1.fill signal 0.;
    init ();
    let notes = Notes.read_notes "assets/notes.txt" in
    let surface = Sdlvideo.set_video_mode width height [`DOUBLEBUF; `HWSURFACE] in
    let two55 = Int32.of_int 255 in
    let frequencies = Sdlvideo.create_RGB_surface [`HWSURFACE] width height 24 two55 two55 two55 two55 in
    let tnr = Sdlttf.open_font "assets/Times_New_Roman.ttf" 32 in
    let stream = Portaudio.open_default_stream 1 1 11025 miclen in
    Portaudio.start_stream stream;
    let loop = ref true in
    let frame = ref 0 in
    let rate = ref 0 in
    let stime = ref (Unix.gettimeofday ()) in
    let readmic = ref 0 in
    while !loop do
        Sdlvideo.fill_rect surface (Int32.of_int 0);
        Sdlevent.pump ();
        match Sdlevent.poll () with
        | Some Sdlevent.QUIT -> loop := false
        | _ -> ();
        if Sdlkey.is_key_pressed Sdlkey.KEY_ESCAPE then
            loop := false;

        Portaudio.read_stream stream micbuf !readmic readlen;
        readmic := !readmic + readlen;
        if !readmic >= miclen then
        begin
            readmic := 0;
            Sdlvideo.fill_rect frequencies (Int32.of_int 0);
            big_copy_array signal micbufc 0 miclen;
            FFT.exec plan;
            let mx = dftmag dft mag in
            let freqs = foldmaxima (fun a i v -> if i > 5 && v > 0.8 *. mx then (i, v)::a else a) [] mag in
            print_freqs notes tnr frequencies freqs;
        end;
        Sdlvideo.blit_surface ~src:frequencies ~dst:surface
        ~dst_rect:{Sdlvideo.r_x=0; Sdlvideo.r_y=0; Sdlvideo.r_w=width; Sdlvideo.r_h=height} ();

        (* print frame rate *)
        incr frame;
        if (Unix.gettimeofday ()) -. !stime > 1. then
            begin
                rate := !frame;
                frame := 0;
                stime := Unix.gettimeofday ()
            end;
        let str = Printf.sprintf "%d" !rate in
        sdl_print_string tnr str surface 300 300;
        Sdlvideo.flip surface;
    done

let _ = main ()
