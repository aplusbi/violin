module FFT = Fftw3.S

let miclen = 1024
let readlen = 256
let micbufc = Array.make miclen 0.
let micbuf = [|micbufc|]

let fftsize = 8192
let signal = FFT.Array1.create FFT.float Bigarray.c_layout fftsize
let dft = FFT.Array1.create FFT.complex Bigarray.c_layout (fftsize/2 + 1)
let mag = Array.make (fftsize/2 + 1) 0.
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
            (*let f = string_of_float ((float_of_int i) *. 11025. /. (float_of_int fftsize)) in*)
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
    Utils.big_array_copy signal (Wav.createsin 1024 11025 [16.; 8.] [262.;440.]) 0 1024;
    Wav.hann_window signal;
    init ();
    let notes = Notes.read_notes "assets/notes.txt" 11025 (fftsize/2) in
    let surface = Sdlvideo.set_video_mode width height [`DOUBLEBUF; `HWSURFACE] in
    let two55 = Int32.of_int 255 in
    let frequencies = Sdlvideo.create_RGB_surface [`HWSURFACE] width height 24 two55 two55 two55 two55 in
    let tnr = Sdlttf.open_font "assets/Times_New_Roman.ttf" 32 in
    let stream = Portaudio.open_default_stream 1 1 11025 readlen in
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
        Portaudio.write_stream stream micbuf !readmic readlen;
        readmic := !readmic + readlen;
        if !readmic >= miclen then
        begin
            readmic := 0;
            Sdlvideo.fill_rect frequencies (Int32.of_int 0);
            Utils.big_array_copy signal micbufc 0 miclen;
            Wav.hann_window signal;
            FFT.exec plan;
            let (_, mx) = Utils.mag dft mag in
            let freqs = Utils.foldmaxima (fun a i v -> if i > 5 && v > 0.8 *. mx then (i, v)::a else a) [] mag in
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
