let fftsize = 8192

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
    init ();
    let notes = Notes.read_notes "assets/notes.txt" 11025 (fftsize/2) in
    let surface = Sdlvideo.set_video_mode width height [`DOUBLEBUF; `HWSURFACE] in
    let two55 = Int32.of_int 255 in
    let frequencies = Sdlvideo.create_RGB_surface [`SWSURFACE] width height 24 two55 two55 two55 two55 in
    let tnr = Sdlttf.open_font "assets/Times_New_Roman.ttf" 32 in
    let stream = Portaudio.open_default_stream 1 1 11025 256 in
    Portaudio.start_stream stream;
    let mag = Array.make (fftsize/2 + 1) 0. in
    let processor = Wav.stream_processor stream 256 fftsize 1024 mag in
    let frame = ref 0 in
    let rate = ref 0 in
    let stime = ref (Unix.gettimeofday ()) in
    let rec loop i =
        Sdlvideo.fill_rect surface (Int32.of_int 0);
        Sdlevent.pump ();
        match Sdlevent.poll () with
        | Some Sdlevent.QUIT -> ()
        | _ -> 
        begin
            if Sdlkey.is_key_pressed Sdlkey.KEY_ESCAPE then
                ()
            else
            begin
                (match  processor i with
                | Some (_, mx) ->
                begin
                    let freqs = Utils.foldmaxima (fun a i v -> if i > 5 && v > 0.8 *. mx then (i, v)::a else a) [] mag in
                    Sdlvideo.fill_rect frequencies (Int32.of_int 0);
                    print_freqs notes tnr frequencies freqs;
                end
                | None -> ());

                Sdlvideo.blit_surface ~src:frequencies ~dst:surface
                    ~dst_rect:{Sdlvideo.r_x=0; Sdlvideo.r_y=0; Sdlvideo.r_w=width;
                    Sdlvideo.r_h=height} ();

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
                loop (i+1)
            end
        end
    in
    loop 0

let _ = main ()
