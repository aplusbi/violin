let fftsize = 8192
let samplerate = 44100

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

let print_text font txt color dest =
    let src = Sdlttf.render_text_solid font txt color in
    let { Sdlvideo.w=w; Sdlvideo.h=h } = Sdlvideo.surface_info src in
    let destrect = { Sdlvideo.r_x=0; Sdlvideo.r_y=0; Sdlvideo.r_w=w; Sdlvideo.r_h=h } in
    Sdlvideo.blit_surface ~src:src ~dst:dest ~dst_rect:destrect ()

let print_freqs font dest fs =
    let rec print fs x y = match fs with
    | [] -> ()
    | (i, _)::t ->
        try
            let f = (float_of_int i) *. (float_of_int samplerate) /. (float_of_int fftsize) in
            let str = Printf.sprintf "%f" f in
            let src = Sdlttf.render_text_solid font str Sdlvideo.white in
            let { Sdlvideo.w=w; Sdlvideo.h=h } = Sdlvideo.surface_info src in
            let destrect = { Sdlvideo.r_x=x; Sdlvideo.r_y=y; Sdlvideo.r_w=w; Sdlvideo.r_h=h } in
            Sdlvideo.blit_surface ~src:src ~dst:dest ~dst_rect:destrect ();
            print t x (y+h)
        with Not_found -> print t x y
    in
    print fs 0 64

let print_notes notes font dest fs =
    let rec print fs x y = match fs with
    | [] -> ()
    | (i, _)::t ->
        try
            let f = Notes.NoteMap.find i notes in
            let str = Printf.sprintf "%s" f in
            let src = Sdlttf.render_text_solid font str Sdlvideo.white in
            let { Sdlvideo.w=w; Sdlvideo.h=h } = Sdlvideo.surface_info src in
            let destrect = { Sdlvideo.r_x=x; Sdlvideo.r_y=y; Sdlvideo.r_w=w; Sdlvideo.r_h=h } in
            Sdlvideo.blit_surface ~src:src ~dst:dest ~dst_rect:destrect ();
            print t x (y+h)
        with Not_found -> print t x y
    in
    print fs 0 0

let load_surface fmt fname =
    let image = Sdlloader.load_image "assets/fingerboard.png" in
    let {Sdlvideo.w=width; Sdlvideo.h=height} = Sdlvideo.surface_info image in
    let surface = Sdlvideo.create_RGB_surface_format fmt [`HWSURFACE] width height in
    Sdlvideo.blit_surface ~src:image ~dst:surface
    ~dst_rect:{Sdlvideo.r_x=0; Sdlvideo.r_y=0; Sdlvideo.r_w=width;
    Sdlvideo.r_h=height} ();
    surface

type finger_pos = {surface:Sdlvideo.surface; pos:Sdlvideo.rect}

let create_notes font fmt f =
    let bg = Sdlloader.load_image "assets/green64x64.png" in
    let note k v =
        let s = Sdlvideo.create_RGB_surface_format fmt [`HWSURFACE] 64 64 in
        Sdlvideo.blit_surface ~src:bg ~dst:s
        ~dst_rect:{Sdlvideo.r_x=0; Sdlvideo.r_y=0; Sdlvideo.r_w=64;
        Sdlvideo.r_h=64} ();
        print_text font k Sdlvideo.white s;
        let (x, y) = v in
        let rect = {Sdlvideo.r_x=x; Sdlvideo.r_y=y; Sdlvideo.r_w=64; Sdlvideo.r_h=64} in
        {surface=s; pos=rect}
    in
    Notes.FingerMap.mapi note f

let draw_finger_pos dest note map =
    try
        let pos = Notes.FingerMap.find note map in
        Sdlvideo.blit_surface ~src:pos.surface ~dst:dest ~dst_rect:pos.pos ();
    with Not_found -> ()

let draw_all fingers dest =
    Notes.FingerMap.iter (fun k v ->
        Sdlvideo.blit_surface ~src:v.surface ~dst:dest ~dst_rect:v.pos ()
    ) fingers


let main () =
    let width, height = 640, 480 in
    init ();
    let notes = Notes.read_notes "assets/notes.txt" samplerate (fftsize/2) in
    let surface = Sdlvideo.set_video_mode width height [`DOUBLEBUF; `HWSURFACE] in
    let tnr = Sdlttf.open_font "assets/Times_New_Roman.ttf" 32 in
    let background = load_surface surface "assets/fingerboard.png" in
    let fingers_lookup = Notes.read_fingers "assets/fingers.txt" (310-128) 0 64 64 in
    let fingers = create_notes tnr surface fingers_lookup in
    let stream = Portaudio.open_default_stream 1 1 samplerate 1024 in
    Portaudio.start_stream stream;
    let mag = Array.make (fftsize/2 + 1) 0. in
    (*let processor = Wav.stream_processor stream 256 fftsize 1024 mag in*)
    let data = Wav.load_wav "assets/LightlyRow.wav" in
    let playbuf = Array.make 1024 0. in
    let buf = [|playbuf|] in
    let processor = Wav.data_processor data samplerate 1024 fftsize mag in
    let frame = ref 0 in
    let rate = ref 0 in
    let stime = ref (Unix.gettimeofday ()) in
    let rec loop i =
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
                | Some (mi, mx) ->
                begin
                    Utils.array_copy_big playbuf data (i*1024) 1024;
                    Portaudio.write_stream stream buf 0 1024;
                    Sdlvideo.blit_surface ~src:background ~dst:surface
                    ~dst_rect:{Sdlvideo.r_x=0; Sdlvideo.r_y=0; Sdlvideo.r_w=width;
                    Sdlvideo.r_h=height} ();
                    print_notes notes tnr surface [mi, mx];
                    print_freqs tnr surface [mi, mx];
                try
                    let n = Notes.NoteMap.find mi notes in
                    draw_finger_pos surface n fingers
                with Not_found -> ();
                end
                | None -> ());

                (* print frame rate *)
                incr frame;
                if (Unix.gettimeofday ()) -. !stime > 1. then
                    begin
                        rate := !frame;
                        frame := 0;
                        stime := Unix.gettimeofday ()
                    end;
                let str = Printf.sprintf "%d" !rate in
                sdl_print_string tnr str surface 30 300;
                Sdlvideo.flip surface;
                loop (i+1)
            end
        end
    in
    loop 0

let _ = main ()
