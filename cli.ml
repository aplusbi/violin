let fftsize = 8192
let samplerate = 44100

let unix_write str =
    ignore (Unix.write Unix.stdout str 0 (String.length str))

let init () =
    Portaudio.init ();
    at_exit Portaudio.terminate

let print_freqs notes fs =
    let rec print fs = match fs with
    | [] -> ()
    | (i, v)::t ->
            let f = 
                try
                    Notes.NoteMap.find i notes
                with Not_found -> string_of_float ((float_of_int i) *.  (float_of_int samplerate) /. (float_of_int fftsize))
            in
            let str = f ^ ": " ^ f ^ "\n" in
            unix_write str;
            print t
    in
    unix_write "\027[H\027[2J";
    print fs

let combine_dups l rate =
    let compare a b =
        if abs_float (a -. b) < 5. then true
        else false
    in
    let rec combine v c acc = function [] -> (v, c)::acc
        | h::t when compare h v -> combine v (c + rate) acc t
        | h::t -> let acc2 = (v, c)::acc in combine h rate acc2 t
    in
    match l with
    | [] -> []
    | h::t -> combine h rate [] t

let to_pcm buff l =
    let rec pcm start = function [] -> ()
    | (f, c)::t ->
    begin
        (*unix_write ((string_of_float f) ^ " " ^ (string_of_int c) ^ "\n");*)
        Wav.createsinbuff c samplerate [5.] [f] buff start;
        pcm (start + c) t
    end
    in
    pcm 0 l

let all_freqs_to_pcm buff l =
    let rec pcm start = function [] -> ()
    | (f, a)::t ->
    begin
        Wav.createsinbuff 1024 samplerate a f buff start;
        pcm (start + 1024) t
    end
    in
    pcm 0 l

let main () =
    init ();
    let notes = Notes.read_notes "assets/notes.txt" samplerate (fftsize/2) in
    let stream = Portaudio.open_default_stream 1 1 samplerate 256 in
    Portaudio.start_stream stream;
    let mag = Array.make (fftsize/2 + 1) 0. in
    let data = Wav.load_wav "assets/MississippiStopStop.wav" in
    let buf = [|data|] in
    let processor = Wav.data_processor data samplerate 1024 fftsize mag in
    let rec loop i acc =
        try
            let acc2 =
                (match  processor i with
                | Some ((mi, mx) as m) ->
                begin
                    (*Portaudio.write_stream stream buf (i*1024) 1024;*)
                    let getfreq i = ((float_of_int i) *.  (float_of_int samplerate) /. (float_of_int fftsize)) in
                    let freqs = Utils.foldmaxima (fun a i v -> if i > 5 && v > 0.5 *. mx then (getfreq i)::a else a) [] mag in
                    let amps = Utils.foldmaxima (fun a i v -> if i > 5 && v > 0.5 *. mx then (log v)::a else a) [] mag in
                    (*if mx > 5. then*)
                    begin
                        (*print_freqs notes [m];*)
                        (*let freq = ((float_of_int mi) *.  (float_of_int*)
                        (*samplerate) /. (float_of_int fftsize)) in*)
                        (freqs, amps)::acc
                    end
                    (*else*)
                        (*acc*)
                end
                | None -> acc)
            in
            loop (i+1) acc2
        with End_of_file -> acc
    in
    let song = loop 0 [] in
    let len = List.length song in
    unix_write ("Playing song " ^ string_of_int len ^ "\n");
    let data = Array.make (len * 1024) 0. in
    (*to_pcm data (combine_dups song 1024);*)
    all_freqs_to_pcm data song;
    for i = 0 to len*1024 do
        Portaudio.write_stream stream [|data|] (i*1024) 1024;
        ()
    done

let _ = main ()

