module Note =
    struct
        type t = int
        let compare a b =
            let dist =  abs (a - b) in
            if dist <= 1 then
                0
            else
                compare a b
    end

module NoteMap = Map.Make(Note)

let split_string str ch =
    let rec split i =
        try
            let pos = String.index_from str i ch in
            (String.sub str i (pos-i))::(split (pos+1))
        with Not_found -> [String.sub str i ((String.length str) - i)]
    in
    split 0

let bucket s =
    let f = float_of_string s in
    int_of_float (f *. 4096. /. 11025.)

let read_notes fname =
    let file = open_in fname in
    let rec read_line () =
        try
            let line = input_line file in
            match split_string line '\t' with
            | [] -> failwith "Error reading notes"
            | h::t -> List.fold_left (fun m x -> NoteMap.add (bucket x) h m) (read_line ()) t
        with End_of_file -> NoteMap.empty
    in
    read_line ()
