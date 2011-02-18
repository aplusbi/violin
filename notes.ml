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

module FingerMap = Map.Make(String)

let split_string str ch =
    let rec split i =
        try
            let pos = String.index_from str i ch in
            (String.sub str i (pos-i))::(split (pos+1))
        with Not_found -> [String.sub str i ((String.length str) - i)]
    in
    split 0

let bucket s rate samples =
    let f = float_of_string s in
    int_of_float (f *. (float_of_int samples) /. (float_of_int rate))

let read_notes fname rate samples =
    let file = open_in fname in
    let rec read_line () =
        try
            let line = input_line file in
            match split_string line '\t' with
            | [] -> failwith "Error reading notes"
            | h::t -> List.fold_left (fun m x -> NoteMap.add (bucket x rate samples) h m) (read_line ()) t
        with End_of_file -> NoteMap.empty
    in
    read_line ()
    
let read_fingers fname x y w h =
    let file = open_in fname in
    let rec read_split x acc = function [] -> acc
        | h::t -> FingerMap.add h (x, y) (read_split (x+1) acc t)
    in
    let rec read_line y =
        try
            let line = input_line file in
            read_split 0 (read_line (y+1)) (split_string line '\t')
        with End_of_file -> FingerMap.empty
    in
    read_line 0
