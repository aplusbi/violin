let big_array_copy ba a first len =
    for i = 0 to len-1 do
        Bigarray.Array1.set ba i a.(first + i)
    done

let mag ba a =
    let len = min (Bigarray.Array1.dim ba) (Array.length a) in
    let mi, mx = ref 0, ref 0. in
    for i = 0 to len-1 do
        a.(i) <- Complex.norm (Bigarray.Array1.get ba i);
        if a.(i) > !mx then
        begin
            mi := i;
            mx := a.(i)
        end
    done;
    !mi, !mx

let max ba =
    let len = Bigarray.Array1.dim ba in
    let mx, mi = ref 0., ref (-1) in
    for i = 0 to len-1 do
        let a = Complex.norm (Bigarray.Array1.get ba i) in
        if  a > !mx then
        begin
            mx := a;
            mi := i
        end;
    done;
    !mi, !mx

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


