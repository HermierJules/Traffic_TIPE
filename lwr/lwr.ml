type road = {
    l : int;
    n : float array;
    freen : float array;
    q : float array}


let rec truc r t =
    let sigm = Array.init r.l (fun i -> min q.(i) .5) in
    let delt = Array.init r.l (fun i -> min .5 (
