open Random

let opt = 4.

let k = 1.
let distance g i j =
    let x1, y1 = g.(i) in
    let x2, y2 = g.(j) in
    sqrt ( (x1 -. x2) ** 2. +. (y1 -. y2) ** 2.)


let minimum_float a b =
    if a < b then a else b


let initial_temp = 5.
let decrease_temp n max_n =
    let cooling_factor = 0.1 in
    initial_temp *. (1. -. ((float_of_int n) /. (float_of_int max_n))) ** cooling_factor


let repulsive_force g i j =
    let d = distance g i j in
    (k ** 2.) /. d

let attractive_force g i j =
    let const = 1. in
    let d = distance g i j in
    (d ** 2.) /. const

let magnitude (x,y) =
    sqrt (x ** 2. +. y ** 2.)

let normalize (x,y) =
    let m = magnitude (x,y) in
    if m <> 0. then
    (x /. m, y /. m)
    else (x,y)

let limit_displacement g i h w =
    let x,y = g.(i) in
    let new_x = 
        if x < 0. then 0.
        else if x > w then w 
        else x 
    in
    let new_y =
        if y < 0. then 0. else if y > h then h else y
    in
    g.(i) <- (new_x, new_y)

let reingold g gr h w n =
    let temp = ref initial_temp in
    let d_graph = Array.make (Array.length g) (0.,0.) in
    for iter = 0 to n do
        temp:= decrease_temp iter n;
    (*repulsive forces*)
    for i = 0 to Array.length g - 1 do
        let dx = ref 0. in
        let dy = ref 0. in
        for j = 0 to Array.length g -1 do
            if i <> j then begin
            let rx = (repulsive_force g i j) in
            let ry = rx in
            let ndx = (distance g i j) in
            let ndy = ndx in
            dx:= !dx +. rx /. ndx;
            dy:= !dy +. ry /. ndy;
            end
        done;
        d_graph.(i) <- (!dx, !dy);
    done;
    (*attractive forces*)
    for i = 0 to Array.length g - 1 do
        List.iter (fun j ->
            let d = distance g i j in
            let displacement = (attractive_force g i j) *. d -. opt /. d in
            let x1,y1 = d_graph.(i) in
            let x2, y2 = d_graph.(j) in
            d_graph.(i) <- (x1 -. displacement, y1 -. displacement);
            d_graph.(j) <- (x2 +. displacement, y2 +. displacement))
        gr.(i)
    done;
    (*update the position*)
    for i = 0 to Array.length g - 1 do
        let dx, dy = d_graph.(i) in
        let dlen = minimum_float
                (sqrt (dx ** 2. +. dy ** 2.))
                !temp in
        let dxn, dyn = normalize d_graph.(i) in
        g.(i) <- (dxn *. dlen, dyn *. dlen)
    done;
    for i = 0 to Array.length g - 1 do
        limit_displacement g i h w
    done; 
    done
                        

let g = Array.init 5 (fun _ -> Random.float 1980., Random.float 1280.)
let gr = [|[1];[2;3];[0];[4];[2]|]

