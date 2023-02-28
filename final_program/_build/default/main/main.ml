open Graphimport
open Sim

let make_road_graph g =
    let road_graph = Hashtbl.create 50 in
    Array.iteri (fun x l -> 
        List.iter 
        (fun (y,w) ->
            let n = (int_of_float w) + 1 in
            Hashtbl.add road_graph (x,y) (make_road n))
        l)
        g

let _ = 
    let g, cache = make_weighted_graph edges in ()

