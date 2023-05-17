open Graphimport
open Sim

let g, c = make_weighted_graph edges

let gr = make_road_graph g

let _ =
    let r, _ = Hashtbl.find gr (0,1) in
    r.(0).(0) <- Voiture (5,6);
    r.(0).(1) <- Voiture (5,6)
