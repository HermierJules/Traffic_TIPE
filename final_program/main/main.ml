open Graphimport
open Sim

let vinit = 3 (*initial speed of vehicles when spawning*)

let g, c = make_weighted_graph edges

let gr = make_road_graph g

let dr = make_density_graph g 

let d = ref [] 

let entry, exit = get_entry_exit g gr

let run_simulation n =
    for i = 1 to n do
        simulate_full g gr dr d entry exit vinit;
        Printf.printf "cycle %d\n" i;
        flush_all();
    done;
    dr, d

