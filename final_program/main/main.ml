open Graphimport
open Sim
open Render
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
        if i mod 50 = 0 then density_graph_to_csv g gr dr (ref (List.map (fun x -> string_of_float x) !d)) c "output.csv";
        flush_all();
    done;
    dr, d


let _ =
    let dr, d = run_simulation 10000 in
    density_graph_to_csv g gr dr (ref (List.map (fun x -> string_of_float x) !d)) c "final.csv"

