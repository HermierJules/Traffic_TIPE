open Csv
open Graphimport


let print_to_csv_file data filename =
  Csv.save filename data

let density_graph_to_csv g gr dr d c filename =
  let t = ref [] in
  let l = List.init (List.length !d) (fun x -> string_of_int (x + 1)) in
  t:= ["id"::l];
  let forw = ref ["forward"] in
  let edge_cache = get_edge_cache edges c in
  for x = 0 to Array.length g - 1 do
    List.iter (fun (y, _, _) -> 
      let id, is_f = Hashtbl.find edge_cache (x,y) in
      let l = List.map (fun x -> string_of_float x) (Hashtbl.find dr (x,y)) in
      t:= (id::(List.rev l))::!t;
      if is_f then forw:= "1"::!forw
      else forw:= "0"::!forw) g.(x)
  done;
  print_to_csv_file ((List.rev !forw)::!t) filename




