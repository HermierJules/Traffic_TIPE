type weighted_graph = (int * float * int) list array
(* destination, weight, number of lanes*)

let nodes = Csv.to_array (Csv.input_all(Csv.of_channel (open_in "nodes.csv")))
let edges = Csv.to_array (Csv.input_all(Csv.of_channel (open_in "edges.csv"))) 


let make_weighted_graph edges : weighted_graph * (string, int) Hashtbl.t =
    let list_of_all_nodes edges =
        let l = ref [] in
        for i = 1 to Array.length edges - 1 do
            let x = edges.(i).(1) in
            let y = edges.(i).(2) in
            l:=x::y::!l
        done;
        List.sort_uniq (fun a b -> if a = b then 0 else 
        let x = int_of_string a in 
        let y = int_of_string b in 
        if x > y then 1 else (-1)) !l
    in
    let l = list_of_all_nodes edges in
    let n = List.length l in
    let cache = Hashtbl.create n in
    let e = Array.make n [] in 
    let count = ref 0 in
    List.iter (fun x -> Hashtbl.add cache x !count; incr count) l;
    for i = 1 to Array.length edges - 1 do
        if int_of_string edges.(i).(5) > 0 then begin
            let lane = int_of_string edges.(i).(5) in
            let x = Hashtbl.find cache edges.(i).(1) in
            let y = Hashtbl.find cache edges.(i).(2) in
            let len = float_of_string edges.(i).(3) in
            e.(x) <- (y,len, lane)::e.(x) 
        end;
        if int_of_string edges.(i).(6) > 0 then begin
            let x = Hashtbl.find cache edges.(i).(1) in
            let lane = int_of_string edges.(i).(5) in
            let y = Hashtbl.find cache edges.(i).(2) in
            let len = float_of_string edges.(i).(3) in
            e.(y) <- (x,len, lane)::e.(y) 
        end;
    done;
    e,cache


let get_edge_cache edges c =
    (*returns cache where Hashtbl.find cache (x,y) will give the (id, forward: bool) associated with the (x,y) edge*)
    let cache = Hashtbl.create 50 in
    for i = 1 to Array.length edges - 1 do
        let x = Hashtbl.find_opt c edges.(i).(1) in
        let y = Hashtbl.find_opt c edges.(i).(2) in
        match x,y with
        |Some x, Some y -> if int_of_string edges.(i).(5) > 0 
        then  Hashtbl.add cache (x,y) (edges.(i).(0),true);
        if int_of_string edges.(i).(6) > 0 then 
            Hashtbl.add cache (y,x) (edges.(i).(0), false)
        |_ -> ()
    done;
    cache



let find_biggest_node g = 
    let m = ref 0 in
    let mn = ref (-1) in
    Array.iteri (fun i (l: 'a list) -> let n = List.length l in if n > !m then begin
        m:=n;
        mn:=i;
    end else ()) g;
    !mn, !m
        
(*------------------------------------------------------------------*)


