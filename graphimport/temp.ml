type voiture = {mutable vitesse: int; mutable destination : int; mutable time: int; mutable has_moved: bool}
type state = Empty | Full of voiture
type graph = {
	vtx: state array; 
	edges : int list array;
	discovered : bool array;
	processed : bool array;
}

type graph_list = {
	mutable vtx : int list;
	mutable edges : (int * int) list;
}
let rec max l c =
match l with 
|[] -> c
|(t,b)::q -> if t > c then max q t else max q c 
(*
let normalize g = 
	let rec edges_list_to_array t l =
		match l with
		|[] -> t 
		|(x,y)::q -> if x <> (-1) && y <> (-1) then t.(x) <- y::t.(x); edges_list_to_array t q 
	in	
	let n = List.length g.vtx + 1 in
	let discovered = Array.make n false in
	let processed = Array.make n false in 
	let vtx = Array.make n Empty in
	let edges = edges_list_to_array (Array.make n [])
									 g.edges in
	{vtx = vtx; edges = edges; discovered = discovered; processed = processed}

let graph_import_node file_node file_edges= 
	let vtx = ref [] in
	let og_id = Hashtbl.create 2048 in
	let ed_temp = ref [] in
	let x = ref 0 in 
	let y = ref 0 in
	let l = ref 0 in
	let node_nb = ref 0 in
	let nodes = Csv.to_array (Csv.input_all(Csv.of_channel (open_in file_node))) in
	let edges = Csv.to_array (Csv.input_all(Csv.of_channel (open_in file_edges))) in
	(*making the cache to make the end graph more manageable*)
	for i =Array.length  nodes -1  to 1  do
		Hashtbl.add og_id  nodes.(i).(0) (i-1) ;
		vtx:= int_of_string(nodes.(i).(0))::!vtx;
		node_nb:= !node_nb + 1; 
	done;
	for i = 1 to Array.length edges-1 do
		x:= (match Hashtbl.find_opt og_id edges.(i).(1)  with
			 |Some x -> x
			 |None -> (-1));
		y:= (match Hashtbl.find_opt og_id edges.(i).(2)  with
			 |Some x ->  x
			 |None -> (-1));
		l:= ((int_of_float (float_of_string edges.(i).(3))) / 5) + 1;
		for j = 1 to !l do
			vtx:= !node_nb::!vtx;
			if !l <> 1 
				then if j <> 1 then ed_temp:= (!node_nb, !node_nb + 1):: !ed_temp
							   else ed_temp:= (!x, !node_nb):: !ed_temp
				else if j <> 1 then ed_temp:= (!node_nb, !y) :: !ed_temp
							   else ed_temp:= (!x, !y)::!ed_temp;
				node_nb:=!node_nb + 1;
			
		done;
		(*if it's a double way road*)
		l:= ((int_of_float (float_of_string edges.(i).(3))) / 5) + 1;
		if int_of_string edges.(i).(6) <> 0 then begin
		for j = 1 to !l do
			vtx:= !node_nb::!vtx;
			if !l <> 1 
				then if j <> 1 then ed_temp:= (!node_nb, !node_nb + 1):: !ed_temp
							   else ed_temp:= (!x, !node_nb + 1):: !ed_temp
				else if j <> 1 then ed_temp:= (!node_nb, !x) :: !ed_temp
							   else ed_temp:= (!y, !x)::!ed_temp;
				node_nb:=!node_nb + 1;
			
		done;
		end

	done;
	{vtx = !vtx; edges = !ed_temp}
    
let g = normalize (graph_import_node "nodes.csv" "edges.csv")	
*)


let nodes = Csv.to_array (Csv.input_all(Csv.of_channel (open_in "nodes.csv")))
let edges = Csv.to_array (Csv.input_all(Csv.of_channel (open_in "edges.csv"))) 


(*IMPROVEMENTS : handle the multiple lanes when car_forward > 1*)
let graph_import_ugly file_node file_edges= 
	let vtx = ref [] in
    let edges_list = ref [] in
	let nodes = Csv.to_array (Csv.input_all(Csv.of_channel (open_in file_node))) in
	let edges = Csv.to_array (Csv.input_all(Csv.of_channel (open_in file_edges))) in
	(*building the nodes list*)
	for i = 1 to Array.length  nodes -1 do
		vtx:= int_of_string(nodes.(i).(0))::!vtx; 
	done;
    (*building the edges list*)
    for i = 1 to Array.length edges - 1 do
        if int_of_string(edges.(i).(5)) <> 0 then 
            edges_list:= ((int_of_string(edges.(i).(1)), int_of_string(edges.(i).(2))),int_of_float(float_of_string(edges.(i).(3)))/5 + 1)::!edges_list;
        if int_of_string(edges.(i).(6)) <> 0 then
            edges_list:= ((int_of_string(edges.(i).(2)),int_of_string(edges.(i).(1))), int_of_float(float_of_string(edges.(i).(3)))/5 + 1) ::!edges_list;
    done;
    (!vtx,!edges_list)

let normalize_nodes vtx edges =
    let cache = Hashtbl.create 2048 in
    let revcache = Hashtbl.create 2048 in
    let rec cache_vtx l cache revcache count = 
        match l with
        |[] -> ()
	|t::q -> Hashtbl.add cache t count;Hashtbl.add revcache count t; cache_vtx q cache revcache (count+1)
    in
    let rec n_vtx l new_l cache =
        match l with
        |[] -> ()
        |t::q -> let x = Hashtbl.find cache t in new_l:= x::!new_l; n_vtx q new_l cache
    in 
    let rec n_edges l new_l cache = 
        match l with
        |[] -> ()
        |((x,y),t)::q -> let nx, ny = Hashtbl.find cache x, Hashtbl.find cache y in 
                     new_l:=((nx,ny),t)::!new_l;
                    n_edges q new_l cache
    in
    cache_vtx vtx cache revcache 0;
    let ne = ref [] in
    let nv = ref [] in
    n_edges edges ne cache; 
    n_vtx vtx nv cache;
    (nv, ne),cache, revcache


let extend_edge nodes edge =
    let ((x,y),l) = edge in
    let n = ref (List.length !nodes) in
    let maillons = ref [] in
    if l = 1 then [(x,y)]
    else begin
        maillons:=(x,!n)::!maillons;
        nodes:=!n::!nodes;
        n:=!n+1;
        for i = 1 to l do
            maillons:=(!n-1,!n)::!maillons;
            nodes:=!n::!nodes;
            n:=!n + 1;
        done;
        maillons:=(!n,y)::!maillons;
        !maillons end

            
let graph_import file_nodes file_edges = 
    let ugly_vtx, ugly_edges = graph_import_ugly file_nodes file_edges in 
    let (vtx, edges), cache, revcache  = normalize_nodes ugly_vtx ugly_edges in
    let rec finalize_edges vtx edges final_edges =
        match edges with
        |[] -> ()
        |t::q -> final_edges:= extend_edge vtx t @ !final_edges; 
                finalize_edges vtx q final_edges
    in
    let rec edges_list_to_array l t =
        let n = Array.length t in
        match l with
        |[] -> t
        |(x,y)::q -> if x < n && y < n then t.(x) <- y::t.(x); edges_list_to_array q t 
    in
    let final_edges = ref [] in
    finalize_edges vtx !edges final_edges;
    let f_vtx = Array.make (List.length !vtx) Empty in
    let f_edges = Array.make (List.length !vtx) [] in
    let discovered = Array.make (List.length !vtx) false in
    let processed = Array.make (List.length !vtx) false in
    {vtx = f_vtx; 
     edges = edges_list_to_array !final_edges f_edges;
     discovered = discovered;
     processed = processed},cache, revcache

let comp_connexe g x =
    Array.iteri (fun i x -> g.discovered.(i) <- false) g.discovered;
    let comp = ref [] in
    let rec aux g x =
        if not(g.discovered.(x)) then 
            List.iter (fun x -> comp:=x::!comp; aux g x)
                      g.edges.(x)
    in
    aux g x 


let dijkstra (g : graph) s = 
    let inf = 999999 in
    let d = Array.make (Array.length g.vtx) inf in
    d.(s) <- 0;
    let l = ref [] in
    let traite = Array.make (Array.length g.vtx) false in
    let a_traiter = Queue.create() in
    Queue.add s a_traiter;
    flush_all();
    while not (Queue.is_empty a_traiter) do
	let x = Queue.take a_traiter in
	l:=x::!l;
	if not traite.(x) then List.iter (fun y -> if d.(x) + 1 < d.(y) then begin d.(y) <- d.(x) + 1; Queue.add y a_traiter end) g.edges.(x)
    done;
    d


let dijkstra_path (g: graph) s =
    let inf = 999999 in
    let d = Array.make (Array.length g.vtx) inf in
    let prev = Array.make (Array.length g.vtx) (-1) in
    d.(s) <- 0;
    let l = ref [] in
    let traite = Array.make (Array.length g.vtx) false in
    let a_traiter = Queue.create() in
    Queue.add s a_traiter;
    while not (Queue.is_empty a_traiter) do
	let x = Queue.take a_traiter in
	l:=x::!l;
	if not traite.(x) then List.iter (fun y -> if d.(x) + 1 < d.(y) then begin d.(y) <- d.(x) + 1; prev.(y) <- x;Queue.add y a_traiter end) g.edges.(x)
    done;
    d, prev

let floyd_warshall (g: graph) =
    let inf = 99999 in
	let n = Array.length g.vtx - 1 in
	let dist = Array.make_matrix (n+1) (n+1) 0 in
	let next = Array.make_matrix (n+1) (n+1) None in
	for i = 0 to n do
		for j = 0 to n do
			dist.(i).(j) <- if List.mem j g.edges.(i) then 1 else inf;
			next.(i).(j) <- Some j
		done
	done;
	for i = 0 to n do
		dist.(i).(i) <- 0;
		next.(i).(i) <- Some i;
	done;
	for k = 0 to n do
		for i = 0 to n do
			for j = 0 to n do
				if dist.(i).(k) + dist.(k).(j) 
					< dist.(i).(j)
					then begin dist.(i).(j) <- dist.(i).(k) + dist.(k).(j);
								next.(i).(j) <- (if next.(i).(k) = Some inf then None else next.(i).(k))
						end
			done
		done
	done;
	dist,next

let g, cache, revcache = graph_import "nodes.csv" "edges.csv"

let get_closest_node lat long =
    let dist x y = 
	(abs_float (x -. lat)) +. (abs_float (y -. long))
	in
    let min_dist = ref 99999999.9 in
    let id = ref "a" in
    for i = 1 to Array.length nodes - 1 do
	let d = dist (float_of_string nodes.(i).(2)) (float_of_string nodes.(i).(1)) in
	if d < !min_dist then begin
	    min_dist:=d;
	    id:=nodes.(i).(0);
	end
    done;
    !id


let build_path prev start finish =
    let rec aux prev x target l =
	if x = target then x::l 
	else aux prev prev.(x) target (x::l)
    in
    aux prev finish start []


let find_id nodes x =
    let check = ref true in
    for i = 1 to Array.length nodes - 1 do
	let id = Hashtbl.find cache (int_of_string nodes.(i).(0)) in
	if id = x then begin check:=false; Printf.printf "%s, %s\n" nodes.(i).(2) nodes.(i).(1) end;
	flush_all();
    done;
    if !check then Printf.printf "arrête artificielle \n"

let rec print_path l =
    match l with
    |[] -> ()
    |t::q -> find_id nodes t; print_path q

let x_start = 
    let lat, long = 45.62779770465385, 5.046790721302015 in
    Hashtbl.find cache (int_of_string (get_closest_node lat long))

let x_finish = 
    let lat, long = 45.62817367607451, 5.073554631566981 in
    Hashtbl.find cache (int_of_string (get_closest_node lat long))

let get_length x y revcache edges = 
    let n = Array.length edges in
    let t = ref "0.0" in 
    let x = string_of_int (Hashtbl.find revcache x) in
    let y = string_of_int (Hashtbl.find revcache y) in
    for i = 1 to n - 1 do
	if (edges.(i).(1) = x || edges.(i).(1) = y) &&
	    (edges.(i).(2) = x || edges.(i).(2) = y)
	then t:=edges.(i).(3) 
    done;
    float_of_string !t
(*
let allonge_arete g (x,y) edges =
    let n = ref g.vtx in 
    let *)

let invcache = 
    let c = Hashtbl.create 2000 in
    Hashtbl.iter 
    (fun a b -> Hashtbl.add c b a) 
    cache;
    c
let d, prev = dijkstra_path g x_start
(*let dist,next = floyd_warshall g *)
