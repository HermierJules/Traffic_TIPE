type voiture = {mutable vitesse: int; mutable destination : int; mutable time: int; mutable has_moved: bool}

type state = Empty | Full of voiture
type w_graph = {
	vtx: state array; 
	edges : (int * int) list array;
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

let nodes = Csv.to_array (Csv.input_all(Csv.of_channel (open_in "nodes.csv")))
let edges = Csv.to_array (Csv.input_all(Csv.of_channel (open_in "edges.csv"))) 

let build_cache nodes = 
	let cache = Hashtbl.create 420 in
	for i = 1 to Array.length nodes - 1 do
		Hashtbl.add cache  (nodes.(i).(0)) (i-1)
	done;
	cache

let build_cache_reverse nodes = 
	let cache = Hashtbl.create 420 in
	for i = 1 to Array.length nodes - 1 do
		Hashtbl.add cache  (i-1) (int_of_string nodes.(i).(0)) 
	done;
	cache
let add_arrete edges s d w front back =
	let aux edges x y w = 
		edges.(x) <- (y,w)::edges.(x)
	in
	match s, d with
	|Some x, Some y -> begin
	if front then aux edges x y w;
	if back then aux edges y x w end
	|_ -> failwith "" 

let gimport nodes edges = 
	let process x y =
		match x,y with 
		|Some x,Some y -> true
		|_ -> false
	in
	let vtx = Array.make (Array.length nodes - 1) Empty in
	let e = Array.make (Array.length nodes - 1) [] in
	let discovered = Array.make (Array.length nodes - 1) false in
	let processed = Array.make (Array.length nodes - 1) false in
	let cache = build_cache nodes in
	for i = 1 to Array.length edges - 1 do
		let front = int_of_string edges.(i).(5) > 0 in
		let back = int_of_string edges.(i).(6) > 0 in
		let x = Hashtbl.find_opt cache edges.(i).(1) in
		let y = Hashtbl.find_opt cache edges.(i).(2) in
		let w = int_of_float (float_of_string edges.(i).(3)) in
		if process x y then 
		add_arrete e x y w front back
	done;
	{vtx = vtx; edges = e; discovered = discovered; processed = processed}
		
let find_truc s t =
	let c = ref (-1) in
for i = 0 to Array.length t - 1 do
	if Array.mem s t.(i) then c:=i
done;
!c


let dijkstra_path (g: w_graph) s =
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
	if not traite.(x) then List.iter (fun (y,w) -> if d.(x) + w < d.(y) then begin d.(y) <- d.(x) + w; prev.(y) <- x;Queue.add y a_traiter end) g.edges.(x)
    done;
    d, prev

let g = gimport nodes edges

let build_path prev start finish =
    let rec aux prev x target l =
	if x = target then x::l 
	else aux prev prev.(x) target (x::l)
    in
    aux prev finish start []

let rec print_path l =
    let rec aux l  = 
    match l with
	|[] -> print_newline(); ()
	|t::q -> Printf.printf "%s, %s \n " nodes.(t+1).(2) nodes.(t+1).(1); aux q
    in
    aux l


