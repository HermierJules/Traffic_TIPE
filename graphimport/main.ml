type voiture = {mutable vitesse: int; mutable destination : int; mutable time: int; mutable has_moved: bool}
type state = Empty | Full of voiture
type graph = {
	vtx: state array; 
	edges : int array array;
	discovered : bool array;
	processed : bool array;
}

type graph_list = {
	mutable vtx : int list;
	mutable edges : int * int list list;
}

let graph_import file = 
	let cache = Hastbl.create 2048 in
	let t = Csv.to_array (Csv.input_all(Csv.from_string (open_in file))) in
	for i = 0 to Array.length t - 1 do
		

let normalize_edge (g: graph_list) (id: int) (target: int) (length: float) (back: bool) = 
	let n = (int_of_float length)/5 in
	let rec create_edges n l = 
		if n = 0 then l else 
		match l with
		|[] -> failwith "fréro tu es cringe"
		|t::q  -> create_edges (n-1) q
	let l = List.init n 
 
let _ = ()
