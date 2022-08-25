type voiture = {mutable vitesse: int; mutable destination : int; mutable time: int; mutable has_moved: bool}
type state = Empty | Full of voiture
type graph = {
	vtx : state array;
	edges : int array array;
	discovered : bool array;
	processed : bool array;
}
let inf = 99999
let initialize g =
	for i = 0 to Array.length g.vtx -1 do
		g.discovered.(i) <- false;
		g.processed.(i) <- false;
	done

let test_graph =
	let n = 50 in
	let vtx = Array.make n Empty in
	let edges = Array.make_matrix n n inf in
	let discovered = Array.make n false in
	let processed = Array.make n false in 
	for i = 0 to n-2 do 
		edges.(i).(i+1) <- 1
	done; 
	{vtx = vtx; edges = edges; discovered = discovered; processed = processed}
let floyd_warshall g =
	let n = Array.length g.vtx - 1 in
	let dist = Array.make_matrix (n+1) (n+1) 0 in
	let next = Array.make_matrix (n+1) (n+1) None in
	for i = 0 to n do
		for j = 0 to n do
			dist.(i).(j) <- g.edges.(i).(j);
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

let dist,next = floyd_warshall test_graph

let rec dfs g x f =	(* all f are f g x*)
	let n = Array.length g.vtx - 1 in  					
	g.discovered.(x) <- true;
	if g.vtx.(x) <> Empty then f g x;
	(*process early*)
	for y = 0 to n do
		if g.edges.(x).(y) <> inf && not(g.discovered.(y)) then begin
			g.discovered.(y) <- true;
			dfs g y f;
		end
	done

let move g = 
	let rec move_aux g x v count = 
		if count = 0 then g.vtx.(x) <- Full (v.has_moved <- true; v) else
		if  x = v.destination then () else	
		move_aux g (match next.(x).(v.destination) with
					|None -> failwith "pas de chemin, move"
					|Some x -> x) v (count-1) 
	in
	for i = 0 to Array.length g.vtx - 1 do
		match g.vtx.(i) with
		|Empty -> ()
		|Full v -> if not(v.has_moved) then begin g.vtx.(i) <- Empty; move_aux g i v v.vitesse end
	done


let normalize g x = 
	let deccelerate g x v= 
		let rec deccelerate_aux g x dest (count:int) =
			if x = dest || count = 0 then 0 else
			match g.vtx.(x) with
			|Empty -> deccelerate_aux g x dest (count-1)(*deccelerate_aux g next.(x).(dest) dest (count-1)*)
			|Full v -> count
		in
		v.vitesse <- deccelerate_aux g x v.destination v.vitesse
	 in
	let random g x = () in
	match g.vtx.(x) with
	|Empty -> ()
	|Full v -> v.has_moved <- false; 
				v.time <- v.time + 1; 
				v.vitesse <- v.vitesse + 1;
				deccelerate g x v; 
				random g x;



