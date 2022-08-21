type voiture = {mutable vitesse: int; mutable destination : int; mutable time: int}
type state = Empty | Full of voiture
type graph = {
	vtx : state array;
	edges : int array array;
}
let inf = 99999
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
								next.(i).(j) <- next.(i).(k)
						end
			done
		done
	done;
	dist

let mvt g = 
	let next = ref 0 in
	for i = 0 to Array.length g.vtx - 1 do
		if g.vtx.(i) <> Empty 
			then begin
				next:=next_pos g g.vtx.(i);
				if g.vtx.(!next) <> Empty then 
					g.gtx.(i) <- (match g.gtx.(i) with
						|Full v -> Full {vitesse = v.vitesse - 1,
						destination = v.destination} 
						|_ -> failwith "erreur mvt")
					else begin
						g.gtx.(i) 
						g.gtx.(i) <- Empty;
						
				
	done

let _ = ()
