type voiture = {mutable vitesse: int; mutable destination : int; mutable time: int}
type state = Empty | Full of voiture
type graph = {
	vtx : state array;
	edges : (int*int option) list array;
}

let next_pos g s = 

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
