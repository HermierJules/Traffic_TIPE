type case = Empty | Voiture of int * int | Block

type road = case array

let accelerate l vmax =
	for i = 0 to Array.length l -1 do
		match l.(i) with 
		|Empty -> ()
                |Block -> ()
		|Voiture (a,d) -> if a < vmax then l.(i) <- Voiture (a+1,d)
	done


	let collision l n v =
                let newv = ref v in
                let check = ref true in 
                for i = n+1 to n+v do
                        if i < Array.length l then
                        match l.(i) with
                        |Empty -> ()
                        |Voiture _ -> if !check then begin newv:= i-n-1;
                                                          check:= false end
                        |Block ->     if !check then begin newv:= i-n-1;
                                                          check:= false end
                done;
                !newv





let deccelerate l = 
for i = 0 to Array.length l - 1 do 
	match l.(i) with
	|Empty -> ()
        |Block -> ()
	|Voiture (a,d) -> l.(i) <- Voiture (collision l i a, d)
done


let randomizer l p =
	for i = 0 to Array.length l-1 do 
	match l.(i) with
	|Empty -> ()
        |Block -> ()
	|Voiture (a,d) -> if a > 0 && Random.int 100 < p then l.(i) <- Voiture ((a-1),d)
done

let mvt l buf= 
    let out = ref [] in
	for i = Array.length l - 1 downto 0 do
		match l.(i) with
		|Empty -> ()
         |Block -> ()
	|Voiture (a, d) -> if (i+a) < Array.length l then (l.(i+a) <- Voiture (a,d); buf) else Voiture (a,d)::buf; if a != 0 then l.(i) <- Empty
done

let simulate road buf =
	(* Definitions*)
	let vmax = 5 in
	let proba = 50 in 
	(*fin des defs*)
	accelerate road vmax;
        deccelerate road;
	randomizer road proba;
	mvt road buf 



let dijkstra g s =
	let rec get_weight y l =
		match l with
		|(y,w)::q -> (int_of_float w) + 1
		|_::q -> get_weight y q 
		|_ -> failwith "not an edge"
	in
	let inf = max_int in
	let d = Array.make (Array.length g.vtx) inf in
	d.(s) <- 0;
	let l = ref [] in
	let traite = Array.make (Array.length g) false in
	let a_traiter = Queue.create() in
	Queue.add s a_traiter;
	while not (Queue.is_empty a_traiter) do
		let x = Queue.take a_traiter in
		l:=x::!l;
		if not traite.(x) then List.iter (fun y,w -> let n = (int_of_float w) + 1 in if d.(x) + n < d.(y) then begin d.(y) <- d.(x) + n; Queue.add y a_traiter end) g.(x);
	done;
	d



let make_road n = 
    Array.make n Empty
