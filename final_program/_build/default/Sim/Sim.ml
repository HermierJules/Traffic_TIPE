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
    let out = ref buf in
	for i = Array.length l - 1 downto 0 do
		match l.(i) with
		|Empty -> ()
         |Block -> ()
	|Voiture (a, d) -> if a != 0 then l.(i) <- Empty; if (i+a) < Array.length l then begin l.(i+a) <- Voiture (a,d) end
						     else  (out:=Voiture (a,d)::!out)
	done;
	!out

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
	let rec get_weight l y =
		match l with
		|(y,w)::q -> (int_of_float w) + 1
		|_::q -> get_weight q s 
		|_ -> failwith "??"
	in
	let inf = max_int in
	let d = Array.make (Array.length g) inf in
	let prev = Array.make (Array.length g) (-1) in
	d.(s) <- 0;
	let l = ref [] in
	let traite = Array.make (Array.length g) false in
	let a_traiter = Queue.create() in
	Queue.add s a_traiter;
	while not (Queue.is_empty a_traiter) do
		let x = Queue.take a_traiter in
		l:=x::!l;
		if not traite.(x) then List.iter (fun (y,_) -> if d.(x) + (get_weight g.(x) y) < d.(y) then begin d.(y) <- d.(x) + (get_weight g.(x) y); prev.(y) <- x;Queue.add y a_traiter end) g.(x)
    done;
    d, prev

let build_path prev start finish = 
	let rec aux prev x target l =
		if x = target then x::l
		else aux prev prev.(x) target (x::l)
	in
	aux prev finish start []

let get_next_node g s d =
	if s = d then 1 else
	let _,prev = dijkstra g s in
	let l = build_path prev s d in
	List.nth l 1


let make_road n = 
    Array.make n Empty
