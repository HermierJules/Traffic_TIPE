type case = Empty | Voiture of int * int | Block
type bufgraph = case list array
type road = case array array



let road_copy (r : road) : road=
	let nl = Array.length r in
	let l = Array.make nl [||] in
	for i = 0 to nl - 1 do
		l.(i) <- Array.copy r.(i) 
	done;
	l

let get_road gr x y =
	let r, _ = Hashtbl.find gr (x,y) in r
let check_safety r lane i =
	if i = 0 then true else begin
	let c =	ref 1 in
	let check = ref true in
	let okay = ref true in
	while !check  do
		if i - !c >= 0 then 
		match r.(lane).(i - !c) with
		|Empty -> incr c
		|Block -> incr c
		|Voiture (v,_) -> if v > !c then (check:=false; okay:=false)
					else check:=false
		else check:=false
	done; 
	!okay end

let collision l n v lane =
	let newv = ref v in
	let check = ref true in 
	for i = n+1 to n+v do
		if i < Array.length l.(0) then
		match l.(lane).(i) with
		|Empty -> ()
		|Voiture _ -> if !check then begin newv:= i-n-1;
						  check:= false end
		|Block ->     if !check then begin newv:= i-n-1;
						  check:= false end
	done;
	!newv

let new_acc r i lane =
	match r.(lane).(i) with
	|Empty -> failwith "not car"
	|Block -> failwith "not car"
	|Voiture (v,_) -> if lane <> 0 && lane <> Array.length r - 1 then begin
		let m = collision r i v lane in
		let m' = collision r i v (lane-1) in
		let m''=  collision r i v (lane+1) in
		if m' > m && check_safety r (lane - 1) i
			then if m'' > m' && check_safety r (lane + 1) i
			then (lane+1,m'')
			else (lane-1,m')
		else if m'' > m && check_safety r (lane + 1) i then (lane+1,m'')
		else (lane,m)
	end
		else if lane = 0 then begin
			let m = collision r i v lane in
			if Array.length r = 1 then (lane,m) else
			let m' = collision r i v (lane + 1) in 
			if m' > m && check_safety r (lane + 1) i then (lane+1,m') else (lane, m) end
		else begin
			let m = collision r i v lane in
			let m' = collision r i v (lane - 1)  in
			if m' > m && check_safety r (lane - 1) i then (lane - 1, m') else (lane, m) end



let accelerate l vmax =
	for lane = 0 to Array.length l -1 do
		for i = 0 to Array.length l.(0) - 1 do
		match l.(lane).(i) with 
		|Empty -> ()
                |Block -> ()
		|Voiture (a,d) -> if a < vmax then l.(lane).(i) <- Voiture (a+1,d)
		done;
	done







let deccelerate l = 
	for lane = 0 to Array.length l - 1 do
		for i = 0 to Array.length l.(0) - 1 do 
			match l.(lane).(i) with
			|Empty -> ()
			|Block -> ()
			|Voiture (a,d) -> l.(lane).(i) <- Voiture (collision l i a lane, d)
	done;
done


let randomizer l p =
	for lane = 0 to Array.length l-1 do 
		for i = 0 to Array.length l.(0) - 1 do
			match l.(lane).(i) with
			|Empty -> ()
			|Block -> ()
			|Voiture (a,d) -> if a > 0 && Random.int 100 < p then l.(lane).(i) <- Voiture ((a-1),d)
	done;
done 


let find_entry_road g gr =
	let l = ref [] in
	for x = 0 to Array.length g - 1 do
		if List.length g.(x) = 1 then 
			match g.(x) with
			|(y,_,_)::_ -> l:= ((x,y),get_road gr x y)::!l
			|_ -> failwith "???"
	done;
	!l

let find_exit_road g =
	let l = ref [] in
	for i = 0 to Array.length g - 1 do
		let n = ref 0 in
		for j = 0 to Array.length g - 1 do
		List.iter (fun (y,_,_) -> 
			if y = i then 

				incr n) g.(j)
		done;
		if !n = 1 then
		l:= i::!l 
	done;
Array.of_list	!l



let mvt r buf= 
    let l = road_copy r in
    let out = ref buf in
    for lane = Array.length l - 1 downto 0 do
	for i = Array.length l.(0) - 1 downto 0 do
		match l.(lane).(i) with
		|Empty -> ()
         |Block -> ()
	|Voiture (_, d) ->
		let (new_l,a) = new_acc l i lane in

		if a <> 0 then begin r.(lane).(i) <- Empty; if (i+a) < Array.length l.(0)
					then r.(new_l).(i+a) <- Voiture (a,d)  
					else  (out:=Voiture (a,d)::!out) 
				end
	done;
    done;
	!out




let dijkstra g s =
	let rec get_weight l y =
		match l with
		|(x,w,_)::q -> if x = y then (int_of_float w) + 1
				else get_weight q y 
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
		if not traite.(x) then List.iter (fun (y,_,_) -> if d.(x) + (get_weight g.(x) y) < d.(y) then begin d.(y) <- d.(x) + (get_weight g.(x) y); prev.(y) <- x;Queue.add y a_traiter end) g.(x)
    done;
    d, prev

let build_path prev start finish = 
	let rec aux prev x target l =
		if x = target then x::l
		else aux prev prev.(x) target (x::l)
	in
	aux prev finish start []

let next_node g i d =
	if i = d then None else begin
	let _, prev = dijkstra g i in
	if prev.(d) = -1 then None else
	let p = build_path prev i d in
	let p = List.tl p in
	if p = [] then None else Some (List.hd p) 
	end

let put_char gr v id = 
	let r, _ = Hashtbl.find gr id in
	let check = ref true in
	for i = 0 to Array.length r - 1 do
		match r.(i).(0) with
		|Empty -> if !check then (r.(i).(0) <- v; check:=false)
		|_ -> ()
	done;
	not (!check)

let make_road n l = 
    Array.make_matrix l n Empty

let make_road_graph g =
    let road_graph = Hashtbl.create 50 in
    Array.iteri (fun x l -> 
        List.iter 
        (fun (y,w,lane) ->
            let n = (int_of_float w) + 1 in
            Hashtbl.add road_graph (x,y) ((make_road n lane), []))
        l)
    g;
    road_graph

let redistribuate_flux g gr =
	let rec aux l i = 
		match l with
		|[] -> []
		|Voiture (a,d)::q ->  begin
				match next_node g i d with
				|None -> aux q i
				|Some p -> if put_char gr (Voiture (a,d)) (i,p) then aux q i else Voiture(a,d)::aux q i end

		|_ -> failwith "not a car"
	in
	for i = 0 to Array.length g - 1 do
		List.iter (fun (d,_,_) -> 
			let (r,l) = Hashtbl.find gr (i,d) in
			let new_l = aux l d
			in Hashtbl.replace gr (i,d) (r,new_l))
		g.(i)
	done



let get_density g gr =
	let n = ref 0 in
	let nb = ref 0 in
	for x = 0 to Array.length g - 1 do
		List.iter (fun (y,_,_) ->
			let r = get_road gr x y in
			Array.iter (fun t ->
				Array.iter(fun v ->
					match v with
					|Voiture _ -> incr n; incr nb
					|_ -> incr n) t) r) g.(x)
	done;
	float_of_int !nb /. float_of_int !n

let make_density_graph g = 
    let road_graph = Hashtbl.create 50 in
    Array.iteri (fun x l -> 
        List.iter 
        (fun (y,_,_) ->
            Hashtbl.add road_graph (x,y) [])
        l)
    g;
    road_graph
	

let get_road_density gr x y =
	let r = get_road gr x y in
	let nb = ref 0 in
	let n = (Array.length r) * (Array.length r.(0)) in
	Array.iter (fun t -> 
		Array.iter (fun v -> 
			match v with
			|Voiture _ -> incr nb 
			|_ -> ()) t) r;
	float_of_int !nb /. float_of_int n

let update_density dr d g gr =
	(*
	 dr: density hashtbl
	 d: average density list
	 *)
	for x = 0 to Array.length g - 1 do
		List.iter (fun (y,_,_) ->
		let l = Hashtbl.find dr (x,y) in
		let den = (get_road_density gr x y) in
		Hashtbl.add dr (x,y) (den::l)) g.(x)
	done;
	let comp_den = get_density g gr in
	d:=comp_den::!d
		

let get_exits g i exit =
	let l = ref [] in
	let _, prev = dijkstra g i in
	for j = 0 to Array.length exit - 1 do
		if prev.(exit.(j)) <> (-1) then 
			l:=j::!l
	done;
	(List.length !l, Array.of_list !l)


let populate entry exit vinit = 
	let rec aux l = 
		match l with
		|[] -> ()
		|(id,r)::q ->
		for i = 0 to Array.length r - 1 do
			if Random.int 2 = 1 then
				if r.(i).(0) = Empty then
					begin
					let nexit, exits = Hashtbl.find exit id in
					let dest = Random.int nexit in
					r.(i).(0) <- Voiture (vinit, exits.(dest))
					end
		done; aux q
	in
	aux entry
	
	
let get_entry_exit g gr =
	let l = find_entry_road g gr in
	let c = Hashtbl.create 50 in
	let exit_list = find_exit_road g in
	let rec aux l = 
		match l with
		|((x,y),_)::q -> Hashtbl.add c (x,y) (get_exits g y exit_list); aux q
		|[] -> ()
	in
	aux l;
	l,c


let simulate road buf =
	(* Definitions*)
	let vmax = 5 in
	let proba = 30 in 
	(*fin des defs*)
	accelerate road vmax;
	randomizer road proba;
	let buf = mvt road buf in
	buf

let simulate_full g gr dr d = 
	for i = 0 to Array.length g - 1 do
		List.iter (fun (y,_,_) -> 
			let (r,b) = Hashtbl.find gr (i,y) in
			let buf = simulate r b in
			Hashtbl.replace gr (i,y) (r,buf))
		g.(i);
	done;
	redistribuate_flux g gr;
	update_density dr d g gr


let find_single_car g gr =
	for i = 0 to Array.length g - 1 do
		List.iter (fun (y,_,_) -> 
			let (r,_) = Hashtbl.find gr (i,y) in
			for x = 0 to Array.length r - 1 do
				for j = 0 to Array.length r.(0) - 1 do
					if r.(x).(j) <> Empty then Printf.printf "the car is in %d -> %d in %d, %d" i y x j done;
			done)
		g.(i);
	done

let draw_road r =
	for i = 0 to Array.length r - 1 do
		for j = 0 to Array.length r.(0) - 1 do
			match r.(i).(j) with
			|Voiture _ -> Printf.printf "#"
			|_ -> Printf.printf "."
		done;
		print_newline();
	done


let l = 
	(let l = make_road 9 5 in
	l.(2).(0) <- Voiture (5,99);
	l.(2).(2) <- Voiture (0,99);
	l.(1).(2) <- Voiture(0,99);
	l.(0).(2) <- Voiture(0,99);
	l.(3).(2) <- Voiture(0,99);
	l.(4).(2) <- Voiture(0,99);
	l)
