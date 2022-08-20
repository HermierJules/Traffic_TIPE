type info = (int * int) list 
type voiture = {mutable vitesse : int; mutable itineraire : (int * int) list; mutable trouve : bool}
type state = Pleine of voiture | Empty  
type case = Route of (state * (int*int)) |Choice of state |Block 

let finale_grille_gen ()  = let l =Array.make_matrix 200 200 Block in
			for y = 1 to 198 do
				l.(y).(102) <- Route (Empty, (102,y-1))
			done;
			l.(131).(102) <- Choice Empty;
			l.(119).(102) <- Choice Empty;
			l.(89).(102) <- Choice Empty;
			l.(41).(102) <- Choice Empty;
			for x = 1 to 101 do 
				l.(89).(x) <- Route(Empty, (x-1, 89))
			done;
			for x = 103 to 198 do
				l.(89).(x) <- Route(Empty, (x+1,89))
			done;
			for x = 103 to 151 do 
				l.(41).(x) <- Route(Empty ,(x+1,41))
			done;
			l.(41).(151) <- Route(Empty ,(151,42));
			for y = 42 to 88 do 
				l.(y).(151) <- Route(Empty, (151,y+1))
			done;
			for y = 90 to 131 do 
				l.(y).(44) <- Route(Empty, (44,y-1))
			done;
			for x = 45 to 101 do
				l.(131).(x) <- Route(Empty, (x-1,131))
			done;
			for x = 43 downto 1 do
				l.(120).(x) <- Route(Empty, (x-1, 131))
			done;
			for y = 90 to 118 do
				l.(y).(158) <- Route(Empty, (158, y+1))
			done;
			for x = 159 to 199 do 
				l.(118).(x) <- Route(Empty, (x+1, 118))
			done;
			for x = 103 to 129 do 
				l.(119).(x) <- Route(Empty, (x+1,119))
			done;
			for y = 119 to 140 do
				l.(y).(130) <- Route(Empty, (130, y+1))
			done;
			for x = 130 to 198 do
				l.(140).(x) <- Route(Empty, (x+1, 140))
			done;		
			l.(120).(44) <- Choice Empty;
			l.(89).(158) <- Choice Empty;
			l

let v_gen () =
	let aux_vgen () =  
	let choice = Random.int 70 in
	if choice < 10 then [(101,131);(5,120)] (*left left*)
	else if choice < 15 then [(102,130);(103,119)](*up right*)
	else if choice < 35 then [(102,130);(102,118);(102,88)](*up up up*)
	else if choice < 40 then [(102,130);(102,118);(103,89);(158,90)](*up up right*)
	else if choice < 50 then [(102,130);(102,118);(103,89);(158,90)](*up up right right*)
	else if choice < 55 then [(102,130);(102,118);(103,89)](*up up right straight*)
	else if choice < 70 then [(102,130);(102,118);(101,89)] (*up up left*)
	else [(101,131);(44,119)] (*left up*)
	in {vitesse = 3; itineraire = aux_vgen(); trouve = false}

let grille_gen() = 
	let l =Array.make_matrix 200 200 Block in
	for x = 2 to 197 do 
		l.(100).(x) <- Route(Empty, (x-1,100))
	done;
	l
let is_next_empty grille (x,y) = 
	match grille.(y).(x) with
	|Block -> false
	|_ -> true


let rec move_aux (grille: case array array) (v: voiture) ((x,y): int*int) (inc:int) : unit=
	let choice_handler v =
		match v.itineraire with 
		|t::q -> v.itineraire <- q; t
		|_ -> (0,0) (* A CORRIGER*)
	in
		if inc = 0 
			then match grille.(y).(x) with
			|Route (_,next) -> begin v.trouve <- true; grille.(y).(x) <-Route (Pleine v, next) end
			|Choice _ -> begin v.trouve <- true; grille.(y).(x) <- Choice (Pleine v) end
			|_ -> ()
		else
			match grille.(y).(x) with
			|Route (_,next) -> move_aux grille v next (inc-1)
			|Choice _ -> (*possible bug car v n'est pas mis à jour, normalement le mutable est une référence*)
						move_aux grille v (choice_handler v) (inc-1)	
			|_ -> ()		

let move (grille: case array array)  s (x,y) : unit=
		match s with 
		|Pleine v ->  move_aux grille v (x,y) v.vitesse  
		|_ -> ()

let unmark grille = 
	let xmax = Array.length grille.(0) in
	let ymax = Array.length grille in 
	for y = ymax-1 downto 0 do
		for x = xmax-1 downto 0 do
			match grille.(y).(x) with
			|Route(Pleine v,_) -> v.trouve <- false
			|_ -> ()
		done
	done
	
let mvt grille =
	let xmax = Array.length grille.(0) in
	let ymax = Array.length grille in 
	for y = ymax-1 downto 0 do
		for x = xmax-1 downto 0 do
			match grille.(y).(x) with
			|Route (s,next) when s <> Empty  && ( match s with	
												|Pleine v -> not (v.trouve)
												|Empty -> false
							)-> 
								begin 
								grille.(y).(x) <- Route (Empty,next);
								move grille s (x,y)  
								end
			|Choice s -> begin 
							grille.(y).(x) <- Choice Empty; 
							move grille s (x,y)
						 end 
			|Block -> ()
			|_ -> ()
		done
	done

let accelerate l vmax =
	let xmax = Array.length l.(0) in
	let ymax = Array.length l in 
	for y = ymax-1 downto 0 do
		for x = xmax-1 downto 0 do
			match l.(y).(x) with
			|Route (Pleine v,_) -> if v.vitesse < vmax then v.vitesse <- v.vitesse + 1
			|Choice Pleine v -> if v.vitesse < vmax then v.vitesse <- v.vitesse + 1
			|_ -> ()
		done
	done	
	
let rec newv grille v (x,y) i =
	if x = v then v else 
	match grille.(y).(x) with
	|Route (Pleine _,_) -> i-1
	|Route (_, next) -> newv grille v next (i+1)
	|_ -> 5

let deccelerate l vmax =
	let xmax = Array.length l.(0) in
	let ymax = Array.length l in 
	for y = ymax-1 downto 0 do
		for x = xmax-1 downto 0 do
			match l.(y).(x) with
			|Route (Pleine v, next) -> v.vitesse <- if (newv l v .vitesse next 1) > vmax then vmax else  newv l v .vitesse next 1
			|Choice (Pleine v) -> begin match v.itineraire with
								|t::_ -> v.vitesse <- if (newv l v .vitesse t 1) > vmax then vmax else  newv l v .vitesse t 1
								|[] -> Printf.printf "problème en %d %d" x y ;()(*failwith "itineraire faux %d %d"*)  end
			|_ -> ()
		done
	done
let randomize l p =
	let xmax = Array.length l.(0) in
	let ymax = Array.length l in 
	for y = ymax-1 downto 0 do
		for x = xmax-1 downto 0 do
			match l.(y).(x) with
			|Route (Pleine v, _) -> if v.vitesse <> 0 && Random.int 99 < p
								then v.vitesse <- v.vitesse-1
			|Choice Pleine v -> if v.vitesse <> 0 && Random.int 99 < p
								then v.vitesse <- v.vitesse-1
			|_ -> ()
	done
done

let heatmap_inc l h = 
	let xmax = Array.length l.(0) in
	let ymax = Array.length l in 
	for y = ymax-1 downto 0 do
		for x = xmax-1 downto 0 do
		match l.(y).(x) with
		|Route(Pleine _, _) -> h.(y).(x) <- h.(y).(x) + 1
		|Choice Pleine _ -> h.(y).(x) <- h.(y).(x) + 1
		|_ -> ()
	done
done

let tout grille heatmap = 
	accelerate grille 5;
	deccelerate grille 5;
	randomize grille 30;
	mvt grille;
	unmark grille;
	heatmap_inc grille heatmap
	
	
(*let fill grille =
let heatmap_inc =
let heatmap_gen = 

*)

let draw_case l (x,y) =
	let xmax = Array.length l.(0) in
    let ymax = Array.length l in	
    let xstep = 800 / xmax in 
    let ystep = 800 / ymax in
    let open Raylib in

    let emptycolor = Color.create 1 25 54 255 in
    let redcolor = Color.create 237 37 78 255 in
    let bluecolor = Color.create 194 234 189 255 in 
	let blackcolor = Color.create 194 54 0 255	in
	let greencolor = Color.create 0 255 0 255 in
    draw_rectangle (x*xstep) (y*ystep) (xstep) (ystep) 
	(match l.(y).(x) with
	|Route (Pleine v, _) -> if v.vitesse >3 then redcolor else bluecolor
	|Choice Pleine v -> if v.vitesse >3 then redcolor else bluecolor
	|Block -> blackcolor
	|Choice Empty -> greencolor
	|_ -> emptycolor)

let draw_tableau l =
	let xmax = Array.length l.(0) in
	let ymax = Array.length l in

	for y = 0 to ymax-1 do
		for x = 0 to xmax-1  do
		    draw_case l (x,y)
        done 
    done


let car_add g (x,y) = 
match g.(y).(x) with
|Route (_,next) -> g.(y).(x) <- Route(Pleine (v_gen()),next)
|_ -> ()

let heatmap_gen () = 
	Array.make_matrix 200 200 0


let get_max l =
	let max = ref 0 in 
	let xmax = Array.length l.(0) in
	let ymax = Array.length l in 
	for y = ymax-1 downto 0 do
		for x = xmax-1 downto 0 do
		match l.(y).(x) with
		|a when a > !max -> max:=a
		|_ -> ()
		done
	done;
	!max
let draw_heatmap l =
	let max = get_max l in
	let open Raylib in
	let xmax = Array.length l.(0) in
	let ymax = Array.length l in 
	draw_rectangle 800 0 800 800 (Color.create 201 228 231 255); 
	for y = ymax-1 downto 0 do
		for x = xmax-1 downto 0 do
		draw_circle (x*4 + 800) (y*4) ((float(l.(y).(x)) /. float(max)) *. 100.) (Color.create 180 160 228 255)
		done
	done
	
let setup () =
	Raylib.init_window 1600 800 "Nasch model";
	Raylib.set_target_fps 110


let rec loop grille heatmap=
	match Raylib.window_should_close () with
	|true -> Raylib.close_window()
	|false ->
	car_add grille (102,198);
	tout grille heatmap;
	let open Raylib in
	begin_drawing();
	draw_tableau grille;
	draw_heatmap heatmap;
	end_drawing();
	loop grille heatmap
let _ =
	setup ();
	loop (finale_grille_gen()) (heatmap_gen())
