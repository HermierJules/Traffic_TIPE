open Graphics
type case = Empty | Voiture of int | Block


let accelerate l vmax =
	for i = 0 to Array.length l -1 do
		match l.(i) with 
		|Empty -> ()
                |Block -> ()
		|Voiture a -> if a < vmax then l.(i) <- Voiture (a+1)
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
	|Voiture a -> l.(i) <- Voiture (collision l i a)
done


let randomizer l p =
	for i = 0 to Array.length l-1 do 
	match l.(i) with
	|Empty -> ()
        |Block -> ()
	|Voiture a -> if a > 0 && Random.int 100 < p then l.(i) <- Voiture (a-1)
done

let mvt l = 
	for i = Array.length l - 1 downto 0 do
		match l.(i) with
		|Empty -> ()
                |Block -> ()
		|Voiture a -> if (i+a) < Array.length l then l.(i+a) <- Voiture a; if a != 0 then  l.(i) <- Empty
done

let tout grille =
	(* Definitions*)
	let vmax = 5 in
	let proba = 50 in 
	(*fin des defs*)
	accelerate grille vmax;
        deccelerate grille;
	randomizer grille proba;
	mvt grille
 
let fill grille =
        grille.(0) <- Voiture 3;
         grille.(2) <- Voiture 2;
         grille.(5) <- Voiture 1

let draw_case x y l =
    let xmax = Array.length l.(0) in
    let ymax = Array.length l in	
    let xstep = 1280 / xmax in 
    let ystep = 720 / ymax in
    let open Raylib in

    let emptycolor = Color.create 1 25 54 255 in
    let redcolor = Color.create 237 37 78 255 in
    let bluecolor = Color.create 194 234 189 255 in 	
    draw_rectangle (x*xstep) (y*ystep) (xstep) (ystep) 
        (match l.(y).(x) with
            |Empty ->emptycolor
            |Voiture x when x > 3 -> redcolor
            |_ -> bluecolor)
     

let draw_tableau l =
	let xmax = Array.length l.(0) in
	let ymax = Array.length l in

	for y = 0 to ymax-1 do
		for x = 0 to xmax-1  do
		    draw_case x y l
        done 
    done



let block l t case startmod  =
        if t mod 100 = 0 then
                if l.(case) = Block 
                then l.(case) <- Empty
                else
                     if t mod startmod = 0 then 
                             l.(case) <- Block
  
let setup () = 
	Raylib.init_window 1280 720 "biham model";
	Raylib.set_target_fps 500

let rec loop grille = 
		match Raylib.window_should_close () with
		|true -> Raylib.close_window ()
		|false ->
        let open Raylib in
		        begin_drawing ();
                clear_background Color.raywhite;
                tout grille;
                draw_tableau grille;
                end_drawing();
				loop grille
  let _ = 
	let grille = Array.make 16000 Empty in
        fill grille;
	setup();
	loop grille;
