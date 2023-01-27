type case = Empty | Blue | Red



let fill_grille p l =
	for y = 0 to Array.length l - 1 do
        for x = 0 to Array.length l.(0) - 1 do
            if Random.int 100 < p
            then l.(y).(x) <- if Random.int 2 = 1 then Blue else Red
        done
    done


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
            |Red -> redcolor
            |Blue -> bluecolor)
     
let draw_tableau l =
	let xmax = Array.length l.(0) in
	let ymax = Array.length l in

	for y = 0 to ymax-1 do
		for x = 0 to xmax-1  do
		    draw_case x y l
        done 
    done



let mvt l = 
	let xmax = Array.length l.(0) in
	let ymax = Array.length l in
	for y = ymax-1 downto 0 do	 
  		for x = xmax - 1 downto 0 do
			match l.(y).(x) with 
			|Empty -> ()
			|Red -> if x+1 = xmax then
				if l.(y).(0) != Empty then ()
						    else begin l.(y).(0) <- Red; 
							l.(y).(x) <- Empty   end
				
				else
				if l.(y).(x+1) <> Empty then ()
				else begin l.(y).(x+1) <- Red;l.(y).(x) <- Empty end
			|Blue -> if y+1 = ymax then
				if l.(0).(x) != Empty then ()
						    else begin l.(0).(x) <- Blue; 
							l.(y).(x) <- Empty end
				
				else
				if l.(y+1).(x) <> Empty then ()
				else begin l.(y+1).(x) <- Blue; l.(y).(x) <- Empty end
		
done
done


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
                mvt grille;
                draw_tableau grille;
                end_drawing();
                loop grille

let _ =
    let grille = Array.make_matrix (720/3) (1280/3) Empty in 
    fill_grille 33 grille;
    setup ();
    loop grille




