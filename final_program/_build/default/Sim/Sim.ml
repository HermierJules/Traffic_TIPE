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

let mvt l = 
    let out = ref [] in
	for i = Array.length l - 1 downto 0 do
		match l.(i) with
		|Empty -> ()
         |Block -> ()
        |Voiture (a, d) -> if (i+a) < Array.length l then l.(i+a) <- Voiture (a,d); if a != 0 then l.(i) <- Empty
done

let simulate road =
	(* Definitions*)
	let vmax = 5 in
	let proba = 50 in 
	(*fin des defs*)
	accelerate road vmax;
        deccelerate road;
	randomizer road proba;
	mvt road


let make_road n = 
    Array.make n Empty
