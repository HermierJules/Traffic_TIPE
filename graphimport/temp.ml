type graph_list = {
	mutable vtx : int list;
	mutable edges : (int * int) list;
}

let graph_import_node file_node file_edges= 
	let vtx = ref [] in
	let og_id = Hashtbl.create 2048 in
	let ed_temp = ref [] in
	let x = ref 0 in 
	let y = ref 0 in
	let l = ref 0 in
	let node_nb = ref 0 in
	let nodes = Csv.to_array (Csv.input_all(Csv.of_channel (open_in file_node))) in
	let edges = Csv.to_array (Csv.input_all(Csv.of_channel (open_in file_edges))) in
	(*making the cache to make the end graph more manageable*)
	for i =Array.length  nodes -1  to 1  do
		Hashtbl.add og_id  nodes.(i).(0) (i-1) ;
		vtx:= int_of_string(nodes.(i).(0))::!vtx;
		node_nb:= !node_nb + 1; 
	done;
	for i = 1 to Array.length edges-1 do
		ed_temp:= [];
		x:= (match Hashtbl.find_opt og_id edges.(i).(1)  with
			 |Some x -> x
			 |None -> (-1));
		y:= (match Hashtbl.find_opt og_id edges.(i).(2)  with
			 |Some x ->  x
			 |None -> (-1));
		l:= ((int_of_float (float_of_string edges.(i).(3))) / 5) + 1;
		for j = 1 to !l do
			vtx:= !node_nb::!vtx;
			if !l <> 1 
				then if j <> 1 then ed_temp:= (!node_nb, !node_nb + 1):: !ed_temp
							   else ed_temp:= (!x, !node_nb + 1):: !ed_temp
				else if j <> 1 then ed_temp:= (!node_nb, !y) :: !ed_temp
							   else ed_temp:= (!x, !y)::!ed_temp;
				node_nb:=!node_nb + 1;
			
		done;
		(*if it's a double way road*)
		l:= ((int_of_float (float_of_string edges.(i).(3))) / 5) + 1;
		if int_of_string edges.(i).(6) <> 0 then begin
		for j = 1 to !l do
			vtx:= !node_nb::!vtx;
			if !l <> 1 
				then if j <> 1 then ed_temp:= (!node_nb, !node_nb + 1):: !ed_temp
							   else ed_temp:= (!x, !node_nb + 1):: !ed_temp
				else if j <> 1 then ed_temp:= (!node_nb, !x) :: !ed_temp
							   else ed_temp:= (!y, !x)::!ed_temp;
				node_nb:=!node_nb + 1;
			
		done;
		end

	done;
	{vtx = !vtx; edges = !ed_temp}
		


