open Terrain
open Random
open Geometry
open Buildings

let _ = Random.self_init () ;;
(*
*)

let w = ref 250 ;;
let h = ref 180 ;;
let nb_rivers = ref 8 ;;

(* Analyse des arguments du programme. *)
let () =
	let set_w value = w := value in
	let set_h value = h := value in
	let set_nb_rivers value = nb_rivers := value in

	let speclist =
		[	("-w", Arg.Int set_w, "Sets width")
		;	("-h", Arg.Int set_h, "Sets height")
		;	("-r", Arg.Int set_nb_rivers, "Sets number of rivers")
		] in
	let usage_msg = "Orogue:" in
	Arg.parse speclist print_endline usage_msg

(* Parametres de la generation de la carte. *)
(* TODO: modifiables par des arguments galement ? *)
let p_heighmap =
	[ (1.0, 1.0 /. 64.0 *. 2.0,  1.0 /. 64.0 *. 2.0)
	; (0.5, 1.0 /. 64.0 *. 4.0,  1.0 /. 64.0 *. 4.0)
	; (0.2, 1.0 /. 64.0 *. 8.0,  1.0 /. 64.0 *. 8.0)
	] ;;
let water_level = -0.3 ;;
let rock_level = 0.4 ;;
let p_forests =	[ (1.0, 1.0 /. 64.0 *. 16.0, 1.0 /. 64.0 *. 16.0) ] ;;
let forests_level = -0.3 ;;

let pattern_width = 128 ;;
let pattern_height = 128 ;;

let heightmap = Noise.make_noise_2d_map w h p_heighmap pattern_width pattern_height ;;
let forests = Noise.make_noise_2d_map w h p_forests pattern_width pattern_height ;;

let add_river map =
	let delta = [ (0, 1) ; (0, -1) ; (1, 0) ; (-1, 0) ] in

	let step_up (z, (x, y)) (dx, dy) =
		let nx, ny = x + dx, y + dy in
		if nx < 0 || nx >= !w || y <= 0 || ny >= !h then (z, (x, y)) else
		let nz = heightmap nx ny in
		if nz > z then (nz, (nx, ny)) else
		(z, (x, y)) in

	let step_down (z, (x, y)) (dx, dy) =
		let nx, ny = x + dx, y + dy in
		if nx < 0 || nx >= !w || y <= 0 || ny >= !h then (z, (x, y)) else
		let nz = heightmap nx ny in
		if nz < z then (nz, (nx, ny)) else
		(z, (x, y)) in

	let rec draw_river = function
		| (x, y) :: path ->
			map.(x).(y) <- Water ;
			draw_river path
		| [] -> () in

	let rec get_path path x y =
		let direction = List.fold_left step_down (heightmap x y, (x, y)) delta in
		let x', y' = snd direction in
		if x' <> x || y' <> y then get_path ((x', y') :: path) x' y' else
		if map.(x).(y) = Water then path else [] in

	let rec locate_source x y =
		let source = List.fold_left step_up (heightmap x y, (x, y)) delta in
		let x', y' = snd source in
		if x' <> x || y' <> y then locate_source x' y' else
		x', y' in

	let x = Random.int !w in
	let y = Random.int !h in
	let x, y = locate_source x y in
	let path = get_path [] x y in
	draw_river path ;;

(* Sur la base d'un diagramme de Whittaker simplifie, generation de la carte. *)
let create_map () =
	let map = Array.make_matrix !w !h Water in
	for y = 0 to !h - 1 do
		for x = 0 to !w - 1 do
			let z = heightmap x y in
			let f = forests x y in
			if z < water_level then map.(x).(y) <- Water else
			if z > rock_level then map.(x).(y) <- Rock else
			if f < forests_level then map.(x).(y) <- Forest else
			map.(x).(y) <- Ground
		done
	done ;
	for _ = 0 to !nb_rivers do
		add_river map
	done ;
	map ;;

let build_road_on_map map r =
	let x1, y1 = int_of_float r.a.x, int_of_float r.a.y in
	let x2, y2 = int_of_float r.b.x, int_of_float r.b.y in
	(* Il faut que la route debute et finissent sur la terre, au moins (pas sur l'eau). *)
	if x1 >= 0 && y1 >= 0 && x1 < !w && y1 < !h && map.(x1).(y1) = Water then () else
	if x2 >= 0 && y2 >= 0 && x2 < !w && y2 < !h && map.(x2).(y2) = Water then () else
	if x1 < x2 then
		if y1 < y2 then
			for i = x1 to x2 do
				for j = y1 to y2 do
					if i >= 0 && j >= 0 && i < !w && j < !h then map.(i).(j) <- Road
				done
			done
		else
			for i = x1 to x2 do
				for j = y1 downto y2 do
					if i >= 0 && j >= 0 && i < !w && j < !h then map.(i).(j) <- Road
				done
			done
	else
		if y1 < y2 then
			for i = x1 downto x2 do
				for j = y1 to y2 do
					if i >= 0 && j >= 0 && i < !w && j < !h then map.(i).(j) <- Road
				done
			done
		else
			for i = x1 downto x2 do
				for j = y1 downto y2 do
					if i >= 0 && j >= 0 && i < !w && j < !h then map.(i).(j) <- Road
				done
			done ;
	map ;;

let create_city_buildings () =
	let map = create_map () in
	let roads = City.make_city !w !h in
	let map = List.fold_left build_road_on_map map roads in
	let buildings = make_buildings map !w !h roads in
	map, buildings ;;

let map, buildings = create_city_buildings () ;;

let () =
	(* Affichage de la carte. *)
	Printf.printf "%d %d %d\n" !w !h 2 ;
	for y = 0 to !h - 1 do
		for x = 0 to !w - 1 do
			(* TODO: on force une seule hauteur (plusieurs niveaux pour plus tard) ! *)
			Printf.printf "%d %d %d\n" x y (terrain_to_int map.(x).(y))
		done
	done ;
	(* Affichage des coordonnees des maisons. *)
	Printf.printf "%d\n" (List.length buildings) ;
	List.iter (fun (i, j, di, dj) -> Printf.printf "%d %d %d %d\n" i j di dj) buildings ;;

(*
DEBUG:
for y = 0 to !h - 1 do
	for x = 0 to !w - 1 do
		match map.(x).(y) with
			| Water -> Printf.printf "\027[48;5;19m\027[39m~"
			| Forest -> Printf.printf "\027[49m\027[\027[92m♣"
			| Rock -> Printf.printf "\027[49m\027[39m#"
			| Ground -> Printf.printf "\027[49m\027[39m "
			| Road -> Printf.printf "\027[48;2;108;108;0m\027[38;2;170;170;170m#"
			| Floor -> Printf.printf "\027[48;2;170;170;170m\027[38;2;255;255;255m."
			| Wall -> Printf.printf "\027[48;2;100;0;0m\027[38;2;255;255;255m#"
			| Door -> Printf.printf "\027[48;2;100;0;0m\027[38;2;255;255;255m+"
	done ;
	Printf.printf "\027[39m\027[49m\n"
done ;;
*)
