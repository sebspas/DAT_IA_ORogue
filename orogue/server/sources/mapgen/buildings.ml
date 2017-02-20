open Geometry
open Terrain

let max_building_width = 12 ;;
let min_building_width = 6 ;;
let max_building_height = 12 ;;
let min_building_height = 6 ;;

let check_room map w h i1 j1 i2 j2 =
	if i1 < 0 || i1 >= w || i2 < 0 || i2 >= w || j1 < 0 || j1 >= h || j2 < 0 || j2 >= h then false else
	let rec loop = function
		| i, _ when i > i2 -> true
		| i, j when j > j2 -> loop (i + 1, j1)
		| i, j ->
			begin match map.(i).(j) with
				| Ground | Forest -> loop (i, j + 1)
				| _ -> false
			end in
	loop (i1, j1) ;;

(* Placement d'un batiment en fonction de l'orientation de la route adjacente.
   Cette fonction retourne les coordonnees de la maison. *)
let make_building_from_top map w h i j =
	if not (check_room map w h i (j - min_building_height + 1) (i + min_building_width - 1) j) then None else
	let candidats = ref [] in
	for di = min_building_width to max_building_width do
		for dj = min_building_height to max_building_height do
			if check_room map w h i (j - dj + 1) (i + di - 1) j then
				candidats := (i, j - dj + 1, di, dj) :: !candidats
		done
	done ;
	(* Choix d'un placement parmi les candidats. *)
	if !candidats = [] then None else
	let n = Random.int (List.length !candidats) in
	let i, j, di, dj = List.nth !candidats n  in
	(* Placement des murs. *)
	for i = i + 1 to i + di - 2 do
		for j = j + 1 to j + dj - 2 do
			map.(i).(j) <- Floor
		done ;
	done ;
	for j = j + 1 to j + dj - 2 do
		map.(i + di - 2).(j) <- Wall ;
		map.(i + 1).(j) <- Wall
	done ;
	for i = i + 1 to i + di - 2 do
		map.(i).(j + dj - 2) <- Wall ;
		map.(i).(j + 1) <- Wall
	done ;
	(* Placement aleatoire de la porte d'entree. *)
	let door = Random.int (di - 4) in
	map.(i + 2 + door).(j + dj - 2) <- Door ;
	Some (i, j, di, dj) ;;

let make_building_from_bottom map w h i j =
	if not (check_room map w h i j (i + min_building_width - 1) (j + min_building_height - 1)) then None else
	let candidats = ref [] in
	for di = min_building_width to max_building_width do
		for dj = min_building_height to max_building_height do
			if check_room map w h i j (i + di - 1) (j + dj - 1) then
				candidats := (i, j, di, dj) :: !candidats
		done
	done ;
	(* Choix d'un placement parmi les candidats. *)
	if !candidats = [] then None else
	let n = Random.int (List.length !candidats) in
	let i, j, di, dj = List.nth !candidats n  in
	(* Placement des murs. *)
	for i = i + 1 to i + di - 2 do
		for j = j + 1 to j + dj - 2 do
			map.(i).(j) <- Floor
		done ;
	done ;
	for j = j + 1 to j + dj - 2 do
		map.(i + di - 2).(j) <- Wall ;
		map.(i + 1).(j) <- Wall
	done ;
	for i = i + 1 to i + di - 2 do
		map.(i).(j + dj - 2) <- Wall ;
		map.(i).(j + 1) <- Wall
	done ;
	(* Placement aleatoire de la porte d'entree. *)
	let door = Random.int (di - 4) in
	map.(i + 2 + door).(j + 1) <- Door ;
	Some (i, j, di, dj) ;;

let make_building_from_left map w h i j =
	if not (check_room map w h i j (i + min_building_width - 1) (j + min_building_height - 1)) then None else
	let candidats = ref [] in
	for di = min_building_width to max_building_width do
		for dj = min_building_height to max_building_height do
			if check_room map w h i j (i + di - 1) (j + dj - 1) then
				candidats := (i, j, di, dj) :: !candidats
		done
	done ;
	(* Choix d'un placement parmi les candidats. *)
	if !candidats = [] then None else
	let n = Random.int (List.length !candidats) in
	let i, j, di, dj = List.nth !candidats n  in
	(* Placement des murs. *)
	for i = i + 1 to i + di - 2 do
		for j = j + 1 to j + dj - 2 do
			map.(i).(j) <- Floor
		done ;
	done ;
	for j = j + 1 to j + dj - 2 do
		map.(i + di - 2).(j) <- Wall ;
		map.(i + 1).(j) <- Wall
	done ;
	for i = i + 1 to i + di - 2 do
		map.(i).(j + dj - 2) <- Wall ;
		map.(i).(j + 1) <- Wall
	done ;
	(* Placement aleatoire de la porte d'entree. *)
	let door = Random.int (dj - 4) in
	map.(i + 1).(j + door + 2) <- Door ;
	Some (i, j, di, dj) ;;

let make_building_from_right map w h i j =
	if not (check_room map w h (i - min_building_width + 1) j i (j + min_building_height - 1)) then None else
	let candidats = ref [] in
	for di = min_building_width to max_building_width do
		for dj = min_building_height to max_building_height do
			if check_room map w h (i - di + 1) j i (j + dj - 1) then
				candidats := (i - di + 1, j, di, dj) :: !candidats
		done
	done ;
	(* Choix d'un placement parmi les candidats. *)
	if !candidats = [] then None else
	let n = Random.int (List.length !candidats) in
	let i, j, di, dj = List.nth !candidats n  in
	(* Placement des murs. *)
	for i = i + 1 to i + di - 2 do
		for j = j + 1 to j + dj - 2 do
			map.(i).(j) <- Floor
		done ;
	done ;
	for j = j + 1 to j + dj - 2 do
		map.(i + di - 2).(j) <- Wall ;
		map.(i + 1).(j) <- Wall
	done ;
	for i = i + 1 to i + di - 2 do
		map.(i).(j + dj - 2) <- Wall ;
		map.(i).(j + 1) <- Wall
	done ;
	(* Placement aleatoire de la porte d'entree. *)
	let door = Random.int (dj - 4) in
	map.(i + di - 2).(j + door + 2) <- Door ;
	Some (i, j, di, dj) ;;

let make_buildings_along_road map w h r =
	let x1, y1 = int_of_float r.a.x, int_of_float r.a.y in
	let x2, y2 = int_of_float r.b.x, int_of_float r.b.y in
	let buildings = ref [] in
	if x1 = x2 then
		let y1, y2 = if y1 < y2 then y1, y2 else y2, y1 in
		for j = y1 to y2 do
			let building_a = make_building_from_right map w h (x1 - 1) j in
			let building_b = make_building_from_left map w h (x1 + 1) j in
			buildings := building_a :: building_b :: !buildings
		done
	else if y1 = y2 then
		let x1, x2 = if x1 < x2 then x1, x2 else x2, x1 in
		for i = x1 to x2 do
			let building_a = make_building_from_bottom map w h i (y1 + 1) in
			let building_b = make_building_from_top map w h i (y1 - 1) in
			buildings := building_a :: building_b :: !buildings
		done
	else failwith "slope road not yet handled" ;
	let buildings = List.filter ((<>) None) !buildings in
	List.map (function | Some building -> building | _ -> failwith "none building") buildings ;;

let make_buildings map w h roads = List.concat (List.map (make_buildings_along_road map w h) roads) ;;
