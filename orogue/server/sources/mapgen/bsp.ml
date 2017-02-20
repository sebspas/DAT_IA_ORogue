type config =
	{ room_min_w : int
	; room_min_h : int
	; leaf_max_w : int
	; leaf_max_h : int
	; split_ratio : float
	; sep_thickness : int (* Separation minimum entre deux surfaces (taille min. des murs). *)
	}

type surface = int * int * int * int

type 'a bsp =
	| Leaf of 'a
	| Node of bool * 'a bsp * 'a bsp (* Le boolean indique si le decoupage est sur h ou non. *)

let rec create_bsp config x y w h =
	(* On ne decoupage pas si il n'y a pas la place pour deux chambres. Sinon,
	 * Si la taille est correcte vis a vis de la surface max des pieces et si les
	 * probas sont avec nous, nous ne decoupons pas egalement.
	 *)
	let can_split_w = 2 * config.room_min_w + 1 <= w in
	let can_split_h = 2 * config.room_min_h + 1 <= h in
	let must_split_w = config.leaf_max_w < w in
	let must_split_h = config.leaf_max_h < h in
	if (not can_split_w && not can_split_h) ||
			(not must_split_w && not must_split_h &&
				Random.float 1.0 <= config.split_ratio) then
		Leaf (x, y, w, h)
	else
		(* On decoupe en h si on ne peut pas faire autrement (i.e. on ne peut pas
		 * decouper en w) ou, si c'est possible a la fois en h et w et on choisit h.
		 *)
		if not can_split_w || (must_split_h && Random.bool ()) then
			(* Nous avons a notre disposition une hauteur h. Mais, si on decoupe en
			 * deux, il faut penser aux murs et a la hauteur minimum : il faut donc
			 * choisir un nombre entre room_min_h et (h - room_min_h - sep_thickness - 1).
			 * Autrement dit il faut additionner room_min_h a un nombre compris entre 0
			 * et (h - 2 * room_min_h - sep_thickness - 1).
			 *)
			let range = h - 2 * config.room_min_h in
			let sep = config.room_min_h + Random.int range in
			let bottom = create_bsp config x y w sep in
			let top = create_bsp config x (y + sep + 1) w (h - sep - 1) in
			Node (true, bottom, top)
		else
			let range = w - 2 * config.room_min_w in
			let sep = config.room_min_w + Random.int range in
			let left = create_bsp config x y sep h in
			let right = create_bsp config (x + sep + 1) y (w - sep - 1) h in
			Node (false, left, right)

let rec create_rooms config = function
	| Leaf (x, y, w, h) ->
		let room_w = config.room_min_w + Random.int (w - config.room_min_w + 1) in
		let room_x = x + Random.int (w - room_w + 1) in
		let room_h = config.room_min_h + Random.int (h - config.room_min_h + 1) in
		let room_y = y + Random.int (h - room_h + 1) in
		Leaf (room_x, room_y, room_w, room_h)
	| Node (split_h, n1, n2) -> Node (split_h, create_rooms config n1, create_rooms config n2)

let rec get_random_room direction bottom_left = function
	| Leaf size -> size
	| Node (split_h, n1, n2) when split_h = direction ->
		let n = if bottom_left then n1 else n2 in
		get_random_room direction bottom_left n
	| Node (_, n1, n2) ->
		let n = if Random.bool () then n1 else n2 in
		get_random_room direction bottom_left n

let get_random_left_room bsp = get_random_room false true bsp
let get_random_right_room bsp = get_random_room false false bsp
let get_random_bottom_room bsp = get_random_room true true bsp
let get_random_top_room bsp = get_random_room true false bsp

let rec trace_corridor (point_1x, point_1y) (point_2x, point_2y) =
	if point_1x = point_2x then
		if point_1y < point_2y
		then [ ( point_1x, point_1y, 1, point_2y - point_1y + 1 ) ]
		else [ ( point_1x, point_2y, 1, point_1y - point_2y + 1 ) ]
	else if point_1y = point_2y then
		if point_1x < point_2x
		then [ ( point_1x, point_1y, point_2x - point_1x + 1, 1 ) ]
		else [ ( point_2x, point_1y, point_1x - point_2x + 1, 1 ) ]
	else if Random.bool () then
		(trace_corridor (point_1x, point_1y) (point_1x, point_2y)) @
		(trace_corridor (point_1x, point_2y) (point_2x, point_2y))
	else
		(trace_corridor (point_1x, point_1y) (point_2x, point_1y)) @
		(trace_corridor (point_2x, point_1y) (point_2x, point_2y))

let choose_point_in_room (x, y, w, h) = x + Random.int w, y + Random.int h

let rec make_corridors corridors = function
	| Leaf _ -> corridors
	| Node (split_h, n1, n2) ->
		(* Pour creer un couloir entre deux surfaces issues d'un decoupage en h, il faut
		 * choisir une chambre en bas de la surface du haut et une chambre en haut de la
		 * surface du bas. Idem pour le decoupage en w.
		 *)
		let r1, r2 =
			if split_h
			then get_random_top_room n1, get_random_bottom_room n2
			else get_random_right_room n1, get_random_left_room n2 in
		let point_1x, point_1y = choose_point_in_room r1 in
		let point_2x, point_2y = choose_point_in_room r2 in
		let corridor = trace_corridor (point_1x, point_1y) (point_2x, point_2y) in
		make_corridors (make_corridors (corridor @ corridors) n1) n2

let rec get_rooms rooms = function
	| Leaf size -> size :: rooms
	| Node (_, n1, n2) -> get_rooms (get_rooms rooms n1) n2

let create_floor config x y w h =
	(* Il ne faut pas oublier les murs ! *)
	let bsp = create_bsp config (x + 1) (y + 1) (w - 2) (h - 2) in
	let bsp_rooms = create_rooms config bsp in
	let rooms = get_rooms [] bsp in
	(* Creation des 4 sorties / fenetres *)
	let point_top1 = choose_point_in_room (get_random_top_room bsp_rooms) in
	let point_top2 = fst point_top1, y + h - 1 in
	let point_bottom1 = choose_point_in_room (get_random_bottom_room bsp_rooms) in
	let point_bottom2 = fst point_bottom1, y in
	let point_left1 = choose_point_in_room (get_random_left_room bsp_rooms) in
	let point_left2 = x, snd point_left1 in
	let point_right1 = choose_point_in_room (get_random_right_room bsp_rooms) in
	let point_right2 = x + w - 1, snd point_right1 in
	let corridors =
		trace_corridor point_top1 point_top2		@
		trace_corridor point_bottom1 point_bottom2	@
		trace_corridor point_left1 point_left2		@
		trace_corridor point_right1 point_right2 in
	let corridors = make_corridors corridors bsp_rooms in
	(* Identification des murs et des cases libres. *)
	let is_in_surface i j (sx, sy, sw, sh) =
		i + x >= sx && i + x < sx + sw && j + y >= sy && j + y < sy + sh in
	let rec wall_and_free walls frees = function
		| i, _ when i = w -> walls, frees
		| i, j when j = h -> wall_and_free walls frees (i + 1, 0)
		| i, j ->
			if List.exists (is_in_surface i j) corridors || List.exists (is_in_surface i j) rooms
			then wall_and_free walls ( (x + i, y + j) :: frees) (i, j + 1)
			else wall_and_free ( (x + i, y + j) :: walls) frees (i, j + 1) in
	wall_and_free [] [] (0, 0)

let can_walk_around frees (x, y) =
	List.mem (x, y) frees         && List.mem (x - 1, y) frees     && List.mem (x - 1, y - 1) frees &&
	List.mem (x, y - 1) frees     && List.mem (x + 1, y - 1) frees && List.mem (x + 1, y) frees     &&
	List.mem (x + 1, y + 1) frees && List.mem (x, y + 1) frees     && List.mem (x - 1, y + 1) frees

let rec create_surface surface x y w h = function
	| i, _ when i = w -> surface
	| i, j when j = h -> create_surface surface x y w h (i + 1, 0)
	| i, j -> create_surface ((i + x, j + y) :: surface) x y w h (i, j + 1)

let create_floors config x y w h z n =
	let ceiling = create_surface [] x y w h (0, 0) in
	let rec create_upper_floor bricks empties holes n m =
		if m >= n then bricks, empties, n else
		let new_walls, new_frees = create_floor config x y w h in
		(* Ce niveau est compatible avec le precedent si un trou candidat pour ce niveau est juste
		 * au dessus d'un trou candidat du niveau precedent.
		 *)
		let new_holes = List.filter (can_walk_around new_frees) new_frees in
		let stairs = Misc.list_intersect new_holes holes in
		if List.length stairs = 0
		(* Si on n'a pas reussi, la tour sera moins haute. *)
		then create_upper_floor bricks empties holes (n - 1) m
		else
			(* Si le niveau est compatible, on choisit l'escalier avec son orientation. *)
			let stair_1x, stair_1y = Misc.list_get_random stairs in
			let stair_dx, stair_dy = Misc.list_get_random [ 1, 0 ; -1, 0 ; 0, 1 ; 0, -1 ] in
			let stair_2x, stair_2y = stair_1x + stair_dx, stair_1y + stair_dy in
			(* On prepare le plafond avec le trou pour l'escalier. *)
			let ceiling = Misc.list_remove (stair_2x, stair_2y) (Misc.list_remove (stair_1x, stair_1y) ceiling) in
			let ceiling = List.map (fun (x, y) -> x, y, z + 2 * m - 1) ceiling in
			(* On cree les bricks. *)
			let new_walls = List.map (fun (x, y) -> x, y, z + 2 * m) new_walls in
			let bricks = List.rev_append ceiling (List.rev_append bricks new_walls) in
			let bricks = (stair_1x, stair_1y, z + 2 * m - 2) :: bricks in
			(* On prepare les trous pour le niveau suivant en les recalculant (on ne peut pas se contenter
			 * de supprimer l'escalier de new_holes sinon on risque d'avoir deux escaliers l'un sur l'autre.
			 *)
			let new_frees = Misc.list_remove (stair_2x, stair_2y) (Misc.list_remove (stair_1x, stair_1y) new_frees) in
			let new_empties = List.map (fun (x, y) -> x, y, z + 2 * m) new_frees in
			let empties = (stair_2x, stair_2y, z + 2 * m - 1) :: (stair_1x, stair_1y, z + 2 * m - 1) :: List.rev_append new_empties empties in
			let new_holes = List.filter (can_walk_around new_frees) new_frees in
			create_upper_floor bricks empties new_holes n (m + 1) in
	(* Preparation du plancher. *)
	let ceiling = List.map (fun (x, y) -> x, y, z - 1) ceiling in
	let new_walls, new_frees = create_floor config x y w h in
	let bricks = List.map (fun (x, y) -> x, y, z) new_walls in
	let bricks = List.rev_append bricks ceiling in
	let empties = List.map (fun (x, y) -> x, y, z) new_frees in
	let holes = List.filter (can_walk_around new_frees) new_frees in
	let bricks, empties, n = create_upper_floor bricks empties holes n 1 in
	(* Preparation du toit. *)
	let ceiling = List.map (fun (x, y, _) -> x, y, z + 2 * n - 1) ceiling in
	let bricks = List.rev_append bricks ceiling in
	bricks, empties
