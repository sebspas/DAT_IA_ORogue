open Unix ;;

open Data_structure ;;
open Terrain ;;
open Misc ;;
open Fov ;;

let hit_point_npc = 2 ;;
let hit_point_player = 8 ;;
let nb_characters = ref 128 ;;
let ratio_foods = ref 0.1 ;;

(* Analyse des arguments du programme. *)
let () =
	let set_nb_characters value = nb_characters := value in
	let set_ratio_foods value = ratio_foods := min (max value 0.0) 1.0 in

	let speclist =
		[	("-nc", Arg.Int set_nb_characters, "Sets the number of characters")
		;	("-foods", Arg.Float set_ratio_foods, "Sets the ratio of foods (ratio of buildings containing foods)")
		] in
	let usage_msg = "Orogue:" in
	Arg.parse speclist print_endline usage_msg

let id_player_pc = 0
let id_player_npc = 1

(*
module Int2Map = Map.Make(struct type t = int * int * int let compare = compare end) ;;
module Int1Map = Map.Make(struct type t = int let compare = compare end) ;;
*)

type character =
	{ c_exist : bool
	; c_player : int
	; c_x : int
	; c_y : int
	; c_symbol : char
	; c_fov : (int * int -> bool) -> int -> int -> (int * int) list
	; c_hit_point : int
	}
and food =
	{ f_x : int
	; f_y : int
	; f_symbol : char
	; f_hit_point : int
	}
and player =
	{ p_visibility : int FunArray.t FunArray.t
	(* Socket vers le joueur. *)
	; p_socket : file_descr
	; p_fd_in : in_channel
	; p_fd_out : out_channel
	}
and key =
	{ k_exist : bool
	; k_x : int
	; k_y : int
	}
and game =
	(* Fin de la partie. *)
	{ g_end : bool
	(* Dimensions de la carte. *)
	; m_width : int
	; m_height : int

	(* Carte du terrain a deux dimensions. *)
	; m_map : terrain FunArray.t FunArray.t

	(* Liste des batiments / maisons. *)
	; l_buildings : (int * int * int * int) list

	(* Carte de la nourriture. *)
	; m_foods : int FunArray.t FunArray.t
	(* Liste de nourriture. *)
	; f_foods : food FunArray.t

	(* Carte des personnages. *)
	; m_characters : int FunArray.t FunArray.t
	(* Liste des personnages. *)
	; c_characters : character FunArray.t

	(* Liste des joueurs. *)
	; l_players : player array
	} ;;

let empty_character =
	{ c_exist = false
	; c_player = id_player_npc
	; c_x = 0 ; c_y = 0
	; c_symbol = 'h'
	; c_fov = fov_raycasting 8 (make_fov_model 8)
	; c_hit_point = hit_point_npc
	}

let empty_food =
	{ f_x = 0 ; f_y = 0
	; f_symbol = 'f'
	; f_hit_point = 0
	}

let board1_get i1 board1 =
	FunArray.get board1 i1

let board2_get i1 i2 board1 =
	let board2 = FunArray.get board1 i1 in
	FunArray.get board2 i2

let board1_set i1 value board1 =
	FunArray.set board1 i1 value

let board2_set i1 i2 value board1 =
	let board2 = FunArray.get board1 i1 in
	let board2 = FunArray.set board2 i2 value in
	let board1 = FunArray.set board1 i1 board2 in
	board1

let board1_init s1 f =
	FunArray.init s1 f

let board2_init s1 s2 f =
	FunArray.init s1 (fun _ -> FunArray.init s2 f) ;;

let rec read_buildings buildings lines remain =
	if remain = 0 then buildings, lines else
	match lines with
		| line :: lines when String.length line > 0 && line.[0] == '#' ->
			read_buildings buildings lines remain
		| line :: lines ->
			let i, j, di, dj = Scanf.sscanf line "%d %d %d %d" (fun x y z t -> x, y, z, t) in
			read_buildings ((i, j, di, dj) :: buildings) lines (remain - 1)
		| _ -> failwith "invalid map file #4" ;;

let rec read_map map lines remain =
	if remain = 0 then map, lines else
	match lines with
		| line :: lines when String.length line > 0 && line.[0] == '#' ->
			read_map map lines remain
		| line :: lines ->
			let x, y, t = Scanf.sscanf line "%d %d %d" (fun x y t -> x, y, t) in
			let t = int_to_terrain t in
			let map = board2_set x y t map in
			read_map map lines (remain - 1)
		| _ -> failwith "invalid map file #3" ;;

let read_file filename =
	let lines = Misc.read_file filename in
	match lines with
		| size :: lines ->
			let width, height = Scanf.sscanf size "%d %d" (fun x y -> x, y) in
			(* Il faut a tout pris creer un tableau unique (de taille a) par case. *)
			let map = board2_init width height (fun _ -> Wall) in
			(* Remplissage de ce tableau. *)
			let map, lines = read_map map lines (width * height) in
			(* Lecture de maisons. *)
			begin
				match lines with
					| nb_buildings :: lines ->
						let nb_buildings = Scanf.sscanf nb_buildings "%d" (fun x -> x) in
						let buildings, _ = read_buildings [] lines nb_buildings in
						width, height, map, buildings
					| _ -> failwith "invalid map file #2"
			end
		| _ -> failwith "invalid map file #1" ;;

let create_game player1_socket player1_fd_in player1_fd_out player2_socket player2_fd_in player2_fd_out filename =
	let width, height, map, buildings = read_file filename in
	(* Carte de nourriture -- rien pour l'instant. *)
	let m_foods = board2_init width height (fun _ -> -1) in
	let f_foods = board1_init 0 (fun _ -> empty_food) in
	(* Carte des personnages -- aucun pour l'instant. *)
	let m_characters = board2_init width height (fun _ -> -1) in
	let c_characters = board1_init 0 (fun _ -> empty_character) in
	(* Liste des joueurs. *)
	let l_players =
		[|	{ p_visibility = board2_init width height (fun _ -> 0)
			; p_socket = player1_socket
			; p_fd_in = player1_fd_in
			; p_fd_out = player1_fd_out
			}
		;	{ p_visibility = board2_init width height (fun _ -> 0)
			; p_socket = player2_socket
			; p_fd_in = player2_fd_in
			; p_fd_out = player2_fd_out
			}
		|] in
	{ g_end = false
	; m_width = width ; m_height = height
	; m_map = map
	; l_buildings = buildings

	; m_foods = m_foods
	; f_foods = f_foods

	; m_characters = m_characters
	; c_characters = c_characters

	; l_players = l_players
	} ;;

let socket_send game player_index msg =
	Printf.eprintf ">%d: %s%!" player_index msg ;
	output_string game.l_players.(player_index).p_fd_out msg ;
	flush game.l_players.(player_index).p_fd_out ;
	flush_all ()

let socket_read game player_index =
	flush game.l_players.(player_index).p_fd_out ;
	input_line game.l_players.(player_index).p_fd_in

(* Affichage des caracters, ou de la nourriture, ou du terrain.
   Durant l'invocation de cette fonction, les tableaux visibility peuvent ne pas
   etre integres (ils sont en cours de memorisation dans inc/dec_visibility). *)
let show_tile game player_index x y =
	let character_index = board2_get x y game.m_characters in
	let food_index = board2_get x y game.m_foods in
	if character_index <> -1 then
		let character = board1_get character_index game.c_characters in
		let camp = if character.c_player = player_index then "ally" else "ennemy" in
		let msg = Printf.sprintf "print x=%d y=%d %s character_index=%d hit_point=%d symbol=%c\n" x y camp character_index character.c_hit_point character.c_symbol in
		socket_send game player_index msg
	else if food_index <> -1 then
		let food = board1_get food_index game.f_foods in
		let msg = Printf.sprintf "print x=%d y=%d food hit_point=%d symbol=%c\n" x y food.f_hit_point food.f_symbol in
		socket_send game player_index msg
	else
		let t = terrain_to_int (board2_get x y game.m_map) in
		let msg = Printf.sprintf "print x=%d y=%d terrain type=%d\n" x y t in
		socket_send game player_index msg

let hide_tile game player_index x y =
	let msg = Printf.sprintf "hide x=%d y=%d\n" x y in
	socket_send game player_index msg

let inc_visibility game player_index coords =
	let visibility = game.l_players.(player_index).p_visibility in
	let rec do_coord visibility (x, y) =
		let value = board2_get x y visibility in
		if value = 0 then show_tile game player_index x y ;
		board2_set x y (value + 1) visibility in
	let visibility = List.fold_left do_coord visibility coords in
	let player = { game.l_players.(player_index) with p_visibility = visibility } in
	game.l_players.(player_index) <- player ;
	game

let dec_visibility game player_index coords =
	let visibility = game.l_players.(player_index).p_visibility in
	let rec do_coord visibility (x, y) =
		let value = board2_get x y visibility in
		let visibility = board2_set x y (value - 1) visibility in
		if value = 1 then hide_tile game player_index x y ;
		visibility in
	let visibility = List.fold_left do_coord visibility coords in
	let player = { game.l_players.(player_index) with p_visibility = visibility } in
	game.l_players.(player_index) <- player ;
	game

let has_visibility game player_index x y =
	let visibility = game.l_players.(player_index).p_visibility in
	board2_get x y visibility > 0

let is_free_cell game x y =
	if x < 0 || y < 0 || x >= game.m_width || y >= game.m_height then false else
	match board2_get x y game.m_map with
		| Water -> false
		| Wall -> false
		| _ -> board2_get x y game.m_characters = -1

let move_character game player_index character_index x y =
	let character = board1_get character_index game.c_characters in
	let old_x = character.c_x in
	let old_y = character.c_y in

	(* Identification des cellules qui bloquent la visibilite. *)
	let is_outside (x, y) = x < 0 || y < 0 || x >= game.m_width || y >= game.m_height in
	let blocked (x, y) = is_outside (x, y) || board2_get x y game.m_map = Wall in

	let old_lit = character.c_fov blocked old_x old_y in
	let new_lit = character.c_fov blocked x y in

	(* CF. COMMENTAIRE de init_visibilities. *)
	let old_lit = List.filter (fun coord -> not (is_outside coord)) old_lit in
	let new_lit = List.filter (fun coord -> not (is_outside coord)) new_lit in

	(* Deplacement du caractere. *)
	let game = { game with m_characters = board2_set old_x old_y (-1) game.m_characters } in
	let game = { game with m_characters = board2_set x y character_index game.m_characters } in
	let character = { character with c_x = x ; c_y = y } in
	let game = { game with c_characters = board1_set character_index character game.c_characters } in

	(* Mise a jour des visibilites du joueur. *)
	let game = inc_visibility game player_index new_lit in
	let game = dec_visibility game player_index old_lit in
	show_tile game player_index old_x old_y ;
	show_tile game player_index x y ;

	(* Mise a jour de la visibilite de l'adversaire si necessaire. *)
	let enemy_index = 1 - player_index in
	if has_visibility game enemy_index old_x old_y then show_tile game enemy_index old_x old_y ;
	if has_visibility game enemy_index x y then show_tile game enemy_index x y ;

	(* Traitement de la nourriture dans un premier temps.
	   Si le joueur s'est deplace sur de la nourriture, alors ses points
	   de vie augmentent. Sinon, les caracteristiques du joueur ne changent pas. *)
	let food_index = board2_get x y game.m_foods in
	if character.c_player <> id_player_npc && food_index <> -1 then
		(* Mise a jour des points de vie. *)
		let food = board1_get food_index game.f_foods in
		let character = { character with c_hit_point = character.c_hit_point + food.f_hit_point } in
		let game = { game with c_characters = board1_set character_index character game.c_characters } in
		(* Retrait de la nourriture du jeu. *)
		let game = { game with m_foods = board2_set x y (-1) game.m_foods } in
		{ game with f_foods = board1_set food_index empty_food game.f_foods }
	else game

let player_try_move game player_index character_index x y =
	if not (is_free_cell game x y) then game else
	move_character game player_index character_index x y

let behavior game player_index character_index =
	let character = board1_get character_index game.c_characters in
	let msg = Printf.sprintf "action? x=%d y=%d character_index=%d hit_point=%d symbol=%c\n" character.c_x character.c_y character_index character.c_hit_point character.c_symbol in
	socket_send game player_index msg ;
	let command = socket_read game player_index in
	let command = trim command in
	if command = "end" then { game with g_end = true } else
	match command with
		| "east" ->
			let x = min (character.c_x + 1) (game.m_width - 1) in
			let y = character.c_y in
			player_try_move game player_index character_index x y
		| "west" ->
			let x = max (character.c_x - 1) 0 in
			let y = character.c_y in
			player_try_move game player_index character_index x y
		| "north" ->
			let y = min (character.c_y - 1) (game.m_height - 1) in
			let x = character.c_x in
			player_try_move game player_index character_index x y
		| "south" ->
			let y = max (character.c_y + 1) 0 in
			let x = character.c_x in
			player_try_move game player_index character_index x y
		| command ->
			begin
				Printf.printf "unknown cmd: '%s'\n" command ;
				game
			end

let rec create_npc game index nb_characters =
	if nb_characters = 0 then game else
	let x = Random.int game.m_width in
	let y = Random.int game.m_height in
	if is_free_cell game x y then
		let character =
			{ c_exist = true
			; c_player = id_player_npc
			; c_x = x ; c_y = y
			; c_symbol = 'h'
			; c_fov = fov_raycasting 8 (make_fov_model 8)
			; c_hit_point = hit_point_npc
			} in
		let game = { game with m_characters = board2_set x y index game.m_characters } in
		let game = { game with c_characters = board1_set index character game.c_characters } in
		create_npc game (index + 1) (nb_characters - 1)
	else create_npc game index (nb_characters - 1) ;;

let create_foods game index nb_foods =
	let selected_buildings = Misc.rand_select game.l_buildings nb_foods in
	let select_location (i, j, di, dj) = i + 2 + Random.int (di - 4), j + 2 + Random.int (dj - 4) in
	let selected_locations = List.map select_location selected_buildings in

	(* Placement des nourritures. Si c'est pas possible (un joueur est present sur la case choisie,
	   alors tant pis, on passe a la nourriture suivante et au total il y en aura moins que prevu. *)
	let rec position_foods game index = function
		| (x, y) :: foods ->
			if not (is_free_cell game x y) then position_foods game (index + 1) foods else
			let food =
				{ f_x = x ; f_y = y
				; f_symbol = 'f'
				; f_hit_point = 1
				} in
			let game = { game with m_foods = board2_set x y index game.m_foods } in
			let game = { game with f_foods = board1_set index food game.f_foods } in
			position_foods game (index + 1) foods
		| [] -> game in

	position_foods game 0 selected_locations

let init_visibilities game =
	(* Identification des cellules qui bloquent la visibilite. *)
	let is_outside (x, y) = x < 0 || y < 0 || x >= game.m_width || y >= game.m_height in
	let blocked (x, y) = is_outside (x, y) || board2_get x y game.m_map = Wall in

	let rec do_character game character_index =
		if character_index = !nb_characters then game else
		let character = board1_get character_index game.c_characters in
		if not character.c_exist then do_character game (character_index + 1) else
		let lit = character.c_fov blocked character.c_x character.c_y in
		(* COMMENTAIRE VALABLE POUR move_character.
		 * Certains blocs sont hors de la carte. Il faut les retirer. On pourrait
		 * revoir la fonction blocked pour considerer les carreaux autours de la
		 * carte comme bloquant pour limiter ce phenomene, mais cela n'empeche pas
		 * le joueur de se positionner sur l'un d'eux et, dans ce cas, la fonction
		 * character.c_fov testera effectivement les positions hors de la carte. *)
		let lit = List.filter (fun coord -> not (is_outside coord)) lit in
		let game = inc_visibility game character.c_player lit in
		do_character game (character_index + 1) in
	do_character game 0

let main () =
	let server_sock = socket PF_INET SOCK_STREAM 0 in
	setsockopt server_sock SO_REUSEADDR true ;
	let address = (gethostbyname "localhost").h_addr_list.(0) in
	bind server_sock (ADDR_INET (address, 1029)) ;

	listen server_sock 2 ;
	let (client1_sock, client1_addr) = accept server_sock in
	let client1_fd_in = Unix.in_channel_of_descr client1_sock in
	let client1_fd_out = Unix.out_channel_of_descr client1_sock in

	let (client2_sock, client1_addr) = accept server_sock in
	let client2_fd_in = Unix.in_channel_of_descr client2_sock in
	let client2_fd_out = Unix.out_channel_of_descr client2_sock in

	let game = create_game client1_sock client1_fd_in client1_fd_out client2_sock client2_fd_in client2_fd_out "world.dat" in

	(* Envoi des parametres du jeu aux joueurs. *)
	let msg = Printf.sprintf "parameters width=%d height=%d\n" game.m_width game.m_height in
	socket_send game id_player_pc msg ;
	socket_send game id_player_npc msg ;

	(* Placement des caracteres. *)
	let game = { game with c_characters = board1_init !nb_characters (fun _ -> empty_character) } in
	let game = create_npc game 0 !nb_characters in

	(* Placement de la nourriture. *)
	let nb_foods = int_of_float (!ratio_foods *. float_of_int (List.length game.l_buildings)) in
	let game = { game with f_foods = board1_init nb_foods (fun _ -> empty_food) } in
	let game = create_foods game 0 nb_foods in

	(* Preparation de l'unite du joueur (c'est le premier caractere). *)
	let player = board1_get 0 game.c_characters in
	let player = { player with c_player = id_player_pc ; c_symbol = '@' ; c_hit_point = hit_point_player } in
	let game = { game with c_characters = board1_set 0 player game.c_characters } in

	(* Mise a jour des visibilites. *)
	let game = init_visibilities game in

	let rec loop1 game character_index =
		if character_index >= !nb_characters then game else
		let character = board1_get character_index game.c_characters in
		if not character.c_exist then loop1 game (character_index + 1) else
		let game = behavior game character.c_player character_index in
		loop1 game (character_index + 1) in
	let rec loop2 game =
		if game.g_end then () else
		let game = loop1 game 0 in
		loop2 game in
	let _ = loop2 game in
	socket_send game id_player_pc "end\n" ;
	socket_send game id_player_npc "end\n" ;
	shutdown client1_sock SHUTDOWN_ALL ;
	shutdown client2_sock SHUTDOWN_ALL ;;

main ()
