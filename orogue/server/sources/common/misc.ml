let shuffle_array a =
	for i = Array.length a - 1 downto 1 do
		let j = Random.int i in
		let k = Array.get a i in
		Array.set a i (Array.get a j) ;
		Array.set a j k
	done

let pi = 4.0 *. atan 1.0

let matrix_fold_left f n m =
	let g n m = Array.fold_left f n m in
	Array.fold_left g n m

let matrix_map f m = Array.map (Array.map f) m

let matrix_iteri f m = Array.iteri (fun i m -> Array.iteri (f i) m) m

let rec list_remove x = function
	| [] -> failwith "x not in list"
	| h :: t -> if h = x then t else h :: (list_remove x t)

let rec list_intersect a b =
	match a with
	| [] -> if b = [] then [] else list_intersect b a
	| h :: t ->
		if List.mem h b
		then h :: (list_intersect t (list_remove h b))
		else list_intersect t b

let list_get_random l = List.nth l (Random.int (List.length l))

let read_file filename =
	let lines = ref [] in
	let chan = open_in filename in
	try
		while true ; do
			lines := input_line chan :: !lines
		done ;
		!lines
	with End_of_file ->
		close_in chan ;
	List.rev !lines ;;

let explode s =
	let l = ref [] in
	String.iter (fun c -> l := c :: !l) s;
	List.rev !l

let implode l =
	let res = Bytes.create (List.length l) in
	let rec imp i = function
		| [] -> res
		| c :: l -> Bytes.set res i c ; imp (i + 1) l in
	imp 0 l

let str_start str len = String.sub str 0 len

let str_end str offset = String.sub str offset (String.length str - offset)

let split str delim =
	let rec aux str toks =
		if String.contains str delim then
			begin
				let i = String.index str delim in
				aux (str_end str (i + 1)) (str_start str i :: toks)
			end else begin
				if String.length str = 0 then List.rev toks else
				List.rev (str :: toks)
			end in
	aux str []

let trim s =
	let is_space = function
		| ' ' | '\012' | '\n' | '\r' | '\t' -> true
		| _ -> false in
	let len = String.length s in
	(* Localisation du dernier espace au debut. *)
	let i = ref 0 in
	while !i < len && is_space (String.get s !i) do
		incr i
	done ;
	(* Localisation du premier espace de la fin. *)
	let j = ref (len - 1) in
	while !j >= !i && is_space (String.get s !j) do
		decr j
	done ;
	(* Creation de la nouvelle chaine. *)
	if !i = 0 && !j = len - 1 then s
	else if !j >= !i then String.sub s !i (!j - !i + 1)
	else "" ;;

let range start stop =
	let rec loop l n =
		if n >= stop then l else
		loop (n :: l) (n + 1) in
	if start > stop then [] else
	loop [] start ;;

let rand_select list n =
	let rec extract acc n = function
		| [] -> raise Not_found
		| h :: t -> if n = 0 then (h, acc @ t) else extract (h :: acc) (n - 1) t in
	let extract_rand list len = extract [] (Random.int len) list in
	let rec aux n acc list len =
		if n = 0 then acc else
		let picked, rest = extract_rand list len in
		aux (n - 1) (picked :: acc) rest (len - 1) in
	let len = List.length list in
	aux (min n len) [] list len ;;
