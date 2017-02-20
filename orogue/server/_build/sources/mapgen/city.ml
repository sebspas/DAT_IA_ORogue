open Geometry
open Data_structure

let eps = 0.01 ;;
let parallel_threshold = 5.0 ;;
let join_threshold = 5.0 ;;
let road_min_length = 8.0 ;;
let road_max_length = 15.0 ;;

let local_constraints r s =
	(* Deux segments sont en conflit si ils sont quasiment paralleles et non connectes et,
	 * ils se croisent ou ils sont presque superposes et trop proches.
	 *)
	let in_conflict a b =
		if not (are_parallel eps a b) then false else
		if r.a = b.b then false else
		if segment_intersect a b then true else
		let superposition =
			inside_rectangle a.a a.b (projection_point_line a b.a) ||
			inside_rectangle a.a a.b (projection_point_line a b.b) ||
			inside_rectangle b.a b.b (projection_point_line b a.a) ||
			inside_rectangle b.a b.b (projection_point_line b a.b) in
		let close =
			point_line_distance a b.a < parallel_threshold ||
			point_line_distance a b.b < parallel_threshold ||
			point_line_distance b a.a < parallel_threshold ||
			point_line_distance b a.b < parallel_threshold in
(*
Printf.printf "A: %f,%f - %f,%f " a.a.x a.a.y a.b.x a.b.y ;
Printf.printf "B: %f,%f - %f,%f " b.a.x b.a.y b.b.x b.b.y ;
Printf.printf "superposition:%b close:%b\n" superposition close ;
*)
		superposition && close in
	if List.exists (in_conflict r) s then None else

	(* Transformation si le segment est proche d'un autre. *)
	let rec transform found d p = function
		| x :: s ->
			if x.b = p.a then transform found d p s else
			begin
				(* Pour tester l'intersection, on utilise r ! *)
				match line_intersection x r with
					| None -> transform found d p s
					| Some i ->
						if not (inside_rectangle x.a x.b i) then transform found d p s else
						let da = point_distance i r.a in
						let db = point_distance i r.b in
						if da < db then
							if found && da < d || da < join_threshold
							then transform true da { a = i ; b = r.b } s
							else transform found d p s
						else
							if found && db < d || db < join_threshold
							then transform true db { a = r.a ; b = i } s
							else transform found d p s
			end
		| [] -> p in
	Some (transform false 0.0 r s) ;;

let global_goals r =
	let d = point_distance r.a r.b in
	let vector_direction = { x = (r.b.x -. r.a.x) /. d ; y = (r.b.y -. r.a.y) /. d } in
	let vector_normal = { x = -. vector_direction.y ; y = vector_direction.x } in

	let s = [] in

	let c = road_min_length +. float_of_int (Random.int (int_of_float (road_max_length -. road_min_length))) in
	let v = { x = c *. vector_direction.x ; y = c *. vector_direction.y } in
	let e = { x = v.x +. r.b.x ; y = v.y +. r.b.y } in
	let t = Random.int 10 in
	let s = (t, { a = r.b ; b = e }) :: s in

	let c = road_min_length +. float_of_int (Random.int (int_of_float (road_max_length -. road_min_length))) in
	let v = { x = c *. vector_normal.x ; y = c *. vector_normal.y } in
	let e = { x = v.x +. r.b.x ; y = v.y +. r.b.y } in
	let t = Random.int 10 in
	let s = (t, { a = r.b ; b = e }) :: s in

	let c = road_min_length +. float_of_int (Random.int (int_of_float (road_max_length -. road_min_length))) in
	let v = { x = c *. vector_normal.x ; y = c *. vector_normal.y } in
	let e = { x = -. v.x +. r.b.x ; y = -. v.y +. r.b.y } in
	let t = Random.int 10 in
	let s = (t, { a = r.b ; b = e }) :: s in

	s ;;

let rec make_road q s =
	if q = PrioQueue.empty then s else
	let t, r, q = PrioQueue.extract q in
	if t > 60 then s else
	match local_constraints r s with
		| None ->
			make_road q s
		| Some p ->
			let s = p :: s in
			let candidats = global_goals r in
			let q = List.fold_left (fun q (t', r') -> PrioQueue.insert q (t + 1 + t') r') q candidats in
			make_road q s ;;

let make_city w h =
	let x = float_of_int (Random.int w) in
	let y = float_of_int (Random.int h) in
	let r = { a = { x = x ; y = y } ; b = { x = x +. 1.0 ; y = y } } in
	let q = PrioQueue.empty in
	let q = PrioQueue.insert q 0 r in
	make_road q [] ;;
