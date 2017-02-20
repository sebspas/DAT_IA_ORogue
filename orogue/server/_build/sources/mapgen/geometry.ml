let eps = 0.01

type point =
	{ x : float
	; y : float
	}

type segment =
	{ a : point
	; b : point
	}

let determinant a b = a.x *. b.y -. a.y *. b.x ;;

let line_intersection l1 l2 =
	let x_diff = { x = l1.a.x -. l1.b.x ; y = l2.a.x -. l2.b.x } in
	let y_diff = { x = l1.a.y -. l1.b.y ; y = l2.a.y -. l2.b.y } in
	let div = determinant x_diff y_diff in
	if -. eps <= div && div < eps then None else
	let d = { x = determinant l1.a l1.b ; y = determinant l2.a l2.b } in
	let x = (determinant d x_diff) /. div in
	let y = (determinant d y_diff) /. div in
	Some { x = x ; y = y } ;;

let in_range a b i = a < b && a <= i && i <= b || a >= b && b <= i && i <= a ;;

let inside_rectangle a b i = (in_range a.x b.x i.x) && (in_range a.y b.y i.y) ;;

let segment_intersect s1 s2 =
	match line_intersection s1 s2 with
		| Some i -> inside_rectangle s1.a s1.b i
		| None -> false ;;

let point_distance a b = ((a.x -. b.x) ** 2.0 +. (a.y -. b.y) ** 2.0) ** 0.5 ;;

let min a b = if a < b then a else b ;;

let segment_distance s1 s2 =
	let d1 = point_distance s1.a s2.a in
	let d2 = point_distance s1.a s2.b in
	let d3 = point_distance s1.b s2.a in
	let d4 = point_distance s1.b s2.b in
	min (min d1 d2) (min d3 d4) ;;

let are_parallel eps a b =
	let da = point_distance a.a a.b in
	let db = point_distance b.a b.b in
	let va = { x = (a.b.x -. a.a.x) /. da ; y = (a.b.y -. a.a.y) /. da } in
	let vb = { x = (b.b.x -. b.a.x) /. db ; y = (b.b.y -. b.a.y) /. db } in
	(* Il faut tester deux cas : si les directions sont identiques ou opposees ! *)
	(abs_float (va.x -. vb.x) < eps && abs_float (va.y -. vb.y) < eps) ||
	(abs_float (va.x +. vb.x) < eps && abs_float (va.y +. vb.y) < eps) ;;

let projection_point_line l i =
	(* L'equation d'une droite definie par deux points est P = P1 + u (P2 - P1)
	 * Nous cherchons la projection de i sur cette droite. Plus exactement, nous
	 * cherchons la valeur de u correspondant a l'intersection entre la droite
	 * et une normale a cette droite passant par i (le plus court chemin pour la
	 * projection). Cette projection est definie par un produit scalaire nul :
	 * produit_scalaire(i - P, P2 - P1) = 0. En remplacant P par sa definition,
	 * nous obtenons : produit_scalaire(i - P1 + u (P2 - P1), P2 - P1) = 0. Soit :
	 * (xi - x1 + u (x2 - x1)) (x2 - x1) + (yi - y1 + u (y2 - y1)) (y2 - y1) = 0
	 * (xi - x1) (x2 - x1) + (yi - y1) (y2 - y1) + u ((x2 - x1)^2 + (y2 - y1)^2) = 0
	 * Nous avons donc la valeur de u :
	 * u = [ (xi - x1) (x2 - x1) + (yi - y1) (y2 - y1) ] / ((x2 - x1)^2 + (y2 - y1)^2)
	 * En remplacant u dans l'equation de la droite, nous obtenons la position de
	 * l'intersection.
	 *)
	let x1, y1, x2, y2 = l.a.x, l.a.y, l.b.x, l.b.y in
	let xi, yi = i.x, i.y in
	let u = ( (xi -. x1) *. (x2 -. x1) +. (yi -. y1) *. (y2 -. y1) ) /. ((x2 -. x1) ** 2.0 +. (y2 -. y1) ** 2.0) in
	{ x = x1 +. u *. (x2 -. x1) ; y = y1 +. u *. (y2 -. y1) } ;;

let point_line_distance l i =
	let x = projection_point_line l i in
	point_distance x i ;;

(*
let a = 0.5, 0.5 in
let b = 1.5, 0.5 in
let c = 1.0, 1.0 in
let d = 4.0, 4.0 in
let i = line_intersection (a, b) (c, d) in
match i with
	| Some (x, y) -> Printf.printf "%f - %f\n" x y
	| _ -> Printf.printf "none"

Printf.printf "%b\n" (in_range 0.5 2.5 3.0) ;;
Printf.printf "%b\n" (in_range 0.5 2.5 1.0) ;;
*)
