(* Le calcul du bruit de perlin est realise en se basant sur un motif de
 * reference pour le bruit. Ce motif a une taille fixe. Il est compose
 * d'elements dont les coordonnees sont alignees sur des valeurs entieres. Ces
 * elements correspondent a des vecteurs unitaires.

 * Lors du calcul du bruit associe aux coordonnees p=(x, y), il est d'abord
 * necessaire d'identifier les quatres elements du motif qui encadrent p (ne pas
 * oublier que x et y sont des reels) : pi=p1, p2, p3 et p4. Ensuite, pour
 * identifier la contribution des vecteurs unitaires vi=v1, v2, v3 et v4, notee
 * ci=c1, c2, c3 et c4, au point p, le produit scalaire de vi est calculer avec
 * le vecteur (p - pi).

 *   v1    v2
 *      p
 *   v3    v4

 * La derniere etape consiste a reduire ces quatres contributions en une seule
 * valeur. Il s'agit simplement de 3 trois interpolations lineaires
 * successives. Tout d'abord, nous effectuons une interpolation lineaire entre
 * v1, (tx=x - floor(x)) et v2 (tx est la position en absisce de p dans le
 * carre du motif). Il faut ensuite faire de meme pour v3, tx et v4. Ces deux
 * valeurs sont ensuite utilisees, a nouveau, pour realiser une interpolation,
 * maintenant avec (ty=y - floor(y)).

 * Rectification : toutefois, utiliser directement tx genere des discontinuites
 * au niveau des coordonnees entieres (\/). Pour eviter ce phenomene, il suffit
 * d'utiliser une fonction qui lisse aux extremites (en 0 et 1). Il faut que
 * cette fonction parte de 0 pour arriver en 1 avec un seul point d'inflexion.
 * Il faudrait utiliser 6ti^5-15ti^4+10ti^3 (ou ti=tx, ty).

 * Avec gnuplot :
 *   clear
 *   set xrange [0:1]
 *   plot 6 * (x ** 5) - 15 * (x ** 4) + 10 * (x ** 3)
 * La fonction cosinus peut aussi faire l'affaire :
 *   plot 1 - (cos(x * pi) / 2 + 0.5)
 *)

let make_pattern w h =
	let size = w * h in
	let make_unit_vector i =
		let angle = 2.0 *. Misc.pi *. float_of_int i /. float_of_int size in
		cos angle, sin angle in
	let pattern = Array.init size make_unit_vector in
	Misc.shuffle_array pattern ;
	let get x y =
		let x = int_of_float x mod w in
		let y = int_of_float y mod h in
		Array.get pattern (y * w + x) in
	get

let lerp a b w = (1.0 -. w) *. a +. w *. b

let dot x1 y1 x2 y2 = x1 *. x2 +. y1 *. y2

let grad n = 1.0 -. (cos (n *. Misc.pi) /. 2.0 +. 0.5)

let noise_2d pattern x y =
	let x1 = floor x in
	let y1 = floor y in
	let x2 = x1 +. 1.0 in
	let y2 = y1 in
	let x3 = x1 in
	let y3 = y1 +. 1.0 in
	let x4 = x1 +. 1.0 in
	let y4 = y1 +. 1.0 in

	let tx = x -. x1 in
	let ty = y -. y1 in

	let v1x, v1y = pattern x1 y1 in
	let v2x, v2y = pattern x2 y2 in
	let v3x, v3y = pattern x3 y3 in
	let v4x, v4y = pattern x4 y4 in

	let c1 = dot v1x v1y (x -. x1) (y -. y1) in 
	let c2 = dot v2x v2y (x -. x2) (y -. y2) in 
	let c3 = dot v3x v3y (x -. x3) (y -. y3) in 
	let c4 = dot v4x v4y (x -. x4) (y -. y4) in

	let lx = grad tx in
	let ly = grad ty in

	let s1 = lerp c1 c2 lx in
	let s2 = lerp c3 c4 lx in
	let s3 = lerp s1 s2 ly in

	s3

let make_noise_2d_map w h p pw ph =
	let pattern = make_pattern pw ph in
	(* Cette fonction initialise un point de la carte en utilisant les octaves et frequences. *)
	let rec make_point x y r = function
		| (o, fx, fy) :: p -> (* octave, frequence_x, frequence_y *)
			let r = r +. o *. noise_2d pattern (x *. fx) (y *. fy) in
			make_point x y r p
		| [] -> r in
	let get x y = make_point (float_of_int x) (float_of_int y) 0.0 p in
	get
