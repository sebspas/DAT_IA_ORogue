open Misc

let bresenham_line start stop =
	let x1, y1 = start in
	let x2, y2 = stop in
	let dx = x2 - x1 in
	let dy = y2 - y1 in
	(* Regarde si la droite est quasiment verticale. Si la droite est trop en pente, une
	 * rotation de 90 degres est realisee pour se ramener a une droite plus horizontale.
	 *)
	let rotate = (abs dy) > (abs dx) in
	let x1, y1 = if rotate then y1, x1 else x1, y1 in
	let x2, y2 = if rotate then y2, x2 else x2, y2 in
	(* Si le premier point est plus a droite que le second, on les inverse. *) 
	let swap = x1 > x2 in
	let x1, x2 = if swap then x2, x1 else x1, x2 in
	let y1, y2 = if swap then y2, y1 else y1, y2 in
	(* Calcul des pentes. *)
	let dx = x2 - x1 in
	let dy = y2 - y1 in
	(* Preparation des erreurs et de la pente. *)
	let error = dx / 2 in
	let ystep = if y1 < y2 then 1 else -1 in
	let rec loop points x y error =
		if x > x2 then points else
		let points = (x, y) :: points in
		let error = error - abs dy in
		let y = if error < 0 then y + ystep else y in
		let error = if error < 0 then error + dx else error in
		loop points (x + 1) y error in

	let points = loop [] x1 y1 error in
	(* Annulation de la rotation et de l'inversion si effectuees.
	 * Concernant l'inversion, a ce niveau, points est deja inverse (ajout en tete) !
	 * Du coup, on invoque List.rev que si aucune inversion n'a ete realisee.
	 *)
	let points = if swap then points else List.rev points in
	let points = List.map (fun (x, y) -> if rotate then y, x else x, y) points in
	points ;;

type 'a tree = Node of 'a * 'a tree list

let make_fov_model r =
	let n = r in

	(* Le champs de vision est definit par un cercle. Pour identifier toutes
	 * les cases qui peuvent etre visibles de ce cercle, on considere le carre
	 * qui englobe le cercle et on tire un trait entre le centre et toutes les
	 * cases du bord du carre. On parcours les lignes en s'arretant si on est
	 * trop loin du centre. Plus le carre est grand, plus precis est le
	 * resultat (cela revient a faire un raycasting avec des delta d'angles
	 * petits).
	 *)

	let border =
		List.map (fun s -> [ - n * r + s, - n * r ; - n * r + s, + n * r ]) (range 0 (n * r * 2 + 1)) @
		List.map (fun s -> [ - n * r, - n * r + s ; + n * r, - n * r + s ]) (range 1 (n * r * 2)) in
	let border = List.concat border in
	let check (x, y) = sqrt (float_of_int x ** 2.0 +. float_of_int y ** 2.0) < float_of_int r in
	let rec loop lit = function
		| [] -> lit
		| p :: border ->
			let line = bresenham_line (0, 0) p in
			let line = List.filter check line in
			loop (line :: lit) border in
	let lines = loop [] border in

	(* Nous avons alors une liste de traits de lumiere. Notez qu'une case peut
	 * apparaitre dans plusieurs lignes (cf fov-2.png). Supposons qu'un seul
	 * objet est present dans la scene (la distance entre @ et X est r) :
	 *
	 *  4    1      9 12
	 * 	3	         ..X
	 *	2	     ...#
	 *  1    ....
	 *	0	@
	 * Alors, lors du parcours de ce trait, on commence par le centre (@) et on
	 * illumine toutes les cases jusqu'a la case 9. Par contre, on stoppe car
	 * les autres ne sont pas visibles via ce trait de lumiere.  Notez que les
	 * cases 10, 11, et 12 peuvent etre rendues visibles par un autre trait de
	 * lumiere.
	 *)

	(* Toutes les lignes sont stockees dans une matrice d'adjacence qui
	 * indique, pour chaque case, quelles sont les autres cases dependantes au
	 * niveau visibilite. Du coup, ces autres cases n'ont pas a etre testees si
	 * aucune des cases dont elles dependent ne sont visibles !  Dans l'exemple
	 * precedent, la position [9,2] de la matrice contiendra une liste dont la
	 * case [3,10] fera partie car la visibilite de [3,10] depend de la
	 * visibilite de [2,9] (mais pas seulement !).
	 *)

	let adjacency_matrix = Array.make_matrix (2 * r + 1) (2 * r + 1) [] in
	let rec fill_adjacency = function
		| a :: b :: line ->
			begin
			let x1, y1 = a in
			if not (List.mem b adjacency_matrix.(x1 + r).(y1 + r)) then
					adjacency_matrix.(x1 + r).(y1 + r) <- b :: adjacency_matrix.(x1 + r).(y1 + r) ;
			fill_adjacency (b :: line)
			end
		| _ -> () in
	List.iter fill_adjacency lines ;
	(* De la matrice d'adjacence, ont deduit le graphe. *)

	(* Notez qu'il vaut mieux proceder en passant par la
	 * matrice d'adjacence car, un utilisant uniquement les lignes, on
	 * effectuera simplement une factorisation des prefixes des lignes sans
	 * permettre aux traits de lumiere de se rejoindre. Du coup, les fils du
	 * graphe pourrait ne pas etre complets.
	 *)

	let rec make_graph (dx, dy) =
		let children = adjacency_matrix.(dx + r).(dy + r) in
		let children = List.map make_graph children in
		Node ((dx, dy), children) in
	let graph = [ make_graph (0, 0) ] in

	(* Les dates nous permettent de savoir si, durant un meme calcul de FOV,
	 * nous avons deja traite une case. Si c'est le cas, c'est que nous avons
	 * atteint cette case via un autre trait de lumiere. Or, le precedent trait
	 * qui a atteint la case a deja traite ce qui est apres cette case (et
	 * toutes les possibilites - grace a la matrice d'adjacence - ce qui
	 * n'aurait pas ete le cas en utilisant simplement un arbre de prefixe). Du
	 * coup, on peut ignorer la case.
	 *)

	let dates = Array.make_matrix (2 * r + 1) (2 * r + 1) 0 in
	let date = ref 0 in

	graph, dates, date ;;

let fov_raycasting r (graph, dates, date) blocked x y =
	(* Les fils d'un noeud et les freres ne sont pas traites par deux
	 * recursivites mais ils sont traites ensemble dans une liste de liste.
	 * Cela permet d'avoir une fonction "tail-recursive" plus rapide.
	 *)

	let rec loop lit = function
		| [] -> lit
		| [] :: todo -> loop lit todo
		| (Node ((x', y'), children) :: brother) :: todo ->
			(* Si la case a deja ete traitee par ailleurs, on ne retraite pas les fils
			 * mais on passe aux freres (ou a todo indirectement).
			 *)
			if dates.(x' + r).(y' + r) = !date then loop lit (brother :: todo) else
			(* Sinon, la case est listee comme visible (meme si elle contient un obstable !).
			 * Par contre si elle contient un obstacle, on ne traite pas ce qui est derierre
			 * (i.e. les fils) et on passe au freres (ou a todo indirectement).
			 *)
			let p = x + x', y + y' in
			let lit = p :: lit in
			if blocked p then loop lit (brother :: todo) else
				(* Sinon on met a jour la date et on passe au fils. *)
				begin
					dates.(x' + r).(y' + r) <- !date ;
					loop lit (children :: brother :: todo)
				end in
	
	date := !date + 1 ;
	loop [] [ graph ]
