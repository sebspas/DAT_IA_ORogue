module PrioQueue =
	struct
		type priority = int
		type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
		let empty = Empty
		exception Queue_is_empty

		let rec insert queue prio elt =
			match queue with
				| Empty -> Node (prio, elt, Empty, Empty)
				| Node (p, e, left, right) ->
						if prio <= p
						then Node (prio, elt, insert right p e, left)
						else Node (p, e, insert right prio elt, left)

		let rec remove_top = function
			| Empty -> raise Queue_is_empty
			| Node (prio, elt, left, Empty) -> left
			| Node (prio, elt, Empty, right) -> right
			| Node (prio, elt, (Node (lprio, lelt, _, _) as left), (Node (rprio, relt, _, _) as right)) ->
					if lprio <= rprio
					then Node(lprio, lelt, remove_top left, right)
					else Node(rprio, relt, left, remove_top right)

		let extract = function
			| Empty -> raise Queue_is_empty
			| Node (prio, elt, _, _) as queue -> (prio, elt, remove_top queue)
	end ;;

module type FUN_ARRAY =
	sig
		type 'a t
		val make : int -> 'a -> 'a t
		val init : int -> (int -> 'a) -> 'a t
		val get : 'a t -> int -> 'a
		val set : 'a t -> int -> 'a -> 'a t
	end ;;

(* Cf.  A persistent union-find data structure Sylvain Conchon, Jean-Christophe Filliâtre *)
module FunArray : FUN_ARRAY =
	struct
		type 'a t = 'a data ref
		and 'a data =
			| Arr of 'a array
			| Diff of int * 'a * 'a t

		let make (n : int) (x : 'a) : 'a t = ref (Arr (Array.make n x))

		let init (n : int) (f : int -> 'a) : 'a t = ref (Arr (Array.init n f))

		let rec reroot (a : 'a t) : unit =
			match !a with
				| Arr _ -> ()
				| Diff (i, x, a') ->
						reroot a' ;
						match !a' with
							| Diff _ -> failwith "impossible"
							| Arr arr ->
								a := Arr arr ;
								a' := Diff (i, arr.(i), a) ;
								arr.(i) <- x

		let rec get (a : 'a t) (i : int) : 'a =
			reroot a ;
			match !a with
				| Diff (j, x, a') -> failwith "impossible"
				| Arr arr -> arr.(i)

		let rec set (a : 'a t) (i : int) (x : 'a) : 'a t =
			reroot a ;
			match !a with
				| Arr arr ->
					let a' = ref (Arr arr) in
					a := Diff (i, arr.(i), a') ;
					arr.(i) <- x ;
					a'
				| Diff _ -> ref (Diff(i, x, a))
	end ;;
