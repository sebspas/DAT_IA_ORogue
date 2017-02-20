type terrain =
	| Water
	| Forest
	| Ground
	| Rock
	| Road
	| Floor
	| Wall
	| Door

let terrain_to_int = function
	| Water -> 0
	| Forest -> 1
	| Ground -> 2
	| Rock -> 3
	| Road -> 4
	| Floor -> 5
	| Wall -> 6
	| Door -> 7

let int_to_terrain = function
	| 0 -> Water
	| 1 -> Forest
	| 2 -> Ground
	| 3 -> Rock
	| 4 -> Road
	| 5 -> Floor
	| 6 -> Wall
	| 7 -> Door
	| _ -> failwith "invalid int for terrain" ;;
