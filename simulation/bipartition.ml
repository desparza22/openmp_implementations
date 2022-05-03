
type node = Empty
          | Node of int * node * int list * node;;

let bipart_tree = Node (0, Node (0, Node (0, Node (0, Empty,
                                                   [0;],
                                                   Empty),
                                          [0;1;],
                                          Node (1, Empty,
                                                [1;],
                                                Empty)),
                                 [0;1;4;5;],
                                 Node (1, Node (2, Empty,
                                                [4;],
                                                Empty),
                                       [4;5;],
                                       Node (3, Empty,
                                             [5;],
                                             Empty))),
                        [0;1;2;3;4;5;6;7;],
                        Node (1, Node (2, Node (4, Empty,
                                                [2;],
                                                Empty),
                                       [2;3;],
                                       Node (5, Empty,
                                             [3;],
                                             Empty)),
                              [2;3;6;7;],
                              Node (3, Node (6, Empty,
                                             [6;],
                                             Empty),
                                    [6;7;],
                                    Node (7, Empty,
                                          [7;],
                                          Empty))));;

let rec print_tree = function
  | Empty -> print_string "e\n"
  | Node (id, left, l, right) -> let () = print_tree left in
                                 let () = print_tree right in
                                 let () = List.iter (Printf.printf "%d ") l in
                                 print_string "\n";;

module Searches = Map.Make(Int);;
let key id_a id_b = (Int.shift_left id_a 8) + id_b;;

let rec calc_distances list_a_current list_b list_b_current machine =
  match list_a_current with
  | [] -> 0
  | node_a::rest_a ->
     match list_b_current with
     | [] -> calc_distances rest_a list_b list_b machine
     | node_b::rest_b ->
        let d1 = distance machine node_a node_b in
        let d2 = distance machine node_b node_a in
        Int.div (d1 + d2) 2 + calc_distances list_a_current list_b rest_b machine;;

let node_distances dp node_a node_b machine =
  match node_a with
  | Empty -> raise (Invalid_argument "Empty node_a passed to node_distances\n")
  | Node (id_a, _, list_a, _) ->
     match node_b with
     | Empty -> raise (Invalid_argument "Empty node_b passed to node_distances\n")
     | Node (id_b, _, list_b, _) ->
        if id_a == id_b then (dp, 0) else
        let key_val = key id_a id_b in
        match Searches.mem key_val dp with
        | true -> (dp, Searches.find key_val dp)
        | false -> let distances = calc_distances list_a list_b list_b machine in
                   let new_dp = Searches.add key_val distances dp in
                   (new_dp, distances);;


