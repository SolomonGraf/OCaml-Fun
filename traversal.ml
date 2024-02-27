type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

let toposort tree =
  let rec dfs t acc = 
    begin match t with
    | Node (Empty, v, Empty) -> v :: acc
    | Node (l, v, r) -> dfs l (dfs r (v :: acc))
    | Empty -> acc
  end in
  List.rev (dfs tree [])

let leaf v = Node (Empty, v, Empty)

let example_tree = Node (leaf 1, 2, leaf 3)

let big_tree = Node (example_tree, 5, Node (Node (leaf 6, 7, Node (leaf 8, 9, leaf 10)), 11, Node (leaf 12, 13, Node (leaf 14, 15, leaf 16))))