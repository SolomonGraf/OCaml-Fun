type 'a tree = Node of 'a * 'a tree list

let rec map_tree f tree = match tree with Node (v, branches) -> Node (f v, List.map (map_tree f) branches)

let rec fold_tree branch_fold root_fold tree =
  let Node (root, branches) = tree in
  root_fold (branch_fold (List.map (fun x -> fold_tree branch_fold root_fold x) branches)) root

let rec mem tree elt = let Node (v, branches) = 
  tree in v = elt || List.exists (fun b -> mem b elt) branches

let max = function
| [] -> failwith "empty list"
| v :: tail -> (List.fold_right (max) tail v)

let rec height tree = let Node (_, branches) = tree in match branches with
| [] -> 0
| _ -> 1 + max (List.map (height) branches)