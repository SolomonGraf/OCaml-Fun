module MAP = struct

  type ('k, 'v) map = ('k * 'v) list

  let empty : ('k, 'v) map = []
  let put map key value : ('k, 'v) map = (key,value) :: map
  let rec remove map key : ('k, 'v) map =
    begin match map with
    | [] -> []
    | (k, v) :: tail -> if key = k then tail else (k, v) :: remove tail key
  end
  let rec key_in_map map key : bool = List.exists (fun (k,v) -> k = key) map
  let rec value_in_map map value : bool = List.exists (fun (k,v) -> v = value) map

  let list_of_map map = map
  let map_of_list (list : ('k * 'v) list) : ('k,'v) map = List.fold_right (fun (k,v) acc -> put acc k v) empty list

  let rec get map elem = match map with
| [] -> None
| (k, v) :: tail -> if elem = k then Some v else get tail elem

end