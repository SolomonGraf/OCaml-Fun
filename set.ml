module type SET = sig

  type 'a set

  val empty : 'a set
  val add : 'a -> 'a set -> 'a set
  val remove : 'a -> 'a set -> 'a set
  val member : 'a -> 'a set -> bool

  val intersection : 'a set -> 'a set -> 'a set
  val union : 'a set -> 'a set -> 'a set
  val symmetric_difference : 'a set -> 'a set -> 'a set
  val difference : 'a set -> 'a set -> 'a set
  val powerset : 'a set -> 'a set set

  val list_of_set : 'a set -> 'a list
  val set_of_list : 'a list -> 'a set
  
end

module SET : SET = struct

  type 'a set = 'a list

  let empty = []

  let add v set = v :: set

  let rec remove v set =
    begin match set with
    | [] -> []
    | hd :: tl -> if v = hd then tl else v :: remove v tl
  end

  let member v set = List.fold_right (fun x acc -> v = x || acc) set false

  let union set1 set2 = (List.filter (fun x -> not (member x set1)) set2) @ set1

  let intersection set1 set2 = List.filter (fun x -> member x set1) set2

  let difference set1 set2 = List.filter (fun x -> not (member x set2)) set1

  let symmetric_difference set1 set2 = difference (union set1 set2) (intersection set1 set2)

  let powerset (set: 'a set) : 'a set set =
    let rec lists_with_and_without (l : 'a list list) (v : 'a) : 'a list list =
      begin match l with
      | [] -> l
      | hd :: tl -> (v::hd) :: lists_with_and_without tl v
    end in
    List.fold_left lists_with_and_without [] set

  let list_of_set set = set

  let set_of_list (list : 'a list) : 'a set = List.fold_right (fun x acc -> add x acc) list empty

end