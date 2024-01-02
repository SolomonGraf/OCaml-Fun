module type SET =
  sig
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
module SET : SET
