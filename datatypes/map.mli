module MAP :
  sig
    type ('k, 'v) map = ('k * 'v) list
    val empty : ('k, 'v) map
    val put : ('k * 'v) list -> 'k -> 'v -> ('k, 'v) map
    val remove : ('k * 'v) list -> 'k -> ('k, 'v) map
    val key_in_map : ('a * 'b) list -> 'a -> bool
    val value_in_map : ('a * 'b) list -> 'b -> bool
    val list_of_map : 'a -> 'a
    val map_of_list : ('k * 'v) list -> ('k, 'v) map
    val get : ('a * 'b) list -> 'a -> 'b option
  end
