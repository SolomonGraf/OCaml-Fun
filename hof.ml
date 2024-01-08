(* List HOFs *)

let rec map f list = match list with [] -> [] | v :: tail -> f v :: map f tail

(* fold left *)
let fold combine base list =
  let rec loop combine list acc = match list with [] -> acc | v :: tail -> loop combine tail (combine v acc) in
  loop combine list base

(* Function HOFs *)

(* adds single guard statement to function *)
let guard (func : 'a -> 'b) (pred : 'a -> bool) : 'a -> 'b option = fun x ->
  if pred x then Some (func x) else None

let guard_opt (func : 'a -> 'b option) (pred : 'a -> bool) : 'a -> 'b option = fun x ->
  if pred x then func x else None

(* adds multiple guard statements to function *)
let guards (func : 'a -> 'b) (preds : ('a -> bool) list) : 'a -> 'b option = fun x ->
  if (fold (&&) true (map (fun f -> f x) preds)) then Some (func x) else None

let compose (functions : ('a -> 'a) list) : 'a -> 'a = fun x ->
  fold (fun f acc -> f acc) x functions

type ('a, 'b) call_counter = ('a -> 'b) * (unit -> int)

let make_call_counter (f : 'a -> 'b) : ('a, 'b) call_counter =
  let count = ref 0 in
  let counted_func (v : 'a) = begin
  count := 1 + !count;
  f v
  end in
  (counted_func, fun () -> !count)

let for_all pred list = fold (&&) true (map pred list)
let exists pred list = fold (||) false (map pred list)
let all elem preds = for_all (fun pred -> pred elem) preds
let any elem preds = exists (fun pred -> pred elem) preds

(* Tree HOFs *)

type 'a tree = Node of 'a * 'a tree list

let rec map_tree f tree = match tree with Node (v, branches) -> Node (f v, map (map_tree f) branches)