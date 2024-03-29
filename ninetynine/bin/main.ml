let make_call_counter (func : 'a -> unit) =
  let count = ref 0 in
  let counted_func (v : 'a) = begin
  count := 1 + !count;
  print_string ((string_of_int !count) ^ ".");
  func v
  end in counted_func


let print_bool bool = print_endline (string_of_bool bool)
let print_result = make_call_counter print_bool
let print_test_result test = print_result (test ())
let print_skip = print_result true

let sublist list sublist =
  List.for_all (fun x -> List.mem x list) sublist

let equal list1 list2 = sublist list1 list2 && sublist list2 list1

let rec last list =
  begin match list with 
  | [] -> None
  | [x] -> Some x
  | _ :: xs -> last xs
end

let test_list = [1;2;3;4]

;; print_result (Some 4 = last test_list)

let rec last2 list = 
  begin match list with
  | [] | [_] -> None
  | [a;b] -> Some (a,b)
  | _ :: xs -> last2 xs
end

;; print_result (Some (3,4) = last2 test_list)

let rec ith list i =
  begin match list with
  | [] -> None
  | v :: tail -> if i = 0 then Some v else ith tail (i-1)
end

;; print_result (Some 3 = ith test_list 2)

let length list =
  let rec loop list acc =
    begin match list with
    | [] -> acc
    | _ :: tail -> loop tail (1 + acc)
  end in
  loop list 0

;; print_result (4 = length test_list)

let reverse list =
  let rec loop list acc =
    begin match list with
    | [] -> acc
    | v :: tail -> loop tail (v :: acc)
  end in
  loop list []

;; print_result ([4;3;2;1] = reverse test_list)

let palindrome list = list = reverse list

;; print_result (palindrome [1;2;3;2;1])

type 'a node =
| One of 'a 
| Many of 'a node list;;

let rec flatten list =
  begin match list with
  | [] -> []
  | One x :: tail -> x :: flatten tail
  | Many x :: tail -> flatten x @ flatten tail
end

;; print_result (flatten [Many [One 1; One 2]; One 3; Many [Many [One 4; One 5]; One 6]] = [1;2;3;4;5;6])

let rec compress list =
  begin match list with
  | [] -> []
  | [x] -> [x]
  | x :: y :: tail -> if x = y then compress (y :: tail) 
  else x :: compress (y :: tail)
end

;; print_result (compress [1;1;1;1;1;2;2;2;3;4;4;4] = test_list)

let pack list =
  let rec loop list current acc =
    begin match list with
    | [] -> acc
    | [x] -> (x :: current) :: acc
    | x :: y :: tail -> if x = y then loop (y :: tail) (x :: current) acc
                                 else loop (y :: tail) [] ((x :: current) :: acc)
    end in List.rev (loop list [] [])

;; print_result (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]])

let encode list =
  let rec loop list current acc =
    begin match list with
    | [] -> acc
    | [x] -> (1 + current, x) :: acc
    | x :: y :: tail -> if x = y 
      then loop (y :: tail) (1 + current) acc
      else loop (y :: tail) 0 ((current + 1, x) :: acc)
    end in List.rev (loop list 0 [])

;; print_result ((encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]) =
  [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")])

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

(* encode modified im lazy *)

let encode_modified list =
  let make_tuple amount value =
    if amount = 1 then One value else Many (amount, value)
  in
  let rec loop list current acc =
    begin match list with
    | [] -> acc
    | [x] -> make_tuple (current + 1) x :: acc
    | x :: y :: tail -> if x = y 
      then loop (y :: tail) (1 + current) acc
      else loop (y :: tail) 0 (make_tuple (current + 1) x :: acc)
    end in List.rev (loop list 0 [])

;; print_result (encode_modified ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
    [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
     Many (4, "e")])

let decode list =
  let rec replicate_single v count acc =
    if count = 0 then acc else replicate_single v (count - 1) (v :: acc)
  in
  let decode_single rle =
    begin match rle with
    | One v -> [v]
    | Many (count, v) -> replicate_single v count []
  end in
  let rec loop list acc =
    begin match list with
    | [] -> List.rev acc
    | v :: tail -> loop tail (decode_single v @ acc)
  end in loop list []

;; print_result (["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")])

;; print_result true (* direct rle is same as previously implemented *)

let duplicate list =
  let rec loop list acc =
    begin match list with 
    | [] -> acc
    | v :: tail -> loop tail (v :: v :: acc)
  end in List.rev (loop list [])

;; print_result (duplicate [1;2;3] = [1;1;2;2;3;3])

let replicate list count =
  let rec replicate_single v count acc =
    if count = 0 then acc else replicate_single v (count - 1) (v :: acc)
  in
  let rec loop list acc =
    begin match list with
    | [] -> acc
    | v :: tail -> loop tail ((replicate_single v count []) @ acc)
  end in List.rev (loop list [])

;; print_result ((replicate ["a"; "b"; "c"] 3) = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"])

let drop list n =
  let rec loop list x =
    begin match list with
    | [] -> []
    | v :: tail -> if x = 1 then loop tail n else v :: loop tail (n-1)
  end in
loop list n

;; print_result (drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
)
let split list count =
  let rec loop list n acc =
    begin match list with
    | [] -> (List.rev acc,[])
    | v :: tail -> if n = count then (List.rev acc, list) else loop tail (n + 1) (v :: acc)
  end in loop list 0 []

let splitted = split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3

;; print_result (splitted = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]))

let rec slice list start stop =
  begin match list with
  | [] -> []
  | v :: tail -> if start > 0 then slice tail (start - 1) (stop - 1)
  else if stop < 0 then [] else v :: slice (tail) (start - 1) (stop - 1)
end

let test () =
  let expected = [2;3;4;5] in
  let actual = slice [1;2;3;4;5;6] 1 4 in
  expected = actual

;; print_test_result test

let rotate list amount =
  let len = List.length list in
  let splitted = split list ((amount + (len * Int.abs amount)) mod len) in
  snd splitted @ fst splitted

let test () =
  let expected = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"] in
  let actual = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) in
  expected = actual

;; print_test_result test

let rec remove list pos =
  begin match list with
  | [] -> []
  | v :: tail -> if pos = 0 then tail else v :: remove tail (1 - pos)
end

let test () =
  let expected = ["a";"c";"d"] in
  let actual = remove ["a"; "b"; "c"; "d"] 1 in
  expected = actual

;; print_test_result test

let insert list pos x = 
  let rec loop list pos acc =
    begin match list with
    | [] -> List.rev acc
    | v :: tail -> if pos = 0 then (List.rev acc) @ (x :: list) else loop tail (pred pos) (v :: acc)
  end in loop list pos []

let test () =
  let expected = ["a";"b";"c";"alfa";"d"] in
  let actual = insert ["a";"b";"c";"d"] 3 "alfa" in
  actual = expected

;; print_test_result test

let range start stop =
  let rec loop start stop step =
    if start = stop then [start] else start :: (loop (start + step) stop step)
  in loop start stop ((stop - start) / (abs (stop - start)))

let test () =
  let expected = [9;8;7;6;5] in
  let actual = range 9 5 in
  actual = expected

;; print_test_result test

let rec rand_select (list : 'a list) (amt : int) : 'a list =
  if amt = 0 then [] else
  let index = (Random.int (List.length list)) in
  let vo = ith list index in
  begin match vo with
  | None -> []
  | Some v -> v :: rand_select (remove list index) (amt - 1)
  end

let test () =
  let letter_list = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] in
  let actual = rand_select letter_list 3 in
  sublist letter_list actual && (3 = length actual)

;; print_test_result test

let lotto_select amount bound = rand_select (range 1 bound) amount

let test () =
  let range = range 1 50 in
  let actual = lotto_select 6 50 in
  sublist range actual && (6 = length actual)

;; print_test_result test

let permutation list = rand_select list (length list)

let test () =
  let letter_list = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] in
  let actual = permutation letter_list in
  sublist letter_list actual && (length letter_list = length actual)

;; print_test_result test

let rec extract size list =
  if size = 0
  then [[]]
  else
    begin match list with 
    | [] -> []
    | v :: tail -> let included = (List.map (fun x -> v :: x) (extract (size - 1) tail)) in
    let excluded = extract (size) tail in included @ excluded
  end

let test () =
  let actual = extract 2 ["a"; "b"; "c"; "d"] in
  let expected = [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]] in
  equal actual expected

;; print_test_result test

let rec group list sizes =
  let group_aux size list =
    let extracts = extract size list in
    let remaining original extracted = List.filter (fun x -> not (List.mem x original)) extracted in
    List.map (fun x -> (x, remaining list x)) extracts in
  begin match sizes with
  | [] -> [[]]
  | v :: tail -> group_aux v list |> List.map (fun (ex, rem) -> group rem tail |> List.map (fun x -> ex :: x)) |> List.flatten
  end


;; print_test_result (fun () -> group [] [] = [])

let rec length_sort = function
| [] -> []
| v :: tail -> let (l, r) = List.partition (fun x -> length x < length v) tail in
(length_sort l) @ [v] @ (length_sort r)

let test () =
  let expected = [[1];[1;2];[1;2;3;4];[1;2;3;4;5;6]] in
  let actual = length_sort [[1;2;3;4];[1;2];[1;2;3;4;5;6];[1]] in
  actual = expected

;; print_test_result test

let frequency elem list =
  let rec loop list acc =
    match list with [] -> acc | v :: tail -> loop tail (acc + (if v = elem then 1 else 0))
  in loop list 0

let rec frequency_sort list = match list with
| [] -> []
| v :: tail -> let (l,r) = List.partition (fun x -> frequency x list < frequency v list) tail in (frequency_sort l) @ [v] @ (frequency_sort r)

let test () =
  let actual = frequency_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
  ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]] in
  let expected =
[["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"];
["d"; "e"]; ["m"; "n"]] in actual = expected

  ;; print_test_result test

;; print_test_result (fun () -> true)

let is_prime int =
  int <> 1 && List.for_all (fun x -> (int mod x) <> 0) (range 2 (int - 1))

let test () = is_prime 7 = true && not (is_prime 1) && not (is_prime 100)
;; print_test_result test

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let test () = 3 = gcd 6 3
;; print_test_result test

let coprime a b = 1 = gcd a b

let test () = coprime 7 8
;; print_test_result test

let phi n =
  let rec loop i acc =
    if i > n then acc else loop (succ i) (acc + (if coprime i n then 1 else 0)) 
  in if n = 1 then 1 else loop 1 0

let test () = 1 = phi 1 && 4 = phi 10
;; print_test_result test

let factorization n =
  let rec loop i acc =
    if acc = 1 then [] else
      if (acc mod i = 0) then i :: loop i (acc / i) else loop (i + 1) acc
  in loop 2 n

let test () = [2;2;3] = factorization 12
;; print_test_result test

let rle_factorization n = List.map (fun (x,y) -> (y,x)) (encode (factorization n))

let test () =
  let actual = rle_factorization 315 in
  let expected = [(3, 2); (5, 1); (7, 1)] in
  actual = expected

;; print_test_result test

let phi_improved n =
  let power n p = let rec aux n p acc = if p = 0 then acc else aux n (p - 1) (n * acc) in aux n p 1 in
  let rec loop list acc = 
    match list with
    | [] -> acc
    | (p,m) :: tail -> loop tail (acc * (p - 1) * (power p (m - 1))) in
  let factors = rle_factorization n in loop factors 1

let test () =
  4 = phi_improved 10 && 12 = phi_improved 13

;; print_test_result test

;; print_skip (* skipping timing the two different implementations of phi *)

