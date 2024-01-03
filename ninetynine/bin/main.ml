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

let valid_sublist list sublist =
  List.for_all (fun x -> List.mem x list) sublist

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
  valid_sublist letter_list actual && (3 = length actual)

;; print_test_result test

let lotto_select amount bound = rand_select (range 1 bound) amount

let test () =
  let range = range 1 50 in
  let actual = lotto_select 6 50 in
  valid_sublist range actual && (6 = length actual)

;; print_test_result test

let permutation list = rand_select list (length list)

let test () =
  let letter_list = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] in
  let actual = permutation letter_list in
  valid_sublist letter_list actual && (length letter_list = length actual)

;; print_test_result test