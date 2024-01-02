let print_bool bool = print_endline (string_of_bool bool)

let rec last list =
  begin match list with 
  | [] -> None
  | [x] -> Some x
  | _ :: xs -> last xs
end

let test_list = [1;2;3;4]

;; print_bool (Some 4 = last test_list)

let rec last2 list = 
  begin match list with
  | [] | [_] -> None
  | [a;b] -> Some (a,b)
  | _ :: xs -> last2 xs
end

;; print_bool (Some (3,4) = last2 test_list)

let rec ith list i =
  begin match list with
  | [] -> None
  | v :: tail -> if i = 0 then Some v else ith tail (i-1)
end

;; print_bool (Some 3 = ith test_list 3)

let length list =
  let rec loop list acc =
    begin match list with
    | [] -> acc
    | _ :: tail -> loop tail (1 + acc)
  end in
  loop list 0

;; print_bool (4 = length test_list)

let reverse list =
  let rec loop list acc =
    begin match list with
    | [] -> acc
    | v :: tail -> loop tail (v :: acc)
  end in
  loop list []

;; print_bool ([4;3;2;1] = reverse test_list)

let palindrome list = list = reverse list

;; print_bool (palindrome [1;2;3;2;1])

type 'a node =
| One of 'a 
| Many of 'a node list;;

let rec flatten list =
  begin match list with
  | [] -> []
  | One x :: tail -> x :: flatten tail
  | Many x :: tail -> flatten x @ flatten tail
end

;; print_bool (flatten [Many [One 1; One 2]; One 3; Many [Many [One 4; One 5]; One 6]] = [1;2;3;4;5;6])

let rec compress list =
  begin match list with
  | [] -> []
  | [x] -> [x]
  | x :: y :: tail -> if x = y then compress (y :: tail) 
  else x :: compress (y :: tail)
end

;; print_bool (compress [1;1;1;1;1;2;2;2;3;4;4;4;5;5] = test_list)

