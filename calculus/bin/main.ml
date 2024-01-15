type differentiable_1d = float -> float

type vector2d = float * float
type differentiable_2d = vector2d -> float

type vector = float list
type matrix = float list list
type differentiable = vector -> float

let test_functions : differentiable_1d list = [(fun x -> 2.0 *. x); exp; log]
let const (x : float) = (fun (_ : float) -> x)
let round float spots = Float.round (float *. (Float.pow 10. (float_of_int spots))) /. (Float.pow 10. (float_of_int spots))
let round_vector vector = List.map (fun x -> round x 2) vector

let floatEquals actual expected delta =
  actual > expected -. delta && actual < expected +. delta

let epsilon = 0.00001

let differentiate (f : differentiable_1d) (x : float) : float =
  let f_x = f x in
  let f_xplush = f (x +. epsilon) in
  (f_xplush -. f_x) /. (epsilon)

let differentiate_test () =
  let test_results = [2.0; exp 1.0; 1.0] in
  let tests = List.combine test_functions test_results in
  List.for_all (fun (f, x) ->  floatEquals (differentiate f 1.0) x 0.001) tests

let integrate (f : differentiable_1d) (a : float) (b : float) : float =
  let rec loop (curr : float) (acc : float) : float =
    if curr > b then acc else
      loop (curr +. epsilon) (acc +. (f (curr +. epsilon) *. epsilon))
  in loop a 0.0

let integrate_test () =
  let test_results = [1.0; exp 1.0 -. 1.0; -1.] in
  let tests = List.combine test_functions test_results in
  List.for_all (fun (f, x) -> floatEquals (integrate f 0. 1.) x 0.001) tests

;; print_endline ("Differential Tests: " ^ string_of_bool (differentiate_test ()))
;; print_endline ("Integral Tests: " ^ string_of_bool (integrate_test ()))

(* Two dimensions *)

let differentiate_2d (f : differentiable_2d) ((x0,y0) : vector2d) : vector2d =
  let partial_x = differentiate (fun x -> (f (x,y0))) x0 in
  let partial_y = differentiate (fun y -> (f (x0,y))) y0 in
  (partial_x, partial_y)

let integrate_2d (f : differentiable_2d) ((x_a,y_a) : vector2d) ((x_b,y_b) : vector2d) : float =
  let rec loop (curr : float) (acc : float) : float =
    let marginal = integrate (fun x -> f (x, curr)) x_a x_b in
    if curr > y_b then acc else
      loop (curr +. epsilon) (acc +. (marginal *. epsilon))
    in loop y_a 0.

let integrate_path_2d (f : differentiable_2d) (px, py) a b =
  let rec loop curr acc =
    if curr > b then acc else
      let ds = epsilon *. Float.sqrt (Float.pow (differentiate px curr) 2. +. Float.pow (differentiate py curr) 2.) in
      loop (curr +. epsilon) (acc +. (f ((px curr),(py curr))) *. ds)
    in loop a 0.

let path_2d_test = integrate_path_2d (fun (x,y) -> x*.x*.y*.y) ((fun x -> x), (fun x -> x)) 0. (Float.sqrt 2.)

(* n-dimensions *)

let add_epsilon vector i =
  let rec loop vec i acc =
    match vec with
    | [] -> vector
    | v :: tail -> if i = 0 then List.rev acc @ [v +. epsilon] @ tail else loop tail (i - 1) (v :: acc)
  in loop vector i []

let rec ( -- ) a b = if a = b then [] else a :: (a+1) -- b

let gradient (f : differentiable) (position: vector) : vector =
  List.map (fun v -> (f (add_epsilon position v) -. f (position)) /. epsilon) 
    (0 -- List.length position) 
  |> round_vector

let total_derivative fs position : matrix =
  List.map (fun f -> gradient f position) fs

let total_derivative_1d (fs: differentiable_1d list) (xs: vector) : vector =
  List.map2 (fun p x -> differentiate p x) fs xs
  |> round_vector

let integrate_path (f : vector -> float) (p : differentiable_1d list) a b =
  let sum_of_squares = List.fold_left (fun acc x -> acc +. Float.pow x 2.) 0. in
  let rec loop curr acc =
    match curr with
    | i when i > b -> acc
    | _ -> 
      let position = List.map (fun f -> f curr) p in
      let derivative = total_derivative_1d p position in
      let ds = epsilon *. Float.sqrt (sum_of_squares derivative) in
      loop (curr +. epsilon) (acc +. f position *. ds)
    in loop a 0. |> round


let x2y2 = function
| [x;y] -> Float.pow x 2. *. Float.pow y 2.
| _ -> failwith "Incorrect dimensions"

let path_test = integrate_path x2y2 [(fun x -> x); (fun x -> x)] 0. (Float.sqrt 2.)