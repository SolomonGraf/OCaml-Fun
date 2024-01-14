type differentiable = float -> float

type vector2d = float * float
type differentiable_2d = point2d -> float

let test_functions : differentiable list = [(fun x -> 2.0 *. x); exp; log]
let const (x : float) = (fun (_ : float) -> x)

let floatEquals actual expected delta =
  actual > expected -. delta && actual < expected +. delta

let epsilon = 0.00001

let differentiate (f : differentiable) (x : float) : float =
  let f_x = f x in
  let f_xplush = f (x +. epsilon) in
  (f_xplush -. f_x) /. (epsilon)

let differentiate_test () =
  let test_results = [2.0; exp 1.0; 1.0] in
  let tests = List.combine test_functions test_results in
  List.for_all (fun (f, x) ->  floatEquals (differentiate f 1.0) x 0.001) tests

let integrate (f : differentiable) (a : float) (b : float) : float =
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