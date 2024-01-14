type differentiable = float -> float

let test_functions = [(fun x -> 2.0 *. x); exp; (fun x -> 1.0 /. x); log]

let floatEquals actual expected delta =
  actual > expected -. delta && actual < expected +. delta

let epsilon = 0.0001

let differentiate (f : differentiable) (x : float) : float =
  let f_x = f x in
  let f_xplush = f (x +. epsilon) in
  (f_xplush -. f_x) /. (epsilon)

let differentiate_test () =
  let test_results = [2.0; 1.0; -1.0; 1.0] in
  let tests = List.combine test_functions test_results in
  List.for_all (fun (f, x) -> floatEquals (differentiate f 1.0) x 0.001) tests

let integrate (f : differentiable) (a : float) (b : float) : float =
  let rec loop (curr : float) (acc : float) : float =
    if curr > b then acc else
      loop (curr +. epsilon) (acc +. (f curr *. epsilon))
  in loop a 0.0