module UnitTests = struct
  
  let print_test_result name bool = print_endline (name ^ ": " ^ string_of_bool bool); bool

  let assertEquals actual expected name = print_test_result name (actual = expected)
  let assertFloatEquals actual expected delta name = print_test_result name (actual > expected -. delta && actual < expected +. delta)
  let assertTrue actual name = assertEquals actual true name
  let assertFalse actual name = assertTrue (not actual) name

end