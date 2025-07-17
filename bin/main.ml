open Graphing_calculator.Calc

let () = 
  let expression = "2*(5*(3+6))/15-2" in
  let result = calculator expression in

  print_float result;
  print_endline "";



