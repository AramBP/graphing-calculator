open Graphing_calculator.Sorter
open Graphing_calculator.Parse_token

let () = 
  let expression = "2+3" in
  let stacks = sorter expression in
  
  (* debug *)
  Array.iter (fun s -> print_float s.value) stacks.n_stack.data;
  print_endline "";
  Array.iter (fun s -> print_char s.op) stacks.o_stack.data;
  print_endline "";


