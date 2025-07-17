type association = ASSOC_NONE | ASSOC_RIGHT | ASSOC_LEFT    
type num = {value: float}
type operator = {op: char; prec: int; assoc: association; unary: bool; eval: num -> num -> float; is_op: bool}

let eval_uminus x _ = -1. *. x.value 
let eval_exp x y = x.value ** y.value
let eval_add x y = x.value +. y.value
let eval_sub x y = x.value -. y.value
let eval_mul x y = x.value *. y.value
let eval_div x y = x.value /. y.value
let eval_none _ _ = 0.

let get_op c = 
    match c with
    |'_' -> {op = '_'; prec = 10; assoc = ASSOC_RIGHT; unary = true; eval = eval_uminus; is_op = true}
    |'^' -> {op = '^'; prec = 9; assoc = ASSOC_RIGHT; unary = false; eval =  eval_exp; is_op = true}
    |'+' -> {op = '+'; prec = 5; assoc = ASSOC_LEFT; unary = false; eval = eval_add; is_op = true}
    |'-' -> {op = '-'; prec = 5; assoc = ASSOC_LEFT; unary = false; eval = eval_sub; is_op = true}
    |'*' -> {op = '*'; prec = 8; assoc = ASSOC_LEFT; unary = false; eval = eval_mul; is_op = true}
    |'/' -> {op = '/'; prec = 8; assoc = ASSOC_LEFT; unary = false; eval = eval_div; is_op = true}
    |'(' -> {op = '('; prec = 0; assoc = ASSOC_NONE; unary = false; eval = eval_none; is_op = true}
    |')' -> {op = ')'; prec = 0; assoc = ASSOC_NONE; unary = false; eval = eval_none; is_op = true}
    | _ -> {op = 'X'; prec = 0; assoc = ASSOC_NONE; unary = false; eval = eval_none; is_op = false}

exception EmptyStack
type 'a t = {
  default: 'a;
  mutable size: int;
  mutable data: 'a array;
}


let length (a: 'a t): int = a.size
let make (n: int) (d: 'a): 'a t = {default = d; size = n; data = Array.make n d}
let push (stack: 'a t) (value: 'a): unit =
  let new_size = stack.size + 1 in
  let a'= Array.make new_size value in
  Array.blit stack.data 0 a' 0 stack.size;
  stack.data <- a';
  stack.size <- new_size

let pop (stack: 'a t): 'a =
  if (stack.size <= 0) then raise EmptyStack;
  let new_size = stack.size - 1 in
  let last_elem = Array.get stack.data new_size in
  let a' = Array.make new_size stack.default in
  Array.blit stack.data 0 a' 0 new_size;
  stack.data <- a';
  stack.size <- new_size;
  last_elem

let sorter expr = 
    let operator_stack = make 64 {op = 'X'; prec = 0; assoc = ASSOC_NONE; unary = false; eval = eval_none; is_op = false} in
    
    




