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