exception InvalidToken of char
exception InvalidOperator of char
exception InvalidNum of string
exception DivisionByZero

type association = ASSOC_NONE | ASSOC_RIGHT | ASSOC_LEFT    
type num = {value: float} 
type operator = {op: char; prec: int; assoc: association; unary: bool; eval: num -> num -> float}
type token = Num | Operator 

let eval_uminus x _ = -1. *. x.value 
let eval_exp x y = x.value ** y.value
let eval_add x y = x.value +. y.value
let eval_sub x y = x.value -. y.value (*not x - y since evaluation is done from left to right*)
let eval_mul x y = x.value *. y.value
let eval_div x y = if y.value = 0. then raise DivisionByZero else x.value /. y.value
let eval_none _ _ = 0.

let get_operator (c: char): operator = 
    match c with
    |'_' -> {op = '_'; prec = 10; assoc = ASSOC_RIGHT; unary = true; eval = eval_uminus}
    |'^' -> {op = '^'; prec = 9; assoc = ASSOC_RIGHT; unary = false; eval =  eval_exp}
    |'+' -> {op = '+'; prec = 5; assoc = ASSOC_LEFT; unary = false; eval = eval_add}
    |'-' -> {op = '-'; prec = 5; assoc = ASSOC_LEFT; unary = false; eval = eval_sub}
    |'*' -> {op = '*'; prec = 8; assoc = ASSOC_LEFT; unary = false; eval = eval_mul}
    |'/' -> {op = '/'; prec = 8; assoc = ASSOC_LEFT; unary = false; eval = eval_div}
    |'(' -> {op = '('; prec = 0; assoc = ASSOC_NONE; unary = false; eval = eval_none}
    |')' -> {op = ')'; prec = 0; assoc = ASSOC_NONE; unary = false; eval = eval_none}
    | _ -> raise (InvalidOperator c)

let get_number (s: string): num =
    match s with
    | "e" -> {value = 2.71828}
    | "pi" -> {value = Float.pi}
    | _ -> {value = Float.of_string s}
    
    
let get_token_type (c: char) = 
    match c with
    |'_' | '^' | '+' | '-' | '*'| '/' | '(' | ')' -> Operator
    | '0'..'9' | '.' | 'e' | 'p' | 'i' -> Num
    | _ -> raise (InvalidToken c)