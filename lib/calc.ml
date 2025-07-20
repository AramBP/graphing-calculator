open Parse_token

let calculate (num_stack) (operator_stack): float =
    let op' = Stack.pop operator_stack in 
    match op'.op with
    | '_' -> eval_uminus (Stack.pop num_stack) 0.
    | _ -> op'.eval (Stack.pop num_stack) (Stack.pop num_stack)

let calculator (expr: string): float =
    let expr_length = String.length expr in
    let num_stack = Stack.make 0 {value = 0.} in
    let operator_stack = Stack.make 0 {op = 'X'; prec = 0; assoc = ASSOC_NONE; unary = false; eval = eval_none} in

    let rec eval_infix_expression (s_number: string) (i: int): unit =
        (*debug*)
        Array.iter (fun s -> print_float s.value) num_stack.data;
        print_endline "";
        Array.iter (fun s -> print_char s.op) operator_stack.data;
        print_endline "";


        if i >= expr_length then begin
            if String.length s_number > 0 then Stack.push num_stack (get_number s_number); 
            ()
        end
        else 
            let c = expr.[i] in
            match get_token_type c with
            | Num -> eval_infix_expression (s_number ^ (String.make 1 c)) (i+1)
            | Operator -> 
                if String.length s_number > 0 then Stack.push num_stack (get_number s_number);
                let op_c = get_operator c in
                match op_c.op with 
                | '(' -> 
                    Stack.push operator_stack (op_c);
                    eval_infix_expression "" (i+1)
                | ')' -> 
                    while (Stack.peek operator_stack).op != '(' do 
                        let ans = calculate num_stack operator_stack in
                        Stack.push num_stack {value = ans};
                    done;
                    Stack.del operator_stack;
                    eval_infix_expression "" (i+1)
                | _ -> 
                    while (Stack.length operator_stack > 0) && ((Stack.peek operator_stack).prec >= op_c.prec) do 
                        let ans = calculate num_stack operator_stack in
                        Stack.push num_stack {value = ans};
                    done;
                    Stack.push operator_stack op_c;
                    eval_infix_expression "" (i+1);
    in
    eval_infix_expression "" 0;
    while Stack.length operator_stack > 0 do 
        let ans = calculate num_stack operator_stack in
        Stack.push num_stack {value = ans};
    done; 
    (Stack.pop num_stack).value
                





