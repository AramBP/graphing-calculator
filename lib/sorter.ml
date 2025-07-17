open Parse_token
type stacks = {o_stack: operator Stack.t; n_stack: num Stack.t}
let sorter (expr: string): stacks=
    let expr_length = String.length expr in
    let num_stack = Stack.make 0 {value = 0.} in
    let operator_stack = Stack.make 0 {op = 'X'; prec = 0; assoc = ASSOC_NONE; unary = false; eval = eval_none} in
    
    let rec shunting_yard (s_number: string) (i: int): unit =
        if i >= expr_length then begin
            if String.length s_number > 0 then Stack.push num_stack {value = (Float.of_string s_number)}; 
            ()
        end
        else 
            let c = expr.[i] in
            match get_token_type c with
            | Num -> shunting_yard (s_number ^ (String.make 1 c)) (i+1)
            | Operator -> 
                if String.length s_number > 0 then Stack.push num_stack {value = (Float.of_string s_number)};
                (*let op_stack_length = Stack.length operator_stack in 
                while op_stack_length > 0 do 
                    if 
                done*)
                
                Stack.push operator_stack (get_operator c);
                shunting_yard "" (i+1);
    in
    shunting_yard "" 0;
    {o_stack = operator_stack; n_stack = num_stack}

                





