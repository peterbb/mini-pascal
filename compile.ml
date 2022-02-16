
open Printf

let rec local_vars = function
    | Ast.Return _ -> 0
    | Ast.Assign _ -> 0
    | Ast.If (_, s0, s1, None) -> max (local_vars s0) (local_vars s1)
    | Ast.If (_, s0, s1, Some k) -> max (local_vars s0) (max (local_vars s1) (local_vars k))
    | Ast.Let (_, _, s) -> 1 + local_vars s

let index_of x =
    let rec loop i = function 
        | [] -> raise Not_found
        | y::_ when x = y -> i
        | _::rest -> loop (i + 1) rest
    in
    loop 0

let gensym =
    let k = ref 0 in
    fun prefix ->
        let name = sprintf "%s_%d" prefix !k in
        k := 1 + !k;
        name

let rec compile_expr ch env = function
    | Ast.Var n ->
        fprintf ch "movq -%d(%%rbp), %%rax\n" (8 * (List.rev env |> index_of n))
    | Ast.Number n ->
        fprintf ch "movq $%s, %%rax\n" n
    | Ast.Plus (e0, e1) ->
        compile_expr ch env e0;
        fprintf ch "pushq %%rax\n";
        compile_expr ch env e1;
        fprintf ch "popq %%rcx\n";
        fprintf ch "addq %%rcx, %%rax\n"
    | Ast.Equal (e0, e1) ->
        compile_expr ch env e0;
        fprintf ch "pushq %%rax\n";
        compile_expr ch env e1;
        fprintf ch "popq %%rcx\n";
        fprintf ch "cmp %%rcx, %%rax\n";
        fprintf ch "movq $1, %%rax\n";
        fprintf ch "je 1f\n";
        fprintf ch "movq $0, %%rax\n";
        fprintf ch "1:\n"

let rec compile_stmt ch env = function
    | Ast.Return expr ->
        compile_expr ch env expr;
        fprintf ch "leave\n";
        fprintf ch "ret\n"
    | Ast.If (expr, s0, s1, k) ->
        let else_label = gensym "else" in
        let if_end = gensym "if_end" in
        compile_expr ch env expr;
        fprintf ch "cmp $1, %%rax\n";
        fprintf ch "jne %s\n" else_label;
        compile_stmt ch env s0;
        fprintf ch "jmp %s\n" if_end;
        fprintf ch "%s:\n" else_label;
        compile_stmt ch env s1;
        fprintf ch "%s:\n" if_end;
        begin match k with
        | None -> ()
        | Some s -> compile_stmt ch env s
        end

    | Ast.Let (name, expr, stmt) ->
        compile_expr ch env expr;
        fprintf ch "movq %%rax, -%d(%%rbp)\n" (8 * List.length env);
        compile_stmt ch (name::env) stmt
    | Ast.Assign (name, expr, stmt) ->
        let ix = index_of name (List.rev env) in
        compile_expr ch env expr;
        fprintf ch "movq %%rax, -%d(%%rbp)\n" (ix * 8);
        begin match stmt with
        | None -> ()
        | Some stmt -> compile_stmt ch env stmt
        end

let stack_size n = 
    let size = 8 * n in
    if size mod 16 = 0 then
        size
    else 
        size + 8
    
let compile ch name body =
    fprintf ch ".global %s\n" name;
    fprintf ch "%s:\n" name;
    fprintf ch "push %%rbp\n";
    fprintf ch "mov %%rsp, %%rbp\n";
    fprintf ch "sub $%d, %%rsp\n" (stack_size (local_vars body));
    compile_stmt ch [] body
    
