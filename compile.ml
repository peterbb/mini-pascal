
open Printf

let max_list = List.fold_left max 0


let rec local_vars = function
    | Ast.Return _ -> 0
    | Ast.Assign _ -> 0
    | Ast.If (_, s0, s1, None) -> max (local_vars s0) (local_vars s1)
    | Ast.If (_, s0, s1, Some k) -> max_list [local_vars s0; local_vars s1; local_vars k]
    | Ast.Let (_, _, s) -> 1 + local_vars s
    | Ast.While (_, s, None) -> local_vars s
    | Ast.While (_, s, Some k) -> max (local_vars s) (local_vars k)

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

let var_offset env var =
    let index = List.rev env |> index_of var in
    8 * (index + 1)

let rec compile_expr ch env = function
    | Ast.Var n ->
        fprintf ch "movq -%d(%%rbp), %%rax\n" (var_offset env n)
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
    | Ast.Less (e0, e1) ->
        compile_expr ch env e0;
        fprintf ch "pushq %%rax\n";
        compile_expr ch env e1;
        fprintf ch "popq %%rcx\n";
        fprintf ch "cmp %%rax, %%rcx\n";
        fprintf ch "movq $1, %%rax\n";
        fprintf ch "jl 1f\n";
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
        let env = name::env in
        fprintf ch "movq %%rax, -%d(%%rbp)\n" (var_offset env name);
        compile_stmt ch (name::env) stmt
    | Ast.Assign (name, expr, stmt) ->
        compile_expr ch env expr;
        fprintf ch "movq %%rax, -%d(%%rbp)\n" (var_offset env name);
        begin match stmt with
        | None -> ()
        | Some stmt -> compile_stmt ch env stmt
        end
    | Ast.While (expr, stmt, k) ->
        let loop_start = gensym "while_start" in
        let loop_end = gensym "while_end" in
        fprintf ch "%s:\n" loop_start;
        compile_expr ch env expr;
        fprintf ch "cmp $0, %%rax\n";
        fprintf ch "je %s\n" loop_end;
        compile_stmt ch env stmt;
        fprintf ch "jmp %s\n" loop_start;
        fprintf ch "%s:\n" loop_end;
        begin match k with
        | None -> ()
        | Some k -> compile_stmt ch env k
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
    
