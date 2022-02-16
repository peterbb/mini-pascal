
open Printf

let gensym =
    let k = ref 0 in
    fun prefix ->
        let name = sprintf "%s_%d" prefix !k in
        k := 1 + !k;
        name

let rec compile_expr ch = function
    | Ast.Number n ->
        fprintf ch "movq $%s, %%rax\n" n
    | Ast.Plus (e0, e1) ->
        compile_expr ch e0;
        fprintf ch "pushq %%rax\n";
        compile_expr ch e1;
        fprintf ch "popq %%rcx\n";
        fprintf ch "addq %%rcx, %%rax\n"
    | Ast.Equal (e0, e1) ->
        compile_expr ch e0;
        fprintf ch "pushq %%rax\n";
        compile_expr ch e1;
        fprintf ch "popq %%rcx\n";
        fprintf ch "cmp %%rcx, %%rax\n";
        fprintf ch "movq $1, %%rax\n";
        fprintf ch "je 1f\n";
        fprintf ch "movq $0, %%rax\n";
        fprintf ch "1:\n"

let rec compile_stmt ch = function
    | Ast.Return expr ->
        compile_expr ch expr;
        fprintf ch "ret\n"
    | Ast.If (expr, s0, s1) ->
        let else_label = gensym "else" in
        compile_expr ch expr;
        fprintf ch "cmp $1, %%rax\n";
        fprintf ch "jne %s\n" else_label;
        compile_stmt ch s0;
        fprintf ch "%s:\n" else_label;
        compile_stmt ch s1

let compile ch name body =
    fprintf ch ".global %s\n" name;
    fprintf ch "%s:\n" name;
    compile_stmt ch body
    
