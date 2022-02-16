
open Printf

let rec compile_expr ch = function
    | Ast.Number n ->
        fprintf ch "movq $%s, %%rax\n" n
    | Ast.Plus (e0, e1) ->
        compile_expr ch e0;
        fprintf ch "pushq %%rax\n";
        compile_expr ch e1;
        fprintf ch "popq %%rcx\n";
        fprintf ch "addq %%rcx, %%rax\n"
    


let compile ch name ret_expr =
    fprintf ch ".global %s\n" name;
    fprintf ch "%s:\n" name;
    compile_expr ch ret_expr;
    fprintf ch "ret\n"

