
%token <string> SYMBOL
%token <string> NUMBER
%token BEGIN END PROCEDURE RETURN
%token LPAR RPAR
%token PLUS
%token EOF

%start <string * Ast.expr> program
%%
program:
| t = toplevel; EOF
    { t }

toplevel:
    | PROCEDURE; name=SYMBOL; BEGIN; RETURN; expr=expr; END
        { (name, expr) }

expr:
    | e0 = expr_0; PLUS; e1 = expr
        { Ast.Plus (e0, e1) }
    | e = expr_0
        { e }

expr_0:
    | number = NUMBER
        { Ast.Number number }
    | LPAR; e=expr; RPAR 
        { e }
    
