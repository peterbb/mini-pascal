
%token <string> SYMBOL
%token <string> NUMBER
%token IF THEN ELSE BEGIN END PROCEDURE RETURN
%token LPAR RPAR 
%token PLUS EQUAL
%token EOF

%start <string * Ast.stmt> program
%%
program:
| t = toplevel; EOF
    { t }

toplevel:
    | PROCEDURE; name=SYMBOL; BEGIN; stmt=stmt; END
        { (name, stmt) }

stmt:
    | RETURN; expr=expr 
        { Ast.Return expr }
    | IF; e=expr; THEN; s0=stmt; ELSE; s1=stmt; END
        { Ast.If (e, s0, s1) }

expr:
    | e0 = expr_1; EQUAL; e1 = expr_1
        { Ast.Equal (e0, e1) }
    | e = expr_1
        { e }

expr_1:
    | e0 = expr_0; PLUS; e1 = expr_1
        { Ast.Plus (e0, e1) }
    | e = expr_0
        { e }

expr_0:
    | number = NUMBER
        { Ast.Number number }
    | LPAR; e=expr; RPAR 
        { e }
    
