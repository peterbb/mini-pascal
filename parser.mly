
%token <string> SYMBOL
%token <string> NUMBER
%token IF THEN ELSE LET BEGIN END PROCEDURE RETURN
%token LPAR RPAR 
%token PLUS EQUAL ASSIGN
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
    | LET; n=SYMBOL; ASSIGN; e=expr; stmt=stmt
        { Ast.Let (n, e, stmt) }
    | n=SYMBOL; ASSIGN; e=expr; stmt=stmt?
        { Ast.Assign (n, e, stmt) }
    | RETURN; expr=expr 
        { Ast.Return expr }
    | IF; e=expr; THEN; s0=stmt; ELSE; s1=stmt; END; k=stmt?
        { Ast.If (e, s0, s1, k) }

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
    | n=SYMBOL
        { Ast.Var n }
    | LPAR; e=expr; RPAR 
        { e }
    
