
%token <string> SYMBOL
%token <string> NUMBER
%token BEGIN END PROCEDURE RETURN
%token EOF

%start <string * string> program
%%
program:
| t = toplevel; EOF
    { t }

toplevel:
    | PROCEDURE; name=SYMBOL; BEGIN; RETURN; number=NUMBER; END
        { (name, number) }
    
