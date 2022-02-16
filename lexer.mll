{ open Parser }

let newline = "\n" | "\r" | "\r\n"  

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let symbol_init = alpha | [ '_' ]
let symbol_rest = symbol_init | digit 
let symbol = symbol_init symbol_rest*
let number = digit+

rule read_token = parse
| [' ' '\t']+ { read_token lexbuf }
| newline { Lexing.new_line lexbuf; read_token lexbuf }

| "=" { EQUAL }
| "+" { PLUS }
| ":=" { ASSIGN }

| "(" { LPAR }
| ")" { RPAR }

| "procedure" { PROCEDURE }
| "begin" { BEGIN }
| "end" { END }
| "return" { RETURN }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "let" { LET }


| symbol { SYMBOL (Lexing.lexeme lexbuf) }
| number { NUMBER (Lexing.lexeme lexbuf) }
| eof { EOF }

