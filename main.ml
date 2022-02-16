let () = print_endline "hello world"

let inch = open_in Sys.argv.(1)
let lexbuf = Lexing.from_channel inch
let () = lexbuf.lex_start_p <- Lexing.{
    pos_fname = Sys.argv.(1);
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0
}
let () = lexbuf.lex_curr_p <- lexbuf.lex_start_p
let (name, number) = 
    try Parser.program Lexer.read_token lexbuf
    with Parser.Error -> begin
        let Lexing.{pos_fname; pos_lnum; pos_cnum; pos_bol} = lexbuf.lex_curr_p in
        Printf.printf "parse error %s line %d col %d\n" pos_fname pos_lnum (pos_cnum - pos_bol);
        exit 1
    end
let () = close_in inch

let outch = open_out Sys.argv.(2)
let () = Compile.compile outch name number
let () = close_out outch
let () = print_endline "ok"

