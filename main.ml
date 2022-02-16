


let () = print_endline "hello world"

let inch = open_in Sys.argv.(1)
let lexbuf = Lexing.from_channel inch
let (name, number) = Parser.program Lexer.read_token lexbuf
let () = close_in inch
let () = Printf.printf "name %s => number %s\n" name number

let outch = open_out Sys.argv.(2)
let () = Compile.compile outch name number
let () = close_out outch
let () = print_endline "ok"

