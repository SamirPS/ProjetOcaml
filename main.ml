(* File calc.ml *)


let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let _ = Parser.main Lexer.token lexbuf in
      print_string "Commande mise en place"; print_newline(); flush stdout
    done
  with Lexer.Eof ->
    exit 0