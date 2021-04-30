type token =
  | ID of (string)
  | NUM of (string)
  | LABELN of (string)
  | CREATENODE
  | CREATEFROM
  | AT
  | LABEL
  | COLOR
  | SIZE
  | TO
  | INITIAL
  | FINAL
  | BGCOLOR
  | DUMP
  | REMOVE
  | REMOVEEDGE
  | MOVE
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit 
