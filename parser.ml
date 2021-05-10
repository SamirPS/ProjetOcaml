type token =
  | ID of (string)
  | NUM of (string)
  | LABELN of (string)
  | LIST of (string)
  | ISCOMPLETE
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
  | RENAME
  | WITH
  | EDIT
  | EDITEDGE
  | PATH
  | EOL
  | N
  | S
  | E
  | O
  | NW
  | NE
  | SW
  | SE

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
        
        open Functionannexe;;


        let nodelist = ref [] ;;
        let transition = ref [];;

        

      
# 50 "parser.ml"
let yytransl_const = [|
  261 (* ISCOMPLETE *);
  262 (* CREATENODE *);
  263 (* CREATEFROM *);
  264 (* AT *);
  265 (* LABEL *);
  266 (* COLOR *);
  267 (* SIZE *);
  268 (* TO *);
  269 (* INITIAL *);
  270 (* FINAL *);
  271 (* BGCOLOR *);
  272 (* DUMP *);
  273 (* REMOVE *);
  274 (* REMOVEEDGE *);
  275 (* MOVE *);
  276 (* RENAME *);
  277 (* WITH *);
  278 (* EDIT *);
  279 (* EDITEDGE *);
  280 (* PATH *);
  281 (* EOL *);
  282 (* N *);
  283 (* S *);
  284 (* E *);
  285 (* O *);
  286 (* NW *);
  287 (* NE *);
  288 (* SW *);
  289 (* SE *);
    0|]

let yytransl_block = [|
  257 (* ID *);
  258 (* NUM *);
  259 (* LABELN *);
  260 (* LIST *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\003\000\005\000\006\000\004\000\
\007\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\010\000\010\000\010\000\
\010\000\010\000\010\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\001\000\002\000\001\000\001\000\001\000\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\002\000\002\000\003\000\
\003\000\003\000\004\000\002\000\002\000\002\000\005\000\003\000\
\003\000\003\000\006\000\002\000\002\000\002\000\002\000\002\000\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\004\000\006\000\006\000\007\000\005\000\006\000\007\000\007\000\
\008\000\006\000\004\000\002\000\004\000\003\000\004\000\004\000\
\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\003\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\074\000\000\000\000\000\000\000\
\000\000\008\000\005\000\068\000\000\000\000\000\006\000\009\000\
\000\000\000\000\000\000\000\000\000\000\001\000\002\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\070\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\022\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000\000\000\000\000\000\000\000\000\000\000\
\069\000\071\000\072\000\073\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\067\000\000\000\000\000\016\000\017\000\
\018\000\020\000\021\000\019\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\058\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\051\000\052\000\053\000\055\000\056\000\054\000\
\000\000\000\000\000\000\066\000\060\000\000\000\063\000\033\000\
\034\000\000\000\057\000\000\000\000\000\000\000\035\000\065\000\
\000\000\042\000\040\000\041\000\000\000\000\000\043\000"

let yydgoto = "\002\000\
\013\000\014\000\015\000\019\000\025\000\000\000\026\000\039\000\
\060\000\090\000\116\000\076\000"

let yysindex = "\008\000\
\069\255\000\000\000\000\010\255\032\255\031\255\043\255\058\255\
\104\255\070\255\076\255\077\255\000\000\059\255\065\255\115\255\
\098\255\000\000\000\000\000\000\102\255\125\255\000\000\000\000\
\125\255\125\255\134\255\114\255\135\255\000\000\000\000\125\255\
\031\255\031\255\125\255\089\255\089\255\031\255\140\255\141\255\
\148\255\125\255\000\000\125\255\149\255\123\255\150\255\125\255\
\130\255\130\255\130\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\130\255\130\255\130\255\125\255\085\255\
\000\000\000\000\000\000\000\000\125\255\031\255\031\255\125\255\
\089\255\089\255\031\255\000\000\131\255\130\255\000\000\000\000\
\000\000\000\000\000\000\000\000\125\255\125\255\031\255\031\255\
\031\255\144\255\125\255\123\255\123\255\123\255\123\255\123\255\
\123\255\056\255\000\000\130\255\125\255\027\255\027\255\027\255\
\031\255\123\255\000\000\000\000\000\000\000\000\000\000\000\000\
\031\255\031\255\031\255\000\000\000\000\027\255\000\000\000\000\
\000\000\027\255\000\000\088\255\056\255\056\255\000\000\000\000\
\125\255\000\000\000\000\000\000\125\255\056\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\129\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\255\254\000\255\006\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\007\255\011\255\015\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\132\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\133\255\136\255\137\255\138\255\139\255\
\142\255\000\000\000\000\143\255\000\000\145\255\252\254\021\255\
\000\000\146\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\033\255\000\000\000\000\
\000\000\147\255\000\000\151\255\152\255\153\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\154\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\224\255\234\255\000\000\000\000\223\255\
\231\255\206\255\192\255\007\000"

let yytablesize = 179
let yytable = "\042\000\
\049\000\050\000\043\000\044\000\030\000\062\000\010\000\011\000\
\001\000\048\000\016\000\061\000\051\000\015\000\013\000\079\000\
\080\000\081\000\014\000\066\000\030\000\067\000\012\000\010\000\
\011\000\078\000\082\000\083\000\084\000\031\000\015\000\013\000\
\017\000\018\000\086\000\014\000\088\000\092\000\093\000\012\000\
\085\000\032\000\097\000\020\000\099\000\031\000\091\000\095\000\
\096\000\094\000\089\000\119\000\120\000\121\000\102\000\103\000\
\104\000\032\000\021\000\130\000\131\000\132\000\100\000\101\000\
\113\000\114\000\117\000\127\000\106\000\135\000\027\000\128\000\
\122\000\003\000\004\000\005\000\028\000\029\000\118\000\115\000\
\124\000\125\000\126\000\030\000\006\000\007\000\008\000\009\000\
\010\000\031\000\011\000\012\000\086\000\087\000\088\000\129\000\
\113\000\114\000\107\000\108\000\109\000\110\000\111\000\112\000\
\022\000\023\000\133\000\024\000\089\000\040\000\134\000\115\000\
\123\000\041\000\052\000\053\000\054\000\055\000\056\000\057\000\
\058\000\059\000\032\000\033\000\034\000\035\000\023\000\036\000\
\037\000\038\000\069\000\070\000\071\000\072\000\046\000\073\000\
\074\000\075\000\033\000\034\000\035\000\064\000\036\000\037\000\
\038\000\045\000\047\000\063\000\065\000\068\000\077\000\098\000\
\105\000\004\000\000\000\000\000\061\000\044\000\000\000\000\000\
\045\000\049\000\047\000\048\000\000\000\000\000\046\000\059\000\
\000\000\062\000\050\000\064\000\000\000\000\000\000\000\036\000\
\037\000\038\000\039\000"

let yycheck = "\022\000\
\033\000\034\000\025\000\026\000\009\001\038\000\008\001\008\001\
\001\000\032\000\001\001\037\000\035\000\008\001\008\001\049\000\
\050\000\051\000\008\001\042\000\025\001\044\000\008\001\025\001\
\025\001\048\000\060\000\061\000\062\000\009\001\025\001\025\001\
\001\001\003\001\008\001\025\001\010\001\070\000\071\000\025\001\
\063\000\009\001\075\000\001\001\078\000\025\001\069\000\073\000\
\074\000\072\000\024\001\102\000\103\000\104\000\087\000\088\000\
\089\000\025\001\001\001\124\000\125\000\126\000\085\000\086\000\
\009\001\010\001\100\000\118\000\091\000\134\000\001\001\122\000\
\105\000\005\001\006\001\007\001\001\001\001\001\101\000\024\001\
\113\000\114\000\115\000\025\001\016\001\017\001\018\001\019\001\
\020\001\025\001\022\001\023\001\008\001\009\001\010\001\008\001\
\009\001\010\001\092\000\093\000\094\000\095\000\096\000\097\000\
\001\001\002\001\129\000\004\001\024\001\012\001\133\000\024\001\
\106\000\012\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\008\001\009\001\010\001\011\001\002\001\013\001\
\014\001\015\001\008\001\009\001\010\001\011\001\021\001\013\001\
\014\001\015\001\009\001\010\001\011\001\001\001\013\001\014\001\
\015\001\012\001\012\001\008\001\001\001\001\001\001\001\021\001\
\009\001\025\001\255\255\255\255\025\001\025\001\255\255\255\255\
\025\001\025\001\025\001\025\001\255\255\255\255\025\001\025\001\
\255\255\025\001\025\001\025\001\255\255\255\255\255\255\025\001\
\025\001\025\001\025\001"

let yynames_const = "\
  ISCOMPLETE\000\
  CREATENODE\000\
  CREATEFROM\000\
  AT\000\
  LABEL\000\
  COLOR\000\
  SIZE\000\
  TO\000\
  INITIAL\000\
  FINAL\000\
  BGCOLOR\000\
  DUMP\000\
  REMOVE\000\
  REMOVEEDGE\000\
  MOVE\000\
  RENAME\000\
  WITH\000\
  EDIT\000\
  EDITEDGE\000\
  PATH\000\
  EOL\000\
  N\000\
  S\000\
  E\000\
  O\000\
  NW\000\
  NE\000\
  SW\000\
  SE\000\
  "

let yynames_block = "\
  ID\000\
  NUM\000\
  LABELN\000\
  LIST\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : unit) in
    Obj.repr(
# 32 "parser.mly"
                                        (    )
# 276 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'display) in
    Obj.repr(
# 33 "parser.mly"
                                     (  )
# 283 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "parser.mly"
                        (Printf.printf "%b \n " (is_complete !nodelist !transition) )
# 289 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                                 ( printlist (!nodelist @ !transition) )
# 295 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 41 "parser.mly"
                                ( createfile  _2  !nodelist !transition)
# 302 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "parser.mly"
                (_1)
# 309 "parser.ml"
               : 'numero))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "parser.mly"
              (_1)
# 316 "parser.ml"
               : 'labelnoeud))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
                (String.sub _1 1 ((String.length _1) -2)  )
# 323 "parser.ml"
               : 'vrailabel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
              ( list_split (String.sub _1 1 ((String.length _1) -2)) )
# 330 "parser.ml"
               : 'glist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 60 "parser.mly"
                              (" LABEL: " ^ _2 )
# 337 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 61 "parser.mly"
                             (" COLOR: " ^ _2 )
# 344 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 62 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 351 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 63 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 358 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 64 "parser.mly"
                            (" FINAL: " ^ _2 )
# 365 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 65 "parser.mly"
                         (" SIZE: " ^ _2)
# 372 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 67 "parser.mly"
                                      (" LABEL: " ^ _2 ^ _3 )
# 380 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 68 "parser.mly"
                                      (" COLOR: " ^ _2 ^ _3 )
# 388 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 69 "parser.mly"
                                  (" BGCOLOR: " ^ _2 ^ _3 )
# 396 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 70 "parser.mly"
                                        (" INITIAL: " ^ _2 ^ _3 )
# 404 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 71 "parser.mly"
                                       ( " FINAL: " ^ _2 ^ _3 )
# 412 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 72 "parser.mly"
                                      ( " SIZE: " ^ _2 ^ _3 )
# 420 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parser.mly"
            ("Nord")
# 426 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
            ("Sud")
# 432 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
            ("Est")
# 438 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
            ("Ouest")
# 444 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
             ("Nord West")
# 450 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
             ("Nord Est")
# 456 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
             ("Sud Ouest")
# 462 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
             ("Sud Est")
# 468 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 87 "parser.mly"
                             (" COLOR: " ^ _2 )
# 475 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 88 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 482 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 89 "parser.mly"
                              (" POSITION: " ^ _2^":"^ _3)
# 490 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 92 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 498 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 93 "parser.mly"
                                     (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 506 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 94 "parser.mly"
                                        (" POSITION: " ^ _2^":"^ _3 ^ _4)
# 515 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 99 "parser.mly"
                            (" LABEL: " ^ _2 )
# 522 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 100 "parser.mly"
                             (" COLOR: " ^ _2 )
# 529 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 101 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 536 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 102 "parser.mly"
                                             (" POSITION: " ^ _4^":"^ _5)
# 545 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 104 "parser.mly"
                                        (" COLOR: " ^ _2 ^ _3 )
# 553 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 105 "parser.mly"
                                      (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 561 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 106 "parser.mly"
                                       (" LABEL: " ^ _2 ^ _3 )
# 569 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 107 "parser.mly"
                                                         (" POSITION: " ^ _4^":"^ _5 ^ _6)
# 579 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 111 "parser.mly"
                              (" LABEL: " ^ _2 )
# 586 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 112 "parser.mly"
                             (" COLOR: " ^ _2 )
# 593 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 113 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 600 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 114 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 607 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 115 "parser.mly"
                            (" FINAL: " ^ _2 )
# 614 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 116 "parser.mly"
                         (" SIZE: " ^ _2)
# 621 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 117 "parser.mly"
                             (" X: " ^ _2 ^ " Y: " ^ _3 )
# 629 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 119 "parser.mly"
                                        (" LABEL: " ^ _2 ^ _3 )
# 637 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 120 "parser.mly"
                                        (" COLOR: " ^ _2 ^ _3 )
# 645 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 121 "parser.mly"
                                    (" BGCOLOR: " ^ _2 ^ _3 )
# 653 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 122 "parser.mly"
                                          (" INITIAL: " ^ _2 ^ _3 )
# 661 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 123 "parser.mly"
                                         ( " FINAL: " ^ _2 ^ _3 )
# 669 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 124 "parser.mly"
                                        ( " SIZE: " ^ _2 ^ _3 )
# 677 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 125 "parser.mly"
                                        (" X: " ^ _2 ^ " Y: " ^ _3 ^ _4 )
# 686 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 131 "parser.mly"
                                                     ( nodelist := add (Noeud(_2, ( _4), ( _5),_6)) @ !nodelist )
# 696 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 132 "parser.mly"
                                                      ( nodelist := add (Noeud(_2, ( _5), ( _6),_3)) @ !nodelist )
# 706 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 133 "parser.mly"
                                                                ( nodelist := add (Noeud(_2, ( _5), ( _6),_3 ^ (" "  ^ _7))) @ !nodelist )
# 717 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 134 "parser.mly"
                                             (  nodelist := add (Noeud(_2, ( _4), ( _5),"")) @ !nodelist)
# 726 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 137 "parser.mly"
                                                 ( transition := add (Edge(_2,_4,_6,"")) @ !transition )
# 735 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 138 "parser.mly"
                                                           ( transition := add (Edge(_2,_4,_6,_7)) @ !transition )
# 745 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 139 "parser.mly"
                                                            ( transition := add (Edge(_2,_4,_7,_5))  @ !transition )
# 755 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 140 "parser.mly"
                                                                       ( transition := add (Edge(_2,_4,_7,_5 ^ (" "^ _8)))  @ !transition )
# 766 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 142 "parser.mly"
                                              ( transition := editt _2 _4 _6  !transition )
# 775 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 143 "parser.mly"
                                    (nodelist := editn _2 _4 !nodelist )
# 783 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 145 "parser.mly"
                      ( nodelist := removenoeud _2 !nodelist  )
# 790 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 146 "parser.mly"
                                (  transition := removetransition _2 _4  !transition )
# 798 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 148 "parser.mly"
                                (nodelist := moveall _2 _3 !nodelist)
# 806 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 149 "parser.mly"
                                   (nodelist := moveallid _2 _3 _4 !nodelist)
# 815 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'glist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 150 "parser.mly"
                                      (nodelist := movelistid _2 _3 _4 !nodelist)
# 824 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 152 "parser.mly"
                            (nodelist:= renamen _2 _4 !nodelist ; transition:= renamet _2 _4 !transition ;)
# 832 "parser.ml"
               : unit))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit )
