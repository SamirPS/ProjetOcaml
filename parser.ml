type token =
  | ID of (string)
  | NUM of (string)
  | LABELN of (string)
  | LIST of (string)
  | SHOW
  | ISRECONNU
  | ISDETERMINISTIC
  | COMPLETE
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
  | DETERMINISTIC
  | DELETEALL

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
        
        open Functionannexe;;


        let nodelist = ref [] ;;
        let transition = ref [];;

        

      
# 56 "parser.ml"
let yytransl_const = [|
  261 (* SHOW *);
  262 (* ISRECONNU *);
  263 (* ISDETERMINISTIC *);
  264 (* COMPLETE *);
  265 (* ISCOMPLETE *);
  266 (* CREATENODE *);
  267 (* CREATEFROM *);
  268 (* AT *);
  269 (* LABEL *);
  270 (* COLOR *);
  271 (* SIZE *);
  272 (* TO *);
  273 (* INITIAL *);
  274 (* FINAL *);
  275 (* BGCOLOR *);
  276 (* DUMP *);
  277 (* REMOVE *);
  278 (* REMOVEEDGE *);
  279 (* MOVE *);
  280 (* RENAME *);
  281 (* WITH *);
  282 (* EDIT *);
  283 (* EDITEDGE *);
  284 (* PATH *);
  285 (* EOL *);
  286 (* N *);
  287 (* S *);
  288 (* E *);
  289 (* O *);
  290 (* NW *);
  291 (* NE *);
  292 (* SW *);
  293 (* SE *);
  294 (* DETERMINISTIC *);
  295 (* DELETEALL *);
    0|]

let yytransl_block = [|
  257 (* ID *);
  258 (* NUM *);
  259 (* LABELN *);
  260 (* LIST *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\003\000\003\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\007\000\006\000\005\000\
\008\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\011\000\011\000\011\000\011\000\011\000\011\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\002\000\002\000\001\000\002\000\004\000\001\000\006\000\
\001\000\002\000\002\000\003\000\003\000\001\000\001\000\001\000\
\001\000\002\000\002\000\002\000\002\000\002\000\001\000\001\000\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\002\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\002\000\002\000\003\000\003\000\003\000\004\000\002\000\
\002\000\002\000\005\000\003\000\003\000\003\000\006\000\002\000\
\002\000\002\000\002\000\002\000\001\000\001\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\002\000\003\000\
\004\000\006\000\006\000\007\000\005\000\006\000\007\000\007\000\
\008\000\006\000\008\000\004\000\002\000\004\000\003\000\004\000\
\004\000\004\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\009\000\000\000\007\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\091\000\092\000\000\000\000\000\000\000\016\000\000\000\000\000\
\011\000\010\000\000\000\000\000\000\000\000\000\085\000\000\000\
\000\000\014\000\017\000\000\000\000\000\000\000\000\000\000\000\
\001\000\002\000\003\000\012\000\013\000\015\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\087\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\000\035\000\036\000\
\037\000\038\000\039\000\040\000\041\000\032\000\000\000\033\000\
\000\000\000\000\000\000\000\000\006\000\086\000\088\000\089\000\
\090\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\084\000\000\000\000\000\000\000\026\000\027\000\028\000\030\000\
\031\000\029\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\070\000\000\000\071\000\
\000\000\000\000\000\000\008\000\074\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\065\000\066\000\072\000\068\000\
\069\000\067\000\000\000\000\000\000\000\000\000\082\000\076\000\
\000\000\079\000\045\000\046\000\000\000\073\000\000\000\000\000\
\000\000\000\000\047\000\081\000\083\000\000\000\054\000\052\000\
\053\000\000\000\000\000\055\000"

let yydgoto = "\002\000\
\018\000\019\000\020\000\021\000\025\000\047\000\036\000\037\000\
\055\000\079\000\112\000\143\000\097\000"

let yysindex = "\007\000\
\136\255\000\000\001\255\008\255\000\000\006\255\000\000\016\255\
\025\255\008\255\036\255\051\255\115\255\057\255\068\255\093\255\
\000\000\000\000\042\255\066\255\071\255\000\000\008\255\008\255\
\000\000\000\000\100\255\185\255\104\255\084\255\000\000\105\255\
\120\255\000\000\000\000\120\255\120\255\098\255\101\255\109\255\
\000\000\000\000\000\000\000\000\000\000\000\000\116\255\120\255\
\008\255\008\255\120\255\177\255\177\255\008\255\118\255\130\255\
\008\255\131\255\120\255\000\000\120\255\132\255\203\255\133\255\
\120\255\120\255\135\255\135\255\135\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\135\255\000\000\
\135\255\135\255\120\255\050\255\000\000\000\000\000\000\000\000\
\000\000\120\255\008\255\008\255\120\255\152\255\152\255\008\255\
\000\000\078\255\120\255\135\255\000\000\000\000\000\000\000\000\
\000\000\000\000\120\255\120\255\008\255\008\255\008\255\127\255\
\120\255\203\255\203\255\203\255\203\255\000\000\203\255\000\000\
\203\255\008\255\052\255\000\000\000\000\135\255\120\255\065\255\
\065\255\065\255\008\255\203\255\000\000\000\000\000\000\000\000\
\000\000\000\000\126\255\008\255\008\255\008\255\000\000\000\000\
\065\255\000\000\000\000\000\000\065\255\000\000\052\255\061\255\
\052\255\052\255\000\000\000\000\000\000\120\255\000\000\000\000\
\000\000\120\255\052\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\139\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\143\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\255\003\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\255\011\255\012\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\255\000\000\
\018\255\039\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\144\255\145\255\000\000\
\000\000\000\000\000\000\147\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\148\255\149\255\150\255\151\255\000\000\164\255\000\000\
\172\255\000\000\000\000\000\000\000\000\176\255\000\000\190\255\
\248\254\070\255\000\000\194\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\073\255\000\000\000\000\000\000\195\255\000\000\000\000\197\255\
\198\255\199\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\200\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\252\255\000\000\233\255\000\000\
\029\000\210\255\129\255\217\255\196\255"

let yytablesize = 229
let yytable = "\026\000\
\146\000\147\000\148\000\022\000\042\000\030\000\081\000\001\000\
\023\000\059\000\022\000\023\000\060\000\061\000\024\000\018\000\
\028\000\155\000\044\000\045\000\042\000\156\000\019\000\025\000\
\066\000\029\000\021\000\069\000\023\000\022\000\027\000\024\000\
\018\000\118\000\120\000\087\000\031\000\088\000\024\000\019\000\
\025\000\099\000\100\000\021\000\067\000\068\000\022\000\117\000\
\119\000\082\000\020\000\032\000\085\000\133\000\134\000\135\000\
\136\000\038\000\137\000\107\000\138\000\108\000\109\000\110\000\
\140\000\141\000\113\000\020\000\039\000\116\000\041\000\150\000\
\158\000\140\000\141\000\124\000\108\000\111\000\110\000\142\000\
\078\000\080\000\043\000\126\000\127\000\044\000\114\000\115\000\
\142\000\132\000\122\000\121\000\111\000\040\000\042\000\101\000\
\102\000\103\000\043\000\043\000\046\000\044\000\123\000\145\000\
\128\000\129\000\130\000\104\000\057\000\105\000\106\000\157\000\
\159\000\160\000\161\000\033\000\034\000\139\000\035\000\056\000\
\058\000\034\000\062\000\164\000\064\000\063\000\149\000\065\000\
\125\000\083\000\084\000\086\000\089\000\098\000\162\000\152\000\
\153\000\154\000\163\000\131\000\003\000\004\000\005\000\006\000\
\007\000\008\000\009\000\049\000\050\000\051\000\151\000\052\000\
\053\000\054\000\144\000\010\000\011\000\012\000\013\000\014\000\
\000\000\015\000\016\000\090\000\091\000\092\000\093\000\004\000\
\094\000\095\000\096\000\005\000\061\000\062\000\017\000\077\000\
\056\000\057\000\063\000\059\000\000\000\070\000\071\000\072\000\
\073\000\074\000\075\000\076\000\077\000\049\000\050\000\051\000\
\060\000\052\000\053\000\054\000\048\000\049\000\050\000\051\000\
\058\000\052\000\053\000\054\000\075\000\000\000\070\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\090\000\091\000\
\092\000\093\000\078\000\094\000\095\000\096\000\064\000\080\000\
\000\000\048\000\049\000\050\000\051\000"

let yycheck = "\004\000\
\128\000\129\000\130\000\003\001\013\001\010\000\053\000\001\000\
\008\001\033\000\003\001\012\001\036\000\037\000\012\001\012\001\
\001\001\145\000\023\000\024\000\029\001\149\000\012\001\012\001\
\048\000\001\001\012\001\051\000\029\001\012\001\025\001\029\001\
\029\001\094\000\095\000\059\000\001\001\061\000\038\001\029\001\
\029\001\065\000\066\000\029\001\049\000\050\000\029\001\094\000\
\095\000\054\000\012\001\001\001\057\000\114\000\115\000\116\000\
\117\000\001\001\119\000\083\000\121\000\012\001\013\001\014\001\
\013\001\014\001\090\000\029\001\001\001\093\000\029\001\132\000\
\012\001\013\001\014\001\099\000\012\001\028\001\014\001\028\001\
\052\000\053\000\013\001\107\000\108\000\013\001\091\000\092\000\
\028\001\113\000\013\001\096\000\028\001\001\001\029\001\067\000\
\068\000\069\000\029\001\029\001\001\001\029\001\025\001\127\000\
\109\000\110\000\111\000\079\000\025\001\081\000\082\000\151\000\
\152\000\153\000\154\000\001\001\002\001\122\000\004\001\016\001\
\016\001\002\001\025\001\163\000\016\001\025\001\131\000\012\001\
\100\000\012\001\001\001\001\001\001\001\001\001\158\000\140\000\
\141\000\142\000\162\000\013\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\013\001\014\001\015\001\025\001\017\001\
\018\001\019\001\126\000\020\001\021\001\022\001\023\001\024\001\
\255\255\026\001\027\001\012\001\013\001\014\001\015\001\029\001\
\017\001\018\001\019\001\029\001\029\001\029\001\039\001\029\001\
\029\001\029\001\029\001\029\001\255\255\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001\037\001\013\001\014\001\015\001\
\029\001\017\001\018\001\019\001\012\001\013\001\014\001\015\001\
\029\001\017\001\018\001\019\001\029\001\255\255\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\012\001\013\001\
\014\001\015\001\029\001\017\001\018\001\019\001\029\001\029\001\
\255\255\029\001\029\001\029\001\029\001"

let yynames_const = "\
  SHOW\000\
  ISRECONNU\000\
  ISDETERMINISTIC\000\
  COMPLETE\000\
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
  DETERMINISTIC\000\
  DELETEALL\000\
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
# 322 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'display) in
    Obj.repr(
# 33 "parser.mly"
                                     (  )
# 329 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'lasf) in
    Obj.repr(
# 34 "parser.mly"
                                  ( )
# 336 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                                 ( printlist (!nodelist @ !transition) )
# 342 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 41 "parser.mly"
                                ( createfile  _2  !nodelist !transition)
# 349 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 42 "parser.mly"
                                            (dumpwithstring _2 !nodelist !transition _4)
# 357 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                     (Printf.printf "%b\n " (is_complete !nodelist !transition))
# 363 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'labelnoeud) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 48 "parser.mly"
                                                    (nodelist := completeaux _3 _5 _6 !nodelist !transition;transition:= complete _3 !nodelist !transition;)
# 372 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                          (Printf.printf "%b\n " (is_deterministic !nodelist !transition))
# 378 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 50 "parser.mly"
                              (Printf.printf "%b\n " (is_accepted !nodelist !transition _2))
# 385 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 51 "parser.mly"
                         (Printf.printf "%s\n " (getchemin !nodelist !transition _2))
# 392 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 52 "parser.mly"
                                  (nodelist:= showcomplet _3 !nodelist !transition )
# 399 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 53 "parser.mly"
                                       (nodelist := showcompletd _3 !nodelist !transition)
# 406 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
                (_1)
# 413 "parser.ml"
               : 'numero))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "parser.mly"
              (_1)
# 420 "parser.ml"
               : 'labelnoeud))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
                (String.sub _1 1 ((String.length _1) -2)  )
# 427 "parser.ml"
               : 'vrailabel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
              ( python_split ',' (String.sub _1 1 ((String.length _1) -2)) )
# 434 "parser.ml"
               : 'glist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 70 "parser.mly"
                              (" LABEL: " ^ _2 )
# 441 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 71 "parser.mly"
                             (" COLOR: " ^ _2 )
# 448 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 72 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 455 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 73 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 462 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 74 "parser.mly"
                            (" FINAL: " ^ _2 )
# 469 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                     (" INITIAL: " ^ "" )
# 475 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                   (" FINAL: " ^ "" )
# 481 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 77 "parser.mly"
                         (" SIZE: " ^ _2)
# 488 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 79 "parser.mly"
                                      (" LABEL: " ^ _2 ^ _3 )
# 496 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 80 "parser.mly"
                                      (" COLOR: " ^ _2 ^ _3 )
# 504 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 81 "parser.mly"
                                  (" SIZE: " ^ _2 ^ _3 )
# 512 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 82 "parser.mly"
                                        (" BGCOLOR: " ^ _2 ^ _3 )
# 520 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 83 "parser.mly"
                                       ( " INITIAL: " ^ _2 ^ _3 )
# 528 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 84 "parser.mly"
                                      ( " FINAL: " ^ _2 ^ _3 )
# 536 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 85 "parser.mly"
                              ( " INITIAL: " ^ "" ^ _2 )
# 543 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 86 "parser.mly"
                             ( " FINAL: " ^ "" ^ _2)
# 550 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
            ("Nord")
# 556 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
            ("Sud")
# 562 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
            ("Est")
# 568 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
            ("Ouest")
# 574 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
             ("Nord-West")
# 580 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
             ("Nord-Est")
# 586 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
             ("Sud-Ouest")
# 592 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
             ("Sud-Est")
# 598 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 101 "parser.mly"
                             (" COLOR: " ^ _2 )
# 605 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 102 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 612 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 103 "parser.mly"
                              (" POSITION: " ^ _2^":"^ _3)
# 620 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 106 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 628 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 107 "parser.mly"
                                     (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 636 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 108 "parser.mly"
                                        (" POSITION: " ^ _2^":"^ _3 ^ _4)
# 645 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 113 "parser.mly"
                            (" LABEL: " ^ _2 )
# 652 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 114 "parser.mly"
                             (" COLOR: " ^ _2 )
# 659 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 115 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 666 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 116 "parser.mly"
                                             (" POSITION: " ^ _4^":"^ _5)
# 675 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 118 "parser.mly"
                                        (" COLOR: " ^ _2 ^ _3 )
# 683 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 119 "parser.mly"
                                      (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 691 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 120 "parser.mly"
                                       (" LABEL: " ^ _2 ^ _3 )
# 699 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 121 "parser.mly"
                                                         (" POSITION: " ^ _4^":"^ _5 ^ _6)
# 709 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 125 "parser.mly"
                              (" LABEL: " ^ _2 )
# 716 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 126 "parser.mly"
                             (" COLOR: " ^ _2 )
# 723 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 127 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 730 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 128 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 737 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 129 "parser.mly"
                            (" FINAL: " ^ _2 )
# 744 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    Obj.repr(
# 130 "parser.mly"
                     (" INITIAL: " ^ "" )
# 750 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
                   (" FINAL: " ^ "" )
# 756 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 132 "parser.mly"
                         (" SIZE: " ^ _2)
# 763 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 133 "parser.mly"
                             (" X: " ^ _2 ^ " Y: " ^ _3 )
# 771 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 135 "parser.mly"
                                        (" LABEL: " ^ _2 ^ _3)
# 779 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 136 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 787 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 137 "parser.mly"
                                         (" BGCOLOR: " ^ _2 ^ _3 )
# 795 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 138 "parser.mly"
                                        (" INITIAL: " ^ _2 ^ _3 )
# 803 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 139 "parser.mly"
                                      (" FINAL: " ^ _2 ^ _3 )
# 811 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 140 "parser.mly"
                               (" INITIAL: " ^  ""  ^ _2 )
# 818 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 141 "parser.mly"
                             (" FINAL: " ^  ""  ^ _2 )
# 825 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 142 "parser.mly"
                                    (" SIZE: " ^ _2 ^ _3)
# 833 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 143 "parser.mly"
                                        (" X: " ^ _2 ^ " Y: " ^ _3 ^ _4)
# 842 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 150 "parser.mly"
                                                     ( nodelist := !nodelist @  (add (Noeud(createid _2, ( _4), ( _5),_6)) !nodelist !transition)  )
# 852 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 151 "parser.mly"
                                                      ( nodelist := !nodelist @ ( add (Noeud(createid _2, ( _5), ( _6),_3)) !nodelist !transition) )
# 862 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 152 "parser.mly"
                                                                ( nodelist :=  !nodelist @ ( add (Noeud( createid _2, ( _5), ( _6),_3 ^ (" "  ^ _7))) !nodelist !transition) )
# 873 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 153 "parser.mly"
                                             (  nodelist := !nodelist @ ( add (Noeud(createid _2, ( _4), ( _5),"")) !nodelist !transition))
# 882 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 156 "parser.mly"
                                                 ( transition := !transition @ (add (Edge(createid _2,createid _4,_6,"")) !nodelist !transition ) )
# 891 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 157 "parser.mly"
                                                           ( transition := !transition @ (add (Edge(createid _2,createid _4,_6,_7)) !nodelist !transition)  )
# 901 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 158 "parser.mly"
                                                            ( transition := !transition @ ( add (Edge(createid _2,createid _4,_7,_5)) !nodelist !transition)   )
# 911 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 159 "parser.mly"
                                                                       ( transition := !transition @ (add (Edge(createid _2,createid _4,_7,_5 ^ (" "^ _8))) !nodelist !transition)   )
# 922 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 161 "parser.mly"
                                              ( Printf.printf "Il faut rajouter le label de la transition :o \n")
# 931 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vrailabel) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 162 "parser.mly"
                                                              ( transition := editt (createid _2) ( createid _4) _8 _6 !transition )
# 941 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 163 "parser.mly"
                                    (nodelist := editn (createid _2) _4 !nodelist )
# 949 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 165 "parser.mly"
                      ( nodelist := removenoeud (createid _2) !nodelist ; transition:= removetransitionafternode (createid _2) !transition;  )
# 956 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 166 "parser.mly"
                                 (  transition := removetransition (createid _2) (createid _4) !transition )
# 964 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 168 "parser.mly"
                                (nodelist := moveall _2 _3 !nodelist)
# 972 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 169 "parser.mly"
                                   (nodelist := moveallid_aux (createid _2) _3 _4 !nodelist)
# 981 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'glist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 170 "parser.mly"
                                      (nodelist := movelistid _2 _3 _4 !nodelist)
# 990 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 172 "parser.mly"
                              (nodelist:= renamen (createid _2) (createid _4) !nodelist ; transition:= renamet (createid _2) (createid _4) !transition ;)
# 998 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    Obj.repr(
# 173 "parser.mly"
                      (nodelist:= [] ; transition:= [];)
# 1004 "parser.ml"
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
