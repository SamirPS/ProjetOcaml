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
  | NONE
  | DETERMINISTIC

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
  294 (* NONE *);
  295 (* DETERMINISTIC *);
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
\002\000\000\000"

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
\008\000\008\000\004\000\002\000\004\000\003\000\004\000\004\000\
\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\009\000\000\000\007\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\090\000\000\000\000\000\000\000\016\000\000\000\000\000\011\000\
\010\000\000\000\000\000\000\000\000\000\084\000\000\000\000\000\
\014\000\017\000\000\000\000\000\000\000\000\000\000\000\001\000\
\002\000\003\000\012\000\013\000\015\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\086\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\034\000\035\000\036\000\037\000\
\038\000\039\000\040\000\041\000\032\000\000\000\033\000\000\000\
\000\000\000\000\000\000\006\000\085\000\087\000\088\000\089\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\083\000\
\000\000\000\000\000\000\026\000\027\000\028\000\030\000\031\000\
\029\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\070\000\000\000\071\000\000\000\
\000\000\008\000\074\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\065\000\066\000\072\000\068\000\069\000\067\000\
\000\000\076\000\000\000\079\000\045\000\046\000\000\000\073\000\
\000\000\047\000\081\000\000\000\000\000\000\000\082\000\000\000\
\000\000\000\000\000\000\054\000\052\000\053\000\000\000\000\000\
\055\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\020\000\024\000\046\000\035\000\036\000\
\054\000\078\000\111\000\151\000\096\000"

let yysindex = "\001\000\
\176\255\000\000\007\255\000\255\000\000\238\254\000\000\013\255\
\034\255\000\255\055\255\062\255\025\255\068\255\076\255\077\255\
\000\000\252\254\061\255\065\255\000\000\000\255\000\255\000\000\
\000\000\083\255\192\255\073\255\072\255\000\000\087\255\108\255\
\000\000\000\000\108\255\108\255\086\255\093\255\096\255\000\000\
\000\000\000\000\000\000\000\000\000\000\101\255\108\255\000\255\
\000\255\108\255\143\255\143\255\000\255\111\255\126\255\000\255\
\136\255\108\255\000\000\108\255\137\255\200\255\139\255\108\255\
\108\255\116\255\116\255\116\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\116\255\000\000\116\255\
\116\255\108\255\047\255\000\000\000\000\000\000\000\000\000\000\
\108\255\000\255\000\255\108\255\135\255\135\255\000\255\000\000\
\128\255\108\255\116\255\000\000\000\000\000\000\000\000\000\000\
\000\000\108\255\108\255\000\255\000\255\000\255\129\255\108\255\
\200\255\200\255\200\255\200\255\000\000\200\255\000\000\200\255\
\000\255\000\000\000\000\116\255\108\255\036\255\036\255\036\255\
\000\255\200\255\000\000\000\000\000\000\000\000\000\000\000\000\
\118\255\000\000\036\255\000\000\000\000\000\000\036\255\000\000\
\088\255\000\000\000\000\000\255\000\255\000\255\000\000\060\255\
\088\255\088\255\108\255\000\000\000\000\000\000\108\255\088\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\122\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\130\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\248\254\005\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\010\255\018\255\024\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\028\255\000\000\029\255\
\039\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\134\255\159\255\000\000\000\000\
\000\000\000\000\160\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\161\255\162\255\163\255\164\255\000\000\165\255\000\000\166\255\
\000\000\000\000\000\000\172\255\000\000\179\255\003\255\041\255\
\000\000\187\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\063\255\000\000\000\000\000\000\191\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\193\255\
\194\255\195\255\000\000\000\000\000\000\000\000\000\000\196\255\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\252\255\000\000\229\255\000\000\
\015\000\205\255\141\255\211\255\006\000"

let yytablesize = 225
let yytable = "\025\000\
\080\000\001\000\021\000\023\000\058\000\029\000\026\000\059\000\
\060\000\021\000\140\000\141\000\142\000\027\000\022\000\042\000\
\024\000\043\000\044\000\065\000\023\000\018\000\068\000\146\000\
\040\000\032\000\033\000\147\000\034\000\019\000\086\000\042\000\
\087\000\024\000\028\000\025\000\098\000\099\000\018\000\021\000\
\022\000\116\000\118\000\066\000\067\000\023\000\019\000\107\000\
\081\000\109\000\020\000\084\000\025\000\043\000\106\000\030\000\
\021\000\022\000\107\000\108\000\109\000\112\000\031\000\110\000\
\115\000\077\000\079\000\020\000\037\000\043\000\122\000\155\000\
\148\000\149\000\110\000\044\000\038\000\039\000\124\000\125\000\
\100\000\101\000\102\000\045\000\130\000\113\000\114\000\150\000\
\055\000\041\000\120\000\044\000\103\000\042\000\104\000\105\000\
\056\000\139\000\117\000\119\000\148\000\149\000\057\000\126\000\
\127\000\128\000\156\000\157\000\158\000\033\000\061\000\063\000\
\064\000\123\000\161\000\150\000\137\000\062\000\131\000\132\000\
\133\000\134\000\082\000\135\000\143\000\136\000\083\000\159\000\
\048\000\049\000\050\000\160\000\051\000\052\000\053\000\144\000\
\085\000\088\000\138\000\097\000\121\000\129\000\145\000\152\000\
\153\000\154\000\089\000\090\000\091\000\092\000\004\000\093\000\
\094\000\095\000\000\000\048\000\049\000\050\000\005\000\051\000\
\052\000\053\000\061\000\000\000\069\000\070\000\071\000\072\000\
\073\000\074\000\075\000\076\000\069\000\070\000\071\000\072\000\
\073\000\074\000\075\000\076\000\003\000\004\000\005\000\006\000\
\007\000\008\000\009\000\062\000\077\000\056\000\057\000\063\000\
\059\000\060\000\058\000\010\000\011\000\012\000\013\000\014\000\
\075\000\015\000\016\000\047\000\048\000\049\000\050\000\078\000\
\051\000\052\000\053\000\089\000\090\000\091\000\092\000\064\000\
\093\000\094\000\095\000\080\000\000\000\048\000\049\000\050\000\
\051\000"

let yycheck = "\004\000\
\052\000\001\000\003\001\012\001\032\000\010\000\025\001\035\000\
\036\000\003\001\126\000\127\000\128\000\001\001\008\001\013\001\
\012\001\022\000\023\000\047\000\029\001\012\001\050\000\139\000\
\029\001\001\001\002\001\143\000\004\001\012\001\058\000\029\001\
\060\000\029\001\001\001\012\001\064\000\065\000\029\001\012\001\
\012\001\093\000\094\000\048\000\049\000\039\001\029\001\012\001\
\053\000\014\001\012\001\056\000\029\001\013\001\082\000\001\001\
\029\001\029\001\012\001\013\001\014\001\089\000\001\001\028\001\
\092\000\051\000\052\000\029\001\001\001\029\001\098\000\012\001\
\013\001\014\001\028\001\013\001\001\001\001\001\106\000\107\000\
\066\000\067\000\068\000\001\001\112\000\090\000\091\000\028\001\
\016\001\029\001\095\000\029\001\078\000\029\001\080\000\081\000\
\025\001\125\000\093\000\094\000\013\001\014\001\016\001\108\000\
\109\000\110\000\152\000\153\000\154\000\002\001\025\001\016\001\
\012\001\099\000\160\000\028\001\121\000\025\001\113\000\114\000\
\115\000\116\000\012\001\118\000\129\000\120\000\001\001\155\000\
\013\001\014\001\015\001\159\000\017\001\018\001\019\001\130\000\
\001\001\001\001\124\000\001\001\013\001\013\001\025\001\148\000\
\149\000\150\000\012\001\013\001\014\001\015\001\029\001\017\001\
\018\001\019\001\255\255\013\001\014\001\015\001\029\001\017\001\
\018\001\019\001\029\001\255\255\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\037\001\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\037\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\029\001\029\001\029\001\029\001\029\001\
\029\001\029\001\029\001\020\001\021\001\022\001\023\001\024\001\
\029\001\026\001\027\001\012\001\013\001\014\001\015\001\029\001\
\017\001\018\001\019\001\012\001\013\001\014\001\015\001\029\001\
\017\001\018\001\019\001\029\001\255\255\029\001\029\001\029\001\
\029\001"

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
  NONE\000\
  DETERMINISTIC\000\
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
                     (" INITIAL: " ^ "none" )
# 475 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                   (" FINAL: " ^ "none" )
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
                              ( " INITIAL: " ^ "none" ^ _2 )
# 543 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 86 "parser.mly"
                             ( " FINAL: " ^ "none" ^ _2)
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
                     (" INITIAL: " ^ "none" )
# 750 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
                   (" FINAL: " ^ "none" )
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
                               (" INITIAL: " ^  "none"  ^ _2 )
# 818 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 141 "parser.mly"
                             (" FINAL: " ^  "none"  ^ _2 )
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
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vrailabel) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 161 "parser.mly"
                                                              ( transition := editt (createid _2) ( createid _4) _8 _6 !transition )
# 932 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 162 "parser.mly"
                                    (nodelist := editn (createid _2) _4 !nodelist )
# 940 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 164 "parser.mly"
                      ( nodelist := removenoeud (createid _2) !nodelist ; transition:= removetransitionafternode (createid _2) !transition;  )
# 947 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 165 "parser.mly"
                                 (  transition := removetransition (createid _2) (createid _4) !transition )
# 955 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 167 "parser.mly"
                                (nodelist := moveall _2 _3 !nodelist)
# 963 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 168 "parser.mly"
                                   (nodelist := moveallid_aux (createid _2) _3 _4 !nodelist)
# 972 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'glist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 169 "parser.mly"
                                      (nodelist := movelistid _2 _3 _4 !nodelist)
# 981 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 171 "parser.mly"
                              (nodelist:= renamen (createid _2) (createid _4) !nodelist ; transition:= renamet (createid _2) (createid _4) !transition ;)
# 989 "parser.ml"
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
