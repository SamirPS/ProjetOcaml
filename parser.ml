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

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
        
        open Functionannexe;;


        let nodelist = ref [] ;;
        let transition = ref [];;

        

      
# 55 "parser.ml"
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
\002\000\002\000\000\000"

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
\004\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\009\000\000\000\007\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\091\000\000\000\000\000\000\000\016\000\000\000\000\000\011\000\
\010\000\000\000\000\000\000\000\000\000\085\000\000\000\000\000\
\014\000\017\000\000\000\000\000\000\000\000\000\000\000\001\000\
\002\000\003\000\012\000\013\000\015\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\087\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\034\000\035\000\036\000\037\000\
\038\000\039\000\040\000\041\000\032\000\000\000\033\000\000\000\
\000\000\000\000\000\000\006\000\086\000\088\000\089\000\090\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\084\000\
\000\000\000\000\000\000\026\000\027\000\028\000\030\000\031\000\
\029\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\070\000\000\000\071\000\000\000\
\000\000\000\000\008\000\074\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\065\000\066\000\072\000\068\000\069\000\
\067\000\000\000\000\000\000\000\000\000\082\000\076\000\000\000\
\079\000\045\000\046\000\000\000\073\000\000\000\000\000\000\000\
\000\000\047\000\081\000\083\000\000\000\054\000\052\000\053\000\
\000\000\000\000\055\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\020\000\024\000\046\000\035\000\036\000\
\054\000\078\000\111\000\142\000\096\000"

let yysindex = "\024\000\
\170\255\000\000\008\255\023\255\000\000\003\255\000\000\031\255\
\053\255\023\255\072\255\073\255\088\255\076\255\077\255\093\255\
\000\000\068\255\069\255\074\255\000\000\023\255\023\255\000\000\
\000\000\101\255\186\255\095\255\087\255\000\000\097\255\113\255\
\000\000\000\000\113\255\113\255\091\255\099\255\102\255\000\000\
\000\000\000\000\000\000\000\000\000\000\116\255\113\255\023\255\
\023\255\113\255\137\255\137\255\023\255\117\255\130\255\023\255\
\131\255\113\255\000\000\113\255\132\255\194\255\138\255\113\255\
\113\255\169\255\169\255\169\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\169\255\000\000\169\255\
\169\255\113\255\047\255\000\000\000\000\000\000\000\000\000\000\
\113\255\023\255\023\255\113\255\129\255\129\255\023\255\000\000\
\063\255\113\255\169\255\000\000\000\000\000\000\000\000\000\000\
\000\000\113\255\113\255\023\255\023\255\023\255\136\255\113\255\
\194\255\194\255\194\255\194\255\000\000\194\255\000\000\194\255\
\023\255\245\254\000\000\000\000\169\255\113\255\036\255\036\255\
\036\255\023\255\194\255\000\000\000\000\000\000\000\000\000\000\
\000\000\120\255\023\255\023\255\023\255\000\000\000\000\036\255\
\000\000\000\000\000\000\036\255\000\000\245\254\056\255\245\254\
\245\254\000\000\000\000\000\000\113\255\000\000\000\000\000\000\
\113\255\245\254\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\124\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\128\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\248\254\251\254\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\254\254\000\255\010\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\024\255\000\000\028\255\
\029\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\156\255\160\255\000\000\000\000\
\000\000\000\000\166\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\173\255\181\255\185\255\187\255\000\000\188\255\000\000\189\255\
\000\000\000\000\000\000\000\000\190\255\000\000\191\255\022\255\
\034\255\000\000\192\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\043\255\
\000\000\000\000\000\000\193\255\000\000\000\000\195\255\196\255\
\197\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\198\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\252\255\000\000\229\255\000\000\
\015\000\205\255\142\255\213\255\007\000"

let yytablesize = 227
let yytable = "\025\000\
\080\000\139\000\140\000\023\000\058\000\029\000\024\000\059\000\
\060\000\018\000\021\000\019\000\145\000\146\000\147\000\022\000\
\141\000\043\000\044\000\065\000\023\000\025\000\068\000\024\000\
\001\000\021\000\018\000\026\000\019\000\154\000\086\000\027\000\
\087\000\155\000\042\000\021\000\098\000\099\000\025\000\022\000\
\020\000\116\000\118\000\066\000\067\000\023\000\043\000\107\000\
\081\000\109\000\042\000\084\000\021\000\028\000\106\000\044\000\
\022\000\020\000\107\000\108\000\109\000\112\000\043\000\110\000\
\115\000\077\000\079\000\157\000\139\000\140\000\123\000\044\000\
\030\000\031\000\110\000\121\000\037\000\038\000\125\000\126\000\
\100\000\101\000\102\000\141\000\131\000\113\000\114\000\122\000\
\032\000\033\000\120\000\034\000\103\000\039\000\104\000\105\000\
\040\000\041\000\144\000\117\000\119\000\045\000\042\000\127\000\
\128\000\129\000\156\000\158\000\159\000\160\000\055\000\056\000\
\057\000\124\000\033\000\061\000\138\000\063\000\163\000\132\000\
\133\000\134\000\135\000\062\000\136\000\148\000\137\000\064\000\
\082\000\161\000\083\000\085\000\088\000\162\000\151\000\152\000\
\153\000\149\000\097\000\143\000\089\000\090\000\091\000\092\000\
\150\000\093\000\094\000\095\000\130\000\048\000\049\000\050\000\
\004\000\051\000\052\000\053\000\005\000\000\000\069\000\070\000\
\071\000\072\000\073\000\074\000\075\000\076\000\069\000\070\000\
\071\000\072\000\073\000\074\000\075\000\076\000\003\000\004\000\
\005\000\006\000\007\000\008\000\009\000\048\000\049\000\050\000\
\061\000\051\000\052\000\053\000\062\000\010\000\011\000\012\000\
\013\000\014\000\077\000\015\000\016\000\047\000\048\000\049\000\
\050\000\056\000\051\000\052\000\053\000\089\000\090\000\091\000\
\092\000\057\000\093\000\094\000\095\000\063\000\000\000\059\000\
\060\000\058\000\075\000\078\000\064\000\080\000\000\000\048\000\
\049\000\050\000\051\000"

let yycheck = "\004\000\
\052\000\013\001\014\001\012\001\032\000\010\000\012\001\035\000\
\036\000\012\001\003\001\012\001\127\000\128\000\129\000\008\001\
\028\001\022\000\023\000\047\000\029\001\012\001\050\000\029\001\
\001\000\003\001\029\001\025\001\029\001\144\000\058\000\001\001\
\060\000\148\000\013\001\012\001\064\000\065\000\029\001\012\001\
\012\001\093\000\094\000\048\000\049\000\038\001\013\001\012\001\
\053\000\014\001\029\001\056\000\029\001\001\001\082\000\013\001\
\029\001\029\001\012\001\013\001\014\001\089\000\029\001\028\001\
\092\000\051\000\052\000\012\001\013\001\014\001\098\000\029\001\
\001\001\001\001\028\001\013\001\001\001\001\001\106\000\107\000\
\066\000\067\000\068\000\028\001\112\000\090\000\091\000\025\001\
\001\001\002\001\095\000\004\001\078\000\001\001\080\000\081\000\
\029\001\029\001\126\000\093\000\094\000\001\001\029\001\108\000\
\109\000\110\000\150\000\151\000\152\000\153\000\016\001\025\001\
\016\001\099\000\002\001\025\001\121\000\016\001\162\000\113\000\
\114\000\115\000\116\000\025\001\118\000\130\000\120\000\012\001\
\012\001\157\000\001\001\001\001\001\001\161\000\139\000\140\000\
\141\000\131\000\001\001\125\000\012\001\013\001\014\001\015\001\
\025\001\017\001\018\001\019\001\013\001\013\001\014\001\015\001\
\029\001\017\001\018\001\019\001\029\001\255\255\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\013\001\014\001\015\001\
\029\001\017\001\018\001\019\001\029\001\020\001\021\001\022\001\
\023\001\024\001\029\001\026\001\027\001\012\001\013\001\014\001\
\015\001\029\001\017\001\018\001\019\001\012\001\013\001\014\001\
\015\001\029\001\017\001\018\001\019\001\029\001\255\255\029\001\
\029\001\029\001\029\001\029\001\029\001\029\001\255\255\029\001\
\029\001\029\001\029\001"

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
# 319 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'display) in
    Obj.repr(
# 33 "parser.mly"
                                     (  )
# 326 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'lasf) in
    Obj.repr(
# 34 "parser.mly"
                                  ( )
# 333 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                                 ( printlist (!nodelist @ !transition) )
# 339 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 41 "parser.mly"
                                ( createfile  _2  !nodelist !transition)
# 346 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 42 "parser.mly"
                                            (dumpwithstring _2 !nodelist !transition _4)
# 354 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                     (Printf.printf "%b\n " (is_complete !nodelist !transition))
# 360 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'labelnoeud) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 48 "parser.mly"
                                                    (nodelist := completeaux _3 _5 _6 !nodelist !transition;transition:= complete _3 !nodelist !transition;)
# 369 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                          (Printf.printf "%b\n " (is_deterministic !nodelist !transition))
# 375 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 50 "parser.mly"
                              (Printf.printf "%b\n " (is_accepted !nodelist !transition _2))
# 382 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 51 "parser.mly"
                         (Printf.printf "%s\n " (getchemin !nodelist !transition _2))
# 389 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 52 "parser.mly"
                                  (nodelist:= showcomplet _3 !nodelist !transition )
# 396 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 53 "parser.mly"
                                       (nodelist := showcompletd _3 !nodelist !transition)
# 403 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
                (_1)
# 410 "parser.ml"
               : 'numero))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "parser.mly"
              (_1)
# 417 "parser.ml"
               : 'labelnoeud))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
                (String.sub _1 1 ((String.length _1) -2)  )
# 424 "parser.ml"
               : 'vrailabel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
              ( python_split ',' (String.sub _1 1 ((String.length _1) -2)) )
# 431 "parser.ml"
               : 'glist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 70 "parser.mly"
                              (" LABEL: " ^ _2 )
# 438 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 71 "parser.mly"
                             (" COLOR: " ^ _2 )
# 445 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 72 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 452 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 73 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 459 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 74 "parser.mly"
                            (" FINAL: " ^ _2 )
# 466 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                     (" INITIAL: " ^ "" )
# 472 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                   (" FINAL: " ^ "" )
# 478 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 77 "parser.mly"
                         (" SIZE: " ^ _2)
# 485 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 79 "parser.mly"
                                      (" LABEL: " ^ _2 ^ _3 )
# 493 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 80 "parser.mly"
                                      (" COLOR: " ^ _2 ^ _3 )
# 501 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 81 "parser.mly"
                                  (" SIZE: " ^ _2 ^ _3 )
# 509 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 82 "parser.mly"
                                        (" BGCOLOR: " ^ _2 ^ _3 )
# 517 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 83 "parser.mly"
                                       ( " INITIAL: " ^ _2 ^ _3 )
# 525 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 84 "parser.mly"
                                      ( " FINAL: " ^ _2 ^ _3 )
# 533 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 85 "parser.mly"
                              ( " INITIAL: " ^ "" ^ _2 )
# 540 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 86 "parser.mly"
                             ( " FINAL: " ^ "" ^ _2)
# 547 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
            ("Nord")
# 553 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
            ("Sud")
# 559 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
            ("Est")
# 565 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
            ("Ouest")
# 571 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
             ("Nord-West")
# 577 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
             ("Nord-Est")
# 583 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
             ("Sud-Ouest")
# 589 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
             ("Sud-Est")
# 595 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 101 "parser.mly"
                             (" COLOR: " ^ _2 )
# 602 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 102 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 609 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 103 "parser.mly"
                              (" POSITION: " ^ _2^":"^ _3)
# 617 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 106 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 625 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 107 "parser.mly"
                                     (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 633 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 108 "parser.mly"
                                        (" POSITION: " ^ _2^":"^ _3 ^ _4)
# 642 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 113 "parser.mly"
                            (" LABEL: " ^ _2 )
# 649 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 114 "parser.mly"
                             (" COLOR: " ^ _2 )
# 656 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 115 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 663 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 116 "parser.mly"
                                             (" POSITION: " ^ _4^":"^ _5)
# 672 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 118 "parser.mly"
                                        (" COLOR: " ^ _2 ^ _3 )
# 680 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 119 "parser.mly"
                                      (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 688 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 120 "parser.mly"
                                       (" LABEL: " ^ _2 ^ _3 )
# 696 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 121 "parser.mly"
                                                         (" POSITION: " ^ _4^":"^ _5 ^ _6)
# 706 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 125 "parser.mly"
                              (" LABEL: " ^ _2 )
# 713 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 126 "parser.mly"
                             (" COLOR: " ^ _2 )
# 720 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 127 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 727 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 128 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 734 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 129 "parser.mly"
                            (" FINAL: " ^ _2 )
# 741 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    Obj.repr(
# 130 "parser.mly"
                     (" INITIAL: " ^ "" )
# 747 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
                   (" FINAL: " ^ "" )
# 753 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 132 "parser.mly"
                         (" SIZE: " ^ _2)
# 760 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 133 "parser.mly"
                             (" X: " ^ _2 ^ " Y: " ^ _3 )
# 768 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 135 "parser.mly"
                                        (" LABEL: " ^ _2 ^ _3)
# 776 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 136 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 784 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 137 "parser.mly"
                                         (" BGCOLOR: " ^ _2 ^ _3 )
# 792 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 138 "parser.mly"
                                        (" INITIAL: " ^ _2 ^ _3 )
# 800 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 139 "parser.mly"
                                      (" FINAL: " ^ _2 ^ _3 )
# 808 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 140 "parser.mly"
                               (" INITIAL: " ^  ""  ^ _2 )
# 815 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 141 "parser.mly"
                             (" FINAL: " ^  ""  ^ _2 )
# 822 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 142 "parser.mly"
                                    (" SIZE: " ^ _2 ^ _3)
# 830 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 143 "parser.mly"
                                        (" X: " ^ _2 ^ " Y: " ^ _3 ^ _4)
# 839 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 150 "parser.mly"
                                                     ( nodelist := !nodelist @  (add (Noeud(createid _2, ( _4), ( _5),_6)) !nodelist !transition)  )
# 849 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 151 "parser.mly"
                                                      ( nodelist := !nodelist @ ( add (Noeud(createid _2, ( _5), ( _6),_3)) !nodelist !transition) )
# 859 "parser.ml"
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
# 870 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 153 "parser.mly"
                                             (  nodelist := !nodelist @ ( add (Noeud(createid _2, ( _4), ( _5),"")) !nodelist !transition))
# 879 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 156 "parser.mly"
                                                 ( transition := !transition @ (add (Edge(createid _2,createid _4,_6,"")) !nodelist !transition ) )
# 888 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 157 "parser.mly"
                                                           ( transition := !transition @ (add (Edge(createid _2,createid _4,_6,_7)) !nodelist !transition)  )
# 898 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 158 "parser.mly"
                                                            ( transition := !transition @ ( add (Edge(createid _2,createid _4,_7,_5)) !nodelist !transition)   )
# 908 "parser.ml"
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
# 919 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 161 "parser.mly"
                                              ( Printf.printf "Il faut rajouter le label de la transition :o \n")
# 928 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vrailabel) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 162 "parser.mly"
                                                              ( transition := editt (createid _2) ( createid _4) _8 _6 !transition )
# 938 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 163 "parser.mly"
                                    (nodelist := editn (createid _2) _4 !nodelist )
# 946 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 165 "parser.mly"
                      ( nodelist := removenoeud (createid _2) !nodelist ; transition:= removetransitionafternode (createid _2) !transition;  )
# 953 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 166 "parser.mly"
                                 (  transition := removetransition (createid _2) (createid _4) !transition )
# 961 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 168 "parser.mly"
                                (nodelist := moveall _2 _3 !nodelist)
# 969 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 169 "parser.mly"
                                   (nodelist := moveallid_aux (createid _2) _3 _4 !nodelist)
# 978 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'glist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 170 "parser.mly"
                                      (nodelist := movelistid _2 _3 _4 !nodelist)
# 987 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 172 "parser.mly"
                              (nodelist:= renamen (createid _2) (createid _4) !nodelist ; transition:= renamet (createid _2) (createid _4) !transition ;)
# 995 "parser.ml"
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
