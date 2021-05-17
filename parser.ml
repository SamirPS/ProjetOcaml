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
\009\000\009\000\009\000\009\000\009\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\011\000\011\000\
\011\000\011\000\011\000\011\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\002\000\002\000\001\000\002\000\004\000\001\000\006\000\
\001\000\002\000\002\000\003\000\003\000\001\000\001\000\001\000\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000\002\000\
\003\000\003\000\003\000\004\000\002\000\002\000\002\000\005\000\
\003\000\003\000\003\000\006\000\002\000\002\000\002\000\002\000\
\002\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\004\000\006\000\006\000\007\000\005\000\006\000\007\000\
\007\000\008\000\008\000\004\000\002\000\004\000\003\000\004\000\
\004\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\009\000\000\000\007\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\083\000\000\000\000\000\000\000\016\000\000\000\000\000\011\000\
\010\000\000\000\000\000\000\000\000\000\077\000\000\000\000\000\
\014\000\017\000\000\000\000\000\000\000\000\000\000\000\001\000\
\002\000\003\000\012\000\013\000\015\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\079\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\030\000\000\000\000\000\000\000\
\000\000\000\000\006\000\078\000\080\000\081\000\082\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\076\000\000\000\
\000\000\000\000\024\000\025\000\026\000\028\000\029\000\027\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\000\067\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\060\000\
\061\000\065\000\063\000\064\000\062\000\000\000\069\000\000\000\
\072\000\042\000\043\000\000\000\066\000\000\000\044\000\074\000\
\000\000\000\000\000\000\075\000\000\000\000\000\000\000\000\000\
\051\000\049\000\050\000\000\000\000\000\052\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\020\000\024\000\046\000\035\000\036\000\
\054\000\078\000\110\000\148\000\095\000"

let yysindex = "\016\000\
\110\255\000\000\004\255\018\255\000\000\007\255\000\000\028\255\
\054\255\018\255\061\255\074\255\087\255\075\255\076\255\079\255\
\000\000\027\255\063\255\064\255\000\000\018\255\018\255\000\000\
\000\000\093\255\145\255\086\255\081\255\000\000\091\255\106\255\
\000\000\000\000\106\255\106\255\084\255\085\255\096\255\000\000\
\000\000\000\000\000\000\000\000\000\000\101\255\106\255\018\255\
\018\255\106\255\114\255\114\255\018\255\111\255\123\255\018\255\
\125\255\106\255\000\000\106\255\126\255\153\255\127\255\106\255\
\106\255\160\255\160\255\160\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\160\255\160\255\160\255\
\106\255\012\255\000\000\000\000\000\000\000\000\000\000\106\255\
\018\255\018\255\106\255\114\255\114\255\018\255\000\000\122\255\
\106\255\160\255\000\000\000\000\000\000\000\000\000\000\000\000\
\106\255\106\255\018\255\018\255\018\255\143\255\106\255\153\255\
\153\255\153\255\153\255\153\255\153\255\018\255\000\000\000\000\
\160\255\106\255\046\255\046\255\046\255\018\255\153\255\000\000\
\000\000\000\000\000\000\000\000\000\000\113\255\000\000\046\255\
\000\000\000\000\000\000\046\255\000\000\055\255\000\000\000\000\
\018\255\018\255\018\255\000\000\059\255\055\255\055\255\106\255\
\000\000\000\000\000\000\106\255\055\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\140\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\147\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\254\254\255\254\001\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\010\255\024\255\038\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\151\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\152\255\
\154\255\155\255\156\255\157\255\158\255\000\000\000\000\000\000\
\159\255\000\000\161\255\022\255\052\255\000\000\162\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\053\255\
\000\000\000\000\000\000\163\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\164\255\165\255\166\255\000\000\
\000\000\000\000\000\000\000\000\167\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\252\255\000\000\229\255\000\000\
\192\255\205\255\179\255\004\000\240\255"

let yytablesize = 196
let yytable = "\025\000\
\079\000\099\000\100\000\101\000\058\000\029\000\021\000\059\000\
\060\000\018\000\019\000\022\000\023\000\102\000\103\000\104\000\
\001\000\043\000\044\000\065\000\021\000\021\000\068\000\106\000\
\107\000\108\000\018\000\019\000\027\000\023\000\085\000\026\000\
\086\000\120\000\039\000\022\000\097\000\098\000\021\000\109\000\
\115\000\116\000\023\000\066\000\067\000\137\000\138\000\139\000\
\080\000\020\000\039\000\083\000\022\000\105\000\028\000\040\000\
\135\000\106\000\143\000\108\000\111\000\030\000\144\000\114\000\
\040\000\041\000\020\000\145\000\146\000\119\000\152\000\145\000\
\146\000\109\000\031\000\037\000\038\000\121\000\122\000\039\000\
\040\000\041\000\147\000\127\000\112\000\113\000\147\000\032\000\
\033\000\117\000\034\000\041\000\042\000\045\000\136\000\128\000\
\129\000\130\000\131\000\132\000\133\000\055\000\123\000\124\000\
\125\000\056\000\057\000\033\000\061\000\062\000\141\000\063\000\
\064\000\134\000\003\000\004\000\005\000\006\000\007\000\008\000\
\009\000\140\000\081\000\082\000\156\000\084\000\087\000\096\000\
\157\000\010\000\011\000\012\000\013\000\014\000\118\000\015\000\
\016\000\142\000\000\000\000\000\149\000\150\000\151\000\069\000\
\070\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
\153\000\154\000\155\000\126\000\047\000\048\000\049\000\050\000\
\158\000\051\000\052\000\053\000\088\000\089\000\090\000\091\000\
\004\000\092\000\093\000\094\000\048\000\049\000\050\000\005\000\
\051\000\052\000\053\000\070\000\053\000\000\000\054\000\058\000\
\056\000\057\000\055\000\068\000\000\000\071\000\059\000\073\000\
\045\000\046\000\047\000\048\000"

let yycheck = "\004\000\
\052\000\066\000\067\000\068\000\032\000\010\000\003\001\035\000\
\036\000\012\001\012\001\008\001\012\001\078\000\079\000\080\000\
\001\000\022\000\023\000\047\000\003\001\012\001\050\000\012\001\
\013\001\014\001\029\001\029\001\001\001\029\001\058\000\025\001\
\060\000\098\000\013\001\012\001\064\000\065\000\029\001\028\001\
\092\000\093\000\039\001\048\000\049\000\123\000\124\000\125\000\
\053\000\012\001\029\001\056\000\029\001\081\000\001\001\029\001\
\121\000\012\001\136\000\014\001\088\000\001\001\140\000\091\000\
\013\001\013\001\029\001\013\001\014\001\097\000\012\001\013\001\
\014\001\028\001\001\001\001\001\001\001\105\000\106\000\001\001\
\029\001\029\001\028\001\111\000\089\000\090\000\028\001\001\001\
\002\001\094\000\004\001\029\001\029\001\001\001\122\000\112\000\
\113\000\114\000\115\000\116\000\117\000\016\001\107\000\108\000\
\109\000\025\001\016\001\002\001\025\001\025\001\127\000\016\001\
\012\001\118\000\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\126\000\012\001\001\001\152\000\001\001\001\001\001\001\
\156\000\020\001\021\001\022\001\023\001\024\001\013\001\026\001\
\027\001\025\001\255\255\255\255\145\000\146\000\147\000\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\038\001\
\149\000\150\000\151\000\013\001\012\001\013\001\014\001\015\001\
\157\000\017\001\018\001\019\001\012\001\013\001\014\001\015\001\
\029\001\017\001\018\001\019\001\013\001\014\001\015\001\029\001\
\017\001\018\001\019\001\029\001\029\001\255\255\029\001\029\001\
\029\001\029\001\029\001\029\001\255\255\029\001\029\001\029\001\
\029\001\029\001\029\001\029\001"

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
# 309 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'display) in
    Obj.repr(
# 33 "parser.mly"
                                     (  )
# 316 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'lasf) in
    Obj.repr(
# 34 "parser.mly"
                                  ( )
# 323 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                                 ( printlist (!nodelist @ !transition) )
# 329 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 41 "parser.mly"
                                ( createfile  _2  !nodelist !transition)
# 336 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 42 "parser.mly"
                                            (dumpwithstring _2 !nodelist !transition _4)
# 344 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                     (Printf.printf "%b\n " (is_complete !nodelist !transition))
# 350 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'labelnoeud) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 48 "parser.mly"
                                                    (nodelist := completeaux _3 _5 _6 !nodelist !transition;transition:= complete _3 !nodelist !transition;)
# 359 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                          (Printf.printf "%b\n " (is_deterministic !nodelist !transition))
# 365 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 50 "parser.mly"
                              (Printf.printf "%b\n " (is_accepted !nodelist !transition _2))
# 372 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 51 "parser.mly"
                         (Printf.printf "%s\n " (getchemin !nodelist !transition _2))
# 379 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 52 "parser.mly"
                                  (nodelist:= showcomplet _3 !nodelist !transition )
# 386 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 53 "parser.mly"
                                       (nodelist := showcompletd _3 !nodelist !transition)
# 393 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
                (_1)
# 400 "parser.ml"
               : 'numero))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "parser.mly"
              (_1)
# 407 "parser.ml"
               : 'labelnoeud))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
                (String.sub _1 1 ((String.length _1) -2)  )
# 414 "parser.ml"
               : 'vrailabel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
              ( python_split ',' (String.sub _1 1 ((String.length _1) -2)) )
# 421 "parser.ml"
               : 'glist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 70 "parser.mly"
                              (" LABEL: " ^ _2 )
# 428 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 71 "parser.mly"
                             (" COLOR: " ^ _2 )
# 435 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 72 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 442 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 73 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 449 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 74 "parser.mly"
                            (" FINAL: " ^ _2 )
# 456 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 75 "parser.mly"
                         (" SIZE: " ^ _2)
# 463 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 77 "parser.mly"
                                      (" LABEL: " ^ _2 ^ _3 )
# 471 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 78 "parser.mly"
                                      (" COLOR: " ^ _2 ^ _3 )
# 479 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 79 "parser.mly"
                                  (" SIZE: " ^ _2 ^ _3 )
# 487 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 80 "parser.mly"
                                        (" BGCOLOR: " ^ _2 ^ _3 )
# 495 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 81 "parser.mly"
                                       ( " INITIAL: " ^ _2 ^ _3 )
# 503 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 82 "parser.mly"
                                      ( " FINAL: " ^ _2 ^ _3 )
# 511 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
               ("none")
# 517 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
            ("Nord")
# 523 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
            ("Sud")
# 529 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
            ("Est")
# 535 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
            ("Ouest")
# 541 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
             ("Nord-West")
# 547 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
             ("Nord-Est")
# 553 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
             ("Sud-Ouest")
# 559 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
             ("Sud-Est")
# 565 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 98 "parser.mly"
                             (" COLOR: " ^ _2 )
# 572 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 99 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 579 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 100 "parser.mly"
                              (" POSITION: " ^ _2^":"^ _3)
# 587 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 103 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 595 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 104 "parser.mly"
                                     (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 603 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 105 "parser.mly"
                                        (" POSITION: " ^ _2^":"^ _3 ^ _4)
# 612 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 110 "parser.mly"
                            (" LABEL: " ^ _2 )
# 619 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 111 "parser.mly"
                             (" COLOR: " ^ _2 )
# 626 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 112 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 633 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 113 "parser.mly"
                                             (" POSITION: " ^ _4^":"^ _5)
# 642 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 115 "parser.mly"
                                        (" COLOR: " ^ _2 ^ _3 )
# 650 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 116 "parser.mly"
                                      (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 658 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 117 "parser.mly"
                                       (" LABEL: " ^ _2 ^ _3 )
# 666 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 118 "parser.mly"
                                                         (" POSITION: " ^ _4^":"^ _5 ^ _6)
# 676 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 122 "parser.mly"
                              (" LABEL: " ^ _2 )
# 683 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 123 "parser.mly"
                             (" COLOR: " ^ _2 )
# 690 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 124 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 697 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 125 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 704 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 126 "parser.mly"
                            (" FINAL: " ^ _2 )
# 711 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 127 "parser.mly"
                         (" SIZE: " ^ _2)
# 718 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 128 "parser.mly"
                             (" X: " ^ _2 ^ " Y: " ^ _3 )
# 726 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 130 "parser.mly"
                                        (" LABEL: " ^ _2 ^ _3)
# 734 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 131 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 742 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 132 "parser.mly"
                                         (" BGCOLOR: " ^ _2 ^ _3 )
# 750 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 133 "parser.mly"
                                        (" INITIAL: " ^ _2 ^ _3 )
# 758 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 134 "parser.mly"
                                      (" FINAL: " ^ _2 ^ _3 )
# 766 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 135 "parser.mly"
                                    (" SIZE: " ^ _2 ^ _3)
# 774 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 136 "parser.mly"
                                        (" X: " ^ _2 ^ " Y: " ^ _3 ^ _4)
# 783 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 143 "parser.mly"
                                                     ( nodelist := ( add (Noeud(createid _2, ( _4), ( _5),_6)) !nodelist !transition) @ !nodelist )
# 793 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 144 "parser.mly"
                                                      ( nodelist := ( add (Noeud(createid _2, ( _5), ( _6),_3)) !nodelist !transition) @ !nodelist )
# 803 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 145 "parser.mly"
                                                                ( nodelist := ( add (Noeud( createid _2, ( _5), ( _6),_3 ^ (" "  ^ _7))) !nodelist !transition) @ !nodelist )
# 814 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 146 "parser.mly"
                                             (  nodelist := ( add (Noeud(createid _2, ( _4), ( _5),"")) !nodelist !transition) @ !nodelist)
# 823 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 149 "parser.mly"
                                                 ( transition := (add (Edge(createid _2,createid _4,_6,"")) !nodelist !transition )@ !transition )
# 832 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 150 "parser.mly"
                                                           ( transition := (add (Edge(createid _2,createid _4,_6,_7)) !nodelist !transition) @ !transition )
# 842 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 151 "parser.mly"
                                                            ( transition := ( add (Edge(createid _2,createid _4,_7,_5)) !nodelist !transition)  @ !transition )
# 852 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 152 "parser.mly"
                                                                       ( transition := (add (Edge(createid _2,createid _4,_7,_5 ^ (" "^ _8))) !nodelist !transition)  @ !transition )
# 863 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vrailabel) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 154 "parser.mly"
                                                              ( transition := editt (createid _2) ( createid _4) _8 _6 !transition )
# 873 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 155 "parser.mly"
                                    (nodelist := editn (createid _2) _4 !nodelist )
# 881 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 157 "parser.mly"
                      ( nodelist := removenoeud (createid _2) !nodelist ; transition:= removetransitionafternode (createid _2) !transition;  )
# 888 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 158 "parser.mly"
                                 (  transition := removetransition (createid _2) (createid _4) !transition )
# 896 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 160 "parser.mly"
                                (nodelist := moveall _2 _3 !nodelist)
# 904 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 161 "parser.mly"
                                   (nodelist := moveallid_aux (createid _2) _3 _4 !nodelist)
# 913 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'glist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 162 "parser.mly"
                                      (nodelist := movelistid _2 _3 _4 !nodelist)
# 922 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 164 "parser.mly"
                              (nodelist:= renamen (createid _2) (createid _4) !nodelist ; transition:= renamet (createid _2) (createid _4) !transition ;)
# 930 "parser.ml"
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
