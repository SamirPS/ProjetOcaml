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

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
        
        open Functionannexe;;


        let nodelist = ref [] ;;
        let transition = ref [];;

        

      
# 54 "parser.ml"
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
    0|]

let yytransl_block = [|
  257 (* ID *);
  258 (* NUM *);
  259 (* LABELN *);
  260 (* LIST *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\003\000\004\000\004\000\004\000\
\004\000\004\000\007\000\006\000\005\000\008\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\011\000\011\000\011\000\011\000\011\000\011\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\002\000\002\000\001\000\002\000\001\000\006\000\001\000\
\002\000\002\000\001\000\001\000\001\000\001\000\002\000\002\000\
\002\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\004\000\
\002\000\002\000\002\000\005\000\003\000\003\000\003\000\006\000\
\002\000\002\000\002\000\002\000\002\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\004\000\006\000\006\000\
\007\000\005\000\006\000\007\000\007\000\008\000\008\000\004\000\
\002\000\006\000\003\000\004\000\004\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\008\000\000\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\079\000\000\000\000\000\000\000\013\000\010\000\009\000\000\000\
\000\000\000\000\005\000\073\000\000\000\000\000\011\000\014\000\
\000\000\000\000\000\000\000\000\000\000\001\000\002\000\003\000\
\012\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\075\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\000\000\
\000\000\000\000\000\000\000\000\000\000\076\000\077\000\078\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\072\000\
\000\000\000\000\000\000\021\000\022\000\023\000\025\000\026\000\
\024\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\007\000\063\000\000\000\000\000\000\000\000\000\000\000\000\000\
\074\000\000\000\056\000\057\000\061\000\059\000\060\000\058\000\
\000\000\065\000\000\000\068\000\038\000\039\000\000\000\062\000\
\000\000\040\000\070\000\000\000\000\000\000\000\071\000\000\000\
\000\000\000\000\000\000\047\000\045\000\046\000\000\000\000\000\
\048\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\020\000\022\000\042\000\033\000\034\000\
\050\000\072\000\103\000\143\000\088\000"

let yysindex = "\002\000\
\083\255\000\000\025\255\025\255\000\000\244\254\000\000\036\255\
\049\255\025\255\053\255\059\255\163\255\062\255\070\255\076\255\
\000\000\243\254\055\255\056\255\000\000\000\000\000\000\085\255\
\136\255\079\255\000\000\000\000\086\255\109\255\000\000\000\000\
\109\255\109\255\088\255\089\255\096\255\000\000\000\000\000\000\
\000\000\105\255\109\255\025\255\025\255\109\255\110\255\110\255\
\025\255\107\255\119\255\120\255\109\255\000\000\109\255\131\255\
\144\255\151\255\109\255\109\255\116\255\116\255\116\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\116\255\
\116\255\116\255\109\255\044\255\135\255\000\000\000\000\000\000\
\109\255\025\255\025\255\109\255\110\255\110\255\025\255\000\000\
\153\255\109\255\116\255\000\000\000\000\000\000\000\000\000\000\
\000\000\109\255\109\255\025\255\025\255\025\255\155\255\025\255\
\109\255\144\255\144\255\144\255\144\255\144\255\144\255\025\255\
\000\000\000\000\116\255\109\255\087\255\087\255\087\255\025\255\
\000\000\144\255\000\000\000\000\000\000\000\000\000\000\000\000\
\145\255\000\000\087\255\000\000\000\000\000\000\087\255\000\000\
\054\255\000\000\000\000\025\255\025\255\025\255\000\000\052\255\
\054\255\054\255\109\255\000\000\000\000\000\000\109\255\054\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\140\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\255\003\255\010\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\255\
\015\255\024\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\142\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\143\255\146\255\147\255\148\255\149\255\150\255\000\000\
\000\000\000\000\152\255\000\000\154\255\245\254\250\254\000\000\
\000\000\156\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\030\255\000\000\000\000\000\000\157\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\158\255\
\159\255\160\255\000\000\000\000\000\000\000\000\000\000\161\255\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\252\255\000\000\227\255\000\000\
\203\255\219\255\172\255\185\255\017\000"

let yytablesize = 190
let yytable = "\023\000\
\053\000\035\000\001\000\054\000\055\000\027\000\036\000\092\000\
\093\000\094\000\073\000\015\000\024\000\060\000\016\000\038\000\
\063\000\035\000\095\000\096\000\097\000\020\000\036\000\078\000\
\018\000\079\000\019\000\021\000\015\000\090\000\091\000\016\000\
\132\000\133\000\134\000\017\000\025\000\114\000\020\000\061\000\
\062\000\018\000\037\000\019\000\074\000\098\000\138\000\109\000\
\110\000\026\000\139\000\105\000\017\000\028\000\108\000\099\000\
\100\000\101\000\037\000\029\000\113\000\130\000\035\000\147\000\
\140\000\141\000\140\000\141\000\115\000\116\000\036\000\102\000\
\148\000\149\000\150\000\122\000\037\000\106\000\107\000\142\000\
\153\000\142\000\111\000\039\000\040\000\041\000\131\000\003\000\
\004\000\005\000\006\000\007\000\008\000\009\000\051\000\117\000\
\118\000\119\000\099\000\121\000\101\000\052\000\010\000\011\000\
\012\000\013\000\014\000\129\000\015\000\016\000\031\000\058\000\
\056\000\057\000\102\000\135\000\059\000\151\000\075\000\076\000\
\077\000\152\000\123\000\124\000\125\000\126\000\127\000\128\000\
\044\000\045\000\046\000\080\000\047\000\048\000\049\000\144\000\
\145\000\146\000\136\000\064\000\065\000\066\000\067\000\068\000\
\069\000\070\000\071\000\043\000\044\000\045\000\046\000\089\000\
\047\000\048\000\049\000\081\000\082\000\083\000\084\000\104\000\
\085\000\086\000\087\000\030\000\031\000\112\000\032\000\120\000\
\004\000\137\000\066\000\049\000\000\000\000\000\050\000\054\000\
\052\000\053\000\051\000\000\000\064\000\000\000\067\000\000\000\
\055\000\069\000\041\000\042\000\043\000\044\000"

let yycheck = "\004\000\
\030\000\013\001\001\000\033\000\034\000\010\000\013\001\061\000\
\062\000\063\000\048\000\012\001\025\001\043\000\012\001\029\001\
\046\000\029\001\072\000\073\000\074\000\012\001\029\001\053\000\
\012\001\055\000\012\001\003\001\029\001\059\000\060\000\029\001\
\117\000\118\000\119\000\012\001\001\001\091\000\029\001\044\000\
\045\000\029\001\013\001\029\001\049\000\075\000\131\000\085\000\
\086\000\001\001\135\000\081\000\029\001\001\001\084\000\012\001\
\013\001\014\001\029\001\001\001\090\000\115\000\001\001\012\001\
\013\001\014\001\013\001\014\001\098\000\099\000\001\001\028\001\
\144\000\145\000\146\000\105\000\001\001\082\000\083\000\028\001\
\152\000\028\001\087\000\029\001\029\001\001\001\116\000\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\016\001\100\000\
\101\000\102\000\012\001\104\000\014\001\016\001\020\001\021\001\
\022\001\023\001\024\001\112\000\026\001\027\001\002\001\016\001\
\025\001\025\001\028\001\120\000\012\001\147\000\012\001\001\001\
\001\001\151\000\106\000\107\000\108\000\109\000\110\000\111\000\
\013\001\014\001\015\001\001\001\017\001\018\001\019\001\140\000\
\141\000\142\000\122\000\030\001\031\001\032\001\033\001\034\001\
\035\001\036\001\037\001\012\001\013\001\014\001\015\001\001\001\
\017\001\018\001\019\001\012\001\013\001\014\001\015\001\025\001\
\017\001\018\001\019\001\001\001\002\001\013\001\004\001\013\001\
\029\001\025\001\029\001\029\001\255\255\255\255\029\001\029\001\
\029\001\029\001\029\001\255\255\029\001\255\255\029\001\255\255\
\029\001\029\001\029\001\029\001\029\001\029\001"

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
# 299 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'display) in
    Obj.repr(
# 33 "parser.mly"
                                     (  )
# 306 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'lasf) in
    Obj.repr(
# 34 "parser.mly"
                                  ( )
# 313 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                                 ( printlist (!nodelist @ !transition) )
# 319 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 41 "parser.mly"
                                ( createfile  _2  !nodelist !transition;dotBDD _2 !nodelist !transition)
# 326 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                     (Printf.printf "%b\n " (is_complete !nodelist !transition))
# 332 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'labelnoeud) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 47 "parser.mly"
                                                    (nodelist := completeaux _3 _5 _6 !nodelist !transition;transition:= complete _3 !nodelist !transition;)
# 341 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                          (Printf.printf "%b\n " (is_deterministic !nodelist !transition))
# 347 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 49 "parser.mly"
                              (Printf.printf "%b\n " (is_accepted !nodelist !transition _2))
# 354 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 50 "parser.mly"
                         (Printf.printf "%s\n " (getchemin !nodelist !transition _2))
# 361 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
                (_1)
# 368 "parser.ml"
               : 'numero))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
              (_1)
# 375 "parser.ml"
               : 'labelnoeud))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "parser.mly"
                (String.sub _1 1 ((String.length _1) -2)  )
# 382 "parser.ml"
               : 'vrailabel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
              ( python_split ',' (String.sub _1 1 ((String.length _1) -2)) )
# 389 "parser.ml"
               : 'glist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 67 "parser.mly"
                              (" LABEL: " ^ _2 )
# 396 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 68 "parser.mly"
                             (" COLOR: " ^ _2 )
# 403 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 69 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 410 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 70 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 417 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 71 "parser.mly"
                            (" FINAL: " ^ _2 )
# 424 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 72 "parser.mly"
                         (" SIZE: " ^ _2)
# 431 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 74 "parser.mly"
                                      (" LABEL: " ^ _2 ^ _3 )
# 439 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 75 "parser.mly"
                                      (" COLOR: " ^ _2 ^ _3 )
# 447 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 76 "parser.mly"
                                  (" SIZE: " ^ _2 ^ _3 )
# 455 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 77 "parser.mly"
                                        (" BGCOLOR: " ^ _2 ^ _3 )
# 463 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 78 "parser.mly"
                                       ( " INITIAL: " ^ _2 ^ _3 )
# 471 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 79 "parser.mly"
                                      ( " FINAL: " ^ _2 ^ _3 )
# 479 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
            ("Nord")
# 485 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
            ("Sud")
# 491 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
            ("Est")
# 497 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
            ("Ouest")
# 503 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
             ("Nord West")
# 509 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
             ("Nord Est")
# 515 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
             ("Sud Ouest")
# 521 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
             ("Sud Est")
# 527 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 94 "parser.mly"
                             (" COLOR: " ^ _2 )
# 534 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 95 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 541 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 96 "parser.mly"
                              (" POSITION: " ^ _2^":"^ _3)
# 549 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 99 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 557 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 100 "parser.mly"
                                     (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 565 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 101 "parser.mly"
                                        (" POSITION: " ^ _2^":"^ _3 ^ _4)
# 574 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 106 "parser.mly"
                            (" LABEL: " ^ _2 )
# 581 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 107 "parser.mly"
                             (" COLOR: " ^ _2 )
# 588 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 108 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 595 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 109 "parser.mly"
                                             (" POSITION: " ^ _4^":"^ _5)
# 604 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 111 "parser.mly"
                                        (" COLOR: " ^ _2 ^ _3 )
# 612 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 112 "parser.mly"
                                      (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 620 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 113 "parser.mly"
                                       (" LABEL: " ^ _2 ^ _3 )
# 628 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 114 "parser.mly"
                                                         (" POSITION: " ^ _4^":"^ _5 ^ _6)
# 638 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 118 "parser.mly"
                              (" LABEL: " ^ _2 )
# 645 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 119 "parser.mly"
                             (" COLOR: " ^ _2 )
# 652 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 120 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 659 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 121 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 666 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 122 "parser.mly"
                            (" FINAL: " ^ _2 )
# 673 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 123 "parser.mly"
                         (" SIZE: " ^ _2)
# 680 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 124 "parser.mly"
                             (" X: " ^ _2 ^ " Y: " ^ _3 )
# 688 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 126 "parser.mly"
                                        (" LABEL: " ^ _2 ^ _3)
# 696 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 127 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 704 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 128 "parser.mly"
                                         (" BGCOLOR: " ^ _2 ^ _3 )
# 712 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 129 "parser.mly"
                                        (" INITIAL: " ^ _2 ^ _3 )
# 720 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 130 "parser.mly"
                                      (" FINAL: " ^ _2 ^ _3 )
# 728 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 131 "parser.mly"
                                    (" SIZE: " ^ _2 ^ _3)
# 736 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 132 "parser.mly"
                                        (" X: " ^ _2 ^ " Y: " ^ _3 ^ _4)
# 745 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 139 "parser.mly"
                                                     ( nodelist := ( add (Noeud(createid _2, ( _4), ( _5),_6)) !nodelist !transition) @ !nodelist )
# 755 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 140 "parser.mly"
                                                      ( nodelist := ( add (Noeud(createid _2, ( _5), ( _6),_3)) !nodelist !transition) @ !nodelist )
# 765 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 141 "parser.mly"
                                                                ( nodelist := ( add (Noeud( createid _2, ( _5), ( _6),_3 ^ (" "  ^ _7))) !nodelist !transition) @ !nodelist )
# 776 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 142 "parser.mly"
                                             (  nodelist := ( add (Noeud(createid _2, ( _4), ( _5),"")) !nodelist !transition) @ !nodelist)
# 785 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 145 "parser.mly"
                                                 ( transition := (add (Edge(createid _2,createid _4,_6,"")) !nodelist !transition )@ !transition )
# 794 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 146 "parser.mly"
                                                           ( transition := (add (Edge(createid _2,createid _4,_6,_7)) !nodelist !transition) @ !transition )
# 804 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 147 "parser.mly"
                                                            ( transition := ( add (Edge(createid _2,createid _4,_7,_5)) !nodelist !transition)  @ !transition )
# 814 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 148 "parser.mly"
                                                                       ( transition := (add (Edge(createid _2,createid _4,_7,_5 ^ (" "^ _8))) !nodelist !transition)  @ !transition )
# 825 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vrailabel) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 150 "parser.mly"
                                                              ( transition := editt (createid _2) ( createid _4) _6 _8  !transition )
# 835 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 151 "parser.mly"
                                    (nodelist := editn (createid _2) _4 !nodelist )
# 843 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 153 "parser.mly"
                      ( nodelist := removenoeud (createid _2) !nodelist ; transition:= removetransitionafternode (createid _2) !transition;  )
# 850 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 154 "parser.mly"
                                               (  transition := removetransition (createid _2) (createid _4) _6 !transition )
# 859 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 156 "parser.mly"
                                (nodelist := moveall _2 _3 !nodelist)
# 867 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 157 "parser.mly"
                                   (nodelist := moveallid_aux (createid _2) _3 _4 !nodelist)
# 876 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'glist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 158 "parser.mly"
                                      (nodelist := movelistid _2 _3 _4 !nodelist)
# 885 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 160 "parser.mly"
                              (nodelist:= renamen (createid _2) (createid _4) !nodelist ; transition:= renamet (createid _2) (createid _4) !transition ;)
# 893 "parser.ml"
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
