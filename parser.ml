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
\002\000\004\000\003\000\004\000\004\000\004\000\002\000"

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
\000\000\000\000\000\000\000\000\074\000\076\000\077\000\078\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\072\000\
\000\000\000\000\000\000\021\000\022\000\023\000\025\000\026\000\
\024\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\063\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\056\000\057\000\058\000\060\000\061\000\059\000\000\000\065\000\
\000\000\068\000\038\000\039\000\000\000\062\000\000\000\040\000\
\070\000\000\000\000\000\000\000\071\000\000\000\000\000\000\000\
\000\000\047\000\045\000\046\000\000\000\000\000\048\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\020\000\022\000\042\000\033\000\034\000\
\050\000\072\000\103\000\141\000\088\000"

let yysindex = "\001\000\
\082\255\000\000\010\255\010\255\000\000\234\254\000\000\034\255\
\035\255\010\255\038\255\053\255\127\255\066\255\070\255\073\255\
\000\000\024\255\047\255\065\255\000\000\000\000\000\000\080\255\
\134\255\057\255\000\000\000\000\079\255\097\255\000\000\000\000\
\097\255\097\255\075\255\088\255\098\255\000\000\000\000\000\000\
\000\000\105\255\097\255\010\255\010\255\097\255\108\255\108\255\
\010\255\107\255\081\255\120\255\097\255\000\000\097\255\129\255\
\142\255\131\255\097\255\097\255\149\255\149\255\149\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\149\255\
\149\255\149\255\097\255\037\255\000\000\000\000\000\000\000\000\
\097\255\010\255\010\255\097\255\108\255\108\255\010\255\000\000\
\137\255\097\255\149\255\000\000\000\000\000\000\000\000\000\000\
\000\000\097\255\097\255\010\255\010\255\010\255\145\255\097\255\
\142\255\142\255\142\255\142\255\142\255\142\255\010\255\000\000\
\000\000\149\255\097\255\052\255\052\255\052\255\010\255\142\255\
\000\000\000\000\000\000\000\000\000\000\000\000\140\255\000\000\
\052\255\000\000\000\000\000\000\052\255\000\000\049\255\000\000\
\000\000\010\255\010\255\010\255\000\000\044\255\049\255\049\255\
\097\255\000\000\000\000\000\000\097\255\049\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\104\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\255\254\000\255\003\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\255\
\013\255\015\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\141\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\143\255\144\255\146\255\147\255\148\255\150\255\000\000\000\000\
\000\000\151\255\000\000\152\255\030\255\055\255\000\000\153\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\072\255\000\000\000\000\000\000\154\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\155\255\156\255\157\255\
\000\000\000\000\000\000\000\000\000\000\158\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\252\255\000\000\227\255\000\000\
\202\255\218\255\161\255\224\255\017\000"

let yytablesize = 187
let yytable = "\023\000\
\053\000\001\000\024\000\054\000\055\000\027\000\092\000\093\000\
\094\000\073\000\015\000\016\000\021\000\060\000\020\000\018\000\
\063\000\095\000\096\000\097\000\130\000\131\000\132\000\078\000\
\019\000\079\000\017\000\015\000\016\000\090\000\091\000\020\000\
\018\000\136\000\025\000\026\000\113\000\137\000\028\000\061\000\
\062\000\019\000\035\000\017\000\074\000\098\000\108\000\109\000\
\099\000\100\000\101\000\104\000\038\000\029\000\107\000\145\000\
\138\000\139\000\035\000\128\000\112\000\138\000\139\000\099\000\
\102\000\101\000\035\000\036\000\114\000\115\000\036\000\140\000\
\051\000\037\000\120\000\039\000\140\000\105\000\106\000\102\000\
\041\000\076\000\110\000\036\000\037\000\129\000\003\000\004\000\
\005\000\006\000\007\000\008\000\009\000\040\000\052\000\116\000\
\117\000\118\000\031\000\056\000\037\000\010\000\011\000\012\000\
\013\000\014\000\127\000\015\000\016\000\146\000\147\000\148\000\
\057\000\058\000\133\000\149\000\059\000\151\000\075\000\150\000\
\077\000\121\000\122\000\123\000\124\000\125\000\126\000\030\000\
\031\000\080\000\032\000\089\000\004\000\142\000\143\000\144\000\
\134\000\064\000\065\000\066\000\067\000\068\000\069\000\070\000\
\071\000\043\000\044\000\045\000\046\000\111\000\047\000\048\000\
\049\000\081\000\082\000\083\000\084\000\119\000\085\000\086\000\
\087\000\044\000\045\000\046\000\135\000\047\000\048\000\049\000\
\000\000\066\000\000\000\049\000\050\000\000\000\054\000\052\000\
\053\000\000\000\051\000\064\000\067\000\055\000\069\000\041\000\
\042\000\043\000\044\000"

let yycheck = "\004\000\
\030\000\001\000\025\001\033\000\034\000\010\000\061\000\062\000\
\063\000\048\000\012\001\012\001\003\001\043\000\012\001\012\001\
\046\000\072\000\073\000\074\000\116\000\117\000\118\000\053\000\
\012\001\055\000\012\001\029\001\029\001\059\000\060\000\029\001\
\029\001\129\000\001\001\001\001\091\000\133\000\001\001\044\000\
\045\000\029\001\013\001\029\001\049\000\075\000\085\000\086\000\
\012\001\013\001\014\001\081\000\029\001\001\001\084\000\012\001\
\013\001\014\001\029\001\114\000\090\000\013\001\014\001\012\001\
\028\001\014\001\001\001\013\001\098\000\099\000\001\001\028\001\
\016\001\001\001\104\000\029\001\028\001\082\000\083\000\028\001\
\001\001\001\001\087\000\029\001\013\001\115\000\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\029\001\016\001\100\000\
\101\000\102\000\002\001\025\001\029\001\020\001\021\001\022\001\
\023\001\024\001\111\000\026\001\027\001\142\000\143\000\144\000\
\025\001\016\001\119\000\145\000\012\001\150\000\012\001\149\000\
\001\001\105\000\106\000\107\000\108\000\109\000\110\000\001\001\
\002\001\001\001\004\001\001\001\029\001\138\000\139\000\140\000\
\120\000\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\012\001\013\001\014\001\015\001\013\001\017\001\018\001\
\019\001\012\001\013\001\014\001\015\001\013\001\017\001\018\001\
\019\001\013\001\014\001\015\001\025\001\017\001\018\001\019\001\
\255\255\029\001\255\255\029\001\029\001\255\255\029\001\029\001\
\029\001\255\255\029\001\029\001\029\001\029\001\029\001\029\001\
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
# 296 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'display) in
    Obj.repr(
# 33 "parser.mly"
                                     (  )
# 303 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'lasf) in
    Obj.repr(
# 34 "parser.mly"
                                  ( )
# 310 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                                 ( printlist (!nodelist @ !transition) )
# 316 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 41 "parser.mly"
                                ( createfile  _2  !nodelist !transition)
# 323 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                     (Printf.printf "%b\n " (is_complete !nodelist !transition))
# 329 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'labelnoeud) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 47 "parser.mly"
                                                    (transition:= complete (createid _3) !nodelist !transition;nodelist := completeaux (createid _3) _5 _6 !nodelist !transition;)
# 338 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                          (Printf.printf "%b\n " (is_deterministic !nodelist !transition))
# 344 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 49 "parser.mly"
                              (Printf.printf "%b\n " (is_accepted !nodelist !transition _2))
# 351 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 50 "parser.mly"
                         (Printf.printf "%s\n " (getchemin !nodelist !transition _2))
# 358 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
                (_1)
# 365 "parser.ml"
               : 'numero))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
              (_1)
# 372 "parser.ml"
               : 'labelnoeud))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "parser.mly"
                (String.sub _1 1 ((String.length _1) -2)  )
# 379 "parser.ml"
               : 'vrailabel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
              ( python_split ',' (String.sub _1 1 ((String.length _1) -2)) )
# 386 "parser.ml"
               : 'glist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 67 "parser.mly"
                              (" LABEL: " ^ _2 )
# 393 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 68 "parser.mly"
                             (" COLOR: " ^ _2 )
# 400 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 69 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 407 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 70 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 414 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 71 "parser.mly"
                            (" FINAL: " ^ _2 )
# 421 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 72 "parser.mly"
                         (" SIZE: " ^ _2)
# 428 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 74 "parser.mly"
                                      (" LABEL: " ^ _2 ^ _3 )
# 436 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 75 "parser.mly"
                                      (" COLOR: " ^ _2 ^ _3 )
# 444 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 76 "parser.mly"
                                  (" BGCOLOR: " ^ _2 ^ _3 )
# 452 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 77 "parser.mly"
                                        (" INITIAL: " ^ _2 ^ _3 )
# 460 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 78 "parser.mly"
                                       ( " FINAL: " ^ _2 ^ _3 )
# 468 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 79 "parser.mly"
                                      ( " SIZE: " ^ _2 ^ _3 )
# 476 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
            ("Nord")
# 482 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
            ("Sud")
# 488 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
            ("Est")
# 494 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
            ("Ouest")
# 500 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
             ("Nord West")
# 506 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
             ("Nord Est")
# 512 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
             ("Sud Ouest")
# 518 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
             ("Sud Est")
# 524 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 94 "parser.mly"
                             (" COLOR: " ^ _2 )
# 531 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 95 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 538 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 96 "parser.mly"
                              (" POSITION: " ^ _2^":"^ _3)
# 546 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 99 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 554 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 100 "parser.mly"
                                     (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 562 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 101 "parser.mly"
                                        (" POSITION: " ^ _2^":"^ _3 ^ _4)
# 571 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 106 "parser.mly"
                            (" LABEL: " ^ _2 )
# 578 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 107 "parser.mly"
                             (" COLOR: " ^ _2 )
# 585 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 108 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 592 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 109 "parser.mly"
                                             (" POSITION: " ^ _4^":"^ _5)
# 601 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 111 "parser.mly"
                                        (" COLOR: " ^ _2 ^ _3 )
# 609 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 112 "parser.mly"
                                      (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 617 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 113 "parser.mly"
                                       (" LABEL: " ^ _2 ^ _3 )
# 625 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 114 "parser.mly"
                                                         (" POSITION: " ^ _4^":"^ _5 ^ _6)
# 635 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 118 "parser.mly"
                              (" LABEL: " ^ _2 )
# 642 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 119 "parser.mly"
                             (" COLOR: " ^ _2 )
# 649 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 120 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 656 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 121 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 663 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 122 "parser.mly"
                            (" FINAL: " ^ _2 )
# 670 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 123 "parser.mly"
                         (" SIZE: " ^ _2)
# 677 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 124 "parser.mly"
                             (" X: " ^ _2 ^ " Y: " ^ _3 )
# 685 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 126 "parser.mly"
                                        (" LABEL: " ^ _2 ^ _3 )
# 693 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 127 "parser.mly"
                                        (" COLOR: " ^ _2 ^ _3 )
# 701 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 128 "parser.mly"
                                    (" BGCOLOR: " ^ _2 ^ _3 )
# 709 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 129 "parser.mly"
                                          (" INITIAL: " ^ _2 ^ _3 )
# 717 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 130 "parser.mly"
                                         ( " FINAL: " ^ _2 ^ _3 )
# 725 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 131 "parser.mly"
                                        ( " SIZE: " ^ _2 ^ _3 )
# 733 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 132 "parser.mly"
                                        (" X: " ^ _2 ^ " Y: " ^ _3 ^ _4 )
# 742 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 138 "parser.mly"
                                                     ( nodelist := ( add (Noeud(createid _2, ( _4), ( _5),_6)) !nodelist !transition) @ !nodelist )
# 752 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 139 "parser.mly"
                                                      ( nodelist := ( add (Noeud(createid _2, ( _5), ( _6),_3)) !nodelist !transition) @ !nodelist )
# 762 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 140 "parser.mly"
                                                                ( nodelist := ( add (Noeud( createid _2, ( _5), ( _6),_3 ^ (" "  ^ _7))) !nodelist !transition) @ !nodelist )
# 773 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 141 "parser.mly"
                                             (  nodelist := ( add (Noeud(createid _2, ( _4), ( _5),"")) !nodelist !transition) @ !nodelist)
# 782 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 144 "parser.mly"
                                                 ( transition := (add (Edge(createid _2,createid _4,_6,"")) !nodelist !transition )@ !transition )
# 791 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 145 "parser.mly"
                                                           ( transition := (add (Edge(createid _2,createid _4,_6,_7)) !nodelist !transition) @ !transition )
# 801 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 146 "parser.mly"
                                                            ( transition := ( add (Edge(createid _2,createid _4,_7,_5)) !nodelist !transition)  @ !transition )
# 811 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 147 "parser.mly"
                                                                       ( transition := (add (Edge(createid _2,createid _4,_7,_5 ^ (" "^ _8))) !nodelist !transition)  @ !transition )
# 822 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vrailabel) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 149 "parser.mly"
                                                              ( transition := editt (createid _2) ( createid _4) _6 _8  !transition )
# 832 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 150 "parser.mly"
                                    (nodelist := editn (createid _2) _4 !nodelist )
# 840 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 152 "parser.mly"
                      ( nodelist := removenoeud (createid _2) !nodelist ; transition:= removetransitionafternode (createid _2) !transition;  )
# 847 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 153 "parser.mly"
                                (  transition := removetransition (createid _2) (createid _4)  !transition )
# 855 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 155 "parser.mly"
                                (nodelist := moveall _2 _3 !nodelist)
# 863 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 156 "parser.mly"
                                   (nodelist := moveallid_aux (createid _2) _3 _4 !nodelist)
# 872 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'glist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 157 "parser.mly"
                                      (nodelist := movelistid _2 _3 _4 !nodelist)
# 881 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 159 "parser.mly"
                              (nodelist:= renamen (createid _2) (createid _4) !nodelist ; transition:= renamet (createid _2) (createid _4) !transition ;)
# 889 "parser.ml"
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
