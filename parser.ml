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
  294 (* NONE *);
    0|]

let yytransl_block = [|
  257 (* ID *);
  258 (* NUM *);
  259 (* LABELN *);
  260 (* LIST *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\003\000\004\000\004\000\004\000\
\004\000\004\000\004\000\007\000\006\000\005\000\008\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\011\000\011\000\011\000\011\000\
\011\000\011\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\000\000"

let yylen = "\002\000\
\002\000\002\000\002\000\001\000\002\000\001\000\006\000\001\000\
\002\000\002\000\003\000\001\000\001\000\001\000\001\000\002\000\
\002\000\002\000\002\000\002\000\002\000\003\000\003\000\003\000\
\003\000\003\000\003\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\002\000\002\000\003\000\003\000\
\003\000\004\000\002\000\002\000\002\000\005\000\003\000\003\000\
\003\000\006\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\004\000\
\006\000\006\000\007\000\005\000\006\000\007\000\007\000\008\000\
\006\000\004\000\002\000\004\000\003\000\004\000\004\000\004\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\008\000\000\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\081\000\000\000\000\000\000\000\014\000\000\000\010\000\009\000\
\000\000\000\000\000\000\005\000\075\000\000\000\000\000\012\000\
\015\000\000\000\000\000\000\000\000\000\000\000\001\000\002\000\
\003\000\011\000\013\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\077\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\028\000\000\000\000\000\000\000\000\000\000\000\076\000\
\078\000\079\000\080\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\074\000\000\000\000\000\000\000\022\000\023\000\
\024\000\026\000\027\000\025\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\007\000\065\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\058\000\059\000\063\000\061\000\062\000\
\060\000\000\000\000\000\000\000\073\000\067\000\000\000\070\000\
\040\000\041\000\000\000\064\000\000\000\000\000\000\000\042\000\
\072\000\000\000\049\000\047\000\048\000\000\000\000\000\050\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\020\000\023\000\044\000\034\000\035\000\
\052\000\075\000\106\000\133\000\091\000"

let yysindex = "\030\000\
\124\255\000\000\002\255\037\255\000\000\020\255\000\000\058\255\
\062\255\037\255\065\255\067\255\069\255\073\255\076\255\079\255\
\000\000\054\255\056\255\061\255\000\000\037\255\000\000\000\000\
\090\255\149\255\078\255\000\000\000\000\080\255\093\255\000\000\
\000\000\093\255\093\255\083\255\088\255\103\255\000\000\000\000\
\000\000\000\000\000\000\108\255\093\255\037\255\037\255\093\255\
\122\255\122\255\037\255\109\255\121\255\123\255\093\255\000\000\
\093\255\135\255\157\255\137\255\093\255\093\255\097\255\097\255\
\097\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\097\255\097\255\097\255\093\255\044\255\000\000\
\000\000\000\000\000\000\093\255\037\255\037\255\093\255\122\255\
\122\255\037\255\000\000\098\255\093\255\097\255\000\000\000\000\
\000\000\000\000\000\000\000\000\093\255\093\255\037\255\037\255\
\037\255\112\255\093\255\157\255\157\255\157\255\157\255\157\255\
\157\255\051\255\000\000\000\000\097\255\093\255\016\255\016\255\
\016\255\037\255\157\255\000\000\000\000\000\000\000\000\000\000\
\000\000\037\255\037\255\037\255\000\000\000\000\016\255\000\000\
\000\000\000\000\016\255\000\000\048\255\051\255\051\255\000\000\
\000\000\093\255\000\000\000\000\000\000\093\255\051\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\110\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\248\254\000\255\
\004\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\005\255\008\255\012\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\111\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\113\255\114\255\120\255\136\255\148\255\
\150\255\000\000\000\000\000\000\151\255\000\000\152\255\006\255\
\025\255\000\000\153\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\038\255\000\000\
\000\000\000\000\154\255\000\000\155\255\156\255\158\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\159\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\252\255\000\000\247\255\000\000\
\194\255\217\255\144\255\202\255\250\255"

let yytablesize = 188
let yytable = "\024\000\
\095\000\096\000\097\000\016\000\021\000\028\000\136\000\137\000\
\138\000\022\000\076\000\017\000\098\000\099\000\100\000\021\000\
\019\000\042\000\037\000\020\000\016\000\055\000\144\000\018\000\
\056\000\057\000\145\000\102\000\017\000\104\000\001\000\116\000\
\021\000\019\000\037\000\062\000\020\000\038\000\065\000\021\000\
\018\000\063\000\064\000\105\000\025\000\081\000\077\000\082\000\
\111\000\112\000\039\000\093\000\094\000\038\000\134\000\102\000\
\103\000\104\000\026\000\146\000\130\000\131\000\027\000\130\000\
\131\000\029\000\039\000\030\000\101\000\031\000\032\000\105\000\
\033\000\036\000\107\000\132\000\037\000\110\000\132\000\038\000\
\108\000\109\000\039\000\115\000\040\000\113\000\147\000\148\000\
\149\000\041\000\043\000\117\000\118\000\053\000\032\000\054\000\
\152\000\123\000\119\000\120\000\121\000\124\000\125\000\126\000\
\127\000\128\000\129\000\058\000\135\000\046\000\047\000\048\000\
\059\000\049\000\050\000\051\000\140\000\139\000\060\000\061\000\
\078\000\079\000\114\000\080\000\122\000\141\000\142\000\143\000\
\003\000\004\000\005\000\006\000\007\000\008\000\009\000\083\000\
\150\000\092\000\004\000\068\000\151\000\051\000\052\000\010\000\
\011\000\012\000\013\000\014\000\056\000\015\000\016\000\066\000\
\067\000\068\000\069\000\070\000\071\000\072\000\073\000\074\000\
\045\000\046\000\047\000\048\000\054\000\049\000\050\000\051\000\
\084\000\085\000\086\000\087\000\000\000\088\000\089\000\090\000\
\055\000\000\000\053\000\066\000\069\000\057\000\071\000\043\000\
\044\000\000\000\045\000\046\000"

let yycheck = "\004\000\
\063\000\064\000\065\000\012\001\003\001\010\000\119\000\120\000\
\121\000\008\001\050\000\012\001\075\000\076\000\077\000\012\001\
\012\001\022\000\013\001\012\001\029\001\031\000\135\000\012\001\
\034\000\035\000\139\000\012\001\029\001\014\001\001\000\094\000\
\029\001\029\001\029\001\045\000\029\001\013\001\048\000\003\001\
\029\001\046\000\047\000\028\001\025\001\055\000\051\000\057\000\
\088\000\089\000\013\001\061\000\062\000\029\001\117\000\012\001\
\013\001\014\001\001\001\012\001\013\001\014\001\001\001\013\001\
\014\001\001\001\029\001\001\001\078\000\001\001\002\001\028\001\
\004\001\001\001\084\000\028\001\001\001\087\000\028\001\001\001\
\085\000\086\000\029\001\093\000\029\001\090\000\141\000\142\000\
\143\000\029\001\001\001\101\000\102\000\016\001\002\001\016\001\
\151\000\107\000\103\000\104\000\105\000\108\000\109\000\110\000\
\111\000\112\000\113\000\025\001\118\000\013\001\014\001\015\001\
\025\001\017\001\018\001\019\001\123\000\122\000\016\001\012\001\
\012\001\001\001\025\001\001\001\013\001\130\000\131\000\132\000\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\001\001\
\146\000\001\001\029\001\029\001\150\000\029\001\029\001\020\001\
\021\001\022\001\023\001\024\001\029\001\026\001\027\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\038\001\
\012\001\013\001\014\001\015\001\029\001\017\001\018\001\019\001\
\012\001\013\001\014\001\015\001\255\255\017\001\018\001\019\001\
\029\001\255\255\029\001\029\001\029\001\029\001\029\001\029\001\
\029\001\255\255\029\001\029\001"

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
# 301 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'display) in
    Obj.repr(
# 33 "parser.mly"
                                     (  )
# 308 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'lasf) in
    Obj.repr(
# 34 "parser.mly"
                                  ( )
# 315 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                                 ( printlist (!nodelist @ !transition) )
# 321 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 41 "parser.mly"
                                ( createfile  _2  !nodelist !transition)
# 328 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                     (Printf.printf "%b\n " (is_complete !nodelist !transition))
# 334 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'labelnoeud) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 47 "parser.mly"
                                                    (nodelist := completeaux _3 _5 _6 !nodelist !transition;transition:= complete _3 !nodelist !transition;)
# 343 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                          (Printf.printf "%b\n " (is_deterministic !nodelist !transition))
# 349 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 49 "parser.mly"
                              (Printf.printf "%b\n " (is_accepted !nodelist !transition _2))
# 356 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 50 "parser.mly"
                         (Printf.printf "%s\n " (getchemin !nodelist !transition _2))
# 363 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 51 "parser.mly"
                                  (nodelist:= showcomplet _3 !nodelist !transition )
# 370 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                (_1)
# 377 "parser.ml"
               : 'numero))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
              (_1)
# 384 "parser.ml"
               : 'labelnoeud))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
                (String.sub _1 1 ((String.length _1) -2)  )
# 391 "parser.ml"
               : 'vrailabel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
              ( python_split ',' (String.sub _1 1 ((String.length _1) -2)) )
# 398 "parser.ml"
               : 'glist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 68 "parser.mly"
                              (" LABEL: " ^ _2 )
# 405 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 69 "parser.mly"
                             (" COLOR: " ^ _2 )
# 412 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 70 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 419 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 71 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 426 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 72 "parser.mly"
                            (" FINAL: " ^ _2 )
# 433 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 73 "parser.mly"
                         (" SIZE: " ^ _2)
# 440 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 75 "parser.mly"
                                      (" LABEL: " ^ _2 ^ _3 )
# 448 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 76 "parser.mly"
                                      (" COLOR: " ^ _2 ^ _3 )
# 456 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 77 "parser.mly"
                                  (" SIZE: " ^ _2 ^ _3 )
# 464 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 78 "parser.mly"
                                        (" BGCOLOR: " ^ _2 ^ _3 )
# 472 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 79 "parser.mly"
                                       ( " INITIAL: " ^ _2 ^ _3 )
# 480 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 80 "parser.mly"
                                      ( " FINAL: " ^ _2 ^ _3 )
# 488 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
               ("none")
# 494 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
            ("Nord")
# 500 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
            ("Sud")
# 506 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
            ("Est")
# 512 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
            ("Ouest")
# 518 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
             ("Nord-West")
# 524 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
             ("Nord-Est")
# 530 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
             ("Sud-Ouest")
# 536 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
             ("Sud-Est")
# 542 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 96 "parser.mly"
                             (" COLOR: " ^ _2 )
# 549 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 97 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 556 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 98 "parser.mly"
                              (" POSITION: " ^ _2^":"^ _3)
# 564 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 101 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 572 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 102 "parser.mly"
                                     (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 580 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 103 "parser.mly"
                                        (" POSITION: " ^ _2^":"^ _3 ^ _4)
# 589 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 108 "parser.mly"
                            (" LABEL: " ^ _2 )
# 596 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 109 "parser.mly"
                             (" COLOR: " ^ _2 )
# 603 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 110 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 610 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 111 "parser.mly"
                                             (" POSITION: " ^ _4^":"^ _5)
# 619 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 113 "parser.mly"
                                        (" COLOR: " ^ _2 ^ _3 )
# 627 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 114 "parser.mly"
                                      (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 635 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 115 "parser.mly"
                                       (" LABEL: " ^ _2 ^ _3 )
# 643 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 116 "parser.mly"
                                                         (" POSITION: " ^ _4^":"^ _5 ^ _6)
# 653 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 120 "parser.mly"
                              (" LABEL: " ^ _2 )
# 660 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 121 "parser.mly"
                             (" COLOR: " ^ _2 )
# 667 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 122 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 674 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 123 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 681 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 124 "parser.mly"
                            (" FINAL: " ^ _2 )
# 688 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 125 "parser.mly"
                         (" SIZE: " ^ _2)
# 695 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 126 "parser.mly"
                             (" X: " ^ _2 ^ " Y: " ^ _3 )
# 703 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 128 "parser.mly"
                                        (" LABEL: " ^ _2 ^ _3)
# 711 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 129 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 719 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 130 "parser.mly"
                                         (" BGCOLOR: " ^ _2 ^ _3 )
# 727 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 131 "parser.mly"
                                        (" INITIAL: " ^ _2 ^ _3 )
# 735 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 132 "parser.mly"
                                      (" FINAL: " ^ _2 ^ _3 )
# 743 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 133 "parser.mly"
                                    (" SIZE: " ^ _2 ^ _3)
# 751 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 134 "parser.mly"
                                        (" X: " ^ _2 ^ " Y: " ^ _3 ^ _4)
# 760 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 141 "parser.mly"
                                                     ( nodelist := ( add (Noeud(createid _2, ( _4), ( _5),_6)) !nodelist !transition) @ !nodelist )
# 770 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 142 "parser.mly"
                                                      ( nodelist := ( add (Noeud(createid _2, ( _5), ( _6),_3)) !nodelist !transition) @ !nodelist )
# 780 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 143 "parser.mly"
                                                                ( nodelist := ( add (Noeud( createid _2, ( _5), ( _6),_3 ^ (" "  ^ _7))) !nodelist !transition) @ !nodelist )
# 791 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 144 "parser.mly"
                                             (  nodelist := ( add (Noeud(createid _2, ( _4), ( _5),"")) !nodelist !transition) @ !nodelist)
# 800 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 147 "parser.mly"
                                                 ( transition := (add (Edge(createid _2,createid _4,_6,"")) !nodelist !transition )@ !transition )
# 809 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 148 "parser.mly"
                                                           ( transition := (add (Edge(createid _2,createid _4,_6,_7)) !nodelist !transition) @ !transition )
# 819 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 149 "parser.mly"
                                                            ( transition := ( add (Edge(createid _2,createid _4,_7,_5)) !nodelist !transition)  @ !transition )
# 829 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 150 "parser.mly"
                                                                       ( transition := (add (Edge(createid _2,createid _4,_7,_5 ^ (" "^ _8))) !nodelist !transition)  @ !transition )
# 840 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 152 "parser.mly"
                                              ( transition := editt (createid _2) ( createid _4) _6   !transition )
# 849 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 153 "parser.mly"
                                    (nodelist := editn (createid _2) _4 !nodelist )
# 857 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 155 "parser.mly"
                      ( nodelist := removenoeud (createid _2) !nodelist ; transition:= removetransitionafternode (createid _2) !transition;  )
# 864 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 156 "parser.mly"
                                 (  transition := removetransition (createid _2) (createid _4) !transition )
# 872 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 158 "parser.mly"
                                (nodelist := moveall _2 _3 !nodelist)
# 880 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 159 "parser.mly"
                                   (nodelist := moveallid_aux (createid _2) _3 _4 !nodelist)
# 889 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'glist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 160 "parser.mly"
                                      (nodelist := movelistid _2 _3 _4 !nodelist)
# 898 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 162 "parser.mly"
                              (nodelist:= renamen (createid _2) (createid _4) !nodelist ; transition:= renamet (createid _2) (createid _4) !transition ;)
# 906 "parser.ml"
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
