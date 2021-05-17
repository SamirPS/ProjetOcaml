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
\001\000\001\000\001\000\003\000\003\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\007\000\006\000\005\000\008\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\011\000\011\000\011\000\
\011\000\011\000\011\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\000\000"

let yylen = "\002\000\
\002\000\002\000\002\000\001\000\002\000\001\000\006\000\001\000\
\002\000\002\000\003\000\003\000\001\000\001\000\001\000\001\000\
\002\000\002\000\002\000\002\000\002\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\002\000\002\000\003\000\
\003\000\003\000\004\000\002\000\002\000\002\000\005\000\003\000\
\003\000\003\000\006\000\002\000\002\000\002\000\002\000\002\000\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\004\000\006\000\006\000\007\000\005\000\006\000\007\000\007\000\
\008\000\006\000\004\000\002\000\004\000\003\000\004\000\004\000\
\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\008\000\000\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\082\000\000\000\000\000\000\000\015\000\000\000\000\000\010\000\
\009\000\000\000\000\000\000\000\005\000\076\000\000\000\000\000\
\013\000\016\000\000\000\000\000\000\000\000\000\000\000\001\000\
\002\000\003\000\011\000\012\000\014\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\078\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\030\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\029\000\000\000\000\000\000\000\000\000\
\000\000\077\000\079\000\080\000\081\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\075\000\000\000\000\000\000\000\
\023\000\024\000\025\000\027\000\028\000\026\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\007\000\066\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\059\000\060\000\064\000\
\062\000\063\000\061\000\000\000\000\000\000\000\074\000\068\000\
\000\000\071\000\041\000\042\000\000\000\065\000\000\000\000\000\
\000\000\043\000\073\000\000\000\050\000\048\000\049\000\000\000\
\000\000\051\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\020\000\024\000\046\000\035\000\036\000\
\054\000\077\000\108\000\135\000\093\000"

let yysindex = "\049\000\
\089\255\000\000\004\255\048\255\000\000\029\255\000\000\021\255\
\054\255\048\255\060\255\071\255\085\255\074\255\077\255\079\255\
\000\000\061\255\062\255\063\255\000\000\048\255\048\255\000\000\
\000\000\106\255\135\255\092\255\000\000\000\000\101\255\116\255\
\000\000\000\000\116\255\116\255\094\255\097\255\108\255\000\000\
\000\000\000\000\000\000\000\000\000\000\111\255\116\255\048\255\
\048\255\116\255\107\255\107\255\048\255\114\255\126\255\158\255\
\116\255\000\000\116\255\165\255\143\255\169\255\116\255\116\255\
\150\255\150\255\150\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\150\255\150\255\150\255\116\255\
\053\255\000\000\000\000\000\000\000\000\116\255\048\255\048\255\
\116\255\107\255\107\255\048\255\000\000\146\255\116\255\150\255\
\000\000\000\000\000\000\000\000\000\000\000\000\116\255\116\255\
\048\255\048\255\048\255\138\255\116\255\143\255\143\255\143\255\
\143\255\143\255\143\255\253\254\000\000\000\000\150\255\116\255\
\046\255\046\255\046\255\048\255\143\255\000\000\000\000\000\000\
\000\000\000\000\000\000\048\255\048\255\048\255\000\000\000\000\
\046\255\000\000\000\000\000\000\046\255\000\000\057\255\253\254\
\253\254\000\000\000\000\116\255\000\000\000\000\000\000\116\255\
\253\254\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\144\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\255\009\255\012\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\019\255\023\255\035\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\145\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\147\255\148\255\149\255\
\151\255\152\255\153\255\000\000\000\000\000\000\154\255\000\000\
\155\255\000\255\044\255\000\000\156\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\050\255\000\000\000\000\000\000\157\255\000\000\159\255\160\255\
\162\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\163\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\252\255\000\000\229\255\000\000\
\193\255\205\255\161\255\217\255\021\000"

let yytablesize = 192
let yytable = "\025\000\
\078\000\097\000\098\000\099\000\057\000\029\000\021\000\058\000\
\059\000\132\000\133\000\022\000\038\000\100\000\101\000\102\000\
\017\000\043\000\044\000\064\000\018\000\027\000\067\000\022\000\
\134\000\138\000\139\000\140\000\038\000\083\000\020\000\084\000\
\118\000\017\000\021\000\095\000\096\000\018\000\113\000\114\000\
\022\000\146\000\023\000\065\000\066\000\147\000\019\000\020\000\
\079\000\001\000\021\000\021\000\103\000\026\000\028\000\136\000\
\039\000\104\000\109\000\106\000\030\000\112\000\040\000\019\000\
\104\000\105\000\106\000\117\000\148\000\132\000\133\000\031\000\
\039\000\107\000\037\000\119\000\120\000\038\000\040\000\039\000\
\107\000\125\000\110\000\111\000\134\000\032\000\033\000\115\000\
\034\000\040\000\041\000\042\000\137\000\003\000\004\000\005\000\
\006\000\007\000\008\000\009\000\121\000\122\000\123\000\149\000\
\150\000\151\000\045\000\055\000\010\000\011\000\012\000\013\000\
\014\000\154\000\015\000\016\000\056\000\033\000\060\000\141\000\
\152\000\061\000\063\000\062\000\153\000\080\000\081\000\143\000\
\144\000\145\000\126\000\127\000\128\000\129\000\130\000\131\000\
\068\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\142\000\047\000\048\000\049\000\050\000\124\000\051\000\
\052\000\053\000\086\000\087\000\088\000\089\000\082\000\090\000\
\091\000\092\000\048\000\049\000\050\000\085\000\051\000\052\000\
\053\000\094\000\116\000\000\000\004\000\069\000\000\000\052\000\
\053\000\057\000\000\000\055\000\056\000\054\000\067\000\070\000\
\058\000\072\000\000\000\044\000\045\000\000\000\046\000\047\000"

let yycheck = "\004\000\
\052\000\065\000\066\000\067\000\032\000\010\000\003\001\035\000\
\036\000\013\001\014\001\008\001\013\001\077\000\078\000\079\000\
\012\001\022\000\023\000\047\000\012\001\001\001\050\000\012\001\
\028\001\121\000\122\000\123\000\029\001\057\000\012\001\059\000\
\096\000\029\001\012\001\063\000\064\000\029\001\090\000\091\000\
\029\001\137\000\039\001\048\000\049\000\141\000\012\001\029\001\
\053\000\001\000\003\001\029\001\080\000\025\001\001\001\119\000\
\013\001\012\001\086\000\014\001\001\001\089\000\013\001\029\001\
\012\001\013\001\014\001\095\000\012\001\013\001\014\001\001\001\
\029\001\028\001\001\001\103\000\104\000\001\001\029\001\001\001\
\028\001\109\000\087\000\088\000\028\001\001\001\002\001\092\000\
\004\001\029\001\029\001\029\001\120\000\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\105\000\106\000\107\000\143\000\
\144\000\145\000\001\001\016\001\020\001\021\001\022\001\023\001\
\024\001\153\000\026\001\027\001\016\001\002\001\025\001\124\000\
\148\000\025\001\012\001\016\001\152\000\012\001\001\001\132\000\
\133\000\134\000\110\000\111\000\112\000\113\000\114\000\115\000\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\037\001\
\038\001\125\000\012\001\013\001\014\001\015\001\013\001\017\001\
\018\001\019\001\012\001\013\001\014\001\015\001\001\001\017\001\
\018\001\019\001\013\001\014\001\015\001\001\001\017\001\018\001\
\019\001\001\001\025\001\255\255\029\001\029\001\255\255\029\001\
\029\001\029\001\255\255\029\001\029\001\029\001\029\001\029\001\
\029\001\029\001\255\255\029\001\029\001\255\255\029\001\029\001"

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
# 307 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'display) in
    Obj.repr(
# 33 "parser.mly"
                                     (  )
# 314 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'lasf) in
    Obj.repr(
# 34 "parser.mly"
                                  ( )
# 321 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                                 ( printlist (!nodelist @ !transition) )
# 327 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 41 "parser.mly"
                                ( createfile  _2  !nodelist !transition)
# 334 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                     (Printf.printf "%b\n " (is_complete !nodelist !transition))
# 340 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'labelnoeud) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 47 "parser.mly"
                                                    (nodelist := completeaux _3 _5 _6 !nodelist !transition;transition:= complete _3 !nodelist !transition;)
# 349 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                          (Printf.printf "%b\n " (is_deterministic !nodelist !transition))
# 355 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 49 "parser.mly"
                              (Printf.printf "%b\n " (is_accepted !nodelist !transition _2))
# 362 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 50 "parser.mly"
                         (Printf.printf "%s\n " (getchemin !nodelist !transition _2))
# 369 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 51 "parser.mly"
                                  (nodelist:= showcomplet _3 !nodelist !transition )
# 376 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 52 "parser.mly"
                                       (nodelist := showcompletd _3 !nodelist !transition)
# 383 "parser.ml"
               : 'lasf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
                (_1)
# 390 "parser.ml"
               : 'numero))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
              (_1)
# 397 "parser.ml"
               : 'labelnoeud))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
                (String.sub _1 1 ((String.length _1) -2)  )
# 404 "parser.ml"
               : 'vrailabel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
              ( python_split ',' (String.sub _1 1 ((String.length _1) -2)) )
# 411 "parser.ml"
               : 'glist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 69 "parser.mly"
                              (" LABEL: " ^ _2 )
# 418 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 70 "parser.mly"
                             (" COLOR: " ^ _2 )
# 425 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 71 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 432 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 72 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 439 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 73 "parser.mly"
                            (" FINAL: " ^ _2 )
# 446 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 74 "parser.mly"
                         (" SIZE: " ^ _2)
# 453 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 76 "parser.mly"
                                      (" LABEL: " ^ _2 ^ _3 )
# 461 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 77 "parser.mly"
                                      (" COLOR: " ^ _2 ^ _3 )
# 469 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 78 "parser.mly"
                                  (" SIZE: " ^ _2 ^ _3 )
# 477 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 79 "parser.mly"
                                        (" BGCOLOR: " ^ _2 ^ _3 )
# 485 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 80 "parser.mly"
                                       ( " INITIAL: " ^ _2 ^ _3 )
# 493 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 81 "parser.mly"
                                      ( " FINAL: " ^ _2 ^ _3 )
# 501 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
               ("none")
# 507 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
            ("Nord")
# 513 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
            ("Sud")
# 519 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
            ("Est")
# 525 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
            ("Ouest")
# 531 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
             ("Nord-West")
# 537 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
             ("Nord-Est")
# 543 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
             ("Sud-Ouest")
# 549 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
             ("Sud-Est")
# 555 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 97 "parser.mly"
                             (" COLOR: " ^ _2 )
# 562 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 98 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 569 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 99 "parser.mly"
                              (" POSITION: " ^ _2^":"^ _3)
# 577 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 102 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 585 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 103 "parser.mly"
                                     (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 593 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 104 "parser.mly"
                                        (" POSITION: " ^ _2^":"^ _3 ^ _4)
# 602 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 109 "parser.mly"
                            (" LABEL: " ^ _2 )
# 609 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 110 "parser.mly"
                             (" COLOR: " ^ _2 )
# 616 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 111 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 623 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 112 "parser.mly"
                                             (" POSITION: " ^ _4^":"^ _5)
# 632 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 114 "parser.mly"
                                        (" COLOR: " ^ _2 ^ _3 )
# 640 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 115 "parser.mly"
                                      (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 648 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 116 "parser.mly"
                                       (" LABEL: " ^ _2 ^ _3 )
# 656 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 117 "parser.mly"
                                                         (" POSITION: " ^ _4^":"^ _5 ^ _6)
# 666 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 121 "parser.mly"
                              (" LABEL: " ^ _2 )
# 673 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 122 "parser.mly"
                             (" COLOR: " ^ _2 )
# 680 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 123 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 687 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 124 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 694 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 125 "parser.mly"
                            (" FINAL: " ^ _2 )
# 701 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 126 "parser.mly"
                         (" SIZE: " ^ _2)
# 708 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 127 "parser.mly"
                             (" X: " ^ _2 ^ " Y: " ^ _3 )
# 716 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 129 "parser.mly"
                                        (" LABEL: " ^ _2 ^ _3)
# 724 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 130 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 732 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 131 "parser.mly"
                                         (" BGCOLOR: " ^ _2 ^ _3 )
# 740 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 132 "parser.mly"
                                        (" INITIAL: " ^ _2 ^ _3 )
# 748 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 133 "parser.mly"
                                      (" FINAL: " ^ _2 ^ _3 )
# 756 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 134 "parser.mly"
                                    (" SIZE: " ^ _2 ^ _3)
# 764 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 135 "parser.mly"
                                        (" X: " ^ _2 ^ " Y: " ^ _3 ^ _4)
# 773 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 142 "parser.mly"
                                                     ( nodelist := ( add (Noeud(createid _2, ( _4), ( _5),_6)) !nodelist !transition) @ !nodelist )
# 783 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 143 "parser.mly"
                                                      ( nodelist := ( add (Noeud(createid _2, ( _5), ( _6),_3)) !nodelist !transition) @ !nodelist )
# 793 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 144 "parser.mly"
                                                                ( nodelist := ( add (Noeud( createid _2, ( _5), ( _6),_3 ^ (" "  ^ _7))) !nodelist !transition) @ !nodelist )
# 804 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 145 "parser.mly"
                                             (  nodelist := ( add (Noeud(createid _2, ( _4), ( _5),"")) !nodelist !transition) @ !nodelist)
# 813 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 148 "parser.mly"
                                                 ( transition := (add (Edge(createid _2,createid _4,_6,"")) !nodelist !transition )@ !transition )
# 822 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 149 "parser.mly"
                                                           ( transition := (add (Edge(createid _2,createid _4,_6,_7)) !nodelist !transition) @ !transition )
# 832 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 150 "parser.mly"
                                                            ( transition := ( add (Edge(createid _2,createid _4,_7,_5)) !nodelist !transition)  @ !transition )
# 842 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 151 "parser.mly"
                                                                       ( transition := (add (Edge(createid _2,createid _4,_7,_5 ^ (" "^ _8))) !nodelist !transition)  @ !transition )
# 853 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 153 "parser.mly"
                                              ( transition := editt (createid _2) ( createid _4) _6   !transition )
# 862 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 154 "parser.mly"
                                    (nodelist := editn (createid _2) _4 !nodelist )
# 870 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 156 "parser.mly"
                      ( nodelist := removenoeud (createid _2) !nodelist ; transition:= removetransitionafternode (createid _2) !transition;  )
# 877 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 157 "parser.mly"
                                 (  transition := removetransition (createid _2) (createid _4) !transition )
# 885 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 159 "parser.mly"
                                (nodelist := moveall _2 _3 !nodelist)
# 893 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 160 "parser.mly"
                                   (nodelist := moveallid_aux (createid _2) _3 _4 !nodelist)
# 902 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'glist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 161 "parser.mly"
                                      (nodelist := movelistid _2 _3 _4 !nodelist)
# 911 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 163 "parser.mly"
                              (nodelist:= renamen (createid _2) (createid _4) !nodelist ; transition:= renamet (createid _2) (createid _4) !transition ;)
# 919 "parser.ml"
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
