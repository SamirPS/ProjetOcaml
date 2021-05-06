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
  | RENAME
  | WITH
  | EDIT
  | EDITEDGE
  | EOL

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
        

        type 'a arbre =
            |Noeud of string * string *  string * string
            |Edge of string * string * string * string ;;


        let nodelist = ref [] ;;
        let transition = ref [];;

        let python_split x =
          String.split_on_char  ' ' x
          |> List.filter (fun x -> x <> "")
        ;;


      
      let rec getvalue valeur default liste =
        match liste with
        |[] -> default 
        |h::q when h = valeur-> List.hd q
        |h::q -> getvalue valeur default q;;


        let contains s1 s2 =
        try
          let len = String.length s2 in
          for i = 0 to String.length s1 - len do
            if String.sub s1 i len = s2 then raise Exit
          done;
          false
        with Exit -> true;;

        let inList element liste = List.exists element liste;; 

        let createid id =
          if (String.length id) <= 15
            then begin id end
            else begin (String.sub id 0 14 ) end;;

        let add elem = 
            match elem with 
              
              | Noeud(x,y,z,t) when t = "" -> [Noeud(createid x,y,z," SIZE: 30 LABEL: " ^ x)]
              | Noeud(x,y,z,t) when ((contains t "SIZE:") == false) && ((contains t " LABEL: ")==false) -> [Noeud(createid x,y,z," SIZE: 30 LABEL: " ^ x ^t)]
              | Noeud(x,y,z,t) when (contains t "SIZE:") == false  -> [Noeud(createid x,y,z," SIZE: 30" ^t)]
              | Noeud(x,y,z,t) when (contains t "LABEL:") == false -> [Noeud(createid x,y,z," LABEL: " ^ x ^t)]
              | Edge(x,y,z,t) -> [Edge(createid x,createid y,z,t)]
              | Noeud(x,y,z,t) -> [Noeud(createid x,y,z,t)] ;;

        let rec containsele x l = match l with
            | [] -> false
            | Noeud(a,b,c,d)::q when a=x -> true
            | t::q -> containsele x q ;;

        let editn e l =
          let rec go l acc = match l with
            | [] -> List.rev acc
            | Noeud(a,b,c,d)::xs -> ( match e with 
                                      |Noeud(w,x,y,z) when createid a = createid w -> go xs ( (add (Noeud(w,x,y,z))) @ acc)
                                      | _ -> go xs (Noeud(a,b,c,d)::acc)
                                    )
            | Edge(a,b,c,d)::xs -> go xs acc

          in go l [];;

        let editt e l =
          let rec go l acc = match l with
            | [] -> List.rev acc
            | Edge(a,b,c,d)::xs -> ( match e with 
                                      |Edge(w,x,y,z) when (createid a = createid w) && (createid b = createid x) -> go xs ( (add (Edge(w,x,y,z))) @ acc)
                                      | _ -> go xs (Edge(a,b,c,d)::acc)
                                    )
            | Noeud(a,b,c,d)::xs -> go xs acc

          in go l [];;

        let deleten e l =
          let rec go l acc = match l with
            | [] -> List.rev acc
            | Noeud(a,b,c,d)::xs when createid e = a -> go xs acc
            | x::xs -> go xs (x::acc)
          in go l [];;

        let removenoeud e l = 
            match containsele e l with
            |false ->failwith "Impossible à supprimer" 
            | _ -> deleten e l ;;

        let rec containte x y l  = 
            match l with
            | [] -> false
            | Edge(a,b,c,d)::q when a=x && b=y -> true
            | t::q -> containte x y q ;;

        let deletee e f l =
        let rec go l acc = match l with
          | [] -> List.rev acc
          | Edge(a,b,c,d)::xs when (createid e = a) && (createid f = b) -> go xs acc
          | x::xs -> go xs (x::acc)
        in go l [];;

        let removetransition e f l =
          match containte e f l with
            |false ->failwith "Impossible à supprimer"
            | _ -> deletee e f l ;;

        let moveall numun numdeux l =
        let rec go l acc = match l with
          | [] -> List.rev acc
          | Noeud(a,b,c,d)::xs  -> go xs (Noeud(a,numun,numdeux,d) :: acc)
          | Edge (_, _, _, _)::xs -> go xs  acc
        in go l [];;

        let moveallid  id numun numdeux l =
        let rec go l acc = match l with
          | [] -> List.rev acc
          | Noeud(a,b,c,d)::xs when a = (createid id) -> go xs (Noeud(a,numun,numdeux,d) :: acc)
          | x::xs -> go xs  (x::acc)

        in go l [];;

        let renamen  ancien nouveau l =
        let rec go l acc = match l with
          | [] -> List.rev acc
          | Noeud(a,b,c,d)::xs when a = (createid ancien) -> go xs (Noeud(nouveau,b,c,d) :: acc)
          | x::xs -> go xs  (x::acc)

        in go l [];;

        let renamet  ancien nouveau l =
        let rec go l acc = match l with
          | [] -> List.rev acc
          | Edge(a,b,c,d)::xs when (a = createid ancien) && (b = createid ancien) -> go xs (Edge(nouveau,nouveau,c,d) :: acc)
          | Edge(a,b,c,d)::xs when (a = createid ancien)  -> go xs (Edge(nouveau,b,c,d) :: acc)
          | Edge(a,b,c,d)::xs when  (b = createid ancien) -> go xs (Edge(b,nouveau,c,d) :: acc)
          | x::xs -> go xs  (x::acc)

        in go l [];;

        let rec printlist e  = 
              match e with
               [] -> Printf.printf "\n"
              | h :: t -> (match h with 
                        |Noeud(x,y,z,k)  -> Printf.printf "%s %s %s %s \n " x y z  k ; printlist t 
                        |Edge(x,y,z,k)  -> Printf.printf "%s %s %s %s \n" x y z k ; printlist t ) ;;

      let getnode idun ideux l =
        let rec search l acc = match l with
          | [] -> List.rev acc
          | Noeud(a,b,c,d)::xs when (a = createid idun) || (a= createid ideux)    -> search xs (Noeud(a,b,c,d) :: acc)
          | _::xs -> search xs  acc
        in search l [];;

      let calcularc idun ideux l label=
          let node = getnode idun ideux l in
          let Noeud(a,b,c,d) = List.hd node in
          let Noeud(e,f,g,h) = List.hd (List.rev node) in 
          let p1x =float_of_string b in 
          let p1y = float_of_string c in 
          let p2x = float_of_string f in 
          let p2y = float_of_string g in 

          let mpx =(  ( p2x) +. ( p1x) ) *. 0.5 in
          let mpy = (  ( p2y) +. ( p1y) ) *. 0.5 in 
          let theta= (atan2 (p2y-.p1y) (p2x-.p1x)) -. (3.14 *. 0.5) in 
          let offset =  30.0 in 


          let c1x = mpx +. offset *. cos(theta) in
          let c1y = mpy +. offset *. sin(theta) in

          let curve = "M" ^ string_of_int ( int_of_float p1x )  ^ " " ^ string_of_int ( int_of_float p1y ) ^ " Q " ^ string_of_int  ( int_of_float c1x ) ^ " " ^ string_of_int ( int_of_float c1y ) ^ " " ^ string_of_int ( int_of_float p2x ) ^ " " ^ string_of_int ( int_of_float p2y ) in
          let curveelement = "<path fill=\"none\"  d=\"" ^ curve ^ "\" stroke=\"black\"></path> \n " in 
          let info = "<text x=\"" ^ string_of_int ( int_of_float c1x )  ^ "\" y=\"" ^ string_of_int ( int_of_float c1y )  ^ "\" fill=\"black\" text-anchor=\"middle\"> " ^label^ " </text> \n " in 
          curveelement ^ " " ^ info;;



      let rec nodefile noeud monstr =
            match noeud with 
            |[] -> monstr;
            |Noeud(x,y,z,t)::q -> let size =  getvalue "SIZE:" "30" (python_split t)  in 
                                  let color = getvalue "COLOR:" "black" (python_split t) in 
                                  let label = getvalue "LABEL:" x (python_split t) in 
                                  let bgcolor = getvalue "BGCOLOR:" "none" (python_split t) in 
                                  nodefile q ( ( "<circle cx=\"" ^y^ "\" cy=\"" ^ z ^ "\" r=\""^size^" \" stroke=\"" ^ color ^ "\" stroke-width=\"2\" fill=\""^bgcolor^"\" > </circle> \n")  ^ 
                                  ("<text x=\"" ^y^ "\" y=\"" ^ z ^ "\" dominant-baseline=\"middle \" fill=\"" ^ color ^"\" > "^ label ^" </text> \n") ^ monstr)
            | _ -> ""^monstr;;

    let rec transitionfile noeud transi monstr = 
            match transi with 
            |[] -> monstr;
            |Edge(x,y,z,t)::q -> let info =  calcularc x y  noeud  z in  
                                  transitionfile noeud  q  ( info ^ monstr)
            | _ -> ""^monstr;;


    let createfile name liste secondl =
      let fic2 = open_out (name^".svg") in
      let mystrfinal=" <svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"800\" height=\"600\" viewBox=\"0 0 800 600\"> \n"^(nodefile liste "") ^ (transitionfile liste secondl "") ^  "</svg>" in

      output_string fic2 mystrfinal;
      close_out fic2;;

      
# 235 "parser.ml"
let yytransl_const = [|
  260 (* CREATENODE *);
  261 (* CREATEFROM *);
  262 (* AT *);
  263 (* LABEL *);
  264 (* COLOR *);
  265 (* SIZE *);
  266 (* TO *);
  267 (* INITIAL *);
  268 (* FINAL *);
  269 (* BGCOLOR *);
  270 (* DUMP *);
  271 (* REMOVE *);
  272 (* REMOVEEDGE *);
  273 (* MOVE *);
  274 (* RENAME *);
  275 (* WITH *);
  276 (* EDIT *);
  277 (* EDITEDGE *);
  278 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* ID *);
  258 (* NUM *);
  259 (* LABELN *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\004\000\005\000\005\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\007\000\007\000\007\000\007\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\002\000\001\000\001\000\004\000\002\000\
\002\000\002\000\002\000\002\000\002\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\002\000\003\000\003\000\006\000\
\006\000\007\000\005\000\007\000\007\000\008\000\006\000\006\000\
\007\000\007\000\008\000\007\000\008\000\008\000\009\000\002\000\
\004\000\003\000\004\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\045\000\000\000\000\000\000\000\000\000\
\004\000\040\000\000\000\000\000\005\000\000\000\000\000\000\000\
\000\000\001\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\042\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\041\000\043\000\044\000\000\000\
\000\000\000\000\000\000\000\000\014\000\015\000\016\000\018\000\
\019\000\017\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\024\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\007\000\026\000\033\000\
\022\000\023\000\000\000\028\000\000\000\000\000\000\000\035\000\
\030\000\037\000\000\000\039\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000\022\000\045\000\035\000\071\000"

let yysindex = "\005\000\
\004\255\000\000\011\255\025\255\034\255\042\255\052\255\054\255\
\059\255\062\255\068\255\000\000\244\254\061\255\081\255\075\255\
\000\000\000\000\076\255\093\255\000\000\093\255\086\255\078\255\
\096\255\000\000\000\000\093\255\110\255\110\255\093\255\110\255\
\110\255\110\255\111\255\121\255\122\255\093\255\000\000\123\255\
\092\255\125\255\093\255\124\255\101\255\101\255\101\255\101\255\
\101\255\101\255\093\255\108\255\000\000\000\000\000\000\093\255\
\126\255\109\255\101\255\093\255\000\000\000\000\000\000\000\000\
\000\000\000\000\093\255\110\255\110\255\110\255\120\255\093\255\
\093\255\112\255\000\000\093\255\101\255\024\255\024\255\024\255\
\110\255\101\255\093\255\110\255\127\255\000\000\000\000\000\000\
\000\000\000\000\024\255\000\000\101\255\024\255\110\255\000\000\
\000\000\000\000\024\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\107\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\069\255\251\254\005\255\008\255\009\255\
\027\255\028\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\061\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\113\255\114\255\022\255\057\255\
\000\000\115\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\116\255\000\000\117\255\118\255\000\000\000\000\
\000\000\000\000\119\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\241\255\226\255\025\000\224\255"

let yytablesize = 141
let yytable = "\046\000\
\008\000\048\000\049\000\050\000\038\000\001\000\039\000\003\000\
\004\000\026\000\009\000\015\000\043\000\013\000\011\000\047\000\
\008\000\005\000\006\000\007\000\008\000\009\000\054\000\010\000\
\011\000\016\000\009\000\059\000\020\000\013\000\011\000\069\000\
\012\000\010\000\017\000\067\000\070\000\078\000\079\000\080\000\
\072\000\085\000\018\000\020\000\076\000\088\000\089\000\090\000\
\012\000\010\000\091\000\077\000\019\000\094\000\020\000\021\000\
\082\000\083\000\096\000\023\000\086\000\098\000\024\000\021\000\
\099\000\057\000\100\000\093\000\025\000\061\000\062\000\063\000\
\064\000\065\000\066\000\006\000\006\000\006\000\021\000\006\000\
\006\000\006\000\027\000\075\000\036\000\037\000\028\000\029\000\
\030\000\031\000\006\000\032\000\033\000\034\000\021\000\040\000\
\041\000\056\000\029\000\030\000\031\000\087\000\032\000\033\000\
\034\000\042\000\092\000\029\000\030\000\031\000\044\000\032\000\
\033\000\034\000\068\000\069\000\051\000\097\000\084\000\069\000\
\070\000\052\000\053\000\055\000\070\000\058\000\081\000\074\000\
\003\000\060\000\000\000\073\000\000\000\095\000\025\000\032\000\
\031\000\034\000\029\000\036\000\038\000"

let yycheck = "\030\000\
\006\001\032\000\033\000\034\000\020\000\001\000\022\000\004\001\
\005\001\022\001\006\001\001\001\028\000\006\001\006\001\031\000\
\022\001\014\001\015\001\016\001\017\001\018\001\038\000\020\001\
\021\001\001\001\022\001\043\000\007\001\022\001\022\001\008\001\
\006\001\006\001\001\001\051\000\013\001\068\000\069\000\070\000\
\056\000\074\000\001\001\022\001\060\000\078\000\079\000\080\000\
\022\001\022\001\081\000\067\000\001\001\084\000\001\001\002\001\
\072\000\073\000\091\000\001\001\076\000\094\000\001\001\007\001\
\095\000\041\000\099\000\083\000\001\001\045\000\046\000\047\000\
\048\000\049\000\050\000\007\001\008\001\009\001\022\001\011\001\
\012\001\013\001\022\001\059\000\010\001\010\001\006\001\007\001\
\008\001\009\001\022\001\011\001\012\001\013\001\002\001\010\001\
\019\001\006\001\007\001\008\001\009\001\077\000\011\001\012\001\
\013\001\010\001\082\000\007\001\008\001\009\001\001\001\011\001\
\012\001\013\001\007\001\008\001\006\001\093\000\007\001\008\001\
\013\001\001\001\001\001\001\001\013\001\001\001\007\001\019\001\
\022\001\006\001\255\255\006\001\255\255\007\001\022\001\022\001\
\022\001\022\001\022\001\022\001\022\001"

let yynames_const = "\
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
  EOL\000\
  "

let yynames_block = "\
  ID\000\
  NUM\000\
  LABELN\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : unit) in
    Obj.repr(
# 226 "parser.mly"
                                        (    )
# 407 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'display) in
    Obj.repr(
# 227 "parser.mly"
                                     (  )
# 414 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    Obj.repr(
# 233 "parser.mly"
                                 ( printlist (!nodelist @ !transition) )
# 420 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 234 "parser.mly"
                                    ( createfile  _2  !nodelist !transition)
# 427 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 237 "parser.mly"
                (_1)
# 434 "parser.ml"
               : 'numero))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 241 "parser.mly"
             (_1)
# 441 "parser.ml"
               : 'labelnoeud))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 242 "parser.mly"
                               (" POSITION: " ^ _3^":"^ _4)
# 450 "parser.ml"
               : 'labelnoeud))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 247 "parser.mly"
                               (" LABEL: " ^ _2 )
# 457 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 248 "parser.mly"
                              (" COLOR: " ^ _2 )
# 464 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 249 "parser.mly"
                                (" BGCOLOR: " ^ _2 )
# 471 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 250 "parser.mly"
                               (" INITIAL: " ^ _2 )
# 478 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 251 "parser.mly"
                             (" FINAL: " ^ _2 )
# 485 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 252 "parser.mly"
                         (" SIZE: " ^ _2)
# 492 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 253 "parser.mly"
                                       (" LABEL: " ^ _2 ^ _3 )
# 500 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 254 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 508 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 255 "parser.mly"
                                  (" BGCOLOR: " ^ _2 ^ _3 )
# 516 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 256 "parser.mly"
                                         (" INITIAL: " ^ _2 ^ _3 )
# 524 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 257 "parser.mly"
                                        ( " FINAL: " ^ _2 ^ _3 )
# 532 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 258 "parser.mly"
                                       ( " SIZE: " ^ _2 ^ _3 )
# 540 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 263 "parser.mly"
                              (" COLOR: " ^ _2 )
# 547 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 264 "parser.mly"
                                 ( " BGCOLOR: " ^ _2 )
# 554 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 265 "parser.mly"
                                        (" COLOR: " ^ _2 ^ _3 )
# 562 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 266 "parser.mly"
                                          (" BGCOLOR: " ^ _2 ^ _3 )
# 570 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 271 "parser.mly"
                                                     ( nodelist := add (Noeud(_2, ( _4), ( _5),_6)) @ !nodelist )
# 580 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 272 "parser.mly"
                                                      ( nodelist := add (Noeud(_2, ( _5), ( _6),_3)) @ !nodelist )
# 590 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 273 "parser.mly"
                                                                ( nodelist := add (Noeud(_2, ( _5), ( _6),_3 ^ (" "  ^ _7))) @ !nodelist )
# 601 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 274 "parser.mly"
                                             (  nodelist := add (Noeud(_2, ( _4), ( _5),"")) @ !nodelist)
# 610 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 277 "parser.mly"
                                                     ( nodelist := editn  (Noeud(_2, ( _5), ( _6),_7))  !nodelist )
# 620 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'attribut) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 278 "parser.mly"
                                                    ( nodelist := editn (Noeud(_2, ( _6), ( _7),_4))  !nodelist )
# 630 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'attribut) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 279 "parser.mly"
                                                              ( nodelist := editn (Noeud(_2, ( _6), ( _7),_4 ^ (" "  ^ _8)))  !nodelist )
# 641 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 280 "parser.mly"
                                           (  nodelist := editn (Noeud(_2, ( _5), ( _6),""))  !nodelist)
# 650 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 282 "parser.mly"
                                                  ( transition := add (Edge(_2,_4,_6,"")) @ !transition )
# 659 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 283 "parser.mly"
                                                            ( transition := add (Edge(_2,_4,_6,_7)) @ !transition )
# 669 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 284 "parser.mly"
                                                             ( transition := add (Edge(_2,_4,_7,_5))  @ !transition )
# 679 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 285 "parser.mly"
                                                                        ( transition := add (Edge(_2,_4,_7,_5 ^ (" "^ _8)))  @ !transition )
# 690 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 287 "parser.mly"
                                                    ( transition := editt (Edge(_2,_4,_7,""))  !transition )
# 699 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 288 "parser.mly"
                                                               ( transition := editt (Edge(_2,_4,_7,_8))  !transition )
# 709 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'attributf) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 289 "parser.mly"
                                                               ( transition := editt (Edge(_2,_4,_8,_6))   !transition )
# 719 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'attributf) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 290 "parser.mly"
                                                                            ( transition := editt (Edge(_2,_4,_8,_6 ^ (" "^ _9)))   !transition )
# 730 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 292 "parser.mly"
                      ( nodelist := removenoeud _2 !nodelist  )
# 737 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 293 "parser.mly"
                                (  transition := removetransition _2 _4  !transition )
# 745 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 295 "parser.mly"
                                (nodelist := moveall _2 _3 !nodelist)
# 753 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 296 "parser.mly"
                                   (nodelist := moveallid _2 _3 _4 !nodelist)
# 762 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 298 "parser.mly"
                            (nodelist:= renamen _2 _4 !nodelist ; transition:= renamet _2 _4 !transition ;)
# 770 "parser.ml"
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
