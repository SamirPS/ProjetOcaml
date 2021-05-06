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
  | PATH
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

        let editt  idun iddeux attributd l =
          let rec go l acc = match l with
            | [] -> List.rev acc
            | Edge(a,b,c,d)::xs when (a= createid idun ) && (b=createid iddeux) ->  let color = getvalue "COLOR:" "" (python_split d) in 
                                                                                   let label = c in 
                                                                                   let position = getvalue "POSITION:" "" (python_split d) in
                                                                                   let path = getvalue "PATH:" "" (python_split d) in

                                                                                   let colore = getvalue "COLOR:" color (python_split attributd) in 
                                                                                   let labele = getvalue "LABEL:" label (python_split attributd) in 
                                                                                   let positione = getvalue "POSITION:" position (python_split attributd) in
                                                                                   let pathe =  getvalue "PATH:" path (python_split attributd) in 

                                                                                   let attfinal = ref "" in 
                                                                                   if  not (colore = "") then attfinal:= " COLOR: " ^ colore ^ !attfinal; 
                                                                                   if not (positione = "") then attfinal:= " POSITION: " ^ positione ^ !attfinal;
                                                                                   if not (pathe = "") then attfinal:= " PATH: " ^ pathe ^ !attfinal;

                                                                                   go xs (Edge(a,b,labele,!attfinal)::acc)                
            | Noeud(a,b,c,d)::xs -> go xs acc
            | x::xs -> go xs (x::xs)

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

      
# 249 "parser.ml"
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
  278 (* PATH *);
  279 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* ID *);
  258 (* NUM *);
  259 (* LABELN *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\004\000\005\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\002\000\001\000\001\000\002\000\002\000\
\002\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000\002\000\002\000\003\000\003\000\003\000\
\003\000\004\000\002\000\002\000\002\000\005\000\003\000\003\000\
\003\000\006\000\006\000\006\000\007\000\005\000\006\000\007\000\
\007\000\008\000\006\000\002\000\004\000\003\000\004\000\004\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\049\000\000\000\000\000\000\000\000\000\004\000\
\044\000\000\000\000\000\005\000\000\000\000\000\000\000\001\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\046\000\000\000\000\000\000\000\
\006\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\045\000\047\000\048\000\000\000\000\000\013\000\014\000\
\015\000\017\000\018\000\016\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\035\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\043\000\
\037\000\000\000\040\000\023\000\024\000\025\000\000\000\000\000\
\000\000\000\000\026\000\042\000\000\000\033\000\031\000\032\000\
\000\000\000\000\034\000"

let yydgoto = "\002\000\
\011\000\012\000\013\000\021\000\042\000\033\000\067\000\080\000"

let yysindex = "\005\000\
\049\255\000\000\007\255\008\255\011\255\036\255\038\255\003\255\
\040\255\043\255\000\000\057\255\060\255\101\255\063\255\000\000\
\000\000\080\255\090\255\000\000\090\255\083\255\084\255\000\000\
\000\000\090\255\099\255\099\255\090\255\099\255\099\255\099\255\
\095\255\103\255\104\255\090\255\000\000\117\255\121\255\090\255\
\000\000\108\255\108\255\108\255\108\255\108\255\108\255\090\255\
\069\255\000\000\000\000\000\000\105\255\108\255\000\000\000\000\
\000\000\000\000\000\000\000\000\090\255\090\255\099\255\099\255\
\099\255\099\255\116\255\081\255\000\000\108\255\090\255\073\255\
\073\255\073\255\073\255\099\255\099\255\099\255\099\255\000\000\
\000\000\073\255\000\000\000\000\000\000\000\000\073\255\050\255\
\081\255\081\255\000\000\000\000\090\255\000\000\000\000\000\000\
\090\255\081\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\079\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\251\254\253\254\010\255\013\255\022\255\032\255\000\000\
\000\000\000\000\000\000\000\000\000\000\102\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\106\255\000\000\107\255\
\028\255\061\255\062\255\000\000\000\000\000\000\000\000\000\000\
\000\000\064\255\000\000\000\000\000\000\000\000\109\255\110\255\
\111\255\112\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\113\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\237\255\239\255\236\255\024\000\198\255"

let yytablesize = 136
let yytable = "\036\000\
\007\000\037\000\008\000\019\000\020\000\001\000\040\000\014\000\
\015\000\044\000\043\000\016\000\045\000\046\000\047\000\012\000\
\051\000\007\000\010\000\008\000\054\000\055\000\056\000\057\000\
\058\000\059\000\060\000\011\000\061\000\094\000\095\000\096\000\
\012\000\069\000\019\000\010\000\017\000\009\000\018\000\099\000\
\022\000\070\000\071\000\023\000\011\000\072\000\073\000\074\000\
\075\000\081\000\019\000\082\000\003\000\004\000\009\000\093\000\
\077\000\078\000\087\000\088\000\089\000\090\000\005\000\006\000\
\007\000\008\000\009\000\020\000\021\000\010\000\022\000\079\000\
\034\000\097\000\062\000\063\000\064\000\098\000\062\000\024\000\
\064\000\065\000\025\000\020\000\021\000\065\000\022\000\077\000\
\078\000\035\000\066\000\020\000\038\000\039\000\066\000\083\000\
\084\000\085\000\086\000\041\000\048\000\003\000\079\000\049\000\
\050\000\091\000\026\000\027\000\028\000\029\000\092\000\030\000\
\031\000\032\000\027\000\028\000\029\000\052\000\030\000\031\000\
\032\000\053\000\076\000\068\000\038\000\000\000\000\000\000\000\
\036\000\039\000\000\000\041\000\027\000\028\000\029\000\030\000"

let yycheck = "\019\000\
\006\001\021\000\006\001\001\001\002\001\001\000\026\000\001\001\
\001\001\029\000\028\000\001\001\030\000\031\000\032\000\006\001\
\036\000\023\001\006\001\023\001\040\000\042\000\043\000\044\000\
\045\000\046\000\047\000\006\001\048\000\088\000\089\000\090\000\
\023\001\054\000\007\001\023\001\001\001\006\001\001\001\098\000\
\001\001\061\000\062\000\001\001\023\001\063\000\064\000\065\000\
\066\000\070\000\023\001\071\000\004\001\005\001\023\001\006\001\
\007\001\008\001\076\000\077\000\078\000\079\000\014\001\015\001\
\016\001\017\001\018\001\007\001\007\001\021\001\007\001\022\001\
\010\001\093\000\006\001\007\001\008\001\097\000\006\001\023\001\
\008\001\013\001\023\001\023\001\023\001\013\001\023\001\007\001\
\008\001\010\001\022\001\002\001\010\001\010\001\022\001\072\000\
\073\000\074\000\075\000\001\001\006\001\023\001\022\001\001\001\
\001\001\082\000\006\001\007\001\008\001\009\001\087\000\011\001\
\012\001\013\001\007\001\008\001\009\001\001\001\011\001\012\001\
\013\001\001\001\007\001\019\001\023\001\255\255\255\255\255\255\
\023\001\023\001\255\255\023\001\023\001\023\001\023\001\023\001"

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
  PATH\000\
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
# 240 "parser.mly"
                                        (    )
# 423 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'display) in
    Obj.repr(
# 241 "parser.mly"
                                     (  )
# 430 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    Obj.repr(
# 247 "parser.mly"
                                 ( printlist (!nodelist @ !transition) )
# 436 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 248 "parser.mly"
                                    ( createfile  _2  !nodelist !transition)
# 443 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 251 "parser.mly"
                (_1)
# 450 "parser.ml"
               : 'numero))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 255 "parser.mly"
              (_1)
# 457 "parser.ml"
               : 'labelnoeud))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 260 "parser.mly"
                               (" LABEL: " ^ _2 )
# 464 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 261 "parser.mly"
                              (" COLOR: " ^ _2 )
# 471 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 262 "parser.mly"
                                (" BGCOLOR: " ^ _2 )
# 478 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 263 "parser.mly"
                               (" INITIAL: " ^ _2 )
# 485 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 264 "parser.mly"
                             (" FINAL: " ^ _2 )
# 492 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 265 "parser.mly"
                         (" SIZE: " ^ _2)
# 499 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 266 "parser.mly"
                                       (" LABEL: " ^ _2 ^ _3 )
# 507 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 267 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 515 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 268 "parser.mly"
                                  (" BGCOLOR: " ^ _2 ^ _3 )
# 523 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 269 "parser.mly"
                                         (" INITIAL: " ^ _2 ^ _3 )
# 531 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 270 "parser.mly"
                                        ( " FINAL: " ^ _2 ^ _3 )
# 539 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 271 "parser.mly"
                                       ( " SIZE: " ^ _2 ^ _3 )
# 547 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 278 "parser.mly"
                              (" COLOR: " ^ _2 )
# 554 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 279 "parser.mly"
                                 ( " BGCOLOR: " ^ _2 )
# 561 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 280 "parser.mly"
                            (" PATH: " ^ _2)
# 568 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 281 "parser.mly"
                              (" POSITION: " ^ _2^":"^ _3)
# 576 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 284 "parser.mly"
                                        (" COLOR: " ^ _2 ^ _3 )
# 584 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 285 "parser.mly"
                                          (" BGCOLOR: " ^ _2 ^ _3 )
# 592 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 286 "parser.mly"
                                      (" PATH: " ^ _2 ^ _3)
# 600 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 287 "parser.mly"
                                        (" POSITION: " ^ _2^":"^ _3 ^ _4)
# 609 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 292 "parser.mly"
                             (" LABEL: " ^ _2 )
# 616 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 293 "parser.mly"
                              (" COLOR: " ^ _2 )
# 623 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 294 "parser.mly"
                            (" PATH: " ^ _2)
# 630 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'labelnoeud) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 295 "parser.mly"
                                              (" POSITION: " ^ _4^":"^ _5)
# 639 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 297 "parser.mly"
                                         (" COLOR: " ^ _2 ^ _3 )
# 647 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 298 "parser.mly"
                                       (" PATH: " ^ _2 ^ _3)
# 655 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 299 "parser.mly"
                                        (" LABEL: " ^ _2 ^ _3 )
# 663 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'labelnoeud) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 300 "parser.mly"
                                                          (" POSITION: " ^ _4^":"^ _5 ^ _6)
# 673 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 305 "parser.mly"
                                                     ( nodelist := add (Noeud(_2, ( _4), ( _5),_6)) @ !nodelist )
# 683 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 306 "parser.mly"
                                                      ( nodelist := add (Noeud(_2, ( _5), ( _6),_3)) @ !nodelist )
# 693 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 307 "parser.mly"
                                                                ( nodelist := add (Noeud(_2, ( _5), ( _6),_3 ^ (" "  ^ _7))) @ !nodelist )
# 704 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 308 "parser.mly"
                                             (  nodelist := add (Noeud(_2, ( _4), ( _5),"")) @ !nodelist)
# 713 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 311 "parser.mly"
                                                  ( transition := add (Edge(_2,_4,_6,"")) @ !transition )
# 722 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 312 "parser.mly"
                                                            ( transition := add (Edge(_2,_4,_6,_7)) @ !transition )
# 732 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 313 "parser.mly"
                                                             ( transition := add (Edge(_2,_4,_7,_5))  @ !transition )
# 742 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 314 "parser.mly"
                                                                        ( transition := add (Edge(_2,_4,_7,_5 ^ (" "^ _8)))  @ !transition )
# 753 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 316 "parser.mly"
                                              ( transition := editt _2 _4 _6  !transition )
# 762 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 318 "parser.mly"
                      ( nodelist := removenoeud _2 !nodelist  )
# 769 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 319 "parser.mly"
                                (  transition := removetransition _2 _4  !transition )
# 777 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 321 "parser.mly"
                                (nodelist := moveall _2 _3 !nodelist)
# 785 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 322 "parser.mly"
                                   (nodelist := moveallid _2 _3 _4 !nodelist)
# 794 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 324 "parser.mly"
                            (nodelist:= renamen _2 _4 !nodelist ; transition:= renamet _2 _4 !transition ;)
# 802 "parser.ml"
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
