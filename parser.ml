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

                                                                    
                                                                      


        let editn  idun attributd l =
          let rec go l acc = match l with
            | [] -> List.rev acc
            | Noeud(a,b,c,d)::xs when (a= createid idun ) ->                       let x = b in 
                                                                                   let y = c in 
                                                                                   let label = getvalue "LABEL:" "" (python_split d) in
                                                                                   let color = getvalue "COLOR:" "" (python_split d) in
                                                                                   let bgcolor = getvalue "BGCOLOR:" "" (python_split d) in
                                                                                   let size = getvalue "SIZE:" "" (python_split d) in
                                                                                   let initial = getvalue "INITIAL:" "" (python_split d) in
                                                                                   let final = getvalue "FINAL:" "" (python_split d) in
                                                                                   

                                                                                   let xe = getvalue "X:" x (python_split attributd) in 
                                                                                   let ye = getvalue "Y:" y (python_split attributd) in 
                                                                                   let labele = getvalue "LABEL:" label (python_split attributd) in
                                                                                   let colore = getvalue "COLOR:" color (python_split attributd) in
                                                                                   let bgcolore = getvalue "BGCOLOR:" bgcolor (python_split attributd) in
                                                                                   let sizee = getvalue "SIZE:" size (python_split attributd) in
                                                                                   let initiale = getvalue "INITIAL:" initial (python_split attributd) in
                                                                                   let finale = getvalue "FINAL:" final (python_split attributd ) in

                                                                                   let attfinal = ref "" in 

                                                                                   if  not (labele = "") then attfinal:= " LABEL: " ^ labele ^ !attfinal; 
                                                                                   if not (colore = "") then attfinal:= " COLOR: " ^ colore ^ !attfinal;
                                                                                   if not (bgcolore = "") then attfinal:= " BGCOLOR: " ^ bgcolore ^ !attfinal;
                                                                                   if  not (sizee = "") then attfinal:= " SIZE: " ^ sizee ^ !attfinal; 
                                                                                   if not (initiale = "") then attfinal:= " INITIAL: " ^ initiale ^ !attfinal;
                                                                                   if not (finale = "") then attfinal:= " FINAL: " ^ finale ^ !attfinal;

                                                                                   go xs (Noeud(a,xe,ye,!attfinal)::acc)                
            | Edge(a,b,c,d)::xs -> go xs acc
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

      
# 298 "parser.ml"
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
  280 (* N *);
  281 (* S *);
  282 (* E *);
  283 (* O *);
  284 (* NW *);
  285 (* NE *);
  286 (* SW *);
  287 (* SE *);
    0|]

let yytransl_block = [|
  257 (* ID *);
  258 (* NUM *);
  259 (* LABELN *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\005\000\006\000\004\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\009\000\009\000\009\000\009\000\009\000\
\009\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\002\000\001\000\001\000\001\000\002\000\
\002\000\002\000\002\000\002\000\002\000\003\000\003\000\003\000\
\003\000\003\000\003\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000\002\000\003\000\003\000\003\000\
\004\000\002\000\002\000\002\000\005\000\003\000\003\000\003\000\
\006\000\002\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\004\000\006\000\
\006\000\007\000\005\000\006\000\007\000\007\000\008\000\006\000\
\004\000\002\000\004\000\003\000\004\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\071\000\000\000\000\000\000\000\000\000\
\007\000\004\000\066\000\000\000\000\000\005\000\000\000\000\000\
\000\000\000\000\001\000\002\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\068\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\
\021\000\022\000\023\000\024\000\025\000\026\000\027\000\000\000\
\000\000\000\000\000\000\000\000\067\000\069\000\070\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\065\000\000\000\
\000\000\014\000\015\000\016\000\018\000\019\000\017\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\056\000\000\000\000\000\
\000\000\000\000\006\000\000\000\000\000\000\000\049\000\050\000\
\051\000\053\000\054\000\052\000\000\000\000\000\000\000\064\000\
\058\000\000\000\061\000\031\000\032\000\000\000\055\000\000\000\
\000\000\000\000\033\000\063\000\000\000\040\000\038\000\039\000\
\000\000\000\000\041\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000\018\000\023\000\100\000\036\000\056\000\
\085\000\112\000\071\000"

let yysindex = "\012\000\
\073\255\000\000\008\255\018\255\034\255\048\255\060\255\054\255\
\062\255\072\255\078\255\000\000\039\255\051\255\119\255\070\255\
\000\000\000\000\000\000\071\255\080\255\000\000\080\255\082\255\
\084\255\099\255\000\000\000\000\080\255\034\255\034\255\080\255\
\093\255\093\255\034\255\105\255\111\255\112\255\080\255\000\000\
\113\255\127\255\128\255\080\255\134\255\134\255\134\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\134\255\
\134\255\134\255\080\255\046\255\000\000\000\000\000\000\080\255\
\034\255\034\255\080\255\093\255\093\255\034\255\000\000\118\255\
\134\255\000\000\000\000\000\000\000\000\000\000\000\000\080\255\
\080\255\034\255\034\255\143\255\141\255\080\255\127\255\127\255\
\127\255\127\255\127\255\127\255\094\255\000\000\134\255\080\255\
\061\255\061\255\000\000\061\255\034\255\127\255\000\000\000\000\
\000\000\000\000\000\000\000\000\034\255\034\255\143\255\000\000\
\000\000\061\255\000\000\000\000\000\000\061\255\000\000\064\255\
\094\255\094\255\000\000\000\000\080\255\000\000\000\000\000\000\
\080\255\094\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\126\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\253\254\254\254\001\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\255\
\011\255\016\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\129\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\130\255\131\255\
\132\255\133\255\135\255\136\255\000\000\000\000\137\255\000\000\
\138\255\021\255\000\000\025\255\000\000\139\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\043\255\000\000\000\000\000\000\140\255\000\000\142\255\
\144\255\145\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\146\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\231\255\235\255\039\000\225\255\223\255\
\189\255\241\255\008\000"

let yytablesize = 169
let yytable = "\039\000\
\057\000\040\000\008\000\009\000\045\000\046\000\013\000\044\000\
\015\000\058\000\047\000\011\000\001\000\074\000\075\000\076\000\
\012\000\062\000\016\000\008\000\009\000\010\000\073\000\013\000\
\077\000\078\000\079\000\028\000\011\000\115\000\116\000\029\000\
\117\000\012\000\090\000\091\000\017\000\080\000\010\000\087\000\
\088\000\094\000\086\000\028\000\092\000\089\000\123\000\029\000\
\019\000\030\000\124\000\081\000\082\000\083\000\021\000\022\000\
\097\000\098\000\095\000\096\000\020\000\027\000\024\000\113\000\
\102\000\030\000\081\000\084\000\083\000\125\000\109\000\110\000\
\025\000\028\000\114\000\118\000\003\000\004\000\026\000\037\000\
\038\000\022\000\084\000\120\000\121\000\111\000\005\000\006\000\
\007\000\008\000\009\000\041\000\010\000\011\000\103\000\104\000\
\105\000\106\000\107\000\108\000\109\000\110\000\042\000\129\000\
\126\000\127\000\128\000\130\000\043\000\119\000\059\000\060\000\
\061\000\063\000\131\000\111\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\029\000\030\000\031\000\032\000\
\072\000\033\000\034\000\035\000\064\000\065\000\066\000\067\000\
\093\000\068\000\069\000\070\000\030\000\031\000\032\000\099\000\
\033\000\034\000\035\000\101\000\003\000\122\000\000\000\059\000\
\042\000\043\000\047\000\045\000\000\000\046\000\044\000\057\000\
\060\000\048\000\062\000\000\000\034\000\000\000\035\000\036\000\
\037\000"

let yycheck = "\021\000\
\034\000\023\000\006\001\006\001\030\000\031\000\006\001\029\000\
\001\001\035\000\032\000\006\001\001\000\045\000\046\000\047\000\
\006\001\039\000\001\001\023\001\023\001\006\001\044\000\023\001\
\056\000\057\000\058\000\007\001\023\001\097\000\098\000\007\001\
\100\000\023\001\068\000\069\000\003\001\059\000\023\001\065\000\
\066\000\073\000\064\000\023\001\070\000\067\000\114\000\023\001\
\001\001\007\001\118\000\006\001\007\001\008\001\001\001\002\001\
\082\000\083\000\080\000\081\000\001\001\023\001\001\001\095\000\
\086\000\023\001\006\001\022\001\008\001\006\001\007\001\008\001\
\001\001\023\001\096\000\101\000\004\001\005\001\001\001\010\001\
\010\001\002\001\022\001\109\000\110\000\022\001\014\001\015\001\
\016\001\017\001\018\001\010\001\020\001\021\001\087\000\088\000\
\089\000\090\000\091\000\092\000\007\001\008\001\019\001\125\000\
\120\000\121\000\122\000\129\000\010\001\102\000\006\001\001\001\
\001\001\001\001\130\000\022\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\006\001\007\001\008\001\009\001\
\001\001\011\001\012\001\013\001\006\001\007\001\008\001\009\001\
\019\001\011\001\012\001\013\001\007\001\008\001\009\001\001\001\
\011\001\012\001\013\001\007\001\023\001\111\000\255\255\023\001\
\023\001\023\001\023\001\023\001\255\255\023\001\023\001\023\001\
\023\001\023\001\023\001\255\255\023\001\255\255\023\001\023\001\
\023\001"

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
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : unit) in
    Obj.repr(
# 281 "parser.mly"
                                        (    )
# 516 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'display) in
    Obj.repr(
# 282 "parser.mly"
                                     (  )
# 523 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    Obj.repr(
# 288 "parser.mly"
                                 ( printlist (!nodelist @ !transition) )
# 529 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 289 "parser.mly"
                                ( createfile  _2  !nodelist !transition)
# 536 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 292 "parser.mly"
                (_1)
# 543 "parser.ml"
               : 'numero))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 296 "parser.mly"
              (_1)
# 550 "parser.ml"
               : 'labelnoeud))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 301 "parser.mly"
                (String.sub _1 1 ((String.length _1) -2)  )
# 557 "parser.ml"
               : 'vrailabel))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 304 "parser.mly"
                              (" LABEL: " ^ _2 )
# 564 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 305 "parser.mly"
                             (" COLOR: " ^ _2 )
# 571 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 306 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 578 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 307 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 585 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 308 "parser.mly"
                            (" FINAL: " ^ _2 )
# 592 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 309 "parser.mly"
                         (" SIZE: " ^ _2)
# 599 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 311 "parser.mly"
                                      (" LABEL: " ^ _2 ^ _3 )
# 607 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 312 "parser.mly"
                                      (" COLOR: " ^ _2 ^ _3 )
# 615 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 313 "parser.mly"
                                  (" BGCOLOR: " ^ _2 ^ _3 )
# 623 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 314 "parser.mly"
                                        (" INITIAL: " ^ _2 ^ _3 )
# 631 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 315 "parser.mly"
                                       ( " FINAL: " ^ _2 ^ _3 )
# 639 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 316 "parser.mly"
                                      ( " SIZE: " ^ _2 ^ _3 )
# 647 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    Obj.repr(
# 321 "parser.mly"
            ("Nord")
# 653 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 322 "parser.mly"
            ("Sud")
# 659 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 323 "parser.mly"
            ("Est")
# 665 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 324 "parser.mly"
            ("Ouest")
# 671 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 325 "parser.mly"
             ("Nord West")
# 677 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 326 "parser.mly"
             ("Nord Est")
# 683 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 327 "parser.mly"
             ("Sud Ouest")
# 689 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 328 "parser.mly"
             ("Sud Est")
# 695 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 331 "parser.mly"
                             (" COLOR: " ^ _2 )
# 702 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 332 "parser.mly"
                            (" PATH: " ^ _2)
# 709 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 333 "parser.mly"
                              (" POSITION: " ^ _2^":"^ _3)
# 717 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 336 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 725 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 337 "parser.mly"
                                      (" PATH: " ^ _2 ^ _3)
# 733 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 338 "parser.mly"
                                        (" POSITION: " ^ _2^":"^ _3 ^ _4)
# 742 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 343 "parser.mly"
                            (" LABEL: " ^ _2 )
# 749 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 344 "parser.mly"
                             (" COLOR: " ^ _2 )
# 756 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'labelnoeud) in
    Obj.repr(
# 345 "parser.mly"
                            (" PATH: " ^ _2)
# 763 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 346 "parser.mly"
                                             (" POSITION: " ^ _4^":"^ _5)
# 772 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 348 "parser.mly"
                                        (" COLOR: " ^ _2 ^ _3 )
# 780 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'labelnoeud) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 349 "parser.mly"
                                       (" PATH: " ^ _2 ^ _3)
# 788 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 350 "parser.mly"
                                       (" LABEL: " ^ _2 ^ _3 )
# 796 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 351 "parser.mly"
                                                         (" POSITION: " ^ _4^":"^ _5 ^ _6)
# 806 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 355 "parser.mly"
                              (" LABEL: " ^ _2 )
# 813 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 356 "parser.mly"
                             (" COLOR: " ^ _2 )
# 820 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 357 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 827 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 358 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 834 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 359 "parser.mly"
                            (" FINAL: " ^ _2 )
# 841 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 360 "parser.mly"
                         (" SIZE: " ^ _2)
# 848 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 361 "parser.mly"
                             (" X: " ^ _2 ^ " Y: " ^ _3 )
# 856 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 363 "parser.mly"
                                        (" LABEL: " ^ _2 ^ _3 )
# 864 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 364 "parser.mly"
                                        (" COLOR: " ^ _2 ^ _3 )
# 872 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 365 "parser.mly"
                                    (" BGCOLOR: " ^ _2 ^ _3 )
# 880 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 366 "parser.mly"
                                          (" INITIAL: " ^ _2 ^ _3 )
# 888 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 367 "parser.mly"
                                         ( " FINAL: " ^ _2 ^ _3 )
# 896 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 368 "parser.mly"
                                        ( " SIZE: " ^ _2 ^ _3 )
# 904 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 369 "parser.mly"
                                        (" X: " ^ _2 ^ " Y: " ^ _3 ^ _4 )
# 913 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 375 "parser.mly"
                                                     ( nodelist := add (Noeud(_2, ( _4), ( _5),_6)) @ !nodelist )
# 923 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 376 "parser.mly"
                                                      ( nodelist := add (Noeud(_2, ( _5), ( _6),_3)) @ !nodelist )
# 933 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 377 "parser.mly"
                                                                ( nodelist := add (Noeud(_2, ( _5), ( _6),_3 ^ (" "  ^ _7))) @ !nodelist )
# 944 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 378 "parser.mly"
                                             (  nodelist := add (Noeud(_2, ( _4), ( _5),"")) @ !nodelist)
# 953 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 381 "parser.mly"
                                                 ( transition := add (Edge(_2,_4,_6,"")) @ !transition )
# 962 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 382 "parser.mly"
                                                           ( transition := add (Edge(_2,_4,_6,_7)) @ !transition )
# 972 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 383 "parser.mly"
                                                            ( transition := add (Edge(_2,_4,_7,_5))  @ !transition )
# 982 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 384 "parser.mly"
                                                                       ( transition := add (Edge(_2,_4,_7,_5 ^ (" "^ _8)))  @ !transition )
# 993 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 386 "parser.mly"
                                              ( transition := editt _2 _4 _6  !transition )
# 1002 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 387 "parser.mly"
                                    (nodelist := editn _2 _4 !nodelist )
# 1010 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 389 "parser.mly"
                      ( nodelist := removenoeud _2 !nodelist  )
# 1017 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 390 "parser.mly"
                                (  transition := removetransition _2 _4  !transition )
# 1025 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 392 "parser.mly"
                                (nodelist := moveall _2 _3 !nodelist)
# 1033 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 393 "parser.mly"
                                   (nodelist := moveallid _2 _3 _4 !nodelist)
# 1042 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 395 "parser.mly"
                            (nodelist:= renamen _2 _4 !nodelist ; transition:= renamet _2 _4 !transition ;)
# 1050 "parser.ml"
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
