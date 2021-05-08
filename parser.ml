type token =
  | ID of (string)
  | NUM of (string)
  | LABELN of (string)
  | LIST of (string)
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

        let list_split x =
          String.split_on_char  ',' x
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

        let inList element liste = List.exists  element liste;; 

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

        let rec containscreateid a l = 
          match l with
        |[] -> false
        | x::xs when (createid x = a) -> true
        | _::xs -> containscreateid a xs;;


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

        let movelistid  id numun numdeux l =
        let rec go l acc = match l with
          | [] -> List.rev acc
          | Noeud(a,b,c,d)::xs when (containscreateid a id ) -> go xs (Noeud(a,numun,numdeux,d) :: acc)
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

    let rec transitionfile noeud transi monstr  = 
            match transi with 
            |[] -> monstr;
            |Edge(x,y,z,t)::q when (contains t "PATH" = false)-> let info =  calcularc x y  noeud  z in  
                                  transitionfile noeud  q  ( info ^ monstr)
            
            |Edge(x,y,z,t)::q  -> let info = getvalue "PATH:" "30" (python_split t)  in
                                  let infosur =  (String.concat " " (String.split_on_char '_' info)) in 
                                  let node = getnode x y noeud in
                                  let Noeud(a,b,c,d) = List.hd node in
                                  let Noeud(e,f,g,h) = List.hd (List.rev node) in 
                                  let p1x = float_of_string b in 
                                  let p1y = float_of_string c in 
                                  let p2x = float_of_string f in 
                                  let p2y = float_of_string g in 

                                  let mpx =(  ( p2x) +. ( p1x) ) *. 0.5 in
                                  let mpy = (  ( p2y) +. ( p1y) ) *. 0.5 in 
                                  let theta = (atan2 (p2y-.p1y) (p2x-.p1x)) -. (3.14 *. 0.5) in 
                                  let offset =  30.0 in 
                                  let c1x = mpx +. offset *. cos(theta) in
                                  let c1y = mpy +. offset *. sin(theta) in
                                  let fill = "<path fill=\"none\"  d=\"" ^ infosur ^ "\" stroke=\"black\"></path> \n " in 
                                  let label = "<text x=\"" ^ string_of_int ( int_of_float c1x )  ^ "\" y=\"" ^ string_of_int ( int_of_float c1y )  ^ "\" fill=\"black\" text-anchor=\"middle\"> " ^z^ " </text> \n " in 
                                  transitionfile noeud  q  (  (fill ^label) ^ monstr)
            | _ -> ""^monstr;;

    let createfile name liste secondl =
      let fic2 = open_out (name^".svg") in
      let mystrfinal=" <svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"800\" height=\"600\" viewBox=\"0 0 800 600\"> \n"^(nodefile liste "") ^ (transitionfile liste secondl "") ^  "</svg>" in

      output_string fic2 mystrfinal;
      close_out fic2;;

      
# 341 "parser.ml"
let yytransl_const = [|
  261 (* CREATENODE *);
  262 (* CREATEFROM *);
  263 (* AT *);
  264 (* LABEL *);
  265 (* COLOR *);
  266 (* SIZE *);
  267 (* TO *);
  268 (* INITIAL *);
  269 (* FINAL *);
  270 (* BGCOLOR *);
  271 (* DUMP *);
  272 (* REMOVE *);
  273 (* REMOVEEDGE *);
  274 (* MOVE *);
  275 (* RENAME *);
  276 (* WITH *);
  277 (* EDIT *);
  278 (* EDITEDGE *);
  279 (* PATH *);
  280 (* EOL *);
  281 (* N *);
  282 (* S *);
  283 (* E *);
  284 (* O *);
  285 (* NW *);
  286 (* NE *);
  287 (* SW *);
  288 (* SE *);
    0|]

let yytransl_block = [|
  257 (* ID *);
  258 (* NUM *);
  259 (* LABELN *);
  260 (* LIST *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\005\000\006\000\004\000\007\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\010\000\010\000\010\000\010\000\
\010\000\010\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\002\000\001\000\001\000\001\000\001\000\
\002\000\002\000\002\000\002\000\002\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\002\000\002\000\003\000\003\000\
\003\000\004\000\002\000\002\000\002\000\005\000\003\000\003\000\
\003\000\006\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\004\000\
\006\000\006\000\007\000\005\000\006\000\007\000\007\000\008\000\
\006\000\004\000\002\000\004\000\003\000\004\000\004\000\004\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\073\000\000\000\000\000\000\000\000\000\
\007\000\004\000\067\000\000\000\000\000\005\000\008\000\000\000\
\000\000\000\000\000\000\000\000\001\000\002\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\069\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\000\022\000\023\000\024\000\025\000\026\000\
\027\000\028\000\000\000\000\000\000\000\000\000\000\000\068\000\
\070\000\071\000\072\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\066\000\000\000\000\000\015\000\016\000\017\000\
\019\000\020\000\018\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\057\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\050\000\051\000\052\000\054\000\055\000\053\000\000\000\
\000\000\000\000\065\000\059\000\000\000\062\000\032\000\033\000\
\000\000\056\000\000\000\000\000\000\000\034\000\064\000\000\000\
\041\000\039\000\040\000\000\000\000\000\042\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000\018\000\024\000\000\000\025\000\038\000\
\059\000\089\000\115\000\075\000"

let yysindex = "\008\000\
\069\255\000\000\010\255\032\255\031\255\043\255\058\255\144\255\
\070\255\076\255\077\255\000\000\046\255\065\255\115\255\090\255\
\000\000\000\000\000\000\091\255\104\255\000\000\000\000\104\255\
\104\255\099\255\092\255\136\255\000\000\000\000\104\255\031\255\
\031\255\104\255\089\255\089\255\031\255\119\255\133\255\140\255\
\104\255\000\000\104\255\148\255\123\255\149\255\104\255\130\255\
\130\255\130\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\130\255\130\255\130\255\104\255\053\255\000\000\
\000\000\000\000\000\000\104\255\031\255\031\255\104\255\089\255\
\089\255\031\255\000\000\131\255\130\255\000\000\000\000\000\000\
\000\000\000\000\000\000\104\255\104\255\031\255\031\255\031\255\
\145\255\104\255\123\255\123\255\123\255\123\255\123\255\123\255\
\057\255\000\000\130\255\104\255\028\255\028\255\028\255\031\255\
\123\255\000\000\000\000\000\000\000\000\000\000\000\000\031\255\
\031\255\031\255\000\000\000\000\028\255\000\000\000\000\000\000\
\028\255\000\000\085\255\057\255\057\255\000\000\000\000\104\255\
\000\000\000\000\000\000\104\255\057\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\128\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\
\001\255\007\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\008\255\012\255\016\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\132\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\134\255\135\255\137\255\138\255\139\255\141\255\
\000\000\000\000\142\255\000\000\143\255\253\254\022\255\000\000\
\146\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\034\255\000\000\000\000\000\000\
\147\255\000\000\150\255\151\255\152\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\153\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\225\255\235\255\000\000\000\000\224\255\
\232\255\207\255\236\255\004\000"

let yytablesize = 177
let yytable = "\041\000\
\048\000\049\000\042\000\043\000\029\000\061\000\009\000\010\000\
\001\000\047\000\015\000\060\000\050\000\014\000\012\000\078\000\
\079\000\080\000\013\000\065\000\029\000\066\000\011\000\009\000\
\010\000\077\000\081\000\082\000\083\000\030\000\014\000\012\000\
\016\000\017\000\085\000\013\000\087\000\091\000\092\000\011\000\
\084\000\031\000\096\000\019\000\098\000\030\000\090\000\094\000\
\095\000\093\000\088\000\118\000\119\000\120\000\101\000\102\000\
\103\000\031\000\020\000\085\000\086\000\087\000\099\000\100\000\
\112\000\113\000\116\000\126\000\105\000\029\000\026\000\127\000\
\121\000\003\000\004\000\088\000\027\000\028\000\117\000\114\000\
\123\000\124\000\125\000\005\000\006\000\007\000\008\000\009\000\
\030\000\010\000\011\000\128\000\112\000\113\000\106\000\107\000\
\108\000\109\000\110\000\111\000\039\000\040\000\129\000\130\000\
\131\000\022\000\132\000\114\000\122\000\044\000\133\000\045\000\
\134\000\051\000\052\000\053\000\054\000\055\000\056\000\057\000\
\058\000\031\000\032\000\033\000\034\000\062\000\035\000\036\000\
\037\000\068\000\069\000\070\000\071\000\063\000\072\000\073\000\
\074\000\032\000\033\000\034\000\064\000\035\000\036\000\037\000\
\021\000\022\000\046\000\023\000\067\000\076\000\097\000\003\000\
\104\000\000\000\000\000\060\000\000\000\043\000\044\000\000\000\
\048\000\046\000\047\000\000\000\045\000\058\000\061\000\000\000\
\000\000\049\000\063\000\000\000\000\000\035\000\036\000\037\000\
\038\000"

let yycheck = "\021\000\
\032\000\033\000\024\000\025\000\008\001\037\000\007\001\007\001\
\001\000\031\000\001\001\036\000\034\000\007\001\007\001\048\000\
\049\000\050\000\007\001\041\000\024\001\043\000\007\001\024\001\
\024\001\047\000\059\000\060\000\061\000\008\001\024\001\024\001\
\001\001\003\001\007\001\024\001\009\001\069\000\070\000\024\001\
\062\000\008\001\074\000\001\001\077\000\024\001\068\000\072\000\
\073\000\071\000\023\001\101\000\102\000\103\000\086\000\087\000\
\088\000\024\001\001\001\007\001\008\001\009\001\084\000\085\000\
\008\001\009\001\099\000\117\000\090\000\024\001\001\001\121\000\
\104\000\005\001\006\001\023\001\001\001\001\001\100\000\023\001\
\112\000\113\000\114\000\015\001\016\001\017\001\018\001\019\001\
\024\001\021\001\022\001\007\001\008\001\009\001\091\000\092\000\
\093\000\094\000\095\000\096\000\011\001\011\001\123\000\124\000\
\125\000\002\001\128\000\023\001\105\000\011\001\132\000\020\001\
\133\000\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\007\001\008\001\009\001\010\001\007\001\012\001\013\001\
\014\001\007\001\008\001\009\001\010\001\001\001\012\001\013\001\
\014\001\008\001\009\001\010\001\001\001\012\001\013\001\014\001\
\001\001\002\001\011\001\004\001\001\001\001\001\020\001\024\001\
\008\001\255\255\255\255\024\001\255\255\024\001\024\001\255\255\
\024\001\024\001\024\001\255\255\024\001\024\001\024\001\255\255\
\255\255\024\001\024\001\255\255\255\255\024\001\024\001\024\001\
\024\001"

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
  LIST\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : unit) in
    Obj.repr(
# 324 "parser.mly"
                                        (    )
# 565 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'display) in
    Obj.repr(
# 325 "parser.mly"
                                     (  )
# 572 "parser.ml"
               : unit ))
; (fun __caml_parser_env ->
    Obj.repr(
# 331 "parser.mly"
                                 ( printlist (!nodelist @ !transition) )
# 578 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 332 "parser.mly"
                                ( createfile  _2  !nodelist !transition)
# 585 "parser.ml"
               : 'display))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 335 "parser.mly"
                (_1)
# 592 "parser.ml"
               : 'numero))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 339 "parser.mly"
              (_1)
# 599 "parser.ml"
               : 'labelnoeud))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 344 "parser.mly"
                (String.sub _1 1 ((String.length _1) -2)  )
# 606 "parser.ml"
               : 'vrailabel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 347 "parser.mly"
              ( list_split (String.sub _1 1 ((String.length _1) -2)) )
# 613 "parser.ml"
               : 'glist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 350 "parser.mly"
                              (" LABEL: " ^ _2 )
# 620 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 351 "parser.mly"
                             (" COLOR: " ^ _2 )
# 627 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 352 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 634 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 353 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 641 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 354 "parser.mly"
                            (" FINAL: " ^ _2 )
# 648 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 355 "parser.mly"
                         (" SIZE: " ^ _2)
# 655 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 357 "parser.mly"
                                      (" LABEL: " ^ _2 ^ _3 )
# 663 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 358 "parser.mly"
                                      (" COLOR: " ^ _2 ^ _3 )
# 671 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 359 "parser.mly"
                                  (" BGCOLOR: " ^ _2 ^ _3 )
# 679 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 360 "parser.mly"
                                        (" INITIAL: " ^ _2 ^ _3 )
# 687 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 361 "parser.mly"
                                       ( " FINAL: " ^ _2 ^ _3 )
# 695 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 362 "parser.mly"
                                      ( " SIZE: " ^ _2 ^ _3 )
# 703 "parser.ml"
               : 'attribut))
; (fun __caml_parser_env ->
    Obj.repr(
# 367 "parser.mly"
            ("Nord")
# 709 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 368 "parser.mly"
            ("Sud")
# 715 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 369 "parser.mly"
            ("Est")
# 721 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 370 "parser.mly"
            ("Ouest")
# 727 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 371 "parser.mly"
             ("Nord West")
# 733 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 372 "parser.mly"
             ("Nord Est")
# 739 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 373 "parser.mly"
             ("Sud Ouest")
# 745 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    Obj.repr(
# 374 "parser.mly"
             ("Sud Est")
# 751 "parser.ml"
               : 'direction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 377 "parser.mly"
                             (" COLOR: " ^ _2 )
# 758 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 378 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 765 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 379 "parser.mly"
                              (" POSITION: " ^ _2^":"^ _3)
# 773 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 382 "parser.mly"
                                       (" COLOR: " ^ _2 ^ _3 )
# 781 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 383 "parser.mly"
                                     (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 789 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 384 "parser.mly"
                                        (" POSITION: " ^ _2^":"^ _3 ^ _4)
# 798 "parser.ml"
               : 'attributf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 389 "parser.mly"
                            (" LABEL: " ^ _2 )
# 805 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 390 "parser.mly"
                             (" COLOR: " ^ _2 )
# 812 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 391 "parser.mly"
                           (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)))
# 819 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 392 "parser.mly"
                                             (" POSITION: " ^ _4^":"^ _5)
# 828 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 394 "parser.mly"
                                        (" COLOR: " ^ _2 ^ _3 )
# 836 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 395 "parser.mly"
                                      (" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' _2)) ^ _3)
# 844 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 396 "parser.mly"
                                       (" LABEL: " ^ _2 ^ _3 )
# 852 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'vrailabel) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 397 "parser.mly"
                                                         (" POSITION: " ^ _4^":"^ _5 ^ _6)
# 862 "parser.ml"
               : 'attributet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 401 "parser.mly"
                              (" LABEL: " ^ _2 )
# 869 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 402 "parser.mly"
                             (" COLOR: " ^ _2 )
# 876 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 403 "parser.mly"
                               (" BGCOLOR: " ^ _2 )
# 883 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 404 "parser.mly"
                              (" INITIAL: " ^ _2 )
# 890 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'direction) in
    Obj.repr(
# 405 "parser.mly"
                            (" FINAL: " ^ _2 )
# 897 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 406 "parser.mly"
                         (" SIZE: " ^ _2)
# 904 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 407 "parser.mly"
                             (" X: " ^ _2 ^ " Y: " ^ _3 )
# 912 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 409 "parser.mly"
                                        (" LABEL: " ^ _2 ^ _3 )
# 920 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 410 "parser.mly"
                                        (" COLOR: " ^ _2 ^ _3 )
# 928 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 411 "parser.mly"
                                    (" BGCOLOR: " ^ _2 ^ _3 )
# 936 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 412 "parser.mly"
                                          (" INITIAL: " ^ _2 ^ _3 )
# 944 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 413 "parser.mly"
                                         ( " FINAL: " ^ _2 ^ _3 )
# 952 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'direction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 414 "parser.mly"
                                        ( " SIZE: " ^ _2 ^ _3 )
# 960 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 415 "parser.mly"
                                        (" X: " ^ _2 ^ " Y: " ^ _3 ^ _4 )
# 969 "parser.ml"
               : 'attributen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 421 "parser.mly"
                                                     ( nodelist := add (Noeud(_2, ( _4), ( _5),_6)) @ !nodelist )
# 979 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 422 "parser.mly"
                                                      ( nodelist := add (Noeud(_2, ( _5), ( _6),_3)) @ !nodelist )
# 989 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'attribut) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'numero) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attribut) in
    Obj.repr(
# 423 "parser.mly"
                                                                ( nodelist := add (Noeud(_2, ( _5), ( _6),_3 ^ (" "  ^ _7))) @ !nodelist )
# 1000 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 424 "parser.mly"
                                             (  nodelist := add (Noeud(_2, ( _4), ( _5),"")) @ !nodelist)
# 1009 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 427 "parser.mly"
                                                 ( transition := add (Edge(_2,_4,_6,"")) @ !transition )
# 1018 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 428 "parser.mly"
                                                           ( transition := add (Edge(_2,_4,_6,_7)) @ !transition )
# 1028 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'vrailabel) in
    Obj.repr(
# 429 "parser.mly"
                                                            ( transition := add (Edge(_2,_4,_7,_5))  @ !transition )
# 1038 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'attributf) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'vrailabel) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'attributf) in
    Obj.repr(
# 430 "parser.mly"
                                                                       ( transition := add (Edge(_2,_4,_7,_5 ^ (" "^ _8)))  @ !transition )
# 1049 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'attributet) in
    Obj.repr(
# 432 "parser.mly"
                                              ( transition := editt _2 _4 _6  !transition )
# 1058 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributen) in
    Obj.repr(
# 433 "parser.mly"
                                    (nodelist := editn _2 _4 !nodelist )
# 1066 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 435 "parser.mly"
                      ( nodelist := removenoeud _2 !nodelist  )
# 1073 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 436 "parser.mly"
                                (  transition := removetransition _2 _4  !transition )
# 1081 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 438 "parser.mly"
                                (nodelist := moveall _2 _3 !nodelist)
# 1089 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 439 "parser.mly"
                                   (nodelist := moveallid _2 _3 _4 !nodelist)
# 1098 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'glist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numero) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'numero) in
    Obj.repr(
# 440 "parser.mly"
                                      (nodelist := movelistid _2 _3 _4 !nodelist)
# 1107 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 442 "parser.mly"
                            (nodelist:= renamen _2 _4 !nodelist ; transition:= renamet _2 _4 !transition ;)
# 1115 "parser.ml"
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
