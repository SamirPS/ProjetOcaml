(*
Type de mon automate par la suite on va faire deux liste 
*)
type 'a arbre =
  |Noeud of string * string *  string * string
  |Edge of string * string * string * string ;;

(*
Fonction annexe
*)

(*
Permet de transformer un string en liste en séparant par rapport en x
*)
let python_split sep x =
  String.split_on_char  sep x
  |> List.filter (fun x -> x <> "")
;;

(* Permet de renvoyer la valeur associé à l'argument *)
let rec getvalue valeur default liste =
  match liste with
  |[] -> default 
  |h::q when h = valeur-> if List.length q != 0 then List.hd q else getvalue valeur default q
  |h::q -> getvalue valeur default q;;

(* Permet de savoir si s1 contient s2  *)
let contains s1 s2 =
try
  let len = String.length s2 in
  for i = 0 to String.length s1 - len do
    if String.sub s1 i len = s2 then raise Exit
  done;
  false
with Exit -> true;;

(* Permet de couper l'id a 15 caractéres *)
let createid id =
  if (String.length id) <= 15
    then begin id end
    else begin (String.sub id 0 14 ) end;;

(* permet de récuperer le node associé à indun *) 
let getnode idun l =
  let rec search l acc = match l with
    | [] ->  List.rev acc
    | Noeud(a,b,c,d)::xs when a = idun   -> search xs (Noeud(a,b,c,d) :: acc)
    | _::xs -> search xs  acc
  in search l [];;

(* permet de savoir si le node d'id x existe dans la liste l  *) 
let rec containsele x l = match l with
  | [] -> false
  | Noeud(a,b,c,d)::q when (a = createid x) -> true
  | t::q -> containsele x q ;;

(* permet de savoir si le edge d'id x d'id y et label t  existe dans la liste l  *) 
let rec containsedge x y t l = match l with
  | [] -> false
  | Edge(a,b,c,d)::q when (a= createid x) && (b=createid y ) && (c=createid t) -> true
  | t::q -> containsele x q ;;
(* permet de savoir si il existe une transition de x à y dans la  liste l  *) 

  let rec containstrans x y l = match l with
  | [] -> false
  | Edge(a,b,c,d)::q when (a= createid x) && (b=createid y ) -> true
  | t::q -> containsele x q ;;


(* Les deux fonctions sont là pour ajouter un noeud/transition et rajoute les parametres par défaut s'il existe pas *)

  let add_aux elem = 
      match elem with 
        
        | Noeud(x,y,z,t) when t = "" -> [Noeud(x,y,z," SIZE: 30 LABEL: " ^ x)]
        | Noeud(x,y,z,t) when ((contains t "SIZE:") = false) && ((contains t " LABEL: ") =false) -> [Noeud( x,y,z," SIZE: 30 LABEL: " ^ x ^t)]
        | Noeud(x,y,z,t) when (contains t "SIZE:") = false  -> [Noeud(x,y,z," SIZE: 30" ^t)]
        | Noeud(x,y,z,t) when (contains t "LABEL:") =  false -> [Noeud(x,y,z," LABEL: " ^ x ^t)]
        | Edge(x,y,z,t) -> [Edge(x,y,z,t)]
        | Noeud(x,y,z,t) -> [Noeud(x,y,z,t)] ;;
    
  let add elem n t  =
    match elem with
    | Noeud(a,b,c,d) when (containsele a n = false) -> add_aux elem
    | Edge(a,b,c,d) when (containsedge a b c t = false) && (containsele a n ) && (containsele b n )  -> add_aux elem
    | _ -> failwith "Ajout impossible" ;;


(*La fonction est là pour edit une transition, la seconde est pour édit un noeud  *)
let editt  idun iddeux  attributd lettre l =
let rec go l acc = match l with
  | [] ->  List.rev acc
  | Edge(a,b,c,d)::xs when (a= idun ) && (b=iddeux) && (c=lettre) ->                   let color = getvalue "COLOR:" "" (python_split ' ' d) in 
                                                                         let label = c in 
                                                                         let position = getvalue "POSITION:" "" (python_split ' ' d) in
                                                                         let path = getvalue "PATH:" "" (python_split ' ' d) in

                                                                         let colore = getvalue "COLOR:" color (python_split ' ' attributd) in 
                                                                         let labele = getvalue "LABEL:" label (python_split ' ' attributd) in 
                                                                         let positione = getvalue "POSITION:" position (python_split ' 'attributd) in
                                                                         let pathe =  getvalue "PATH:" path (python_split ' ' attributd) in 

                                                                         let attfinal = ref "" in 
                                                                         if  not (colore = "") then attfinal:= " COLOR: " ^ colore ^ !attfinal; 
                                                                         if not (positione = "") then attfinal:= " POSITION: " ^ positione ^ !attfinal;
                                                                         if not (pathe = "") then attfinal:= " PATH: " ^ pathe ^ !attfinal;

                                                                         go xs (Edge(a,b,labele,!attfinal)::acc)                
  | x::xs -> go xs (x::acc)

in go l [];;

                                                        

let editn  idun attributd l =
let rec go l acc = match l with
  | [] ->  List.rev acc
  | Noeud(a,b,c,d)::xs when (a= idun ) ->                                let x = b in 
                                                                         let y = c in 
                                                                         let label = getvalue "LABEL:" "" (python_split ' ' d) in
                                                                         let color = getvalue "COLOR:" "" (python_split ' ' d) in
                                                                         let bgcolor = getvalue "BGCOLOR:" "" (python_split ' ' d) in
                                                                         let size = getvalue "SIZE:" "" (python_split ' ' d) in
                                                                         let initial = getvalue "INITIAL:" "" (python_split ' ' d) in
                                                                         let final = getvalue "FINAL:" "" (python_split ' ' d) in
                                                                         

                                                                         let xe = getvalue "X:" x (python_split ' ' attributd) in 
                                                                         let ye = getvalue "Y:" y (python_split ' ' attributd) in 
                                                                         let labele = getvalue "LABEL:" label (python_split ' ' attributd) in
                                                                         let colore = getvalue "COLOR:" color (python_split ' ' attributd) in
                                                                         let bgcolore = getvalue "BGCOLOR:" bgcolor (python_split ' ' attributd) in
                                                                         let sizee = getvalue "SIZE:" size (python_split ' ' attributd) in
                                                                         let initiale = getvalue "INITIAL:" initial (python_split ' ' attributd) in
                                                                         let finale = getvalue "FINAL:" final (python_split ' ' attributd ) in

                                                                         let attfinal = ref "" in 

                                                                         if  not (labele = "") then attfinal:= " LABEL: " ^ labele ^ !attfinal; 
                                                                         if not (colore = "") then attfinal:= " COLOR: " ^ colore ^ !attfinal;
                                                                         if not (bgcolore = "") then attfinal:= " BGCOLOR: " ^ bgcolore ^ !attfinal;
                                                                         if  not (sizee = "") then attfinal:= " SIZE: " ^ sizee ^ !attfinal; 
                                                                         if not (initiale = "") then attfinal:= " INITIAL: " ^ initiale ^ !attfinal;
                                                                         if not (finale = "") then attfinal:= " FINAL: " ^ finale ^ !attfinal;
                                                                         go xs (Noeud(a,xe,ye,!attfinal)::acc)                
  | x::xs -> go xs (x::acc)

in go l [];;

(* DUMP *)
(*La fonction est là pour dump une liste de transition/edge*)
let rec printlist e  = 
  match e with
 [] -> Printf.printf "\n"
| h :: t -> (match h with 
          |Noeud(x,y,z,k)  -> Printf.printf "ID:%s (X,Y):(%s,%s) Args: %s \n " x y z  k ; printlist t 
          |Edge(x,y,z,k)  -> Printf.printf "TO(%s,%s) Label:%s Args:%s \n" x y z k ; printlist t ) ;;
(*pour enlever le warning*)
(*recuperer la position x d'un noeud*)
let getx node = 
  match node with
  | Noeud(a,b,c,d) -> b
  | _ -> "";;
 
(*recuperer la position y d'un noeud*)
let gety node = 
  match node with
  | Noeud(a,b,c,d) -> c
  | _ -> "";;
  
 (*recuperer arguments d'un noeud*)

let getargs node valeur defaut  = 
  match node with
  | Noeud(a,b,c,d) -> getvalue valeur defaut (python_split ' ' d)
  | _ -> defaut;;

(* converti en str un nombre *)
let isinteger numero = 
  match Float.is_integer numero with
  | true -> string_of_int (int_of_float numero)
  | false -> string_of_float numero;;

(* compte le nombre de transition entre a et b, b et a *)
let counttransi idun iddeux listtransition =
  let rec go listtransition acc = 
    match listtransition  with
    | [] ->  acc
    | Edge(a,b,c,d)::q when (a=idun && b=iddeux) -> go q (acc+1)
    | Edge(a,b,c,d)::q when (a=iddeux && b=idun) -> go q (acc+1)
    | _::q -> go q acc
  in go listtransition 0 ;;

(* recuperer les labels des transitions entre idun et iddeux*)
let getlabel idun iddeux listtransition =
  let rec go listtransition acc = 
    match listtransition  with
    | [] ->  acc
    | Edge(a,b,c,d)::q when (a=idun && b=iddeux) -> go q (c^" "^acc)
    | _::q -> go q acc
  in go listtransition "" ;;
  

(* fait le calcul des arcs si jamais on a pas de path*)
let calcularc idun ideux l transi label argstransi=
  let nodeun = List.hd (getnode idun l) in
  let nodedeux = List.hd (getnode ideux l) in 
  let p1x = float_of_string (getx nodeun) in 
  let p1y = float_of_string (gety nodeun) in 
  let p2x = float_of_string (getx nodedeux) in 
  let p2y = float_of_string (gety nodedeux) in 


  let c1x = ref 0. in
  let c1y = ref 0. in

  let position =  getvalue "POSITION:" "0:0" (python_split ' ' argstransi) in
  let xy =  python_split ':' position in
  let xajout = float_of_string (List.nth xy 0) in 
  let yajout = float_of_string (List.nth xy 1) in 
  
  let color = getvalue "COLOR:" "black" (python_split ' ' argstransi) in
  let curve = ref "" in 
  let curveedebut = "<path fill=\"none\"  d=\""  in 
  let curvefin = "\" stroke=\""^color^"\"></path> \n " in 
  let infox = "<text x=\"" in 
  let infoy = "\" y=\"" in 
  let infoatt = "\" fill=\"black\" text-anchor=\"middle\"> " in 
  let labelinfo = " </text> \n " in 
  let sizeun = float_of_string ( getargs nodeun "SIZE:" "30") in
  let sizedeux = float_of_string (getargs nodedeux "SIZE:" "30" ) in

  let fleche = "<path fill=\""^color^"\"  d=\"" in 
  let flechefin = "\" stroke=\""^color^"\"></path> \n" in 
  let flecheref = ref "" in 

  if ( ( not (idun = ideux))  && (p1y>p2y) ) then (* bas  à haut+ fleche **)
    curve := "M" ^ isinteger(p1x) ^ " " ^isinteger(p1y-.sizeun) ^ " Q " ^ isinteger(p2x -. sizedeux/. 2.) ^ " "^ isinteger((p1y+.p2y)/.2.)^" " ^isinteger(p2x) ^ " " ^isinteger(p2y+.sizedeux);
  if ( ( not (idun = ideux))  && (p1y>p2y) ) then (* bas  à haut+ fleche **)
    flecheref := "M"^isinteger(p2x)^","^isinteger (p2y+.sizedeux)^" l -11 2 l 9 9 Z";

  
  if ( ( not (idun = ideux)) && (p2y>p1y) ) then (* haut à bas+ fleche **)
    curve := "M" ^ isinteger(p1x) ^ " " ^isinteger(p1y+.sizedeux) ^ " Q " ^ isinteger(p2x +. sizedeux/. 2.) ^ " "^ isinteger((p1y+.p2y)/.2.)^" " ^isinteger(p2x) ^ " " ^isinteger(p2y-.sizedeux);
  if ( ( not (idun = ideux)) && (p2y>p1y) ) then 
    flecheref := "M"^isinteger(p2x)^","^isinteger (p2y-.sizedeux)^" l 11,-2 l -9,-9 Z";
  
  if ( ( not (idun = ideux)) && (p1x>p2x) ) then (* droite à gauche+ fleche **)
    curve := "M" ^ isinteger(p1x-.sizeun) ^ " " ^isinteger(p1y) ^ " Q " ^ isinteger((p2x +. p1x)/. 2.) ^ " "^ isinteger(p2y+.sizedeux/.2.)^" "  ^ isinteger(  p2x+.sizedeux ) ^ " " ^isinteger (  p2y );
  if ( ( not (idun = ideux)) && (p1x>p2x) ) then
    flecheref := "M"^isinteger(p2x+.sizedeux)^","^isinteger (p2y)^"l 10 4 l -10 9 Z";

  if ( ( not (idun = ideux)) && (p2x>p1x) ) then (* gauche à droite + fleche *)
    curve := "M" ^ isinteger(p1x+.sizeun) ^ " " ^isinteger(p1y) ^ " Q " ^ isinteger((p2x +. p1x)/. 2.) ^ " "^ isinteger(p2y-.sizedeux/.2.)^" " ^ isinteger(p2x-.sizedeux) ^ " " ^isinteger (  p2y );
  if ( ( not (idun = ideux)) && (p2x>p1x) ) then
    flecheref := "M"^isinteger(p2x-.sizedeux)^","^isinteger (p2y)^" l -10 -4 l 10 -9 Z";

 

  if ( (not (idun = ideux))  && (p1y>p2y)) then (* bas  à haut+ fleche **)
    c1x := p2x -. sizedeux/. 2.;
  if ( (not (idun = ideux))  && (p1y>p2y)) then
    c1y := (p1y+.p2y)/.2.;

  if ( (not (idun = ideux))  && (p2y>p1y)) then (* haut  à bas+ fleche **)
    c1x := p2x +. sizedeux/. 2.;
  if ( (not (idun = ideux))  && (p2y>p1y)) then
    c1y := (p1y+.p2y)/.2. ;
  
  if ( (not (idun = ideux))  && (p1x>p2x)) then (* droite  à gauche+ fleche **)
    c1x := (p2x +. p1x)/. 2.;
  if ( (not (idun = ideux))  && (p1x>p2x)) then
    c1y := p2y+.sizedeux/.2.;
  

  if ( (not (idun = ideux))  && (p2x>p1x)) then (* gauche   à droite+ fleche **)
    c1x := (p2x +. p1x)/. 2.;
  if ( (not (idun = ideux))  && (p2x>p1x)) then
    c1y := p2y -. sizedeux/. 2.;

  if ( (not (idun = ideux))  && (p2x>p1x) && (p2y>p1y)) then 
    c1x := (p2x +. p1x)/. 2. -. sizeun;
  if ( (not (idun = ideux))  && (p2x>p1x) && (p2y>p1y) ) then
    c1y := (p2x +. p1x)/. 2.;

  if ( (not (idun = ideux))  && (p2x<p1x) && (p2y<p1y)) then 
    c1x := (p2x +. p1x)/. 2. +. sizedeux ;
  if ( (not (idun = ideux))  && (p2x<p1x) && (p2y<p1y) ) then
    c1y := (p2x +. p1x)/. 2.;

  if ( (not (idun = ideux))  && (p1x<p2x) && (p2y>p1y)) then 
    c1x := (p2x +. p1x)/. 2. ;
  if ( (not (idun = ideux))  && (p1x<p2x) && (p2y>p1y) ) then
    c1y := (p2x +. p1x)/. 2.;
    
  if ( (not (idun = ideux))  && (p1x>p2x) && (p2y<p1y)) then 
    c1x := (p2x +. p1x)/. 2. +. sizeun/. 2. ;
  if ( (not (idun = ideux))  && (p1x>p2x) && (p2y>p1y) ) then
    c1y := (p2y +. p1y)/. 2. ;



  (*sur lui même plus fléche*)
  if (idun = ideux) then 
    curve := "M" ^ isinteger(p1x +. (sizeun *. 5. /. 30.)) ^ " " ^isinteger(p1y +. sizedeux -. 1.) ^ " A 15 20 0 1 1 " ^ isinteger(p2x -. (sizeun *. 5. /. 30.) ) ^ " " ^isinteger (p2y +. sizedeux -. 1.);
  if (idun = ideux) then 
    flecheref := "M"^isinteger(p2x -. (sizeun *. 5. /. 30.) )^","^isinteger (p2y +. sizedeux -. 1.)^"l -11 2 l 9 9 Z";

  if (idun = ideux) then 
    c1x := p1x +. sizeun ;
  if (idun = ideux) then 
    c1y := p1y +. sizedeux *. 2.;

 
  curveedebut^ !curve ^curvefin^infox^isinteger(!c1x +. xajout)^infoy^isinteger(!c1y +. yajout)^ infoatt ^label^ labelinfo^fleche^ !flecheref^flechefin;;

(* renvoie le str pour le svg pour les noeuds*)
let rec nodefile noeud monstr =
  match noeud with 
  |[] -> monstr;
  |Noeud(x,y,z,t)::q -> let size =  getvalue "SIZE:" "30" (python_split ' ' t)  in 
                        let color = getvalue "COLOR:" "black" (python_split ' ' t) in 
                        let label = getvalue "LABEL:" x (python_split ' ' t) in 
                        let bgcolor = getvalue "BGCOLOR:" "none" (python_split ' ' t) in 
                        nodefile q ( ( "<circle cx=\"" ^y^ "\" cy=\"" ^ z ^ "\" r=\""^size^" \" stroke=\"" ^ color ^ "\" stroke-width=\"2\" fill=\""^bgcolor^"\" > </circle> \n")  ^ 
                        ("<text x=\"" ^y^ "\" y=\"" ^ z ^ "\" dominant-baseline=\"middle \" fill=\"" ^ color ^"\" >"^ label ^"</text> \n") ^ monstr)
  | _ -> ""^monstr;;
  
 (*renvoie le str pour le svg pour les transitions*)
      
let rec transitionfile noeud transi l monstr  = 
      match transi with 
      |[] -> monstr;
      |Edge(x,y,z,t)::q when (contains t "PATH" = false)-> let info =  calcularc x y  noeud l  (getlabel x y l) t in  
                            transitionfile noeud  q  l ( info ^ monstr)
      
      |Edge(x,y,z,t)::q  -> let info = getvalue "PATH:" "30" (python_split ' ' t)  in
                            let color = getvalue "COLOR:" "black"  (python_split ' ' t) in 
                            let infosur =  (String.concat " " (String.split_on_char '_' info)) in 
                            let nodeun = List.hd (getnode x noeud) in
                            let nodedeux = List.hd (getnode y noeud) in 
                            let sizeun = float_of_string (getargs nodeun "SIZE:" "30")  in 
                            let sizedeux = float_of_string (getargs nodedeux "SIZE:" "30")  in 
                            let idun = x in 
                            let ideux = y in 
                            let infopath = python_split ' ' infosur in 
                            let p1x =float_of_string (List.nth infopath 1) in 
                            let p1y = float_of_string (List.nth infopath 2) in 
                            let p2x = float_of_string (List.nth infopath (List.length infopath - 2)) in 
                            let p2y = float_of_string (List.nth infopath (List.length infopath - 1))  in 

                            let c1x = ref 0. in
                            let c1y = ref 0. in

                            let position =  getvalue "POSITION:" "0:0" (python_split ' ' t) in
                            let xy =  python_split ':' position in
                            let xajout = float_of_string (List.nth xy 0) in 
                            let yajout = float_of_string (List.nth xy 1) in 

                            let fleche = "<path fill=\""^color^"\"  d=\"" in 
                            let flechefin = "\" stroke=\""^color^"\"></path> \n" in 
                            let flecheref = ref "" in 

                          
                            if ( (not (idun = ideux))  && (p1y>p2y)) then (* bas  à haut+ fleche **)
                              c1x := p2x -. sizedeux/. 2.;
                            if ( (not (idun = ideux))  && (p1y>p2y)) then
                              c1y := (p1y+.p2y)/.2.;
                          
                            if ( (not (idun = ideux))  && (p2y>p1y)) then (* haut  à bas+ fleche **)
                              c1x := p2x +. sizedeux/. 2.;
                            if ( (not (idun = ideux))  && (p2y>p1y)) then
                              c1y := (p1y+.p2y)/.2. ;
                            
                            if ( (not (idun = ideux))  && (p1x>p2x)) then (* droite  à gauche+ fleche **)
                              c1x := (p2x +. p1x)/. 2.;
                            if ( (not (idun = ideux))  && (p1x>p2x)) then
                              c1y := p2y+.sizedeux/.2.;
                            
                          
                            if ( (not (idun = ideux))  && (p2x>p1x)) then (* gauche   à droite+ fleche **)
                              c1x := (p2x +. p1x)/. 2.;
                            if ( (not (idun = ideux))  && (p2x>p1x)) then
                              c1y := p2y -. sizedeux/. 2.;
                          
                            if ( (not (idun = ideux))  && (p2x>p1x) && (p2y>p1y)) then 
                              c1x := (p2x +. p1x)/. 2. -. sizeun;
                            if ( (not (idun = ideux))  && (p2x>p1x) && (p2y>p1y) ) then
                              c1y := (p2x +. p1x)/. 2.;
                          
                            if ( (not (idun = ideux))  && (p2x<p1x) && (p2y<p1y)) then 
                              c1x := (p2x +. p1x)/. 2. +. sizedeux ;
                            if ( (not (idun = ideux))  && (p2x<p1x) && (p2y<p1y) ) then
                              c1y := (p2x +. p1x)/. 2.;
                          
                            if ( (not (idun = ideux))  && (p1x<p2x) && (p2y>p1y)) then 
                              c1x := (p2x +. p1x)/. 2. ;
                            if ( (not (idun = ideux))  && (p1x<p2x) && (p2y>p1y) ) then
                              c1y := (p2x +. p1x)/. 2.;
                              
                            if ( (not (idun = ideux))  && (p1x>p2x) && (p2y<p1y)) then 
                              c1x := (p2x +. p1x)/. 2. +. sizeun/. 2. ;
                            if ( (not (idun = ideux))  && (p1x>p2x) && (p2y>p1y) ) then
                              c1y := (p2y +. p1y)/. 2. ;
                          
                          
                            (*sur lui même plus fléche*)
                            
                            if (idun = ideux) then 
                              c1x := p1x +. sizeun ;
                            if (idun = ideux) then 
                              c1y := p1y +. sizedeux *. 2.;
                          
                          
                          
                            (*sur lui même plus fléche*)
                            if (idun = ideux) then 
                              flecheref := "M"^isinteger(p2x)^","^isinteger (p2y)^"l -11 2 l 9 9 Z";
                          
                          
                            (*count multiple*)
                            if ( (not (idun = ideux)) && (p1y>p2y)) then
                              flecheref := "M"^isinteger(p2x)^","^isinteger (p2y)^"l -11 2 l 9 9 Z";
                          
                            if ( (not (idun = ideux)) && (p2y>p1y)) then
                              flecheref := "M"^isinteger(p2x  )^","^isinteger (p2y)^"l 11,-2 l -9,-9 Z";
                            
                            if ( (not (idun = ideux)) && (p1x>p2x)) then
                              flecheref := "M"^isinteger(p2x )^","^isinteger(p2y)^" l 10 4 l -10 9 Z";
                            
                          
                           if ( (not (idun = ideux)) && (p2x>p1x)) then
                              flecheref := "M"^isinteger(p2x )^","^isinteger(p2y)^" l -10 -4 l 10 -9 Z";
                          
                            
                          
                            let fill = "<path fill=\"none\"  d=\"" ^ infosur ^ "\" stroke=\""^color^"\"></path> \n " in 
                            let label = "<text x=\"" ^isinteger (  !c1x +. xajout)  ^ "\" y=\"" ^isinteger (  !c1y+.yajout )  ^ "\" fill=\"black\" text-anchor=\"middle\"> " ^ (getlabel x y l)^ " </text> \n " ^fleche^ !flecheref^flechefin in 
                            transitionfile noeud  q l (  (fill ^label) ^ monstr)
      | _ -> ""^monstr;;

(* Fait le svg pour les fleches des états initaux et final*)
let rec initfinal listenoeud monstr color fill = 
  match listenoeud with
  | [] -> monstr
  |Noeud(x,y,z,t)::q when (contains t "INITIAL" ) &&  (contains  t " INITIAL: DELETE" =false) -> let direct =  getvalue "INITIAL:" "Ouest" (python_split ' ' t) in 
                                                    let x1 = (float_of_string y) in
                                                    let y1 = ( float_of_string z ) in
                                                    let fleche = "<path fill=\""^fill^"\"  d=\"" in 
                                                    let flechefin = "\" stroke=\""^color^"\"></path> \n" in 
                                                    let flecheref = ref "" in 
                                                    let sizeun = float_of_string (getvalue "SIZE:" "30" (python_split ' ' t)) in 
                                                    let myinfo = ref "" in 
                                                    
                                                    (*fleche pour la direction*)
                                                    if (direct = "Ouest" || direct = "none") then myinfo := "<path stroke=\""^color^"\" d=\"M "^isinteger(x1 -. sizeun)^" "^isinteger(y1)^" l"^ isinteger(-.sizeun)^" 0\">";
                                                    if (direct = "Nord") then myinfo := "<path stroke=\""^color^"\" d=\"M "^isinteger(x1)^" "^isinteger(y1+. sizeun)^" l 0 "^ isinteger(sizeun)^"\">";
                                                    if (direct = "Sud") then myinfo := "<path stroke=\""^color^"\" d=\"M "^isinteger(x1)^" "^isinteger(y1-. sizeun)^" l 0 "^ isinteger(-.sizeun)^"\">";
                                                    if (direct = "Est") then myinfo := "<path stroke=\""^color^"\" d=\"M "^isinteger(x1 +. sizeun)^" "^isinteger(y1)^" l"^ isinteger(sizeun)^" 0\">";
                                                    
                                                    (* fleche *)
                                                    if (direct = "Ouest" || direct = "none") then flecheref := "M"^isinteger(x1-.sizeun)^","^isinteger (y1)^"l -8,-8 l 0,16 Z";
                                                    if (direct = "Nord") then flecheref := "M"^isinteger(x1)^","^isinteger (y1+.sizeun)^" l -8 8 m 16 0 l -8 -8";
                                                    if (direct = "Sud") then flecheref := "M"^isinteger(x1)^","^isinteger (y1-.sizeun)^" l 8 -8 m -16 0 l 8 8";

                                                    if (direct = "Est") then flecheref := "M"^isinteger(x1+.sizeun)^","^isinteger (y1)^"l 8,-8 l 0,16 Z";


                                                    (* fleche direction *)
                                                    if (direct = "Nord-West" ) then myinfo := "<path stroke=\""^color^"\" d=\"M "^isinteger(x1 +. sizeun)^" "^isinteger(y1 +. sizeun *. 2.)^" l"^ isinteger(-. sizeun)^isinteger(-.sizeun)^"\">";

                                                    if (direct = "Nord-Est") then  myinfo := "<path stroke=\""^color^"\" d=\"M "^isinteger(x1 -. sizeun)^" "^isinteger(y1 +. sizeun *. 2.)^" l"^ isinteger(sizeun)^isinteger(-.sizeun)^"\">";

                                                    if (direct = "Sud-Ouest") then myinfo := "<path stroke=\""^color^"\" d=\"M "^isinteger(x1 +. sizeun)^" "^isinteger(y1)^" l"^ isinteger(sizeun)^isinteger(-.sizeun)^"\">";

                                                    if (direct = "Sud-Est") then myinfo := "<path stroke=\""^color^"\" d=\"M "^isinteger(x1 -. sizeun)^" "^isinteger(y1)^" l"^ isinteger(-.sizeun)^isinteger(-.sizeun)^"\">";
                                                    (*fleche noir *)
                                                    
                                                    if (direct = "Nord-West" ) then flecheref := "M"^isinteger(x1)^","^isinteger(y1+.sizeun)^"l 10 4 l -10 9 Z";

                                                    if (direct = "Nord-Est") then  flecheref := "M"^isinteger(x1)^","^isinteger(y1+.sizeun)^"l -11 2 l 9 9 Z";
                                                    
                                                    if (direct = "Sud-Ouest") then flecheref := "M"^isinteger(x1+.sizeun)^","^isinteger(y1)^" l 11,-2 l -9,-9 Z";
                                                    if (direct = "Sud-Est") then flecheref := "M"^isinteger(x1-.sizeun)^","^isinteger(y1)^" l -10 -4 l 10 -9 Z";

                                                    initfinal (editn  x " INITIAL: DELETE" (Noeud(x,y,z,t)::q))  (!myinfo^"</path>"^fleche^ !flecheref ^ flechefin^monstr) color fill


 | Noeud(x,y,z,t)::q when (contains t "FINAL" ) && (contains  t " FINAL: DELETE" =false) ->   let direct =  getvalue "FINAL:" "Est" (python_split ' ' t) in 
                                                    let x1 = (float_of_string y) in
                                                    let y1 = ( float_of_string z ) in
                                                    let fleche = "<path fill=\""^fill^"\"  d=\"" in 
                                                    let flechefin = "\" stroke=\""^color^"\"></path> \n" in 
                                                    let flecheref = ref "" in 
                                                    let sizeun = float_of_string (getvalue "SIZE:" "30" (python_split ' ' t)) in 
                                                    let myinfo = ref "" in 
                                                    
                                                    (*fleche pour la direction*)
                                                    if (direct = "Est" || direct = "none") then myinfo := "<path stroke=\""^color^"\" d=\"M "^isinteger(x1 +. sizeun)^" "^isinteger(y1)^" l"^ isinteger(sizeun)^" 0\">";
                                                    if (direct = "Nord") then myinfo := "<path stroke=\""^color^"\" d=\"M "^isinteger(x1)^" "^isinteger(y1-. sizeun)^" l 0 "^ isinteger(-.sizeun)^"\">";
                                                    if (direct = "Sud") then myinfo := "<path stroke=\""^color^"\" d=\"M "^isinteger(x1)^" "^isinteger(y1+. sizeun)^" l 0 "^ isinteger(sizeun)^"\">";
                                                    if (direct = "Ouest") then myinfo := "<path stroke=\""^color^"\" d=\"M "^isinteger(x1 -. sizeun)^" "^isinteger(y1)^" l"^ isinteger(-.sizeun)^" 0\">";
                                                    
                                                    (* fleche *)
                                                    if (direct = "Est" || direct = "none") then flecheref := "M"^isinteger(x1+.sizeun*.2.)^","^isinteger (y1)^"l -8,-8 l 0,16 Z";
                                                    if (direct = "Nord") then flecheref := "M"^isinteger(x1)^","^isinteger (y1-.sizeun*.2.)^" l -8 8 m 16 0 l -8 -8";
                                                    if (direct = "Sud") then flecheref := "M"^isinteger(x1)^","^isinteger (y1+.sizeun *. 2.)^" l 8 -8 m -16 0 l 8 8";

                                                    if (direct = "Ouest") then flecheref := "M"^isinteger(x1-.sizeun*. 2.)^","^isinteger (y1)^"l 8,-8 l 0,16 Z";



                                                    if (direct = "Nord-West" ) then myinfo := "<path stroke=\""^color^"\" d=\"M "^isinteger(x1 -. sizeun)^" "^isinteger(y1)^" l"^ isinteger(-.sizeun)^isinteger(-.sizeun)^"\">";

                                                    if (direct = "Nord-Est") then  myinfo := "<path stroke=\""^color^"\" d=\"M "^isinteger(x1 +. sizeun)^" "^isinteger(y1)^" l"^ isinteger(sizeun)^isinteger(-.sizeun)^"\">";

                                                    if (direct = "Sud-Ouest") then  myinfo := "<path stroke=\""^color^"\" d=\"M "^isinteger(x1 -. sizeun)^" "^isinteger(y1 +. sizeun *. 2.)^" l"^ isinteger(sizeun)^isinteger(-.sizeun)^"\">";

                                                    if (direct = "Sud-Est") then myinfo := "<path stroke=\""^color^"\" d=\"M "^isinteger(x1 +. sizeun)^" "^isinteger(y1 +. sizeun *. 2.)^" l"^ isinteger(-. sizeun)^isinteger(-.sizeun)^"\">";

                                                    (*fleche noir *)
                                                    
                                                    if (direct = "Nord-West" ) then flecheref := "M"^isinteger(x1-.sizeun)^","^isinteger(y1)^" l -10 -4 l 10 -9 Z";

                                                    if (direct = "Nord-Est") then  flecheref := "M"^isinteger(x1+.sizeun)^","^isinteger(y1)^" l 11,-2 l -9,-9 Z";
                                                    
                                                    if (direct = "Sud-Ouest") then flecheref := "M"^isinteger(x1)^","^isinteger(y1+.sizeun)^"l -11 2 l 9 9 Z";
                                                    if (direct = "Sud-Est") then flecheref := "M"^isinteger(x1)^","^isinteger(y1+.sizeun)^"l 10 4 l -10 9 Z";

                                                    initfinal  (editn  x " FINAL: DELETE" (Noeud(x,y,z,t)::q))  (!myinfo^"</path>"^fleche^ !flecheref ^ flechefin^monstr) color fill 
                                                                                                                                                  
 | _::q -> initfinal q monstr color fill
;;

(*crée le fichier svg et son contenu*)
let createfile name liste secondl =
  let fic2 = open_out (name^".svg") in
  let mystrfinal=" <svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"800\" height=\"600\" viewBox=\"0 0 800 600\"> \n"^(nodefile liste "") ^ (transitionfile liste secondl secondl "") ^ (initfinal liste "" "black" "black") ^ "</svg>" in
  
  output_string fic2 mystrfinal;
  close_out fic2;;

  

  



(* Remove *)

(*Permet de supprimer un noeud *)
let deleten e l =
  let rec go l acc = match l with
    | [] ->  List.rev acc
    | Noeud(a,b,c,d)::xs when e = a -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l [];;

(*verifie si le noeud existe si oui on le supprime sinon erreur*)
let removenoeud e l = 
  match containsele e l with
  |false ->failwith "Impossible à supprimer" 
  | _ -> deleten e l ;;

(*supprime les transitions associée a un noeud supprimé*)
let removetransitionafternode e l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | Edge(a,b,c,d)::xs when (e = a) || (e = b) -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l [];;

(* remove pour transition *)
(*permet de savoir si la transition existe entre x et y*)
let rec containte x y l  = 
  match l with
  | [] -> false
  | Edge(a,b,c,d)::q when a=x && b=y -> true
  | t::q -> containte x y q ;;

(* permet de supprimer une transition*)
let deletee e f  l =
  let rec go l acc = match l with
    | [] -> List.rev  acc
    | Edge(a,b,c,d)::xs when (e = a) && (f = b)  -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l [];;

(*verifie si la transition existe si oui on le supprime sinon erreur*)

let removetransition e f  l =
  match containte e f l with
    |false ->failwith "Impossible à supprimer"
    | _ -> deletee e f  l ;;

(* MOVE *)

(* permet de renvoyer le string de l'addition pour move*)
let getnumber numero stringnu = 
  string_of_float( float_of_string(numero) +. float_of_string(stringnu) );;

(*move tout les noeuds de numun numdeux*)
let moveall numun numdeux l =
  let rec go l acc = match l with
    | [] ->  List.rev acc
    | Noeud(a,b,c,d)::xs  -> go xs (Noeud(a,getnumber numun b,getnumber numdeux c,d) :: acc)
    | Edge (_, _, _, _)::xs -> go xs  acc
  in go l [];;

(*move le noeud dont l'id est donnée*)
let moveallid  id numun numdeux l =
  let rec go l acc = match l with
    | [] ->  List.rev acc
    | Noeud(a,b,c,d)::xs when a = (id) -> go xs (Noeud(a,getnumber numun b,getnumber numdeux c,d) :: acc)
    | x::xs -> go xs  (x::acc)

  in go l [];;
(*si le noeud existe on le move sinon erreur*)
let moveallid_aux id numun numdeux l =
  match containsele (id) l with
  | false -> failwith "Noeud existe pas"
  | true -> moveallid id numun numdeux l;;

(*verifie si l'id existe dans la liste donné par l'user*)
let rec containscreateid a l = 
      match l with
    |[] -> false
    | x::xs when (createid x = a) -> true
    | _::xs -> containscreateid a xs;;

(* permet de move les noeuds d'une liste donnée , si noeud existe pas alors on fait une erreur*)
let movelistid  id numun numdeux l =
  let mylist = l in
  let rec go l acc = match l with
    | [] ->  List.rev acc
    | Noeud(a,b,c,d)::xs when (containscreateid a id ) -> if (containsele a mylist = false  ) then failwith "Noeud inconnu"  else go xs (Noeud(a,getnumber numun b,getnumber numdeux c,d) :: acc)
    | x::xs -> go xs  (x::acc)

  in go l [];; 

(*RENAME*)

(* rename un noeud si existe déjà alors une erreur*)
let renamen  ancien nouveau l =
  let mylist=l in 
  let rec go l acc = match l with
    | [] -> List.rev acc
    | Noeud(a,b,c,d)::xs when (a = ancien) && (containsele nouveau mylist = true ) ->failwith "Noeud existe déjà avec ce nom" 
    | Noeud(a,b,c,d)::xs when (a = ancien) && (containsele nouveau mylist = false ) ->go xs (Noeud(nouveau,b,c,d) :: acc)
    | x::xs -> go xs  (x::acc)

  in go l [];;

(*rename les transitions associé au noeud renommé*)
let renamet  ancien nouveau l =
    let mylist=l in
    let rec go l acc = match l with
      | [] ->  List.rev acc 
      | Edge(a,b,c,d)::xs when (a =  ancien) && (b = ancien) -> if (containstrans nouveau nouveau mylist ) then failwith "Noeud existe déjà avec ce nom"  else go xs (Edge(nouveau,nouveau,c,d) :: acc)
      | Edge(a,b,c,d)::xs when (a =  ancien)  -> if (containstrans nouveau b mylist ) then failwith "Noeud existe déjà avec ce nom"  else go xs (Edge(nouveau,b,c,d) :: acc)
      | Edge(a,b,c,d)::xs when  (b =  ancien) -> if (containstrans a nouveau mylist ) then failwith "Noeud existe déjà avec ce nom"  else go xs (Edge(b,nouveau,c,d) :: acc)
      | x::xs -> go xs  (x::acc)

    in go l [];;

    (*Les deux fonctions calcule la transposé d'un automate la premiere inverse les etats finaux et intiaux et l'autre inverse les transitions*)
  let transpoenode listenoeud =
    let rec go l acc = match l with
      | [] ->  List.rev acc 
      | Noeud(a,b,c,t)::xs when (contains t "FINAL") && (contains t "INITIAL") ->
                                                        let label = getvalue "LABEL:" "" (python_split ' ' t) in
                                                        let color = getvalue "COLOR:" "" (python_split ' ' t) in
                                                        let bgcolor = getvalue "BGCOLOR:" "" (python_split ' ' t) in
                                                        let size = getvalue "SIZE:" "" (python_split ' ' t) in
                                                        let initial = getvalue "INITIAL:" "" (python_split ' ' t) in
                                                        let final = getvalue "FINAL:" "" (python_split ' ' t) in

                                                        let attfinal = ref "" in 
                                                        if  not (label = "") then attfinal:= " LABEL: " ^ label ^ !attfinal; 
                                                        if not (color = "") then attfinal:= " COLOR: " ^ color ^ !attfinal;
                                                        if not (bgcolor = "") then attfinal:= " BGCOLOR: " ^ bgcolor ^ !attfinal;
                                                        if  not (size = "") then attfinal:= " SIZE: " ^ size ^ !attfinal; 
                                                        attfinal:= " INITIAL: " ^ final ^ !attfinal;
                                                        attfinal:= " FINAL: " ^ initial ^ !attfinal;
                                                        go xs ((Noeud(a,b,c,!attfinal))::acc)

      | Noeud(a,b,c,t)::xs when (contains t "FINAL") ->
                                                        let label = getvalue "LABEL:" "" (python_split ' ' t) in
                                                        let color = getvalue "COLOR:" "" (python_split ' ' t) in
                                                        let bgcolor = getvalue "BGCOLOR:" "" (python_split ' ' t) in
                                                        let size = getvalue "SIZE:" "" (python_split ' ' t) in
                                                        let final = getvalue "FINAL:" "" (python_split ' ' t) in
                                                        let attfinal = ref "" in 

                                                        if  not (label = "") then attfinal:= " LABEL: " ^ label ^ !attfinal; 
                                                        if not (color = "") then attfinal:= " COLOR: " ^ color ^ !attfinal;
                                                        if not (bgcolor = "") then attfinal:= " BGCOLOR: " ^ bgcolor ^ !attfinal;
                                                        if  not (size = "") then attfinal:= " SIZE: " ^ size ^ !attfinal; 
                                                        attfinal:= " INITIAL: " ^ final ^ !attfinal;
                                                        go xs ((Noeud(a,b,c,!attfinal))::acc)

      | Noeud(a,b,c,t)::xs when (contains t "INITIAL")  ->let label = getvalue "LABEL:" "" (python_split ' ' t) in
                                                          let color = getvalue "COLOR:" "" (python_split ' ' t) in
                                                          let bgcolor = getvalue "BGCOLOR:" "" (python_split ' ' t) in
                                                          let size = getvalue "SIZE:" "" (python_split ' ' t) in
                                                          let initial = getvalue "INITIAL:" "" (python_split ' ' t) in
                                                          let attfinal = ref "" in 

                                                          if  not (label = "") then attfinal:= " LABEL: " ^ label ^ !attfinal; 
                                                          if not (color = "") then attfinal:= " COLOR: " ^ color ^ !attfinal;
                                                          if not (bgcolor = "") then attfinal:= " BGCOLOR: " ^ bgcolor ^ !attfinal;
                                                          if  not (size = "") then attfinal:= " SIZE: " ^ size ^ !attfinal; 
                                                          attfinal:= " FINAL: " ^ initial ^ !attfinal;
                                                          go xs ((Noeud(a,b,c,!attfinal))::acc)
      | x::xs -> go xs  (x::acc)

    in go listenoeud [];;

    let transpoeedge transitionlist =
      let rec go l acc = match l with
        | [] ->  List.rev acc 
        | Edge(a,b,c,t)::xs -> go xs ((Edge(b,a,c,t))::acc)
        | _::xs -> go xs acc
  
      in go transitionlist [];;

    

(* LASF *)

(*function annexe*)

(*récupere tout les labels de l'automate sans doublon*)
let getlettre listtransition =
  let rec go listtransition acc = match listtransition with
    | [] -> List.rev acc
    | Edge(a,b,c,d)::xs when ( (List.mem  c acc) = false) -> go xs ( c :: acc)
    | _::xs -> go xs  acc

  in go listtransition [];;

(*permet de savoir si il existe une transition partant de nomnoeud avec la lettre lettre*)
let rec existe nomnoeud transitionlist lettre =
  match transitionlist with 
  | [] -> false
  | Edge(a,b,c,d)::q when (a = nomnoeud) && ( c = lettre) -> true
  | _::q -> existe nomnoeud q lettre;;
 
 (* permet de savoir si il existe une transition pour chaque lettre et chaque noeud*)
let rec check nomnoeud transitionlist listeletter = 
  match listeletter with 
  | [] -> true
  | x::q when (existe nomnoeud transitionlist x = false ) -> false 
  | _::q -> check nomnoeud transitionlist q;;

  (* IS COMPLETE*)
(*Regarde si il existe une moins une transition par lettre pour chaque noeud*)
let rec is_completeaux listenoeud listtransition =
  match listenoeud with 
    | [] -> true
    | Noeud(a,b,c,d)::q when (check a listtransition (getlettre listtransition) = false) -> false
    | _::q -> is_completeaux q listtransition;;

(*retourne vrai si l'automate est complet*)
let is_complete listenoeud listtransition = 
  if (List.length listenoeud = 0 ) || (List.length listtransition = 0 ) then false
  else (is_completeaux listenoeud listtransition);;

(* Show complete *)
(*recupere les sommets non complet*)
let showcompleteaux listenoeud listtransition =
  let rec go listenoeud acc = 
    match listenoeud  with
    | [] -> List.rev acc
    | Noeud(a,b,c,d)::q when (check a listtransition (getlettre listtransition) = false) -> go q (a::acc)
    | _::q -> go q acc
  in go listenoeud [];;
(*change la couleur de ces sommets non complet*)
let changecolor color listenoeud listtransition nodepascomplet  = 
  let rec go listenoeud acc listakeep= 
      match listenoeud  with
      | [] ->  acc
      | Noeud(a,b,c,d)::q when (List.mem a nodepascomplet) -> go q ((editn a (" BGCOLOR: "^color) listakeep)) ((editn a (" BGCOLOR: "^color) listakeep))

      | _::q -> go q acc listakeep
    in go listenoeud [] listenoeud;;
 (*main des deux fonctions au dessus*)  
let showcomplet color listenoeud listtransition =
  let nodeamodifier = showcompleteaux listenoeud listtransition in 
  changecolor color listenoeud listtransition nodeamodifier;;


(*Complete le truc *)
 (* Crée les transitions là ou les noeuds ne sont pas complet*)
let gettransicreeaux a identifiant listtransition listeletter =
  let rec go listeletter acc = 
    match listeletter  with
    | [] ->  List.rev acc
    | x::q when (existe a listtransition x =false ) -> go q ((Edge(a,identifiant,x,""))::acc)
    | _::q -> go q acc
  in go listeletter [];;

(* Crée les transitions là ou les noeuds ne sont pas complet*)
let gettransicree identifiant listtransition listenoeud =
  let rec go listenoeud acc = match listenoeud with
    | [] ->  acc
    | Noeud(a,b,c,d)::xs  -> go xs ( acc@(gettransicreeaux a identifiant listtransition (getlettre listtransition)))
    | _::xs -> go xs acc

  in go listenoeud [];;


 (* Ajoute les transtions de la fonction d'avant à notre liste de transition*)
let transiajouter identifiant listenoeud listtransition =
  let ajout = gettransicree  identifiant listtransition listenoeud in 
  listtransition@ajout;;

(*complete notre automate*)
let complete identifiant listenoeud listtransition =
  if (is_complete listenoeud listtransition = false) && (containsele identifiant listenoeud = true ) then
    transiajouter identifiant listenoeud listtransition
  else listtransition;;

(*ajoute le noeud puit a notre liste de noeud*)
let completeaux identifiant numun numdeux listenoeud listtransition = 
  if (is_complete listenoeud listtransition = false) && (containsele identifiant listenoeud = false ) then
      listenoeud @ ( add (Noeud(identifiant,numun,numdeux,"")) listenoeud listtransition)
  else listenoeud;;
  
(*Deterministe*)

(*vérifie si le nombre d'états initial est de 1*)
let numberinitial listenoeud  =
  let rec go listenoeud acc = match listenoeud with
    | [] -> acc
    | Noeud(a,b,c,d)::xs  when (contains d "INITIAL:")-> go xs (acc+1)
    | _::xs -> go xs acc
  in (go listenoeud 0) = 1;;

(*compte le nombre de transition partant de a avec la lettre c*)
let countnumberlettre identifiant listtransition lettre =
  let rec go listtransition acc = 
    match listtransition  with
    | [] ->  acc
    | Edge(a,b,c,d)::q when (a=identifiant ) && (c= lettre) -> go q (acc+1)
    | _::q -> go q acc
  in (go listtransition 0) <= 1 ;;
(*renvoie faux si on a plus d'une transition par noeud pour la meme lettre*)
let rec countallletter nomnoeud transitionlist listeletter = 
  match listeletter with 
  | [] -> true
  | x::q when (countnumberlettre nomnoeud transitionlist x = false ) ->  false 
  | _::q -> countallletter nomnoeud transitionlist q;;

(* Renvoie faux si un noeud n'est pas determininste*)
let rec is_deterministicaux listenoeud listtransition =
  match listenoeud with 
  | [] -> true
  | Noeud(a,b,c,d)::q when countallletter a listtransition (getlettre listtransition) = false -> false
  | _::q -> is_deterministicaux q listtransition;;

(*renvoie vrai si l'automate est deterministe*)
let is_deterministic listenoeud listtransition = 
  if (List.length listenoeud = 0 ) || (List.length listtransition = 0 ) then false
  else (is_deterministicaux listenoeud listtransition) && (numberinitial listenoeud = true) ;;

(* Show determinste *)
(* récupere les noeuds qui empeche que l'automate soit deterministe*)
let showdeterminsteaux listenoeud listtransition =
  let rec go listenoeud acc = 
    match listenoeud  with
    | [] -> List.rev acc
    | Noeud(a,b,c,d)::q when (countallletter a listtransition (getlettre listtransition) = false ) -> go q (a::acc)
    | _::q -> go q acc
  in go listenoeud [];;

(* change la couleurs des noeuds récuperer au dessus *)
let changecolord color listenoeud listtransition nodepascomplet  = 
  let rec go listenoeud acc listakeep= 
      match listenoeud  with
      | [] ->  acc
      | Noeud(a,b,c,d)::q when (List.mem a nodepascomplet) -> go q ((editn a (" BGCOLOR: "^color) listakeep)) ((editn a (" BGCOLOR: "^color) listakeep))

      | _::q -> go q acc listakeep
    in go listenoeud [] listenoeud;;
 (* modifie la couleur des noeud qui empeche l'automate d'être deterministe*)
let showcompletd color listenoeud listtransition =
  let nodeamodifier = showdeterminsteaux listenoeud listtransition in 
  changecolord color listenoeud listtransition nodeamodifier;;

(* mot reconnu *)
(* permet de récuperer le noeud initial*)
let rec getnodeinitial listenoeud = 
  match listenoeud with
  | [] -> Noeud("","","","")
  | Noeud(a,b,c,d)::xs  when (contains d "INITIAL:")->Noeud(a,b,c,d)
  | _::xs -> getnodeinitial xs;;
(*recuper les arguments d'un noeud*)
let getargs noeud = 
  match noeud with
  |Noeud(a,b,c,d) -> d
  | _ -> "";;
(*recupere l'id du noeud*)
let getid noeud = 
  match noeud with
  |Noeud(a,b,c,d) -> a
  | _ -> "";;

(* permet de savoir l'autre id de la transition qui part de a avec la lettre x*)
let rec getautresommet transitionlist identifiant x = 
  match transitionlist with
  | [] -> ""
  | Edge(a,b,c,d)::q when (a = identifiant) && (c=x) -> b
  | _::q -> getautresommet q identifiant x;;
(*renvoie le noeud qui a l'id egal à identifiant*)
let rec getnodecourant identifiant listenoeud =
  match listenoeud with
  | [] -> Noeud("","","","")
  | Noeud(a,b,c,d)::xs  when (a=identifiant)->Noeud(a,b,c,d)
  | _::xs -> getnodecourant identifiant xs;;

(*renvoie vrai si le mot est accepté par l'automate*)
let is_accepted listenoeud listtransition mot = 
  let noeudcourant = ref (getnodeinitial listenoeud) in
  let lettre = ref ' ' in 
  let result = ref true  in 
  for i = 0 to String.length mot - 1 do 
    lettre := mot.[i];
    noeudcourant := getnodecourant ( getautresommet listtransition (getid !noeudcourant) (Char.escaped !lettre)) listenoeud;
  done;
  if ((contains (getargs !noeudcourant) "FINAL:") ) then result:=true else result:=false; 
  !result ;;

(*renvoie le chemin du mot dans l'automate*)
let getchemin listenoeud listtransition mot =
  let noeudcourant = ref (getnodeinitial listenoeud) in
  let lettre = ref ' ' in 
  let chemin = ref ((getid !noeudcourant) ) in
  for i = 0 to String.length mot - 1 do 
    lettre := mot.[i];
    noeudcourant := getnodecourant ( getautresommet listtransition (getid !noeudcourant) (Char.escaped !lettre)) listenoeud;
    if not ( getid !noeudcourant = "") then chemin := !chemin ^ "-" ^ (getid !noeudcourant);

  done;
  (!chemin) ;; 

(*dump string*)
(*cree le sgv pour la commande dump string*)

(*cree le fichier css*)
let createframesvg nombrechemin =
  let cssdebut = "<style type=\"text/css\">\n" in 
  let tempstoal = float_of_int (List.length nombrechemin) *. 1.5 in
  let cssm = ".frame {visibility:hidden;animation:frames " ^ isinteger(tempstoal)^"s linear infinite}\n" in 
  let fin = "@keyframes frames {0% {visibility:visible} 33.34% {visibility:hidden}} </style>\n" in 
  let milieucss = ref "" in 
  for i=0 to List.length nombrechemin - 1 do
    milieucss := !milieucss^"#_frame_"^string_of_int(i)^"{ animation-delay:"^isinteger(float_of_int(i)*.1.5)^"s}\n";
  done;
  cssdebut^cssm^ !milieucss^fin;;

(*recupere le edge qui part de idun avec la lettre lettre*)
let rec gettransiv idun lettre liste =
  match liste with 
  | [] -> []
  |Edge(a,b,c,d)::q when (a=idun) && (lettre=c) -> [Edge(a,b,c,d)]
  | _::q -> gettransiv idun lettre q;;

(*renvoie le str pour le svg pour les noeuds*)
let rec nodefilev noeud monstr color =
  match noeud with 
  |[] -> monstr;
  |Noeud(x,y,z,t)::q -> let size =  getvalue "SIZE:" "30" (python_split ' ' t)  in 
                        let label = getvalue "LABEL:" x (python_split ' ' t) in 
                        nodefilev q ( ( "<circle cx=\"" ^y^ "\" cy=\"" ^ z ^ "\" r=\""^size^" \" > </circle> \n")  ^ 
                        ("<text x=\"" ^y^ "\" y=\"" ^ z ^ "\" dominant-baseline=\"middle \" fill=\"" ^ color ^"\" >"^ label ^"</text> \n") ^ monstr) color
  | _ -> ""^monstr;;
 
(*crée le svg*)
let dumpwithstring name listenoeud listtransition mot =
  let fic2 = open_out (name^".svg") in

  let mystrfinal =" <svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"800\" height=\"600\" viewBox=\"0 0 800 600\"> \n " ^(nodefile listenoeud "") ^ (transitionfile listenoeud listtransition listtransition "") ^ (initfinal listenoeud "" "black" "black") in
  let chemin = python_split '-' (getchemin listenoeud listtransition mot) in 
  let strmodifier = ref "" in 
  let noeudcourant = ref [(getnodeinitial listenoeud)] in
  let transicourant = ref [] in 
  let color = ref "red" in 
  let colorsub = ref "lightCoral" in 
  if (is_accepted listenoeud listtransition mot) then color:="green"; 
  if (is_accepted listenoeud listtransition mot) then colorsub:="lightGreen"; 
  let debut = "  <g stroke=\""^ !color^"\" stroke-width=\"2\" fill=\""^ !colorsub^"\">" in
  
  for i=0 to ( min (String.length mot - 1) (List.length chemin - 1)) do
     
    noeudcourant := getnode (List.nth chemin i) listenoeud;
    transicourant := gettransiv (getid (List.hd !noeudcourant)) (String.make 1  mot.[i]) listtransition;
     transicourant := editt (getid (List.hd !noeudcourant)) ( getautresommet listtransition (getid (List.hd !noeudcourant)) (String.make 1  mot.[i]))  (" COLOR: "^ !color) (String.make 1  mot.[i])  !transicourant;
  
    strmodifier := "<g id=\"_frame_"^string_of_int(i)^"\" class=\"frame\">\n"^debut^" "^(nodefilev !noeudcourant "" !color) ^ (initfinal !noeudcourant "" !color !color)^(transitionfile listenoeud !transicourant listtransition "")^"\n"  ^ "</g></g> "^ !strmodifier;
   done;
  
  let i = List.length chemin - 1 in 
  noeudcourant := getnode (List.nth chemin i) listenoeud;
  strmodifier := "<g id=\"_frame_"^string_of_int(i)^"\" class=\"frame\">\n"^debut^" "^(nodefilev !noeudcourant "" !color) ^ (initfinal !noeudcourant "" !color !color) ^ "\n"  ^ "</g></g> "^ !strmodifier;
  
  output_string fic2 (mystrfinal^(createframesvg chemin)^ !strmodifier ^ "</svg>" );
  close_out fic2;;

    
