(*
Type de mon automate par la suite on va faire deux liste 
*)
type 'a arbre =
  |Noeud of string * string *  string * string
  |Edge of string * string * string * string ;;

(*
Fonction annexe
*)

let python_split sep x =
  String.split_on_char  sep x
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


let createid id =
  if (String.length id) <= 15
    then begin id end
    else begin (String.sub id 0 14 ) end;;

let getnode idun l =
  let rec search l acc = match l with
    | [] -> List.rev acc
    | Noeud(a,b,c,d)::xs when a = idun   -> search xs (Noeud(a,b,c,d) :: acc)
    | _::xs -> search xs  acc
  in search l [];;

let rec containsele x l = match l with
  | [] -> false
  | Noeud(a,b,c,d)::q when (a = createid x) -> true
  | t::q -> containsele x q ;;

let rec containsedge x y t l = match l with
  | [] -> false
  | Edge(a,b,c,d)::q when (a= createid x) && (b=createid y ) && (c=createid t) -> true
  | t::q -> containsele x q ;;

  let rec containstrans x y l = match l with
  | [] -> false
  | Edge(a,b,c,d)::q when (a= createid x) && (b=createid y ) -> true
  | t::q -> containsele x q ;;


(* fonction pour create *)

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
    | Edge(a,b,c,d) when (containsedge a b c t = false) && (containsele b n ) && (containsele b n )  -> add_aux elem
    | _ -> failwith "Ajout impossible" ;;

(* DUMP *)
let rec printlist e  = 
  match e with
 [] -> Printf.printf "\n"
| h :: t -> (match h with 
          |Noeud(x,y,z,k)  -> Printf.printf "%s %s %s %s \n " x y z  k ; printlist t 
          |Edge(x,y,z,k)  -> Printf.printf "%s %s %s %s \n" x y z k ; printlist t ) ;;
(*pour enlever le warning*)

let getx node = 
  match node with
  | Noeud(a,b,c,d) -> b
  | _ -> "";;

let gety node = 
  match node with
  | Noeud(a,b,c,d) -> c
  | _ -> "";;

let getargs node valeur defaut  = 
  match node with
  | Noeud(a,b,c,d) -> getvalue valeur defaut (python_split ' ' d)
  | _ -> defaut;;

let isinteger numero = 
  match Float.is_integer numero with
  | true -> string_of_int (int_of_float numero)
  | false -> string_of_float numero;;


let counttransi idun iddeux listtransition =
  let rec go listtransition acc = 
    match listtransition  with
    | [] ->  acc
    | Edge(a,b,c,d)::q when (a=idun && b=iddeux) -> go q (acc+1)
    | Edge(a,b,c,d)::q when (a=iddeux && b=idun) -> go q (acc+1)
    | _::q -> go q acc
  in go listtransition 0 ;;

let getlabel idun iddeux listtransition =
  let rec go listtransition acc = 
    match listtransition  with
    | [] ->  acc
    | Edge(a,b,c,d)::q when (a=idun && b=iddeux) -> go q (c^" "^acc)
    | _::q -> go q acc
  in go listtransition "" ;;
  


let calcularc idun ideux l transi label argstransi=
  let count = counttransi idun ideux transi in 
  let nodeun = List.hd (getnode idun l) in
  let nodedeux = List.hd (getnode ideux l) in 
  let p1x = float_of_string (getx nodeun) in 
  let p1y = float_of_string (gety nodeun) in 
  let p2x = float_of_string (getx nodedeux) in 
  let p2y = float_of_string (gety nodedeux) in 


  let c1x = ref 0. in
  let c1y = ref 0. in
  
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

  if (count < 2 &&  ( not (idun = ideux))  && (p1y>p2y) ) then (* bas  à haut+ fleche **)
    curve := "M" ^ isinteger(p1x) ^ " " ^isinteger(p1y-.sizeun) ^ " L " ^ isinteger(p2x) ^ " " ^isinteger(p2y+.sizedeux);
  if (count < 2 &&  ( not (idun = ideux))  && (p1y>p2y) ) then (* bas  à haut+ fleche **)
    flecheref := "M"^isinteger(p2x)^","^isinteger (p2y+.sizedeux)^" l -8 8 m 16 0 l -8 -8";

  
  if (count < 2 &&  ( not (idun = ideux)) && (p2y>p1y) ) then (* haut à bas+ fleche **)
    curve := "M" ^ isinteger(p1x) ^ " " ^isinteger(p1y+.sizedeux) ^ " L " ^ isinteger  (  p2x ) ^ " " ^isinteger (  p2y-.sizedeux );
  if (count < 2 &&  ( not (idun = ideux)) && (p2y>p1y) ) then 
    flecheref := "M"^isinteger(p2x)^","^isinteger (p2y-.sizedeux)^" l 8 -8 m -16 0 l 8 8";
  
  if (count < 2 &&  ( not (idun = ideux)) && (p1x>p2x) ) then (* droite à gauche+ fleche **)
    curve := "M" ^ isinteger(p1x-.sizeun) ^ " " ^isinteger(p1y) ^ " L " ^ isinteger(  p2x+.sizedeux ) ^ " " ^isinteger (  p2y );
  if (count < 2 &&  ( not (idun = ideux)) && (p1x>p2x) ) then
    flecheref := "M"^isinteger(p2x+.sizedeux)^","^isinteger (p2y)^"l 8,-8 l 0,16 Z";

  if (count < 2 &&  ( not (idun = ideux)) && (p2x>p1x) ) then (* gauche à droite + fleche *)
    curve := "M" ^ isinteger(p1x+.sizeun) ^ " " ^isinteger(p1y) ^ " L " ^ isinteger(p2x-.sizedeux) ^ " " ^isinteger (  p2y );
  if (count < 2 &&  ( not (idun = ideux)) && (p2x>p1x) ) then
    flecheref := "M"^isinteger(p2x-.sizedeux)^","^isinteger (p2y)^"l -8,-8 l 0,16 Z";

  (*label direction normal *)
  if (count < 2 &&  ( not (idun = ideux))  && (p1y>p2y) ) then (* bas  à haut+ fleche **)
    c1x := ((p1x)+.(p2x))/.2. -. 5.;
  if (count < 2 &&  ( not (idun = ideux))  && (p1y>p2y) ) then (* bas  à haut+ fleche **)
    c1y := ((p1y-.sizeun)+.(p2y+. sizedeux))/.2.;

  if (count < 2 &&  ( not (idun = ideux)) && (p2y>p1y) ) then (* haut à bas+ fleche **)
    c1x := ((p1x)+.(p2x))/.2. -. 5.;
  if (count < 2 &&  ( not (idun = ideux)) && (p2y>p1y) ) then 
    c1y := ((p1y+.sizeun)+.(p2y-. sizedeux))/.2.;
  
  if (count < 2 &&  ( not (idun = ideux)) && (p1x>p2x) ) then (* droite à gauche+ fleche **)
    c1x := ((p1x-.sizeun)+.(p2x+. sizedeux))/.2.;
  if (count < 2 &&  ( not (idun = ideux)) && (p1x>p2x) ) then
    c1y := ((p1y)+.(p2y))/.2. -. 5.;

  if (count < 2 &&  ( not (idun = ideux)) && (p2x>p1x) ) then (* gauche à droite + fleche *)
    c1x := ((p1x+.sizeun)+.(p2x -. sizedeux))/.2.;
  if (count < 2 &&  ( not (idun = ideux)) && (p2x>p1x) ) then
    c1y := ((p1y)+.(p2y))/.2. -. 5.;

  (*fleche sud east *)
  if (count < 2 &&  ( not (idun = ideux)) && (p2x>p1x) && (p2y>p1y)) then
    flecheref := "M"^isinteger(p2x-.sizedeux)^","^isinteger (p2y)^"l -10 -4 l 10 -9 Z";
  (* fleche nord west *)
  if (count < 2 &&  ( not (idun = ideux))  && (p1y>p2y) && (p1x>p2x)) then 
    flecheref := "M"^isinteger(p2x+.sizedeux)^","^isinteger (p2y)^"l 10 4 l -10 9 Z";
  (* fleche sud wesh *)
  if (count < 2 &&  ( not (idun = ideux))  && (p2y>p1y) && (p1x>p2x)) then 
    flecheref := "M"^isinteger(p2x+.sizedeux)^","^isinteger (p2y)^"l 11,-2 l -9,-9 Z";
  (* fleche nord east*)
  if (count < 2 &&  ( not (idun = ideux))  && (p1y>p2y) && (p2x>p1x)) then 
    flecheref := "M"^isinteger(p2x-.sizedeux)^","^isinteger (p2y)^"l -11 2 l 9 9 Z";


  (*sur lui même plus fléche*)
  if (idun = ideux) then 
    curve := "M" ^ isinteger(p1x +. (sizeun *. 5. /. 30.)) ^ " " ^isinteger(p1y +. sizedeux -. 1.) ^ " A 15 20 0 1 1 " ^ isinteger(p2x -. (sizeun *. 5. /. 30.) ) ^ " " ^isinteger (p2y +. sizedeux -. 1.);
  if (idun = ideux) then 
    flecheref := "M"^isinteger(p2x -. (sizeun *. 5. /. 30.) )^","^isinteger (p2y +. sizedeux -. 1.)^"l -11 2 l 9 9 Z";

  if (idun = ideux) then 
    c1x := p1x +. sizeun ;
  if (idun = ideux) then 
    c1y := p1y +. sizedeux *. 2.;

  (*count multiple*)
  if ( (not (idun = ideux)) && count >= 2 && (p1y>p2y)) then (* bas  à haut+ fleche **)
    curve := "M" ^ isinteger(p1x -. (sizeun *. 5. /. 30.)) ^ "," ^isinteger(p1y -. sizeun) ^ " A 5,15 0 0 1 " ^ isinteger(p2x -. (sizedeux *. 5. /. 30.) ) ^ "," ^isinteger (p2y +. sizedeux);
  if ( (not (idun = ideux)) && count >= 2 && (p1y>p2y)) then
    flecheref := "M"^isinteger(p2x -. (sizedeux *. 5. /. 30.) )^","^isinteger (p2y +. sizedeux)^"l -11 2 l 9 9 Z";

  if ( (not (idun = ideux)) && count >= 2 && (p2y>p1y)) then (* haut  à bas+ fleche **)
    curve := "M" ^ isinteger(p1x +. (sizeun *. 5. /. 30.)) ^ "," ^isinteger(p1y +. sizeun) ^ " A 5,15 0 0 1 " ^ isinteger(p2x +. (sizedeux *. 5. /. 30.) ) ^ "," ^isinteger (p2y -. sizedeux);
  if ( (not (idun = ideux)) && count >= 2 && (p2y>p1y)) then
    flecheref := "M"^isinteger(p2x +. (sizedeux *. 5. /. 30.) )^","^isinteger (p2y -. sizedeux)^"l 11,-2 l -9,-9 Z";
  
  if ( (not (idun = ideux)) && count >= 2 && (p1x>p2x)) then (* droite  à gauche+ fleche **)
    curve := "M" ^ isinteger(p1x -. sizeun) ^ "," ^isinteger(p1y) ^ " A 10,10 0 0 1 " ^ isinteger(p2x +. sizedeux) ^ "," ^isinteger(p2y);
  if ( (not (idun = ideux)) && count >= 2 && (p1x>p2x)) then
    flecheref := "M"^isinteger(p2x +. sizedeux )^","^isinteger(p2y)^" l 10 4 l -10 9 Z";
  

  if ( (not (idun = ideux)) && count >= 2 && (p2x>p1x)) then (* gauche   à droite+ fleche **)
    curve := "M" ^ isinteger(p1x +. sizeun) ^ "," ^isinteger(p1y) ^ " A 10,10 0 0 1 " ^ isinteger(p2x -. sizedeux) ^ "," ^isinteger(p2y);
  if ( (not (idun = ideux)) && count >= 2 && (p2x>p1x)) then
    flecheref := "M"^isinteger(p2x -. sizedeux )^","^isinteger(p2y)^" l -10 -4 l 10 -9 Z";

  (*position*)

  if ( (not (idun = ideux)) && count >= 2 && (p1y>p2y)) then (* bas  à haut+ fleche **)
    c1x := p1x -. sizeun;
  if ( (not (idun = ideux)) && count >= 2 && (p1y>p2y)) then
    c1y := p2y +. sizedeux *. 1.8;

  if ( (not (idun = ideux)) && count >= 2 && (p2y>p1y)) then (* haut  à bas+ fleche **)
    c1x := p1x +. sizeun;
  if ( (not (idun = ideux)) && count >= 2 && (p2y>p1y)) then
    c1y := p1y +. sizeun *. 1.8 ;
  
  if ( (not (idun = ideux)) && count >= 2 && (p1x>p2x)) then (* droite  à gauche+ fleche **)
    c1x := p1x -. sizeun *. 1.8;
  if ( (not (idun = ideux)) && count >= 2 && (p1x>p2x)) then
    c1y := p2y -. sizedeux;
  

  if ( (not (idun = ideux)) && count >= 2 && (p2x>p1x)) then (* gauche   à droite+ fleche **)
    c1x := p2x -. sizedeux *. 1.8;
  if ( (not (idun = ideux)) && count >= 2 && (p2x>p1x)) then
    c1y := p2y +. sizedeux;
 
  curveedebut^ !curve ^curvefin^infox^isinteger(!c1x)^infoy^isinteger(!c1y)^ infoatt ^label^ labelinfo^fleche^ !flecheref^flechefin;;

let rec nodefile noeud monstr =
  match noeud with 
  |[] -> monstr;
  |Noeud(x,y,z,t)::q -> let size =  getvalue "SIZE:" "30" (python_split ' ' t)  in 
                        let color = getvalue "COLOR:" "black" (python_split ' ' t) in 
                        let label = getvalue "LABEL:" x (python_split ' ' t) in 
                        let bgcolor = getvalue "BGCOLOR:" "none" (python_split ' ' t) in 
                        nodefile q ( ( "<circle cx=\"" ^y^ "\" cy=\"" ^ z ^ "\" r=\""^size^" \" stroke=\"" ^ color ^ "\" stroke-width=\"2\" fill=\""^bgcolor^"\" > </circle> \n")  ^ 
                        ("<text x=\"" ^y^ "\" y=\"" ^ z ^ "\" dominant-baseline=\"middle \" fill=\"" ^ color ^"\" > "^ label ^" </text> \n") ^ monstr)
  | _ -> ""^monstr;;
      
let rec transitionfile noeud transi l monstr  = 
      match transi with 
      |[] -> monstr;
      |Edge(x,y,z,t)::q when (contains t "PATH" = false)-> let info =  calcularc x y  noeud l  (getlabel x y l) t in  
                            transitionfile noeud  q  l ( info ^ monstr)
      
      |Edge(x,y,z,t)::q  -> let info = getvalue "PATH:" "30" (python_split ' ' t)  in
                            let color = getvalue "COLOR:" "black"  (python_split ' ' t) in 
                            let infosur =  (String.concat " " (String.split_on_char '_' info)) in 
                            let nodeun = List.hd (getnode x l) in
                            let nodedeux = List.hd (getnode y l) in  
                            let p1x =float_of_string (getx nodeun) in 
                            let p1y = float_of_string (gety nodeun) in 
                            let p2x = float_of_string (getx nodedeux) in 
                            let p2y = float_of_string (gety nodedeux) in 

                            let mpx =(  ( p2x) +. ( p1x) ) *. 0.5 in
                            let mpy = (  ( p2y) +. ( p1y) ) *. 0.5 in 
                            let theta = (atan2 (p2y-.p1y) (p2x-.p1x)) -. (3.14 *. 0.5) in 
                            let offset =  30.0 in 
                            let c1x = mpx +. offset *. cos(theta) in
                            let c1y = mpy +. offset *. sin(theta) in
                            let fill = "<path fill=\"none\"  d=\"" ^ infosur ^ "\" stroke=\""^color^"\"></path> \n " in 
                            let label = "<text x=\"" ^isinteger (  c1x )  ^ "\" y=\"" ^isinteger (  c1y )  ^ "\" fill=\"black\" text-anchor=\"middle\"> " ^z^ " </text> \n " in 
                            transitionfile noeud  q l (  (fill ^label) ^ monstr)
      | _ -> ""^monstr;;

let rec initfinal listenoeud monstr= 
  match listenoeud with
  | [] -> monstr
  |Noeud(x,y,z,t)::q when (contains t "INITIAL" )-> let direct =  getvalue "INITIAL:" "Ouest" (python_split ' ' t) in 
                                                    let x1 = (float_of_string y) in
                                                    let y1 = ( float_of_string z ) in
                                                    let fleche = "<path fill=\""^"black"^"\"  d=\"" in 
                                                    let flechefin = "\" stroke=\""^"black"^"\"></path> \n" in 
                                                    let flecheref = ref "" in 
                                                    let sizeun = float_of_string (getvalue "SIZE:" "30" (python_split ' ' t)) in 
                                                    let myinfo = ref "" in 
                                                    
                                                    (*fleche pour la direction*)
                                                    if (direct = "Ouest" || direct = "none") then myinfo := "<path stroke=\"black\" d=\"M "^isinteger(x1 -. sizeun)^" "^isinteger(y1)^" l"^ isinteger(-.sizeun)^" 0\">";
                                                    if (direct = "Nord") then myinfo := "<path stroke=\"black\" d=\"M "^isinteger(x1)^" "^isinteger(y1+. sizeun)^" l 0 "^ isinteger(sizeun)^"\">";
                                                    if (direct = "Sud") then myinfo := "<path stroke=\"black\" d=\"M "^isinteger(x1)^" "^isinteger(y1-. sizeun)^" l 0 "^ isinteger(-.sizeun)^"\">";
                                                    if (direct = "Est") then myinfo := "<path stroke=\"black\" d=\"M "^isinteger(x1 +. sizeun)^" "^isinteger(y1)^" l"^ isinteger(sizeun)^" 0\">";
                                                    
                                                    (* fleche *)
                                                    if (direct = "Ouest" || direct = "none") then flecheref := "M"^isinteger(x1-.sizeun)^","^isinteger (y1)^"l -8,-8 l 0,16 Z";
                                                    if (direct = "Nord") then flecheref := "M"^isinteger(x1)^","^isinteger (y1+.sizeun)^" l -8 8 m 16 0 l -8 -8";
                                                    if (direct = "Sud") then flecheref := "M"^isinteger(x1)^","^isinteger (y1-.sizeun)^" l 8 -8 m -16 0 l 8 8";

                                                    if (direct = "Est") then flecheref := "M"^isinteger(x1+.sizeun)^","^isinteger (y1)^"l 8,-8 l 0,16 Z";


                                                    (* fleche direction *)
                                                    if (direct = "Nord-West" ) then myinfo := "<path stroke=\"black\" d=\"M "^isinteger(x1 +. sizeun)^" "^isinteger(y1 +. sizeun *. 2.)^" l"^ isinteger(-. sizeun)^isinteger(-.sizeun)^"\">";

                                                    if (direct = "Nord-Est") then  myinfo := "<path stroke=\"black\" d=\"M "^isinteger(x1 -. sizeun)^" "^isinteger(y1 +. sizeun *. 2.)^" l"^ isinteger(sizeun)^isinteger(-.sizeun)^"\">";

                                                    if (direct = "Sud-Ouest") then myinfo := "<path stroke=\"black\" d=\"M "^isinteger(x1 +. sizeun)^" "^isinteger(y1)^" l"^ isinteger(sizeun)^isinteger(-.sizeun)^"\">";

                                                    if (direct = "Sud-Est") then myinfo := "<path stroke=\"black\" d=\"M "^isinteger(x1 -. sizeun)^" "^isinteger(y1)^" l"^ isinteger(-.sizeun)^isinteger(-.sizeun)^"\">";
                                                    (*fleche noir *)
                                                    
                                                    if (direct = "Nord-West" ) then flecheref := "M"^isinteger(x1)^","^isinteger(y1+.sizeun)^"l 10 4 l -10 9 Z";

                                                    if (direct = "Nord-Est") then  flecheref := "M"^isinteger(x1)^","^isinteger(y1+.sizeun)^"l -11 2 l 9 9 Z";
                                                    
                                                    if (direct = "Sud-Ouest") then flecheref := "M"^isinteger(x1+.sizeun)^","^isinteger(y1)^" l 11,-2 l -9,-9 Z";
                                                    if (direct = "Sud-Est") then flecheref := "M"^isinteger(x1-.sizeun)^","^isinteger(y1)^" l -10 -4 l 10 -9 Z";

                                                    initfinal q  (!myinfo^"</path>"^fleche^ !flecheref ^ flechefin^monstr)


 | Noeud(x,y,z,t)::q when (contains t "FINAL" )-> let direct =  getvalue "FINAL:" "Est" (python_split ' ' t) in 
                                                    let x1 = (float_of_string y) in
                                                    let y1 = ( float_of_string z ) in
                                                    let fleche = "<path fill=\""^"black"^"\"  d=\"" in 
                                                    let flechefin = "\" stroke=\""^"black"^"\"></path> \n" in 
                                                    let flecheref = ref "" in 
                                                    let sizeun = float_of_string (getvalue "SIZE:" "30" (python_split ' ' t)) in 
                                                    let myinfo = ref "" in 
                                                    
                                                    (*fleche pour la direction*)
                                                    if (direct = "Est" || direct = "none") then myinfo := "<path stroke=\"black\" d=\"M "^isinteger(x1 +. sizeun)^" "^isinteger(y1)^" l"^ isinteger(sizeun)^" 0\">";
                                                    if (direct = "Nord") then myinfo := "<path stroke=\"black\" d=\"M "^isinteger(x1)^" "^isinteger(y1-. sizeun)^" l 0 "^ isinteger(-.sizeun)^"\">";
                                                    if (direct = "Sud") then myinfo := "<path stroke=\"black\" d=\"M "^isinteger(x1)^" "^isinteger(y1+. sizeun)^" l 0 "^ isinteger(sizeun)^"\">";
                                                    if (direct = "Ouest") then myinfo := "<path stroke=\"black\" d=\"M "^isinteger(x1 -. sizeun)^" "^isinteger(y1)^" l"^ isinteger(-.sizeun)^" 0\">";
                                                    
                                                    (* fleche *)
                                                    if (direct = "Est" || direct = "none") then flecheref := "M"^isinteger(x1+.sizeun*.2.)^","^isinteger (y1)^"l -8,-8 l 0,16 Z";
                                                    if (direct = "Nord") then flecheref := "M"^isinteger(x1)^","^isinteger (y1-.sizeun*.2.)^" l -8 8 m 16 0 l -8 -8";
                                                    if (direct = "Sud") then flecheref := "M"^isinteger(x1)^","^isinteger (y1+.sizeun *. 2.)^" l 8 -8 m -16 0 l 8 8";

                                                    if (direct = "Ouest") then flecheref := "M"^isinteger(x1-.sizeun*. 2.)^","^isinteger (y1)^"l 8,-8 l 0,16 Z";



                                                    if (direct = "Nord-West" ) then myinfo := "<path stroke=\"black\" d=\"M "^isinteger(x1 -. sizeun)^" "^isinteger(y1)^" l"^ isinteger(-.sizeun)^isinteger(-.sizeun)^"\">";

                                                    if (direct = "Nord-Est") then  myinfo := "<path stroke=\"black\" d=\"M "^isinteger(x1 +. sizeun)^" "^isinteger(y1)^" l"^ isinteger(sizeun)^isinteger(-.sizeun)^"\">";

                                                    if (direct = "Sud-Ouest") then  myinfo := "<path stroke=\"black\" d=\"M "^isinteger(x1 -. sizeun)^" "^isinteger(y1 +. sizeun *. 2.)^" l"^ isinteger(sizeun)^isinteger(-.sizeun)^"\">";

                                                    if (direct = "Sud-Est") then myinfo := "<path stroke=\"black\" d=\"M "^isinteger(x1 +. sizeun)^" "^isinteger(y1 +. sizeun *. 2.)^" l"^ isinteger(-. sizeun)^isinteger(-.sizeun)^"\">";

                                                    (*fleche noir *)
                                                    
                                                    if (direct = "Nord-West" ) then flecheref := "M"^isinteger(x1-.sizeun)^","^isinteger(y1)^" l -10 -4 l 10 -9 Z";

                                                    if (direct = "Nord-Est") then  flecheref := "M"^isinteger(x1+.sizeun)^","^isinteger(y1)^" l 11,-2 l -9,-9 Z";
                                                    
                                                    if (direct = "Sud-Ouest") then flecheref := "M"^isinteger(x1)^","^isinteger(y1+.sizeun)^"l -11 2 l 9 9 Z";
                                                    if (direct = "Sud-Est") then flecheref := "M"^isinteger(x1)^","^isinteger(y1+.sizeun)^"l 10 4 l -10 9 Z";

                                                    initfinal q  (!myinfo^"</path>"^fleche^ !flecheref ^ flechefin^monstr)
                                                                                                                                                  
 | _::q -> initfinal q monstr
;;

let createfile name liste secondl =
  let fic2 = open_out (name^"create.svg") in
  let mystrfinal=" <svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"800\" height=\"600\" viewBox=\"0 0 800 600\"> \n"^(nodefile liste "") ^ (transitionfile liste secondl secondl "") ^ (initfinal liste "") ^ "</svg>" in
  
  output_string fic2 mystrfinal;
  close_out fic2;;

  

  



(* Remove *)

let deleten e l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | Noeud(a,b,c,d)::xs when e = a -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l [];;

let removenoeud e l = 
  match containsele e l with
  |false ->failwith "Impossible à supprimer" 
  | _ -> deleten e l ;;

let removetransitionafternode e l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | Edge(a,b,c,d)::xs when (e = a) || (e = b) -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l [];;

(* remove pour transition *)
let rec containte x y l  = 
  match l with
  | [] -> false
  | Edge(a,b,c,d)::q when a=x && b=y -> true
  | t::q -> containte x y q ;;

let deletee e f  l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | Edge(a,b,c,d)::xs when (e = a) && (f = b)  -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l [];;

let removetransition e f  l =
  match containte e f l with
    |false ->failwith "Impossible à supprimer"
    | _ -> deletee e f  l ;;

(* MOVE *)

let getnumber numero stringnu = 
  string_of_float( float_of_string(numero) +. float_of_string(stringnu) );;

let moveall numun numdeux l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | Noeud(a,b,c,d)::xs  -> go xs (Noeud(a,getnumber numun b,getnumber numdeux c,d) :: acc)
    | Edge (_, _, _, _)::xs -> go xs  acc
  in go l [];;


let moveallid  id numun numdeux l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | Noeud(a,b,c,d)::xs when a = (id) -> go xs (Noeud(a,getnumber numun b,getnumber numdeux c,d) :: acc)
    | x::xs -> go xs  (x::acc)

  in go l [];;

let moveallid_aux id numun numdeux l =
  match containsele (id) l with
  | false -> failwith "Noeud existe pas"
  | true -> moveallid id numun numdeux l;;

let rec containscreateid a l = 
      match l with
    |[] -> false
    | x::xs when (createid x = a) -> true
    | _::xs -> containscreateid a xs;;

let movelistid  id numun numdeux l =
  let mylist = l in
  let rec go l acc = match l with
    | [] -> List.rev acc
    | Noeud(a,b,c,d)::xs when (containscreateid a id ) -> if (containsele a mylist = false  ) then failwith "Noeud inconnu"  else go xs (Noeud(a,getnumber numun b,getnumber numdeux c,d) :: acc)
    | x::xs -> go xs  (x::acc)

  in go l [];; 

(*RENAME*)

let renamen  ancien nouveau l =
  let mylist=l in 
  let rec go l acc = match l with
    | [] -> List.rev acc
    | Noeud(a,b,c,d)::xs when (a = ancien) && (containsele nouveau mylist = true ) ->failwith "Noeud existe déjà avec ce nom" 
    | Noeud(a,b,c,d)::xs when (a = ancien) && (containsele nouveau mylist = false ) ->go xs (Noeud(nouveau,b,c,d) :: acc)
    | x::xs -> go xs  (x::acc)

  in go l [];;

let renamet  ancien nouveau l =
    let mylist=l in
    let rec go l acc = match l with
      | [] -> List.rev acc 
      | Edge(a,b,c,d)::xs when (a =  ancien) && (b = ancien) -> if (containstrans nouveau nouveau mylist ) then failwith "Noeud existe déjà avec ce nom"  else go xs (Edge(nouveau,nouveau,c,d) :: acc)
      | Edge(a,b,c,d)::xs when (a =  ancien)  -> if (containstrans nouveau b mylist ) then failwith "Noeud existe déjà avec ce nom"  else go xs (Edge(nouveau,b,c,d) :: acc)
      | Edge(a,b,c,d)::xs when  (b =  ancien) -> if (containstrans a nouveau mylist ) then failwith "Noeud existe déjà avec ce nom"  else go xs (Edge(b,nouveau,c,d) :: acc)
      | x::xs -> go xs  (x::acc)

    in go l [];;

(*edit *)
  let editt  idun iddeux  attributd l =
    let rec go l acc = match l with
      | [] -> List.rev acc
      | Edge(a,b,c,d)::xs when (a= idun ) && (b=iddeux) ->                   let color = getvalue "COLOR:" "" (python_split ' ' d) in 
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
      | [] -> List.rev acc
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

(* LASF *)

(*function annexe*)

let getlettre listtransition =
  let rec go listtransition acc = match listtransition with
    | [] -> List.rev acc
    | Edge(a,b,c,d)::xs when ( (List.mem  c acc) = false) -> go xs ( c :: acc)
    | _::xs -> go xs  acc

  in go listtransition [];;

let rec existe nomnoeud transitionlist lettre =
  match transitionlist with 
  | [] -> false
  | Edge(a,b,c,d)::q when (a = nomnoeud) && ( c = lettre) -> true
  | _::q -> existe nomnoeud q lettre;;
  
let rec check nomnoeud transitionlist listeletter = 
  match listeletter with 
  | [] -> true
  | x::q when (existe nomnoeud transitionlist x = false ) -> false 
  | _::q -> check nomnoeud transitionlist q;;

  (* IS COMPLETE*)

let rec is_completeaux listenoeud listtransition =
  match listenoeud with 
    | [] -> true
    | Noeud(a,b,c,d)::q when (check a listtransition (getlettre listtransition) = false) -> false
    | _::q -> is_completeaux q listtransition;;

let is_complete listenoeud listtransition = 
  if (List.length listenoeud = 0 ) || (List.length listtransition = 0 ) then false
  else (is_completeaux listenoeud listtransition);;

(* Show complete *)
let showcompleteaux listenoeud listtransition =
  let rec go listenoeud acc = 
    match listenoeud  with
    | [] -> List.rev acc
    | Noeud(a,b,c,d)::q when (check a listtransition (getlettre listtransition) = false) -> go q (a::acc)
    | _::q -> go q acc
  in go listenoeud [];;

let changecolor color listenoeud listtransition nodepascomplet  = 
  let rec go listenoeud acc listakeep= 
      match listenoeud  with
      | [] -> List.rev acc
      | Noeud(a,b,c,d)::q when (List.mem a nodepascomplet) -> go q ((editn a (" BGCOLOR: "^color) listakeep)) ((editn a (" BGCOLOR: "^color) listakeep))

      | _::q -> go q acc listakeep
    in go listenoeud [] listenoeud;;
  
let showcomplet color listenoeud listtransition =
  let nodeamodifier = showcompleteaux listenoeud listtransition in 
  changecolor color listenoeud listtransition nodeamodifier;;


(*Complete le truc *)
 
let gettransicreeaux a identifiant listtransition listeletter =
  let rec go listeletter acc = 
    match listeletter  with
    | [] -> List.rev acc
    | x::q when (existe a listtransition x =false ) -> go q ((Edge(a,identifiant,x,""))::acc)
    | _::q -> go q acc
  in go listeletter [];;

let gettransicree identifiant listtransition listenoeud =
  let rec go listenoeud acc = match listenoeud with
    | [] -> List.rev acc
    | Noeud(a,b,c,d)::xs  -> go xs ( (gettransicreeaux a identifiant listtransition (getlettre listtransition))@acc)
    | _::xs -> go xs acc

  in go listenoeud [];;


  
let transiajouter identifiant listenoeud listtransition =
  let ajout = gettransicree  identifiant listtransition listenoeud in 
  listtransition@ajout;;

let complete identifiant listenoeud listtransition =
  if (is_complete listenoeud listtransition = false) && (containsele identifiant listenoeud = true ) then
    transiajouter identifiant listenoeud listtransition
  else listtransition;;

let completeaux identifiant numun numdeux listenoeud listtransition = 
  if (is_complete listenoeud listtransition = false) && (containsele identifiant listenoeud = false ) then
    ( add (Noeud(identifiant,numun,numdeux,"")) listenoeud listtransition) @ listenoeud
  else listenoeud;;
  
(*Deterministe*)

let numberinitial listenoeud  =
  let rec go listenoeud acc = match listenoeud with
    | [] -> acc
    | Noeud(a,b,c,d)::xs  when (contains d "INITIAL:")-> go xs (acc+1)
    | _::xs -> go xs acc
  in (go listenoeud 0) = 1;;

let countnumberlettre identifiant listtransition lettre =
  let rec go listtransition acc = 
    match listtransition  with
    | [] ->  acc
    | Edge(a,b,c,d)::q when (a=identifiant ) && (c= lettre) -> go q (acc+1)
    | _::q -> go q acc
  in (go listtransition 0) = 1 ;;

let rec countallletter nomnoeud transitionlist listeletter = 
  match listeletter with 
  | [] -> true
  | x::q when (countnumberlettre nomnoeud transitionlist x = false ) -> false 
  | _::q -> check nomnoeud transitionlist q;;

let rec is_deterministicaux listenoeud listtransition =
  match listenoeud with 
  | [] -> true
  | Noeud(a,b,c,d)::q when countallletter a listtransition (getlettre listtransition) = false -> false
  | _::q -> is_deterministicaux q listtransition;;

let is_deterministic listenoeud listtransition = 
  if (List.length listenoeud = 0 ) || (List.length listtransition = 0 ) then false
  else (is_deterministicaux listenoeud listtransition) && (numberinitial listenoeud = true) ;;

(* Show determinste *)

let showdeterminsteaux listenoeud listtransition =
  let rec go listenoeud acc = 
    match listenoeud  with
    | [] -> List.rev acc
    | Noeud(a,b,c,d)::q when (countallletter a listtransition (getlettre listtransition) = false ) -> go q (a::acc)
    | _::q -> go q acc
  in go listenoeud [];;

let changecolord color listenoeud listtransition nodepascomplet  = 
  let rec go listenoeud acc listakeep= 
      match listenoeud  with
      | [] -> List.rev acc
      | Noeud(a,b,c,d)::q when (List.mem a nodepascomplet) -> go q ((editn a (" BGCOLOR: "^color) listakeep)) ((editn a (" BGCOLOR: "^color) listakeep))

      | _::q -> go q acc listakeep
    in go listenoeud [] listenoeud;;
  
let showcompletd color listenoeud listtransition =
  let nodeamodifier = showdeterminsteaux listenoeud listtransition in 
  changecolord color listenoeud listtransition nodeamodifier;;

(* mot reconnu *)

let rec getnodeinitial listenoeud = 
  match listenoeud with
  | [] -> Noeud("","","","")
  | Noeud(a,b,c,d)::xs  when (contains d "INITIAL:")->Noeud(a,b,c,d)
  | _::xs -> getnodeinitial xs;;

let getargs noeud = 
  match noeud with
  |Noeud(a,b,c,d) -> d
  | _ -> "";;

let getid noeud = 
  match noeud with
  |Noeud(a,b,c,d) -> a
  | _ -> "";;

let rec getautresommet transitionlist identifiant x = 
  match transitionlist with
  | [] -> ""
  | Edge(a,b,c,d)::q when (a = identifiant) && (c=x) -> b
  | _::q -> getautresommet q identifiant x;;

let rec getnodecourant identifiant listenoeud =
  match listenoeud with
  | [] -> Noeud("","","","")
  | Noeud(a,b,c,d)::xs  when (a=identifiant)->Noeud(a,b,c,d)
  | _::xs -> getnodecourant identifiant xs;;


let is_accepted listenoeud listtransition mot = 
  let noeudcourant = ref (getnodeinitial listenoeud) in
  let lettre = ref ' ' in 
  let result = ref true  in 
  for i = 0 to String.length mot - 1 do 
    lettre := mot.[i];
    noeudcourant := getnodecourant ( getautresommet listtransition (getid !noeudcourant) (Char.escaped !lettre)) listenoeud;

  done;
  if (contains (getargs !noeudcourant) "FINAL:") then result:=true else result:=false; 
  !result ;;

let getchemin listenoeud listtransition mot =
  let noeudcourant = ref (getnodeinitial listenoeud) in
  let lettre = ref ' ' in 
  let chemin = ref ( ("Begin->")^(getid !noeudcourant) ) in
  for i = 0 to String.length mot - 1 do 
    lettre := mot.[i];
    noeudcourant := getnodecourant ( getautresommet listtransition (getid !noeudcourant) (Char.escaped !lettre)) listenoeud;
    if not ( getid !noeudcourant = "") then chemin := !chemin ^ "->" ^ (getid !noeudcourant);

  done;
  if (contains (getargs !noeudcourant) "FINAL:") then (!chemin)^"->END \n " else (!chemin)^"->NOTEND \n";; 