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

let getnode idun ideux l =
  let rec search l acc = match l with
    | [] -> List.rev acc
    | Noeud(a,b,c,d)::xs when (a = createid idun) || (a= createid ideux)    -> search xs (Noeud(a,b,c,d) :: acc)
    | _::xs -> search xs  acc
  in search l [];;


(* fonction pour create *)

  let add elem = 
      match elem with 
        
        | Noeud(x,y,z,t) when t = "" -> [Noeud(createid x,y,z," SIZE: 30 LABEL: " ^ x)]
        | Noeud(x,y,z,t) when ((contains t "SIZE:") = false) && ((contains t " LABEL: ") =false) -> [Noeud(createid x,y,z," SIZE: 30 LABEL: " ^ x ^t)]
        | Noeud(x,y,z,t) when (contains t "SIZE:") = false  -> [Noeud(createid x,y,z," SIZE: 30" ^t)]
        | Noeud(x,y,z,t) when (contains t "LABEL:") =  false -> [Noeud(createid x,y,z," LABEL: " ^ x ^t)]
        | Edge(x,y,z,t) -> [Edge(createid x,createid y,z,t)]
        | Noeud(x,y,z,t) -> [Noeud(createid x,y,z,t)] ;;

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

let calcularc idun ideux l label=
  let node = getnode idun ideux l in
  let nodeun = List.hd node in
  let nodedeux = List.hd (List.rev node) in 
  let p1x =float_of_string (getx nodeun) in 
  let p1y = float_of_string (gety nodeun) in 
  let p2x = float_of_string (getx nodedeux) in 
  let p2y = float_of_string (gety nodedeux) in 

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
  |Noeud(x,y,z,t)::q -> let size =  getvalue "SIZE:" "30" (python_split ' ' t)  in 
                        let color = getvalue "COLOR:" "black" (python_split ' ' t) in 
                        let label = getvalue "LABEL:" x (python_split ' ' t) in 
                        let bgcolor = getvalue "BGCOLOR:" "none" (python_split ' ' t) in 
                        nodefile q ( ( "<circle cx=\"" ^y^ "\" cy=\"" ^ z ^ "\" r=\""^size^" \" stroke=\"" ^ color ^ "\" stroke-width=\"2\" fill=\""^bgcolor^"\" > </circle> \n")  ^ 
                        ("<text x=\"" ^y^ "\" y=\"" ^ z ^ "\" dominant-baseline=\"middle \" fill=\"" ^ color ^"\" > "^ label ^" </text> \n") ^ monstr)
  | _ -> ""^monstr;;
      
let rec transitionfile noeud transi monstr  = 
      match transi with 
      |[] -> monstr;
      |Edge(x,y,z,t)::q when (contains t "PATH" = false)-> let info =  calcularc x y  noeud  z in  
                            transitionfile noeud  q  ( info ^ monstr)
      
      |Edge(x,y,z,t)::q  -> let info = getvalue "PATH:" "30" (python_split ' ' t)  in
                            let infosur =  (String.concat " " (String.split_on_char '_' info)) in 
                            let node = getnode x y noeud in
                            let nodeun = List.hd node in
                            let nodedeux = List.hd (List.rev node) in 
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
                            let fill = "<path fill=\"none\"  d=\"" ^ infosur ^ "\" stroke=\"black\"></path> \n " in 
                            let label = "<text x=\"" ^ string_of_int ( int_of_float c1x )  ^ "\" y=\"" ^ string_of_int ( int_of_float c1y )  ^ "\" fill=\"black\" text-anchor=\"middle\"> " ^z^ " </text> \n " in 
                            transitionfile noeud  q  (  (fill ^label) ^ monstr)
      | _ -> ""^monstr;;

let createfile name liste secondl =
  let fic2 = open_out (name^".svg") in
  let mystrfinal=" <svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"800\" height=\"600\" viewBox=\"0 0 800 600\"> \n"^(nodefile liste "") ^ (transitionfile liste secondl "") ^  "</svg>" in
  
  output_string fic2 mystrfinal;
  close_out fic2;;

(* Remove *)

let rec containsele x l = match l with
  | [] -> false
  | Noeud(a,b,c,d)::q when a=x -> true
  | t::q -> containsele x q ;;

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

let removetransitionafternode e l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | Edge(a,b,c,d)::xs when (createid e = a) || (createid e = b) -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l [];;

(* remove pour transition *)
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

(* MOVE *)

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

let rec containscreateid a l = 
      match l with
    |[] -> false
    | x::xs when (createid x = a) -> true
    | _::xs -> containscreateid a xs;;

let movelistid  id numun numdeux l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | Noeud(a,b,c,d)::xs when (containscreateid a id ) -> go xs (Noeud(a,numun,numdeux,d) :: acc)
    | x::xs -> go xs  (x::acc)

  in go l [];; 

(*RENAME*)
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

(*edit *)
  let editt  idun iddeux attributd l =
    let rec go l acc = match l with
      | [] -> List.rev acc
      | Edge(a,b,c,d)::xs when (a= createid idun ) && (b=createid iddeux) ->  let color = getvalue "COLOR:" "" (python_split ' ' d) in 
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
      | Noeud(a,b,c,d)::xs -> go xs acc
      | x::xs -> go xs (x::xs)

    in go l [];;

                                                            

  let editn  idun attributd l =
    let rec go l acc = match l with
      | [] -> List.rev acc
      | Noeud(a,b,c,d)::xs when (a= createid idun ) ->                       let x = b in 
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
      | Edge(a,b,c,d)::xs -> go xs acc
      | x::xs -> go xs (x::xs)

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

(*Complete le truc *)
 
let gettransicreeaux a identifiant listtransition listeletter =
  let rec go listeletter acc = 
    match listeletter  with
    | [] -> List.rev acc
    | x::q when (existe a listtransition x = false ) -> (Edge(a,identifiant,x,""))::acc
    | _::q -> go q acc
  in go listeletter [];;

let gettransicree identifiant listtransition listenoeud =
  let rec go listenoeud acc = match listenoeud with
    | [] -> List.rev acc
    | Noeud(a,b,c,d)::xs  -> go xs ( (gettransicreeaux a identifiant listtransition (getlettre listtransition))@acc)
    | _::xs -> go xs acc

  in go listenoeud [];;


let addauto identifiant listtransition listenoeud listlettre =
  let rec go listlettre acc = match listlettre with
    | [] -> List.rev acc
    | x::xs  -> go xs ((Edge(identifiant,identifiant,x,"")) :: acc)

  in go listlettre [];;
  
let transiajouter identifiant listenoeud listtransition =
  let ajout = gettransicree  identifiant listtransition listenoeud in 
  let aussiajout= addauto identifiant listtransition listenoeud (getlettre listtransition) in
  (aussiajout@listtransition)@ajout;;

let complete identifiant listenoeud listtransition =
  if is_complete listenoeud listtransition = false then
    transiajouter identifiant listenoeud listtransition
  else listtransition;;

let completeaux identifiant numun numdeux listenoeud listtransition = 
  if is_complete listenoeud listtransition = false then
    ( add (Noeud(identifiant,numun,numdeux,"")) ) @ listenoeud
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