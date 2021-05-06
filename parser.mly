/* File parser.mly */
      %{
        

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

      %}


        %token <string> ID
        %token <string> NUM
        %token <string> LABELN

        %token  CREATENODE CREATEFROM AT LABEL COLOR SIZE TO INITIAL FINAL BGCOLOR DUMP REMOVE REMOVEEDGE MOVE RENAME WITH EDIT EDITEDGE PATH 
        %token EOL N S E O NW NE SW SE
        
                
        
        %start main             /* the entry point */
        %type <unit > main
        %type <unit> noeud

        %%

        main:
            |noeud   EOL                {    }
            |display EOL             {  }
            
        ;
        
        display:

            | DUMP               { printlist (!nodelist @ !transition) }
            | DUMP vrailabel    { createfile  $2  !nodelist !transition}
        ;
        numero:
            NUM {$1}
          ;

        labelnoeud:
           ID {$1}
          
          ;

        vrailabel:
         LABELN {String.sub $1 1 ((String.length $1) -2)  } ;

        attribut:
          | LABEL vrailabel   {" LABEL: " ^ $2 }
          | COLOR vrailabel  {" COLOR: " ^ $2 }
          | BGCOLOR vrailabel  {" BGCOLOR: " ^ $2 }
          | INITIAL direction {" INITIAL: " ^ $2 }
          | FINAL direction {" FINAL: " ^ $2 }
          | SIZE numero  {" SIZE: " ^ $2}

          | LABEL vrailabel attribut  {" LABEL: " ^ $2 ^ $3 }
          | COLOR vrailabel  attribut {" COLOR: " ^ $2 ^ $3 }
          | SIZE numero attribut  {" BGCOLOR: " ^ $2 ^ $3 }
          | BGCOLOR vrailabel attribut  {" INITIAL: " ^ $2 ^ $3 }
          | INITIAL direction attribut { " FINAL: " ^ $2 ^ $3 }
          | FINAL direction  attribut { " SIZE: " ^ $2 ^ $3 }
          
        ;

        direction :
        | N {"Nord"}
        | S {"Sud"}
        | E {"Est"}
        | O {"Ouest"}
        | NW {"Nord West"}
        | NE {"Nord Est"}
        | SW {"Sud Ouest"}
        | SE {"Sud Est"}

        attributf:
          | COLOR vrailabel  {" COLOR: " ^ $2 }
          | PATH vrailabel {" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' $2))}
          | AT numero numero  {" POSITION: " ^ $2^":"^ $3}


          | COLOR vrailabel  attributf {" COLOR: " ^ $2 ^ $3 }      
          | PATH vrailabel attributf {" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' $2)) ^ $3}
          | AT numero numero  attributf {" POSITION: " ^ $2^":"^ $3 ^ $4}
        ;

        attributet:

          | LABEL vrailabel {" LABEL: " ^ $2 }
          | COLOR vrailabel  {" COLOR: " ^ $2 }
          | PATH vrailabel {" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' $2))}
          | LABEL vrailabel AT numero numero {" POSITION: " ^ $4^":"^ $5}

          | COLOR vrailabel  attributet {" COLOR: " ^ $2 ^ $3 }      
          | PATH vrailabel attributet {" PATH: " ^ ( String.concat "_" (String.split_on_char ' ' $2)) ^ $3}
          | LABEL vrailabel attributet {" LABEL: " ^ $2 ^ $3 }
          | LABEL vrailabel AT numero numero  attributet {" POSITION: " ^ $4^":"^ $5 ^ $6}
        ;

        attributen:
          | LABEL vrailabel   {" LABEL: " ^ $2 }
          | COLOR vrailabel  {" COLOR: " ^ $2 }
          | BGCOLOR vrailabel  {" BGCOLOR: " ^ $2 }
          | INITIAL direction {" INITIAL: " ^ $2 }
          | FINAL direction {" FINAL: " ^ $2 }
          | SIZE numero  {" SIZE: " ^ $2}
          | AT numero numero {" X: " ^ $2 ^ " Y: " ^ $3 }

          | LABEL vrailabel attributen  {" LABEL: " ^ $2 ^ $3 }
          | COLOR vrailabel  attributen {" COLOR: " ^ $2 ^ $3 }
          | SIZE numero attributen  {" BGCOLOR: " ^ $2 ^ $3 }
          | BGCOLOR vrailabel attributen  {" INITIAL: " ^ $2 ^ $3 }
          | INITIAL direction attributen { " FINAL: " ^ $2 ^ $3 }
          | FINAL direction  attributen { " SIZE: " ^ $2 ^ $3 }
          | AT numero numero attributen {" X: " ^ $2 ^ " Y: " ^ $3 ^ $4 }
          
        ;
        
        noeud:

          | CREATENODE ID  AT numero numero attribut { nodelist := add (Noeud($2, ( $4), ( $5),$6)) @ !nodelist }
          | CREATENODE ID   attribut AT numero numero { nodelist := add (Noeud($2, ( $5), ( $6),$3)) @ !nodelist }
          | CREATENODE ID   attribut AT numero numero  attribut { nodelist := add (Noeud($2, ( $5), ( $6),$3 ^ (" "  ^ $7))) @ !nodelist }
          | CREATENODE ID   AT numero numero {  nodelist := add (Noeud($2, ( $4), ( $5),"")) @ !nodelist}

          
          | CREATEFROM ID TO ID  LABEL vrailabel { transition := add (Edge($2,$4,$6,"")) @ !transition }
          | CREATEFROM ID TO ID  LABEL vrailabel attributf { transition := add (Edge($2,$4,$6,$7)) @ !transition }
          | CREATEFROM ID TO ID   attributf LABEL vrailabel { transition := add (Edge($2,$4,$7,$5))  @ !transition }
          | CREATEFROM ID TO ID   attributf LABEL vrailabel  attributf { transition := add (Edge($2,$4,$7,$5 ^ (" "^ $8)))  @ !transition }

          | EDITEDGE ID TO ID WITH attributet { transition := editt $2 $4 $6  !transition }
          | EDIT ID WITH attributen {nodelist := editn $2 $4 !nodelist }

          | REMOVE ID { nodelist := removenoeud $2 !nodelist  }
          | REMOVEEDGE ID TO ID {  transition := removetransition $2 $4  !transition }
          
          |  MOVE numero numero {nodelist := moveall $2 $3 !nodelist}
          |  MOVE ID numero numero {nodelist := moveallid $2 $3 $4 !nodelist}

          | RENAME ID TO ID {nodelist:= renamen $2 $4 !nodelist ; transition:= renamet $2 $4 !transition ;}
        
        ;
