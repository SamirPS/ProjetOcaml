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

      %}


        %token <string> ID
        %token <string> NUM
        %token <string> LABELN

        %token  CREATENODE CREATEFROM AT LABEL COLOR SIZE TO INITIAL FINAL BGCOLOR DUMP REMOVE REMOVEEDGE MOVE RENAME WITH EDIT EDITEDGE PATH
        %token EOL
                
        
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
            | DUMP ID               { createfile  $2  !nodelist !transition}
        ;
        numero:
            NUM {$1}
          ;

        labelnoeud:
          ID {$1}
          |ID AT numero numero {" POSITION: " ^ $3^":"^ $4}
          ;


        attribut:
          | LABEL labelnoeud   {" LABEL: " ^ $2 }
          | COLOR labelnoeud  {" COLOR: " ^ $2 }
          | BGCOLOR labelnoeud  {" BGCOLOR: " ^ $2 }
          | INITIAL labelnoeud {" INITIAL: " ^ $2 }
          | FINAL labelnoeud {" FINAL: " ^ $2 }
          | SIZE numero  {" SIZE: " ^ $2}
          | LABEL labelnoeud attribut  {" LABEL: " ^ $2 ^ $3 }
          | COLOR labelnoeud  attribut {" COLOR: " ^ $2 ^ $3 }
          | SIZE numero attribut  {" BGCOLOR: " ^ $2 ^ $3 }
          | BGCOLOR labelnoeud attribut  {" INITIAL: " ^ $2 ^ $3 }
          | INITIAL labelnoeud attribut { " FINAL: " ^ $2 ^ $3 }
          | FINAL labelnoeud  attribut { " SIZE: " ^ $2 ^ $3 }
          
        ;

        attributf:
          | COLOR labelnoeud  {" COLOR: " ^ $2 }
          | BGCOLOR labelnoeud   { " BGCOLOR: " ^ $2 }
          | PATH labelnoeud {" PATH: " ^ $2}
          | COLOR labelnoeud  attributf {" COLOR: " ^ $2 ^ $3 }      
          | BGCOLOR labelnoeud attributf  {" BGCOLOR: " ^ $2 ^ $3 }   
          | PATH labelnoeud attributf {" PATH: " ^ $2 ^ $3}
        ;
        
        noeud:

          | CREATENODE ID  AT numero numero attribut { nodelist := add (Noeud($2, ( $4), ( $5),$6)) @ !nodelist }
          | CREATENODE ID   attribut AT numero numero { nodelist := add (Noeud($2, ( $5), ( $6),$3)) @ !nodelist }
          | CREATENODE ID   attribut AT numero numero  attribut { nodelist := add (Noeud($2, ( $5), ( $6),$3 ^ (" "  ^ $7))) @ !nodelist }
          | CREATENODE ID   AT numero numero {  nodelist := add (Noeud($2, ( $4), ( $5),"")) @ !nodelist}


          | EDIT ID  WITH  AT numero numero attribut { nodelist := editn  (Noeud($2, ( $5), ( $6),$7))  !nodelist }
          | EDIT ID  WITH attribut AT numero numero { nodelist := editn (Noeud($2, ( $6), ( $7),$4))  !nodelist }
          | EDIT ID  WITH attribut AT numero numero  attribut { nodelist := editn (Noeud($2, ( $6), ( $7),$4 ^ (" "  ^ $8)))  !nodelist }
          | EDIT ID  WITH AT numero numero {  nodelist := editn (Noeud($2, ( $5), ( $6),""))  !nodelist}
          
          | CREATEFROM ID TO ID  LABEL labelnoeud { transition := add (Edge($2,$4,$6,"")) @ !transition }
          | CREATEFROM ID TO ID  LABEL labelnoeud attributf { transition := add (Edge($2,$4,$6,$7)) @ !transition }
          | CREATEFROM ID TO ID   attributf LABEL labelnoeud { transition := add (Edge($2,$4,$7,$5))  @ !transition }
          | CREATEFROM ID TO ID   attributf LABEL labelnoeud  attributf { transition := add (Edge($2,$4,$7,$5 ^ (" "^ $8)))  @ !transition }

          | EDITEDGE ID TO ID WITH LABEL labelnoeud { transition := editt (Edge($2,$4,$7,""))  !transition }
          | EDITEDGE ID TO ID WITH LABEL labelnoeud  attributf { transition := editt (Edge($2,$4,$7,$8))  !transition }
          | EDITEDGE ID TO ID  WITH attributf LABEL labelnoeud { transition := editt (Edge($2,$4,$8,$6))   !transition }
          | EDITEDGE ID TO ID    WITH attributf LABEL labelnoeud  attributf { transition := editt (Edge($2,$4,$8,$6 ^ (" "^ $9)))   !transition }

          | REMOVE ID { nodelist := removenoeud $2 !nodelist  }
          | REMOVEEDGE ID TO ID {  transition := removetransition $2 $4  !transition }
          
          |  MOVE numero numero {nodelist := moveall $2 $3 !nodelist}
          |  MOVE ID numero numero {nodelist := moveallid $2 $3 $4 !nodelist}

          | RENAME ID TO ID {nodelist:= renamen $2 $4 !nodelist ; transition:= renamet $2 $4 !transition ;}
        
        ;
