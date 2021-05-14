/* File parser.mly */
      %{
        
        open Functionannexe;;


        let nodelist = ref [] ;;
        let transition = ref [];;

        

      %}


        %token <string> ID
        %token <string> NUM
        %token <string> LABELN
        %token <string> LIST

        %token SHOW ISRECONNU ISDETERMINISTIC COMPLETE ISCOMPLETE CREATENODE CREATEFROM AT LABEL COLOR SIZE TO INITIAL FINAL BGCOLOR DUMP REMOVE REMOVEEDGE MOVE RENAME WITH EDIT EDITEDGE PATH 
        %token EOL N S E O NW NE SW SE
        
                
        
        %start main             /* the entry point */
        %type <unit > main
        %type <unit> noeud

        %%

        main:
            |noeud   EOL                {    }
            |display EOL             {  }
            |lasf EOL             { }
            
        ;
        
        display:

            | DUMP               { printlist (!nodelist @ !transition) }
            | DUMP vrailabel    { createfile  $2  !nodelist !transition}

        ;

        lasf : 
        | ISCOMPLETE {Printf.printf "%b\n " (is_complete !nodelist !transition)}
        | COMPLETE WITH labelnoeud AT numero numero {nodelist := completeaux (createid $3) $5 $6 !nodelist !transition;transition:= complete (createid $3) !nodelist !transition;}
        | ISDETERMINISTIC {Printf.printf "%b\n " (is_deterministic !nodelist !transition)}
        | ISRECONNU vrailabel {Printf.printf "%b\n " (is_accepted !nodelist !transition $2)}
        | SHOW vrailabel {Printf.printf "%s\n " (getchemin !nodelist !transition $2)}
        numero:
            NUM {$1}
          ;

        labelnoeud:
           ID {$1}
          
          ;

        vrailabel:
         LABELN {String.sub $1 1 ((String.length $1) -2)  } ;

        glist :
         LIST { python_split ',' (String.sub $1 1 ((String.length $1) -2)) } ;

        attribut:
          | LABEL vrailabel   {" LABEL: " ^ $2 }
          | COLOR vrailabel  {" COLOR: " ^ $2 }
          | BGCOLOR vrailabel  {" BGCOLOR: " ^ $2 }
          | INITIAL direction {" INITIAL: " ^ $2 }
          | FINAL direction {" FINAL: " ^ $2 }
          | SIZE numero  {" SIZE: " ^ $2}

          | LABEL vrailabel attribut  {" LABEL: " ^ $2 ^ $3 }
          | COLOR vrailabel  attribut {" COLOR: " ^ $2 ^ $3 }
          | SIZE numero attribut  {" SIZE: " ^ $2 ^ $3 }
          | BGCOLOR vrailabel attribut  {" BGCOLOR: " ^ $2 ^ $3 }
          | INITIAL direction attribut { " INITIAL: " ^ $2 ^ $3 }
          | FINAL direction  attribut { " FINAL: " ^ $2 ^ $3 }
          
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

          | LABEL vrailabel attributen  {" LABEL: " ^ $2 ^ $3}
          | COLOR vrailabel attributen {" COLOR: " ^ $2 ^ $3 }
          | BGCOLOR vrailabel attributen {" BGCOLOR: " ^ $2 ^ $3 }
          | INITIAL direction attributen{" INITIAL: " ^ $2 ^ $3 }
          | FINAL direction attributen{" FINAL: " ^ $2 ^ $3 }
          | SIZE numero  attributen {" SIZE: " ^ $2 ^ $3}
          | AT numero numero attributen {" X: " ^ $2 ^ " Y: " ^ $3 ^ $4}

          
        ;
        
        noeud:

          | CREATENODE ID  AT numero numero attribut { nodelist := ( add (Noeud(createid $2, ( $4), ( $5),$6)) !nodelist !transition) @ !nodelist }
          | CREATENODE ID   attribut AT numero numero { nodelist := ( add (Noeud(createid $2, ( $5), ( $6),$3)) !nodelist !transition) @ !nodelist }
          | CREATENODE ID   attribut AT numero numero  attribut { nodelist := ( add (Noeud( createid $2, ( $5), ( $6),$3 ^ (" "  ^ $7))) !nodelist !transition) @ !nodelist }
          | CREATENODE ID   AT numero numero {  nodelist := ( add (Noeud(createid $2, ( $4), ( $5),"")) !nodelist !transition) @ !nodelist}

          
          | CREATEFROM ID TO ID  LABEL vrailabel { transition := (add (Edge(createid $2,createid $4,$6,"")) !nodelist !transition )@ !transition }
          | CREATEFROM ID TO ID  LABEL vrailabel attributf { transition := (add (Edge(createid $2,createid $4,$6,$7)) !nodelist !transition) @ !transition }
          | CREATEFROM ID TO ID   attributf LABEL vrailabel { transition := ( add (Edge(createid $2,createid $4,$7,$5)) !nodelist !transition)  @ !transition }
          | CREATEFROM ID TO ID   attributf LABEL vrailabel  attributf { transition := (add (Edge(createid $2,createid $4,$7,$5 ^ (" "^ $8))) !nodelist !transition)  @ !transition }

          | EDITEDGE ID TO ID LABEL vrailabel WITH attributet { transition := editt (createid $2) ( createid $4) $6 $8  !transition }
          | EDIT ID WITH attributen {nodelist := editn (createid $2) $4 !nodelist }

          | REMOVE ID { nodelist := removenoeud (createid $2) !nodelist ; transition:= removetransitionafternode (createid $2) !transition;  }
          | REMOVEEDGE ID TO ID WITH vrailabel {  transition := removetransition (createid $2) (createid $4) $6 !transition }
          
          |  MOVE numero numero {nodelist := moveall $2 $3 !nodelist}
          |  MOVE ID numero numero {nodelist := moveallid_aux (createid $2) $3 $4 !nodelist}
          |  MOVE glist numero numero {nodelist := movelistid $2 $3 $4 !nodelist}

          | RENAME ID WITH ID {nodelist:= renamen (createid $2) (createid $4) !nodelist ; transition:= renamet (createid $2) (createid $4) !transition ;}
          
        ;
