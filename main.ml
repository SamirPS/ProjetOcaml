(* File calc.ml *)

let numberofarg =  Array.length (Sys.argv) - 1  ;;

let python_split x =
  String.split_on_char  ';' x
  |> List.filter (fun x -> x <> "")
;;

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
    try
      while true; do
    lines :=  !lines @ python_split (input_line chan) 
      done; []
    with End_of_file ->
      close_in chan;
     !lines ;;

let rec print_numbers oc = function 
  | [] -> ()
  | e::tl -> Printf.fprintf oc "%s \n" e; print_numbers oc tl


let oc = ref stdin;;

if numberofarg >= 1 then 
   let mylines = read_file Sys.argv.(1) in 
   let myfile = open_out "sortie.txt" in 
   print_numbers myfile mylines;
   close_out myfile;
  
   oc := open_in  "sortie.txt" ;;
   

try

  let lexbuf = Lexing.from_channel !oc  in
  while true do
    
    let _ = Parser.main Lexer.token lexbuf in
        flush stdout

  done
with Lexer.Eof ->

  if numberofarg >= 1 then 
    close_in !oc ;
    Sys.remove "sortie.txt";;
