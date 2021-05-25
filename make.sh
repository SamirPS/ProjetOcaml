ocamllex lexer.mll       # generates lexer.ml
ocamlyacc  parser.mly     # generates parser.ml and parser.mli
ocamlc -c -g functionannexe.ml str.cma
ocamlc -c  -g parser.mli str.cma
ocamlc -c -g lexer.ml str.cma
ocamlc -c -g parser.ml str.cma
ocamlc -c -g main.ml str.cma
ocamlc -g -o rendu lexer.cmo functionannexe.cmo parser.cmo main.cmo
rm *.cmo
rm *.cmi
