ocamllex lexer.mll       # generates lexer.ml
ocamlyacc  parser.mly     # generates parser.ml and parser.mli
ocamlc -c functionannexe.ml str.cma
ocamlc -c parser.mli str.cma
ocamlc -c lexer.ml str.cma
ocamlc -c parser.ml str.cma
ocamlc -c main.ml str.cma
ocamlc -o calc lexer.cmo functionannexe.cmo parser.cmo main.cmo
rm *.cmo
rm *.cmi