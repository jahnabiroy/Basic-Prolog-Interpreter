make:
	ocamlc -c backend.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c main.ml
	ocamlc -o main backend.cmo parser.cmo lexer.cmo main.cmo

clean:
	rm -f lexer.ml parser.ml parser.mli
	rm -f *.c*
	rm -f main
