all:
	ocamlc -c types.ml
	menhir --infer parser.mly
	ocamllex lexer.mll
	ocamlc -I +unix unix.cma -g -o emlt types.ml parser.mli parser.ml lexer.ml emlt.ml

clean:
	rm -f emlt parser.ml parser.mli lexer.ml *.o *.cm[ixao]
