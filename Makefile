all:
	menhir parser.mly
	ocamllex lexer.mll
	ocamlc -g -o emlt unix.cma types.ml parser.mli parser.ml lexer.ml emlt.ml

clean:
	rm -f emlt parser.ml parser.mli lexer.ml *.o *.cm[ixao]
