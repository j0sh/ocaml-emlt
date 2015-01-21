all:
	menhir parser.mly
	ocamllex lexer.mll
	ocamlc -g -o main types.ml parser.mli parser.ml lexer.ml main.ml

clean:
	rm -f a.out main parser.ml parser.mli lexer.ml *.o *.cm[ixao]
