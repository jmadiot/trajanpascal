all:compiler


lexer.ml: parser.ml
	ocamllex lexer.mll -o lexer.ml

parser.ml:
	ocamlyacc parser.mly -o parser.ml

compiler:parser.ml
	#?

