CAMLC=ocamlc -g
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc

all: types.cmi parser.cmi types.cmo parser.cmo lexer.cmo compiler.cmo
	ocamlc -o compiler lexer.cmo parser.cmo compiler.cmo types.cmo

clean:
	rm -f *.cmo *.cmi
	rm -f lexer.ml lexer.mli
	rm -f parser.ml parser.mli

.SUFFIXES: .mll .mly .mli .ml .cmi .cmo

.mll.mli:
	$(CAMLLEX) $<

.mll.ml:
	$(CAMLLEX) $<

.mly.mli:
	$(CAMLYACC) -v $<

.mly.ml:
	$(CAMLYACC) -v $<

.mli.cmi:
	$(CAMLC) -c $(FLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(FLAGS) $<

.ml.mli:
	$(CAMLC) -i $(FLAGS) $< > $@


