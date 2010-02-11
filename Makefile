OBJECTS= \
	cps.cmi cps.cmo \
	anf.cmi anf.cmo \
	anf0.cmi anf0.cmo \
	parser.cmi parser.cmo \
	lexer.cmi lexer.cmo

all: $(OBJECTS)

parser.mli parser.ml: parser.mly
	ocamlyacc $<

lexer.ml: lexer.mll
	ocamllex $<

%.cmi: %.mli
	ocamlc -c -o $@ $<

%.cmo: %.ml %.mli
	ocamlc -c -o $@ $<

clean:
	rm -f *.cmi *.cmo
	rm -f lexer.ml parser.ml parser.mli

.PHONY: clean
