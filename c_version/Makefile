all: run

run: lexer.cmo parser.cmo csub.cmo main.cmo
	ocamlc -o run lexer.cmo parser.cmo csub.cmo main.cmo

csub.cmo : csub.ml
	ocamlc -c csub.ml

parser.ml: parser.mly csub.cmo
	ocamlyacc -v parser.mly

parser.mli: parser.mly
	ocamlyacc -v parser.mly

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -c parser.ml

main.cmo : csub.cmo main.ml
	ocamlc -c main.ml

lexer.cmo: lexer.ml
	ocamlc -c lexer.ml

test.cmo: test.ml
	ocamlc -c test.ml

lexer.ml: lexer.mll parser.cmo
	ocamllex lexer.mll

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo
