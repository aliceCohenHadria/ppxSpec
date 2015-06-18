all: 
	ocamlbuild -use-ocamlfind ppxSpec.native
	ocamlc -o test -ppx ./ppxSpec.native testSpecPar.ml
clean:
	-ocamlbuild -clean
	-rm -rf *.cm* test
