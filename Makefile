all: 
	ocamlbuild -use-ocamlfind  ppxSpec.native
	ocamlc -o test -ppx ./ppxSpec.native testUnaryTernary.ml
clean:
	-ocamlbuild -clean
	-rm -rf *.cm* test
