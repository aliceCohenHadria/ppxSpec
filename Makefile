all: 
	ocamlbuild -use-ocamlfind  ppxSpec.native
	ocamlbuild -use-ocamlfind -cflags '-ppx ./ppxSpec.native' testSpecPar.native
clean:
	-ocamlbuild -clean
	-rm -rf *.cm* test
