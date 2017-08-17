OCAMLC=ocamlopt
FLAGS=-O3 -w +A-4-44-48
LIB_MLS=cnt.ml nat.ml int.ml util.ml rat.ml


lib:
	$(OCAMLC) $(FLAGS) $(LIB_MLS)


pi:
	$(OCAMLC) $(FLAGS) $(LIB_MLS) pi.ml -o pi


clean:
	rm -f a.out *.cmi *.cmo *.cmx *.o *.dump pi test


test:
	$(OCAMLC) $(FLAGS) $(LIB_MLS) test.ml -o test


dopi:
	make clean && make pi && ./pi


dotest:
	make clean && make test && ./test
