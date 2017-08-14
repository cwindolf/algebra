OCAMLC=ocamlopt
FLAGS=-w +A-4-44
MLS=bool.ml cnt.ml nat.ml int.ml util.ml rat.ml
CMXS=util.cmx bool.cmx cnt.cmx nat.cmx int.cmx rat.cmx

lib:
	$(OCAMLC) $(FLAGS) $(MLS)


pi:
	$(OCAMLC) $(FLAGS) $(MLS) pi.ml -o pi


clean:
	rm -f a.out *.cmi *.cmo *.cmx *.o *.dump pi test


test:
	$(OCAMLC) $(FLAGS) $(MLS)
	$(OCAMLC) $(FLAGS) $(CMXS) test.ml -o test
