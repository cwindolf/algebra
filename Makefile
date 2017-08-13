MLS=bool.ml cnt.ml nat.ml int.ml util.ml rat.ml
CMOS=util.cmo bool.cmo cnt.cmo nat.cmo int.cmo rat.cmo

lib:
	ocamlc -verbose -w +A-4-44 $(MLS)


pi:
	make lib
	ocamlc -verbose -w +A-4 $(CMOS) pi.ml -o pi


clean:
	rm -f a.out *.cmi *.cmo pi test


test:
	ocamlc -verbose -g -w +A-4-44 $(MLS)
	ocamlc -verbose -g -w +A-4 $(CMOS) test.ml -o test
