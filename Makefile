all:
	ocamlc -verbose -w +A-4 bool.ml nat.ml int.ml rat.ml

clean:
	rm -f *.cmi *.cmo