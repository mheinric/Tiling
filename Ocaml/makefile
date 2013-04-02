all :
	ocamllex get_matrix.mll
	ocamlc -c get_matrix.ml
	ocamlc -c -thread unix.cma threads.cma recuit.ml
	ocamlc -o recuit.exe -thread unix.cma threads.cma graphics.cma get_matrix.cmo recuit.cmo 
