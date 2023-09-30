build:
	dune clean
	dune build

utop:
	OCAMLRUNPARAM=b dune utop lib

clean:
	dune clean

test:
	OCAMLRUNPARAM=b dune exec src/test.exe