build:
	dune clean
	dune build

utop:
	OCAMLRUNPARAM=b dune utop lib

clean:
	dune clean

test:
	OCAMLRUNPARAM=b dune exec test/main.exe
	
game:
	OCAMLRUNPARAM=b dune exec main/main.exe