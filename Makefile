.PHONY: test

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop lib

clean:
	dune clean

cloc:	
	dune clean
	cloc --by-file --include-lang=OCaml .
	dune build
	
test:
	OCAMLRUNPARAM=b dune exec test/main.exe
	
game:
	OCAMLRUNPARAM=b dune exec main/main.exe

zip:
	rm -f chess.zip
	zip -r chess.zip . -x@exclude.lst