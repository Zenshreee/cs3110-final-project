.PHONY: test

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

cloc:	
	dune clean
	cloc --by-file --include-lang=OCaml .
	dune build
	
game:
	OCAMLRUNPARAM=b dune exec main/main.exe

zip:
	rm -f chess.zip
	zip -r chess.zip . -x@exclude.lst

clean:
	dune clean

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh