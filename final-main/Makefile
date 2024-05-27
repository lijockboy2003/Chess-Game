.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

doc: 
	dune build @doc
	dune build @doc

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

cloc:
	cloc --by-file --include-lang=OCaml ./src
	cloc --by-file --include-lang=OCaml ./test
	cloc --by-file --include-lang=OCaml ./bin

zip:
	rm -f final.zip
	zip -r final.zip . -x@exclude.lst

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage