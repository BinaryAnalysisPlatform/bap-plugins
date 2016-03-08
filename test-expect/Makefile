install : build
		ocaml setup.ml -install

build : setup.data *.ml
		ocaml setup.ml -build


setup.data : _oasis
		oasis setup
		ocaml setup.ml -configure --prefix `opam config var prefix`
