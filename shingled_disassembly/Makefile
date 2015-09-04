PIQIC=piqic-ocaml
STMT_LOCATION=${CMUBAP}/lib/bap_types
PIQI_INCLUDES=-I ${STMT_LOCATION}
SETUP=ocaml setup.ml -quiet

default: all

lifted_image_piqi.ml: lifted_image.piqi ${STMT_LOCATION}/stmt.piqi
	$(PIQIC) ${STMT_LOCATION}/stmt.piqi
	$(PIQIC) ${PIQI_INCLUDES} lifted_image.piqi
	mv lifted_image_piqi.ml tmp.ml
	echo "open Bap.Std" >> lifted_image_piqi.ml
	echo "module Stmt = Bil_piqi" >> lifted_image_piqi.ml
	tail -n +2 tmp.ml >> lifted_image_piqi.ml
	rm -f tmp.ml

all: lifted_image_piqi.ml $(SOURCES)
	oasis setup -setup-update dynamic
	touch setup.data
	ocaml setup.ml -build -cflag -annot -cflag -bin-annot

install:
	$(SETUP) -install

clean:
	ocaml setup.ml -clean
	rm -f lifted_image_piqi.ml stmt_piqi.ml
