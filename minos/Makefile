.phony: all
all :
	bapbuild minos.plugin -pkg bin_prot.syntax -pkg FrontC -I checks

test:
	ocaml test.ml
	rm *.dot
	rm *.py

clean :
	bapbuild -clean
	rm *.plugin
