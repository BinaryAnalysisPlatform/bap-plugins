.phony: all
all :
	bapbuild minos.plugin -pkg FrontC -I checks -pkg bap-x86-cpu -pkg cmdliner -pkg bap-arm
	bapbundle install minos.plugin

test:
	ocaml test.ml
	rm *.dot
	rm *.py

clean :
	bapbuild -clean
	rm *.plugin
