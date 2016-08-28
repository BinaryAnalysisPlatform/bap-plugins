all :
	bapbuild -no-hygiene main.plugin -pkg cmdliner -pkg bap-microx -pkg bap-arm -I debugger

test:
	ocaml test.ml

clean:
		bapbuild -clean
		rm *.plugin
