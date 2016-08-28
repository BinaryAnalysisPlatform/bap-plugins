all:
	arm-linux-gnueabi-gcc -o dead-simpl-uaf-arm dead-simpl-uaf.c
	arm-linux-gnueabi-gcc -o dead-simpl-uaf-arm-2 dead-simpl-uaf-2.c
	arm-linux-gnueabi-gcc -o super-simpl-uaf-arm super-simpl-uaf.c
	arm-linux-gnueabi-gcc -o simpl-uaf-arm simpl-uaf.c
	arm-linux-gnueabi-gcc -o simpl-uaf-arm-2 simpl-uaf-2.c
	arm-linux-gnueabi-gcc -o simpl-uaf-arm-bug simpl-uaf-bug.c
	arm-linux-gnueabi-gcc -o uaf-no-assign-arm uaf-no-assign.c
	arm-linux-gnueabi-gcc -o gueb-example-uaf-arm-O0 gueb-example-uaf.c
	arm-linux-gnueabi-gcc -o gueb-example-ite-uaf-arm-O0 gueb-example-ite-uaf.c
	arm-linux-gnueabi-gcc -o gueb-full-arm-O0 gueb-full-uaf.c
	arm-linux-gnueabi-gcc -o gueb-full-arm-O0-buggy gueb-full-uaf-buggy.c
	arm-linux-gnueabi-gcc -o structs-arm structs.c

blah:
	arm-linux-gnueabi-gcc -c vector.c
	arm-linux-gnueabi-gcc -shared -o libvector.so vector.o
	arm-linux-gnueabi-gcc -L/home/vagrant/rvantonder-bap-plugins/uaf/tests/all/ -o structs-arm-2 structs-2.c -lvector

clean:
	rm dead-simpl-uaf-arm dead-simpl-uaf-arm-2 super-simpl-uaf-arm simpl-uaf-arm gueb-example-uaf-arm-O0 gueb-example-ite-uaf-arm-O0 gueb-full-arm-O0 gueb-full-arm-O0-buggy uaf-no-assign-arm simpl-uaf-arm-2 simpl-uaf-arm-bug structs-arm
