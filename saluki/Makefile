OPTS = --symbolizer=ida ${options} --saluki-seed --quarantine --saluki-solve
case = *
TEST = tests/test${case}.c

plugins : saluki quarantine

saluki : *.ml
		bapbuild saluki.plugin
		bapbundle install saluki.plugin

quarantine:
		make -C ../quarantine

test-expect:
		make -C ../test-expect

test : plugins test-expect
		@TEST_OPTIONS="${OPTS}" bap-test-expect $(TEST)


bap: plugins
		bap ${binary} ${OPTS}

clean:
	bapbuild -clean
	make -C ../quarantine clean
