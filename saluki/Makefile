OPTS = --symbolizer=ida ${options} --saluki-seed --propagate-taint --saluki-solve
case = *
TEST = tests/test${case}.c

all : build install
build : saluki

saluki : *.ml
		bapbuild saluki.plugin

install: saluki
		bapbundle install saluki.plugin

test-expect:
		make -C ../test-expect

test : all test-expect
		@TEST_OPTIONS="${OPTS}" bap-test-expect $(TEST)


bap: saluki
		bap ${binary} ${OPTS}

clean:
	bapbuild -clean
