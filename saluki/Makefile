OPTS = --symbolizer=ida ${options} --saluki --saluki-print-models
case = *
TEST = tests/test${case}.c

all : build install
build : saluki

saluki : *.ml
		bapbuild -tag 'ppx(ppx-bap)' -package cmdliner saluki.plugin

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
