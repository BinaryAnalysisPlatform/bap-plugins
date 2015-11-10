OPTS = -L../header -lheader --header-file=../header/libc.h ${options}
case = *
TEST = tests/test${case}.c


quarantine : *.ml
		bapbuild quarantine.plugin

header : ../header/*.ml
		make -C ../header

test : quarantine header test.ml
		@TEST_USER_OPTIONS="${OPTS}" bapbuild test.native -- quarantine $(TEST)

bap: quarantine header
		bap ${OPTS} --use-ida -lquarantine ${binary}

clean:
		bapbuild -clean
		make -C ../header clean
