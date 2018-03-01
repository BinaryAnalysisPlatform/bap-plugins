ifeq (is_${TAINT_ENGINE}, is_primus)
OPTS = ${options} \
	--no-byteweight --saluki-print-models --propagate-taint-print-coverage \
	--passes=trivial-condition-form,saluki-taint,run,saluki-solve \
	--primus-propagate-taint-from-attr --primus-propagate-taint-to-attr \
	--primus-promiscuous-mode --primus-greedy-scheduler \
	--primus-limit-max-visited=64 --primus-limit-max-length=4096
else
OPTS = ${options} --no-byteweight --saluki --saluki-print-models --propagate-taint-print-coverage
endif

case = *
TEST = tests/test${case}.c

all : build install
build : saluki

saluki : *.ml
		bapbuild -package cmdliner saluki.plugin

install: saluki
		bapbundle install saluki.plugin

test-expect:
		make -C ../test-expect

test : all test-expect
		TEST_OPTIONS="${OPTS}" bap-test-expect $(TEST)


bap: all
		bap ${binary} ${OPTS}

clean:
	bapbuild -clean
