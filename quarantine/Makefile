options = -L../header -lheader --header-file=../header/libc.h

quarantine : *.ml
		bapbuild quarantine.plugin

test : quarantine test.ml
		@TEST_USER_OPTIONS="${options}" bapbuild test.native -- quarantine tests/*


article: quarantine
		bap ${options} --use-ida -lquarantine ${article}


clean:
		bapbuild -clean
