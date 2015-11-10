plugin_options = -L../header -lheader --header-file=../header/libc.h

quarantine : *.ml
		bapbuild quarantine.plugin

test : quarantine test.ml
		@TEST_USER_OPTIONS="${plugin_options}" bapbuild test.native -- quarantine tests/*

clean:
		bapbuild -clean
