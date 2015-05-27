#### Arg finder plugin.

This is a plugin that can be used to infer arguments of functions. This
particular instance checks for arguments to "fortified" functions such as
`__printf_chk`. See `custom_abi_arm.ml` for the complete list. When the verbose
option is specified `--arg_finder-verbose`, it will output whether arguments
(such as constant strings) are in the ELF `.rodata` section.

Use `run.sh` to run the plugin on `bin/coreutils_O1_cat`.

```
run.sh bin/coreutils_O1_cat
```

This also produces a
`out.txt` file containing the addresses of the arguments to fortified
functions. This file can be fed to the `data_dependency` plugin which will
determine data dependencies of the arguments.

As an example, the following was run to check the number of fortified functions
in the coreutils suite which had a constant string as an argument.

```
$ time ls ~/arm-binaries/coreutils/coreutils_O1_* | xargs -L 1 ./run.sh &> fortified.txt

real    3m56.215s
user    3m17.858s

# number of functions
$ grep "_chk" fort.txt | wc -l
963

# number of arguments which are in rodata (there's roughly one per unique function, but some sprintf functions may have two arguments in rodata)
$ grep "rodata" fort.txt  | wc -l
114
```
