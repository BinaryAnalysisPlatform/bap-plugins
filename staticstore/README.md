# Overview

This plugin will classify all functions into three categories:

- red
- yellow
- green

`green` functions perform all writes to memory only to a statically
known offsets, i.e, a compile time constants. SP is also considered a
constant iff it is only defined with constant in the ENTRY or EXIT
blocks. If it is defined by a non-constant value, like `SP := SP - R0`
it is considered unsafe. If it is defined by a constant value, but
outside of the ENTRY or EXIT blocks, but the overall function is
classified as green, then such function will be classified as
`yellow`.

# Input

Should work fine on any input data. The plugin is not architecture
specific and should work on any first tier architecture.


# Output

The core plugin tags memory with appropriate color. The tagname is
`staticstore`. The core plugin is accompanied with a helper plugins,
that will dump the results of annotations in different format:

- green: will only print green functions
- yellow: will only print yellow functions
- red: your guess
- print_all: will print all functions in a format: `name color`,
  where color is one of `yellow | red | green`
- printstats: will print statistics
- toida: will transform annotations to IDA annotations, use `--emit-ida-script`
  to get the results

# Compilation
```sh
$ bapbuild -pkg cmdliner staticstore.plugin
```

# Example
```sh
  bap-objdump --use-ida -L ~/bap-plugins/staticstore -lstaticstore -ltoida --emit-ida-script=color.py 1241.exe
```

The resulting script can be loaded into IDA PRO with `Alt-F7`.

# Implementation Details

The implementation relies on a constant propogation, that is made for
each basic block independently. The only information that is
propogated to the input of the block, is the value of SP and/or BP,
defined in the entry block. Actually, even this is not needed, as it is
a part of future development of this algorithm, when we will address,
locals and structures more accurately.
