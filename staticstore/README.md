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

The core plugin tags memory with `color` tag. The core plugin is
accompanied with two helper plugins:
- printstats - will output statistics, i.e., the percentage of green
  and red functions
- toida - will emit appropriate annotations to the project data
  structure, that can be used to produce python script that will color
  functions inside IDA.

# Compilation
```sh
$ make clean
$ make
```

# Example
```sh
  bap --use-ida -L ~/bap-plugins/staticstore -ltoida --emit-ida-script=color.py 1241.exe
```

The resulting script can be loaded into IDA PRO with `Alt-F7` keybinding.

# Implementation Details

The implementation relies on a constant propagation, that is made for
each basic block independently. The only information that is
propagated to the input of the block, is the value of SP and/or BP,
defined in the entry block. Actually, even this is not needed, as it is
a part of future development of this algorithm, when we will address,
locals and structures more accurately.
