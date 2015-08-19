# Constant Propagation

This plugin performs global constant propagation.  Plugin works on IR
level. Currently the input should be in non SSA-form, but the output
will be in SSA.

Algorithm relies on SSA form, to perform a propagation. But, before
doing the propagation, we insert definitions that will clobber
registers and memory after calls or interrupts.

To propagate constants through memory we represent memory as a graph
of memory states with edges annotated by definitions. Each memory state
represents a static state of memory in a particular point of program.
We represent program points with a variable, representing a memory.

Any write to a non-constant address clobbers all writable addresses.
Whenever control flow is undecided (calls or interrupts) we explicitly
clobber all general purpose registers and writable memory. We do not
rely on calling conventions, since they are usually not followed by
compilers.

Plugin will also dereference memory access that points to statically
known read-only memory regions.

# Usage

```sh
$ make
$ bap -lfold_consts /bin/true
```

See run with `--fold-consts-help` to get more documentation.
