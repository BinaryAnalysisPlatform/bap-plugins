# Overview

This plugin will classify all functions into three categories:

- RED
- YELLOW
- GREEN

GREEN functions perform all writes to memory only to a statically
known offsets, i.e, a compile time constants. SP is also considered a
constant iff it is only defined with constant in the ENTRY or EXIT
blocks. If it is defined by a non-constant value, like `SP := SP - R0`
it is considered unsafe. If it is defined by a constant value, but
outside of the ENTRY or EXIT blocks, but the overall function is
classified as green, then such function will be classified as YELLOW.

# Input

Should work fine on any input data. The plugin is not architecture
specific and should work on any first tier architecture.


# Output

Currently plugin just outputs all function names marked with the color
keyword, e.g.,

main RED

Lately, we will use `project.annots` field to push this information
forward.

# Implementation Details

The implementation relies on a constant propogation, that is made for
each basic block independently. The only information that is
propogated to the input of the block, is the value of SP and/or BP,
defined in the entry block. Actually, even this is not needed, as it is
a part of future development of this algorithm, when we will address,
locals and structures more accurately.
