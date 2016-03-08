## Overview

This is a micro testing framework, usefull in binary analysis. In short,
you write a program in `C` language, and put expected output in the comments.
The framework will compile the C program, run your passes over it, and verify,
that all expectations are met.

For example, consider the following `C` program:

```c
//!  000.....: p := malloc()
//!  000.....: when c jmp _

#include <stdlib.h>

int init(void *p) {
    p = NULL;
}

int main() {
    char *p;
    init(p);
    p = (char *) malloc(42);
    if (p)
        p[0] = '\0';
}

```

Strings starting with `//!` are expectations. The expectation is a posix regular expression,
so `.` means here match with any character.

## Invoking

The program accepts a list of c-files, where each file constitutes a test. All parameter to
bap are passed via environment variable `TEST_OPTIONS`. A compiler can be controlled by setting the following five variables:

    - TEST_ARCH -- architecture, e.g, arm
    - TEST_ABI  -- used abi, e.g., linux-gnueabi
    - TEST_CC   -- C compiler name, e.g., gcc
    - TEST_VER  -- C compiler version, e.g., 4.7
    - TEST_OPT  -- compiler options, e.g., -O3


For example, to check saluki we can use:

    TEST_OPTIONS='--saluki-seed --qurantine --saluki-solve' bap-test-expect saluki/tests/*


To get a more verbose output, with explanations on what and why didn't match, set
`VERBOSE` environment variable to `1`.


## Meeting expectations

Each expectation, specified in the input file, should be met at least once. A line
in the program output can satisfy only one expectation. Formally, this is the Maximum
Bipartile Matching Problem. If solution to the problem is not found, then it is marked
as a test failure, and missing expectations are optionally printed.


## Building
