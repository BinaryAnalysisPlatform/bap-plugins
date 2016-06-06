/* example of sanitization via call (CWE-22) */

//! 00000...: fgets.p,_,_,_.
//! 00000...: fopen.t.
//! unproved: realpath.s,r.

#include <limits.h>
#include <stdlib.h>
#include <stdio.h>

int main(void) {
    int size = PATH_MAX + 1;
    char input[size];
    char fixed[size];

    fgets(input, size, stdin);

    if (!realpath(input,fixed)) {
        exit(1);
    } else {
        FILE *f = fopen(input, "r");
        while (fgets(input,size,f))
            puts(input);
    }
}
