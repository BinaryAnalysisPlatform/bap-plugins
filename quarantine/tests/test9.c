//!rule malloc_maybe_checked_if_some_jmp_depends ::=
//!  : p := malloc()
//!  --------------------------------
//!  unproved: when c jmp x
//!
//!   :: rule malloc_maybe_checked is unsatisfied
//!rule malloc_maybe_checked_if_some_jmp_depends ::=
//!  : p := malloc()
//!  --------------------------------
//!  : when c jmp x
//!
//!
//!rule malloc_maybe_checked_if_some_jmp_depends ::=
//!  : p := malloc()
//!  --------------------------------
//!  : when c jmp x
//!
#include <stdio.h>
#include <stdlib.h>

void check() {
    printf("There is nothing to check\n");
}

int main() {
    check();
    check();
    check();
    printf("let's get ready for ruuuuumble\n");
    printf("first malloc\n");
    if (malloc(1)) {
        printf("second malloc\n");
        if (malloc(2)) {
            printf("third malloc \n");
            malloc(3);
        }
    }
}
