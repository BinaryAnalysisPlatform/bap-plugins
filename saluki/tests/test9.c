//!  000.....: p := malloc()
//!  unproved: when c jmp .
//!  000.....: p := malloc()
//!  000.....: when c jmp .
//!  000.....: p := malloc()
//!  000.....: when c jmp .
//!  Coverage: .* 100%

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
