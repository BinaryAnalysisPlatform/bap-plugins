//!  ........: p := malloc()
//!  unproved: when c jmp .
//!  Coverage: .* 100%

#include <stdlib.h>
#include <stdio.h>

int print_result(FILE *output, const char *msg) {
    fputs(msg,output);
}

int main() {
    malloc(0);
    print_result(stderr, "hello, world\n");
}
