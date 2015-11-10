//!  00000208: p := malloc()
//!  --------------------------------
//!  unproved: when c jmp x
//!
//!  00000207: p := malloc()
//!  --------------------------------
//!  0000014c: when c jmp x


#include <stdlib.h>

void init(void *p) {
    p = NULL;
}

char *check(void *p) {
    if (!p) exit(1);
    return p;
}

char *c_malloc(size_t size) {
    return check(malloc(size));
}

int main() {
    char *p;
    init(p);
    p = malloc(42);
    p = c_malloc(42);
    p[0] = '\n';
}
