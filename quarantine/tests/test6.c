//! :: rule malloc_is_safe is satisfied
//! :: rule malloc_is_safe is unsatisfied


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
    char *p,*q;
    init(p);
    q = malloc(42);
    p = c_malloc(42);
    p[0] = '\n';
    q[0] = '\n';
}
