//! :: rule malloc_is_safe is unsatisfied

#include <stdlib.h>

int init(void *p) {
    p = NULL;
}

int main() {
    char *p;
    init(p);
    p = (char *) malloc(42);
    p[0] = '\n';
}
