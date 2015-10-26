//! c:malloc@00000165

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
    p = c_malloc(42);
    p[0] = '\n';
}
