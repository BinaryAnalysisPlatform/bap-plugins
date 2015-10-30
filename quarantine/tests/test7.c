//! c:malloc@00000165
//! u:malloc@00000181


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
