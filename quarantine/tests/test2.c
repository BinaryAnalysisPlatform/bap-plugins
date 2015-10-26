//! c:malloc@00000171

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
