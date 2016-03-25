#include <stdlib.h>
#include <stdio.h>

int main(int argc, const char *argv[]) {
    char *p;
    int i;
    int n = argc * (argc + 1);
    if (n <= 0 || n > 4096)
        n = 4096;
    
    p = malloc(n);
    if (!p) {
        exit(1);
    }

    for (i = 0; i <= n; i++) {
        putchar(p[i]);
    }
}
