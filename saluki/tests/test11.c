#include <stdlib.h>
#include <stdio.h>

int main(void) {
    int x = getchar();
    char *p = malloc(x);
    puts(p);
}
