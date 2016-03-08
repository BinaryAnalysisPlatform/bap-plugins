//!  000.....: p := malloc()
//!  000.....: when c jmp _
#include <stdlib.h>

int main(int argc, const char* argv[]) {
    char *ptr = NULL;
    int i;
    for (i; i < argc + 1; i++) {
        if (argv[i][0] == '1') {
            ptr = malloc(1);
        } else {
            if (i == 0) {
                ptr = malloc(42);
            } else {
                ptr = malloc(56);
            }
        }
    }
    if (ptr)
        free(ptr);
}
