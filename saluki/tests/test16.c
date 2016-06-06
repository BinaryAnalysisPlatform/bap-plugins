#include <stdlib.h>

//!  00000...: p := malloc()
//!  00000...: when c jmp _
//!  Coverage: .* 100%


int init(void *p) {
    p = NULL;
}

int main(int argc, const char* argv[]) {
    char *p;
    init(p);
    p = (char *) malloc(42);
    if (argc > 0) {
        if (!p) {
            return 1;
        }
    } else {
            return 2;
    }

    p[0] = '\0';
}
