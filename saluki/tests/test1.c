//!  00000...: p := malloc()
//!  unproved: when c jmp _
//!  Coverage: .* 100%


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
