//!  ........: p := malloc()
//!  unproved: when c jmp .
//!  Coverage: .* 100%

#include <stdlib.h>

int main() {
    malloc(0);
}
