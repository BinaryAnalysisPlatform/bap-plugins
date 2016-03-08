//!  ........: p := malloc()
//!  unproved: when c jmp .

#include <stdlib.h>

int main() {
    malloc(0);
}
