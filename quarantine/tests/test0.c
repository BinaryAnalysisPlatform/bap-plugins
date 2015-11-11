//!  ........: p := malloc()
//!  --------------------------------
//!  unproved: when c jmp x
//!
//!   :: rule malloc_maybe_checked is unsatisfied

#include <stdlib.h>

int main() {
    malloc(0);
}
