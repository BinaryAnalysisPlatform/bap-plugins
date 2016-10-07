#include <stdlib.h>
#include <stdio.h>

int * p_global;

int main (int argc, char * argv[]) {
  int * p_global = malloc(4);
  *p_global = 100;
  int * p_local = malloc(4);

  // save global
  int * p_ptr_to_global = p_global;
  // overwrite global (aliases p_local)
  p_global = p_local;
  if (atoi(argv[1]) > 10) {
    // do not restore global, if p_local is free'd, a use of p_global
    // will be bad, since p_global aliases p_local
    printf("something\n"); // currently a bug in bap, needs content to create block for microx to follow
  } else {
    // restore global, safe is p_local is free'd
    p_global = p_ptr_to_global;
  }

  free(p_local);

  int use_after_free = *p_global;

  printf("Hi mom: %d\n", use_after_free);
  free(p_global);

}
