#include <stdio.h>
#include <stdlib.h>

int main (int argc , char * argv []) {
  int * p_index ,* p_pass ;

// If I uncomment this, I don't see UAF anymore. Why? It's the return that
// trips it up.

  if (argc !=2) {
     return 0;
  }

  p_index = (int *) malloc (0xff) ;
  *p_index = 100;
  free(p_index) ;
  int val = *p_index;

  return 0;
}
