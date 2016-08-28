/*
 In this test, we hide the explicit mallocs in a separate library, which we won't
 expose to BAP. We will manually free the entries however, like in gnu-nettool.
 */

/* Also: bug in conqueror on return values */

#include <stdlib.h>
#include <stdio.h>
#include "vector.h"

void free_vector (struct Vector *vector) {
  if(vector != NULL) {
    free(vector->data);
    free(vector);
  }
}

int main(void) {
  struct Vector *v = malloc_vector(sizeof(struct Vector));
  free_vector(v);

  printf("%f\n", *v->data);
}
