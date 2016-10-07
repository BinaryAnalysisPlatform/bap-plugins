#include <stdlib.h>
#include <stdio.h>

int main (int argc, char * argv[]) {
  int * foo = malloc(4);

  free(foo);

  int number = *foo;

  printf("Use: %d\n", number);
}
