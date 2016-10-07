#include <stdlib.h>
#include <stdio.h>

int main (int argc, char * argv[]) {
  int * foo = malloc(4);
  int * bar = foo;

  free(foo);

  int number = *bar;

  printf("Use: %d\n", number);
}
