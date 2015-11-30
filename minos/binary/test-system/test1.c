#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv) {

  char s[100];

  if (argc > 2) {
    sprintf(s, "WRONG should be hidden branch left %s", argv[1]);
    sprintf(s, "most recent left %s", argv[1]);
  } else {
    sprintf(s, "WRONG should be hidden branch right %s", argv[1]);
    sprintf(s, "most recent right %s", argv[1]);
  }

  system(s);
}

