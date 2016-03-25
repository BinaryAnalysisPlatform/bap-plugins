#include <stdlib.h>
#include <string.h>
#include <stdio.h>

//! 00......: strcpy\(p\)
//! unproved: system\(q\)
//! Coverage: .* 100%

int max_cmd = 511;

int main(int argc, const char *argv[]) {
    const char *shell = "/bin/sh -c ";
    char buf[max_cmd+1];
    if (argc < 2 || strlen(argv[1]) + strlen(shell) > max_cmd)
        exit(1);
    strcpy(buf, argv[1]);
    return system(argv[1]);
}
