#include <stdio.h>
#include <stdarg.h>

int main(int argc, char **argv) {
  const char * format = "Subbing: %s";
  char buffer[256];
  va_list args;
  va_start (args, format);
  vsprintf(buffer, format, args);
  perror(buffer);
  va_end(args);
}
