#include <unistd.h>

extern char **environ;

static char* charpp_test[4] = { "first", "segundo", "troisieme", NULL };

char** return_charpp() {
  return charpp_test;
}
