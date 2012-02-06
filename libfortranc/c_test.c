#include <unistd.h>

extern char **environ;

static char *c_ptr_ptr_test[4] = { "first", "segundo", "troisieme", NULL };
static char *charp_empty = "", *charp_8="8risotto";

char* return_null_charp() {
  return NULL;
}

char* return_empty_charp() {
  return charp_empty;
}

char* return_8_charp() {
  return charp_8;
}

char** return_c_ptr_ptr() {
  return c_ptr_ptr_test;
}

