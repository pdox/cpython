#include "Python.h"

#include <stdio.h>
#include <stdlib.h>
#include "Jit/macros.h"

void _always_assert_fail(const char *expr, const char *file, long line) {
    fprintf(stderr, "assert failed [%s:%ld]: %s\n", file, line, expr);
    abort();
}

void PyJIT_FatalError(const char *msg) {
    Py_FatalError(msg);
}
