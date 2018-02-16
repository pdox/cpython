#include "Python.h"

#ifdef Py_DEBUG
void _jit_macros_assert(int expr, const char *expr_str) {
    if (!expr) {
        Py_FatalError(expr_str);
    }
}
#endif

void _jit_macros_decref_helper(PyObject *obj) {
    Py_DECREF(obj);
}

void _jit_macros_xdecref_helper(PyObject *obj) {
    Py_XDECREF(obj);
}
