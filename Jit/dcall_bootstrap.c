#include "Python.h"
#include "Jit/dcall_signature.h"
#include "Jit/dcall_bootstrap.h"
#include "Jit/dcall_function.h"
#include "Jit/dcall_cfunction.h"

static void*
setup_object(PyObject *callable, PyJIT_DCall_Signature *css) {
    if (PyFunction_Check(callable)) {
        PyFunctionObject *func = (PyFunctionObject*)callable;
        return PyJIT_DCall_SetupFunction(func, css);
    } else if (PyCFunction_Check(callable)) {
        PyCFunctionObject *cfunc = (PyCFunctionObject*)callable;
        return PyJIT_DCall_SetupCFunction(cfunc, css);
    }
    abort(); /* Unrecognized object */
}

static void *_make_bootstrap_for_arch(void) {
    return PyJIT_DCall_MakeBootstrap_X86_64(setup_object);
}

static void *_bootstrap;

void* PyJIT_DCall_GetBootstrapEntrypoint(void) {
    if (_bootstrap == NULL) {
        _bootstrap = _make_bootstrap_for_arch();
    }
    return _bootstrap;
}
