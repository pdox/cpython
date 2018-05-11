#include "Python.h"
#include "Jit/entrypoint.h"
#include "Jit/dcall_signature.h"
#include "Jit/dcall_function.h"

typedef struct {
  void *entrypoint;
} TrampolineInfo;

#define TRAMPOLINE(jf)  ((TrampolineInfo*)(jf)->trampoline)

static void*
_make_trampoline(PyJITFunctionObject *jf) {
    return PyJIT_DCall_MakeTrampolineForFunction_X86_64(jf);
}

void*
PyJIT_DCall_SetupFunction(PyFunctionObject *func, PyJIT_DCall_Signature *css) {
    if (func->func_jit_function == NULL) {
        /* Function has not yet been JIT'd. Do so now. */
        PyObject *hint = PyJIT_Prepare((PyObject*)func);
        Py_XDECREF(hint);
    }
    PyJITFunctionObject *jf = (PyJITFunctionObject*)func->func_jit_function;
    PyCodeObject *co = (PyCodeObject*)func->func_code;
    int use_generic = (
        jf == NULL ||
        jf->uses_frame_object ||
        func->func_defaults != NULL ||
        func->func_kwdefaults != NULL ||
        func->func_closure != NULL ||
        co->co_kwonlyargcount != 0 ||
        (co->co_flags & CO_VARKEYWORDS) ||
        (co->co_flags & CO_VARARGS) ||
        (co->co_flags & ~PyCF_MASK) != (CO_OPTIMIZED | CO_NEWLOCALS | CO_NOFREE));
    void *entrypoint;
    if (use_generic) {
        entrypoint = PyJIT_DCall_GetGenericEntrypoint();
    } else if (TRAMPOLINE(jf) != NULL) {
        entrypoint = TRAMPOLINE(jf)->entrypoint;
    } else {
        jf->trampoline = malloc(sizeof(TrampolineInfo));
        entrypoint = TRAMPOLINE(jf)->entrypoint = _make_trampoline(jf);
    }
    func->func_dcall = entrypoint;
    return entrypoint;
}

void PyJIT_DCall_ClearFunctionTrampoline(void *trampoline_info) {
    free(trampoline_info);
}
