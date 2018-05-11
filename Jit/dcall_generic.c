#include "Python.h"
#include "Jit/dcall_signature.h"

PyObject *call_function_extern(PyObject **, Py_ssize_t, PyObject *);

static PyObject* _GenericTrampoline(PyObject *callable, PyJIT_DCall_Signature *css, ...) {
    if (Py_JITDebugFlag > 0) {
        if (PyFunction_Check(callable)) {
            PyFunctionObject *func = (PyFunctionObject*)callable;
            PyCodeObject *co = (PyCodeObject*)func->func_code;
            fprintf(stderr, "Entering generic trampoline for func=%p, code=%p (%s:%s)\n",
                    func,
                    co,
                    PyUnicode_AsUTF8(co->co_filename),
                    PyUnicode_AsUTF8(co->co_name));
        } else if (PyCFunction_Check(callable)) {
            PyCFunctionObject *func = (PyCFunctionObject*)callable;
            fprintf(stderr, "Entering generic trampoline for method %s\n", func->m_ml->ml_name);
        } else {
            fprintf(stderr, "Entering generic trampoline for callable=%p, type '%s'\n",
                callable,
                Py_TYPE(callable)->tp_name);
        }
    }
    assert(css->argcount < 1024); /* Sanity check */
    PyObject *stack[css->argcount + 1];
    va_list ap;
    stack[0] = callable;
    va_start(ap, css);
    for (size_t i = 0; i < css->argcount; i++) {
        stack[1 + i] = va_arg(ap, PyObject*);
    }
    return call_function_extern(&stack[css->argcount + 1], css->argcount, css->kwnames);
}

void* PyJIT_DCall_GetGenericEntrypoint(void) {
    return _GenericTrampoline;
}
