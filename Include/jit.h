#ifndef Py_JIT_H
#define Py_JIT_H
#ifdef __cplusplus
extern "C" {
#endif

/* Interface exposed to the rest of cPython */

PyAPI_FUNC(PyObject*) PyJIT_EvalFrame(PyFrameObject *f);

typedef struct {
    void *entry;
} PyJIT_Trampoline;

PyJIT_Trampoline *
_PyJIT_generate_trampoline(
    PyObject *callable,    /* Any Python callable */
    size_t nargs,          /* Number of positional arguments */
    PyObject *kwnames,     /* Tuple of names of keyword arguments */
    int has_kwdict);       /* Caller also provides dictionary object */

#ifdef __cplusplus
}
#endif
#endif /* !Py_JIT_H */
