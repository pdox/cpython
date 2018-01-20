#ifndef Py_JIT_H
#define Py_JIT_H
#ifdef __cplusplus
extern "C" {
#endif

/* Interface exposed to the rest of cPython */
extern int Py_JITFlag;
extern int Py_JITDebugFlag;
extern char *Py_JITDebugFunc;
extern char *Py_JITDebugFile;

typedef PyObject* (*PyJIT_EntryPoint)(PyFrameObject *f, int throwflag);

/* The stackmap for an error site always has:
    1) The first value is the frame pointer for the funtion.
    2) If has_exc_restore is true, the next three values are
       the final exception triple to restore to tstate->exc_info.
    3) The remaining 'decref_count' values are to be XDECREF'd.
 */
typedef struct {
    int has_exc_restore; /* Has tuple to restore to tstate->exc_info.
                            If true, these will be the first 3 values in the stackmap. */
    int decref_count; /* Number of values to xdecref.
                         These are the remaining values in the stackmap. */
} PyJIT_ErrorInfo;

typedef struct {
    void *context;
    PyJIT_EntryPoint entry;
    PyJIT_ErrorInfo *error_info[1];
} PyJIT_Handle;

extern int _PyJIT_CodeGen(PyCodeObject *co);

static inline PyObject*
PyJIT_Execute(PyFrameObject *f, int throwflag) {
    PyCodeObject *co = f->f_code;
    if (co->co_jit_handle == NULL) {
        if (Py_JITDebugFlag > 0) {
            fprintf(stderr, "Entering CodeGen for %s from %s (f->f_code == %p)\n",
                PyUnicode_AsUTF8(co->co_name),
                PyUnicode_AsUTF8(co->co_filename),
                co);
        }
        if (_PyJIT_CodeGen(co) != 0) {
            abort();
        }
        assert(co->co_jit_handle != NULL);
    }
    if (Py_JITDebugFlag > 4) {
        fprintf(stderr, "Calling %s from %s (f->f_code == %p)\n",
            PyUnicode_AsUTF8(co->co_name),
            PyUnicode_AsUTF8(co->co_filename),
            co);
    }
    return ((PyJIT_Handle*)co->co_jit_handle)->entry(f, throwflag);
}

#ifdef __cplusplus
}
#endif
#endif /* !Py_JIT_H */
