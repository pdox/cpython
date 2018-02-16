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

typedef PyObject* (*PyJIT_EvalEntryPoint)(PyFrameObject *f);
typedef PyObject* (*PyJIT_GenEntryPoint)(PyFrameObject *f, int throwflag);
typedef PyObject* (*PyJIT_DirectEntryPoint)(PyFrameObject *f, ...);

typedef struct {
    void *object; /* ir_object */
    void *eval_entrypoint_object;

    PyJIT_EvalEntryPoint eval_entrypoint;
    PyJIT_GenEntryPoint gen_entrypoint;
    PyJIT_DirectEntryPoint direct_entrypoint;
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
    PyJIT_Handle *handle = co->co_jit_handle;
    if (handle->gen_entrypoint) {
        return handle->gen_entrypoint(f, throwflag);
    } else {
        assert(!throwflag);
        return handle->eval_entrypoint(f);
    }
}

#ifdef __cplusplus
}
#endif
#endif /* !Py_JIT_H */
