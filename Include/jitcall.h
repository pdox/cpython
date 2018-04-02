#ifndef Py_LIMITED_API
#ifndef Py_JITCALL_H
#define Py_JITCALL_H
#ifdef __cplusplus
extern "C" {
#endif

/* PyJITCallSiteSig
   This describes the signature of a CALL_FUNCTION or CALL_FUNCTION_KW callsite.
   This is an interned type, so that pointer comparison can be used to test for
   equality. */
typedef struct _PyJITCallSiteSig {
    long hash;
    size_t argcount;
    PyObject *kwnames;
} PyJITCallSiteSig;

PyJITCallSiteSig*
PyJIT_CallSiteSig_GetOrCreate(int argcount, PyObject *kwnames);

typedef void *PyJIT_CallTrampolineEntryPoint;

typedef struct _PyJIT_CallTrampoline {
    PyJITCallSiteSig* handled[4];
    void *entrypoint;
} PyJIT_CallTrampoline;

void* PyJIT_CallTrampoline_GetBootstrap(void);
void* PyJIT_CallTrampoline_GetEntryPoint(PyJIT_CallTrampoline *t);
void PyJIT_CallTrampoline_Free(PyJIT_CallTrampoline *t);

PyObject* _PyJIT_GenericTrampoline(PyObject *callable, PyJITCallSiteSig *css, ...);

#ifdef __cplusplus
}
#endif
#endif /* !Py_JITCALL_H */
#endif /* Py_LIMITED_API */

