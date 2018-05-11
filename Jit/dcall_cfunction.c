#include "Jit/dcall_signature.h"
#include "Jit/dcall_cfunction.h"
#include "Python.h"

typedef struct {
    void *entrypoint;
} _trampoline;

#define TRAMPOLINE(ml)   ((_trampoline*)(ml)->ml_trampoline)

void* PyJIT_DCall_GetMethodEntrypoint(PyMethodDef *ml) {
    if (TRAMPOLINE(ml) == NULL)
        return PyJIT_DCall_GetBootstrapEntrypoint();
    return TRAMPOLINE(ml)->entrypoint;
}

static void*
_make_trampoline(PyMethodDef *ml, PyJIT_DCall_Signature *css) {
    return PyJIT_DCall_MakeTrampolineForCFunction_X86_64(ml, css);
}

void* PyJIT_DCall_SetupCFunction(PyCFunctionObject *func, PyJIT_DCall_Signature *css) {
    _trampoline *t = TRAMPOLINE(func->m_ml);
    if (t == NULL) {
        t = func->m_ml->ml_trampoline = malloc(sizeof(_trampoline));
        t->entrypoint = _make_trampoline(func->m_ml, css);
    }
    func->m_dcall = t->entrypoint;
    return func->m_dcall;
}
