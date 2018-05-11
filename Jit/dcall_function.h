#ifndef Py_DCALL_FUNCTION_H
#define Py_DCALL_FUNCTION_H
#ifdef __cplusplus
extern "C" {
#endif

#include "Jit/dcall_signature.h"

void* PyJIT_DCall_SetupFunction(PyFunctionObject *func, PyJIT_DCall_Signature *css);
void  PyJIT_DCall_ClearFunctionTrampoline(void *trampoline_info);

void* PyJIT_DCall_MakeTrampolineForFunction_X86_64(PyJITFunctionObject *jf);

#ifdef __cplusplus
}
#endif
#endif /* !Py_DCALL_FUNCTION_H */
