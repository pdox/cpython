#ifndef Py_DCALL_CFUNCTION_H
#define Py_DCALL_CFUNCTION_H
#ifdef __cplusplus
extern "C" {
#endif

void* PyJIT_DCall_SetupCFunction(PyCFunctionObject *func, PyJIT_DCall_Signature *css);
void* PyJIT_DCall_MakeTrampolineForCFunction_X86_64(PyMethodDef *method, PyJIT_DCall_Signature *cursite);

#ifdef __cplusplus
}
#endif
#endif /* !Py_DCALL_CFUNCTION_H */

