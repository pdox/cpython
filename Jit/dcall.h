#ifndef Py_DCALL_H
#define Py_DCALL_H
#ifdef __cplusplus
extern "C" {
#endif

/* All interaction between the dcall_* modules and the rest of the
   Jit or cPython, should go through this header.
 */
#include "Jit/dcall_signature.h"

void* PyJIT_DCall_GetBootstrapEntrypoint(void);
void* PyJIT_DCall_GetMethodEntrypoint(PyMethodDef *ml);
void* PyJIT_DCall_GetGenericEntrypoint(void);

#ifdef __cplusplus
}
#endif
#endif /* !Py_DCALL_H */
