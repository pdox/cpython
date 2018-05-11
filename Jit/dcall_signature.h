#ifndef Py_DCALL_SIGNATURE_H
#define Py_DCALL_SIGNATURE_H
#ifdef __cplusplus
extern "C" {
#endif

#include "Python.h"
#include <stddef.h>

/* PyJIT_DCall_Signature
   This describes the signature of a CALL_FUNCTION or CALL_FUNCTION_KW callsite.
   This is an interned type, so that pointer comparison can be used to test for
   equality. */
typedef struct _PyJIT_DCall_Signature {
    long hash;
    size_t argcount;
    PyObject *kwnames;
} PyJIT_DCall_Signature;

PyJIT_DCall_Signature*
PyJIT_DCall_GetSignature(int argcount, PyObject *kwnames);

#ifdef __cplusplus
}
#endif
#endif /* !Py_DCALL_SIGNATURE_H */
