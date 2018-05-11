#ifndef Py_JIT_JEVAL_H
#define Py_JIT_JEVAL_H
#ifdef __cplusplus
extern "C" {
#endif

PyJITFunctionObject* PyJIT_CodeGen(PyCodeObject *co, PyObject *globals, PyObject *builtins);

void PyJIT_CodeGenCleanup(PyJITFunctionObject *jf);

#ifdef __cplusplus
}
#endif
#endif /* !Py_JIT_JEVAL_H */
