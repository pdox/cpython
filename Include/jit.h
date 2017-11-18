#ifndef Py_JIT_H
#define Py_JIT_H
#ifdef __cplusplus
extern "C" {
#endif

PyAPI_FUNC(PyObject*) PyJIT_EvalFrame(PyFrameObject *f);

#ifdef __cplusplus
}
#endif
#endif /* !Py_JIT_H */
