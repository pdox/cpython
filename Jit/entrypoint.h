#ifndef Py_JIT_ENTRYPOINT_H
#define Py_JIT_ENTRYPOINT_H
#ifdef __cplusplus
extern "C" {
#endif

/* Forward declaration */
struct _frame;
typedef struct _frame PyFrameObject;

PyObject* PyJIT_Execute(PyFrameObject *f, int throwflag);

/* Prepare a function for JIT execution.

   If the function is JIT eligible, this compiles the code,
   and sets the func_jit_function field in the function.

   Returns a new reference to the PyJITFunctionObject, or NULL.
 */
PyObject* PyJIT_Prepare(PyObject *func);

/* Return the PyJITFunctionObject* corresponding to the (co, globals, builtins) triple. */
PyObject* PyJIT_ForFrame(PyObject *hint, PyCodeObject *co, PyObject *globals, PyObject *builtins);

#ifdef __cplusplus
}
#endif
#endif /* !Py_JIT_ENTRYPOINT_H */
