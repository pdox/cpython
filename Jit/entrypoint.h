#ifndef Py_JIT_ENTRYPOINT_H
#define Py_JIT_ENTRYPOINT_H
#ifdef __cplusplus
extern "C" {
#endif

#include "code.h"
#include "Jit/options.h"

/* Forward declaration */
struct _frame;
typedef struct _frame PyFrameObject;

typedef PyObject* (*PyJIT_EvalEntryPoint)(PyFrameObject *f, int throwflag);
typedef PyObject* (*PyJIT_DirectEntryPoint)(void *unused1, void *unused2, ...);

/* PyJITFunctionObject is a Python object that holds a reference to
   JIT-generated native code and all other Python objects needed to keep
   the generated code valid.

   A single PyJITFunction may be used by multiple PyFunctionObjects.

   For example, in the following:

      def foo(x):
          def bar():
              print(x)
          return bar

   Each invocation of 'foo' will create a new PyFunction object for
   'bar', but all 'bar' functions will link to the same PyJITFunctionObject.
 */

struct _PyJITFunctionObject;
typedef struct _PyJITFunctionObject PyJITFunctionObject;
struct _PyJITFunctionObject {
    PyObject_HEAD

    PyObject *code;
    PyObject *globals;
    PyObject *builtins;

    /* Trampoline */
    void *trampoline;

    /* This is a linked list of all PyJITFunctionObject's that may be
       needed as a result of MAKE_FUNCTION calls inside 'code'. This is here
       to ensure that these generated objects stay alive while this function is
       alive, instead of being created/destroyed repeatedly.

       Everything on this list must have identical globals and builtins.
     */
    PyJITFunctionObject *deps_head;
    PyJITFunctionObject *deps_tail;

    /* The PyJITFunction that encloses this one, if this is a nested function.
       NOTE: This is a borrowed reference, which may vanish. When the parent is
             deallocated, it will set 'parent' to NULL on all its children. */
    PyJITFunctionObject *parent;

    /* These are the corresponding prev/next pointers for the above linked list */
    PyJITFunctionObject *prev;
    PyJITFunctionObject *next;

    /* Everything below must be filled in by jeval */
    int uses_frame_object;

    PyJIT_EvalEntryPoint eval_entrypoint;
    PyJIT_DirectEntryPoint direct_entrypoint;

    void *object; /* ir_object */
    void *eval_entrypoint_object;
    void *jstate;
};

PyAPI_DATA(PyTypeObject) PyJITFunction_Type;
#define PyJITFunction_Check(op)  (Py_TYPE(op) == &PyJITFunction_Type)

PyObject *
PyJITFunction_New(PyObject *code, PyObject *globals, PyObject *builtins);

void
PyJITFunction_SetParent(PyJITFunctionObject *self, PyJITFunctionObject *new_parent);

static inline int PyJITFunction_UsesFrameObject(PyObject *jf) {
    assert(PyJITFunction_Check(jf));
    return ((PyJITFunctionObject*)jf)->uses_frame_object;
}

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
