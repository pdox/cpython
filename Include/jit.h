#ifndef Py_JIT_H
#define Py_JIT_H
#ifdef __cplusplus
extern "C" {
#endif

/* Interface exposed to the rest of cPython */
extern int Py_JITFlag;
extern int Py_JITDebugFlag;
extern char *Py_JITDebugFunc;
extern char *Py_JITDebugFile;

/* Forward declaration */
struct _frame;
typedef struct _frame PyFrameObject;

typedef PyObject* (*PyJIT_EvalEntryPoint)(PyFrameObject *f);
typedef PyObject* (*PyJIT_GenEntryPoint)(PyFrameObject *f, int throwflag);
typedef PyObject* (*PyJIT_DirectEntryPoint)(PyObject *arg0, ...);

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
    int uses_virtual_locals;

    PyJIT_EvalEntryPoint eval_entrypoint;
    PyJIT_GenEntryPoint gen_entrypoint;
    PyJIT_DirectEntryPoint direct_entrypoint;

    void *object; /* ir_object */
    void *eval_entrypoint_object;
};

PyAPI_DATA(PyTypeObject) PyJITFunction_Type;
#define PyJITFunction_Check(op)  (Py_TYPE(op) == &PyJITFunction_Type)

PyObject *
PyJITFunction_New(PyObject *code, PyObject *globals, PyObject *builtins);

void
PyJITFunction_SetParent(PyJITFunctionObject *self, PyJITFunctionObject *new_parent);

static inline int PyJITFunction_HasVirtualLocals(PyObject *jf) {
    assert(PyJITFunction_Check(jf));
    return ((PyJITFunctionObject*)jf)->uses_virtual_locals;
}

PyJITFunctionObject*
_PyJIT_CodeGen(PyCodeObject *co, PyObject *globals, PyObject *builtins);

PyObject* PyJIT_Execute(PyFrameObject *f, int throwflag);

static int _should_jit(PyCodeObject *co) {
    return Py_JITFlag != 0 &&
           (!Py_JITDebugFunc || strcmp(Py_JITDebugFunc, PyUnicode_AsUTF8(co->co_name)) == 0) &&
           (!Py_JITDebugFile || strstr(PyUnicode_AsUTF8(co->co_filename), Py_JITDebugFile) != NULL);
}

/* Returns a new reference to jit function, or NULL. */
static inline
PyObject*
_PyJIT_GetFunction(PyObject *parent, PyCodeObject *co, PyObject *globals, PyObject *builtins) {
    if (!_should_jit(co))
        return NULL;

    /* See if the parent is already keeping track of the jit function for us */
    PyJITFunctionObject *candidate = NULL;
    if (parent != NULL) {
        assert(PyJITFunction_Check(parent));
        PyJITFunctionObject *p = (PyJITFunctionObject*)parent;
        PyJITFunctionObject *cursor = p->deps_head;
        while (cursor != NULL) {
            if (cursor->code == (PyObject*)co)
                break;
            cursor = cursor->next;
        }
        candidate = cursor;
    }

    /* Use the candidate if it is a perfect match */
    PyJITFunctionObject *ret;
    if (candidate &&
        candidate->globals == globals &&
        candidate->builtins == builtins) {
        ret = candidate;
        Py_INCREF(ret);
    } else if (co->co_jit_function_generic) {
        /* There's already a generic version, use it */
        ret = co->co_jit_function_generic;
        Py_INCREF(ret);
    } else if (candidate) {
        /* There was a candidate mismatch. Generate generic version. */
        ret = _PyJIT_CodeGen(co, NULL, NULL);
        Py_INCREF(ret);
        co->co_jit_function_generic = ret;
    } else {
        /* Try to generate non-generic version */
        ret = _PyJIT_CodeGen(co, globals, builtins);

        /* If the output is generic, attach it to the code object.
           Otherwise, attach it to our parent function. */
        if (ret->globals == NULL) {
            Py_INCREF(ret);
            co->co_jit_function_generic = ret;
        } else {
            /* Attach to parent */
            PyJITFunction_SetParent(ret, (PyJITFunctionObject*)parent);
        }
    }
    return (PyObject*)ret;
}

PyObject* _PyJIT_Compute_Builtins(PyObject *globals);

/* Prepare a function for JIT execution.

   If the function is JIT eligible, this compiles the code,
   and sets the func_jit_function field in the function.

   Returns a new reference to the PyJITFunctionObject, or NULL.
 */
static inline PyObject* PyJIT_Prepare(PyObject *func) {
    assert(PyFunction_Check(func));
    PyObject *ret = ((PyFunctionObject*)func)->func_jit_function;
    PyCodeObject *co = (PyCodeObject*)PyFunction_GET_CODE(func);
    if (ret == NULL && _should_jit(co)) {
        PyObject *globals = PyFunction_GET_GLOBALS(func);
        PyObject *builtins = _PyJIT_Compute_Builtins(globals);
        PyObject *parent = ((PyFunctionObject*)func)->func_jit_parent;
        ret = _PyJIT_GetFunction(parent, co, globals, builtins);
        ((PyFunctionObject*)func)->func_jit_function = ret; /* func owns reference */
    }
    if (ret == NULL)
        return NULL;

    Py_INCREF(ret);
    return ret;
}

static inline PyObject* PyJIT_ForFrame(PyObject *hint, PyCodeObject *co, PyObject *globals, PyObject *builtins) {
    if (hint != NULL) {
        return hint;
    } else if (Py_JITFlag > 0) {
        if (Py_JITDebugFlag > 0) {
            fprintf(stderr, "JIT_WARNING: One-time code generation for %s:%s\n",
                    PyUnicode_AsUTF8(co->co_name),
                    PyUnicode_AsUTF8(co->co_filename));
        }
        return (PyObject*)_PyJIT_CodeGen(co, globals, builtins);
    }
    return NULL;
}

#ifdef __cplusplus
}
#endif
#endif /* !Py_JIT_H */
