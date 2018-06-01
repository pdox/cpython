#include "Python.h"
#include "frameobject.h"
#include "Jit/ir.h"
#include "Jit/jit_eval_entrypoint.h"
#include "Jit/dcall_function.h"
#include "Jit/function.h"
#include "Jit/jeval.h"
#include "Jit/entrypoint.h"


PyObject* PyJIT_Execute(PyFrameObject *f, int throwflag) {
    PyCodeObject *co = f->f_code;
    PyJITFunctionObject *jf = (PyJITFunctionObject*)f->f_jit_function;
    assert(jf && PyJITFunction_Check(jf));
    assert(jf->globals == NULL || f->f_globals == jf->globals);
    assert(jf->builtins == NULL || f->f_builtins == jf->builtins);

    if (Py_JITDebugFlag > 4) {
        fprintf(stderr, "Entering JIT: %s from %s (f->f_code == %p)\n",
            PyUnicode_AsUTF8(co->co_name),
            PyUnicode_AsUTF8(co->co_filename),
            co);
    }
    if (jf->eval_entrypoint == NULL) {
        ir_object object = (ir_object)jf->object;
        ir_object eval_entrypoint_object = PyJIT_MakeEvalEntrypoint((PyCodeObject*)jf->code, object->entrypoint);
        jf->eval_entrypoint_object = eval_entrypoint_object;
        jf->eval_entrypoint = (PyJIT_EvalEntryPoint)eval_entrypoint_object->entrypoint;
    }
    return jf->eval_entrypoint(f, throwflag);
}

_Py_IDENTIFIER(__builtins__);

PyObject* _PyJIT_Compute_Builtins(PyObject *globals) {
    PyObject *builtins = _PyDict_GetItemId(globals, &PyId___builtins__);
    if (builtins && PyModule_Check(builtins)) {
        builtins = PyModule_GetDict(builtins);
        assert(builtins != NULL);
    }
    return builtins;
}

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
        ret = PyJIT_CodeGen(co, NULL, NULL);
        Py_INCREF(ret);
        co->co_jit_function_generic = ret;
    } else {
        /* Try to generate non-generic version */
        ret = PyJIT_CodeGen(co, globals, builtins);

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

PyObject* PyJIT_Prepare(PyObject *func) {
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

PyObject* PyJIT_ForFrame(PyObject *hint, PyCodeObject *co, PyObject *globals, PyObject *builtins) {
    if (hint != NULL) {
        return hint;
    } else if (_should_jit(co)) {
        if (Py_JITDebugFlag > 0) {
            fprintf(stderr, "JIT_WARNING: One-time code generation for %s:%s\n",
                    PyUnicode_AsUTF8(co->co_name),
                    PyUnicode_AsUTF8(co->co_filename));
        }
        return (PyObject*)PyJIT_CodeGen(co, globals, builtins);
    }
    return NULL;
}
