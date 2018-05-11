#include "Python.h"
#include "internal/pystate.h"
#include "frameobject.h"
#include "Jit/ir.h"
#include "Jit/jit_eval_entrypoint.h"
#include "Jit/dcall_function.h"
#include "Jit/jeval.h"

PyObject *
PyJITFunction_New(PyObject *code, PyObject *globals, PyObject *builtins) {
    PyJITFunctionObject *op;
    op = (PyJITFunctionObject*)PyObject_GC_New(PyJITFunctionObject, &PyJITFunction_Type);
    if (op == NULL)
        return NULL;

    Py_INCREF(code);
    op->code = code;
    Py_XINCREF(globals);
    op->globals = globals;
    Py_XINCREF(builtins);
    op->builtins = builtins;

    op->trampoline = NULL;

    op->parent = NULL;
    op->deps_head = NULL;
    op->deps_tail = NULL;
    op->prev = NULL;
    op->next = NULL;

    /* Other fields must be filled in by the caller */
    _PyObject_GC_TRACK(op);
    return (PyObject*)op;
}

void
PyJITFunction_SetParent(PyJITFunctionObject *self, PyJITFunctionObject *new_parent) {
    if (self->parent != NULL) {
        /* Removing parent */
        PyJITFunctionObject *old_parent = self->parent;
        assert(new_parent == NULL);
        self->parent = NULL;
        ADT_LL_REMOVE(old_parent->deps_head, old_parent->deps_tail, self);
        Py_DECREF(self);
    } else if (new_parent != NULL) {
        assert(self->parent == NULL); /* Cannot re-parent */
        self->parent = new_parent;
        ADT_LL_INSERT_FIRST(new_parent->deps_head, new_parent->deps_tail, self);
        Py_INCREF(self);
    }
}

static int
jf_traverse(PyJITFunctionObject *op, visitproc visit, void *arg)
{
    Py_VISIT(op->code);
    Py_VISIT(op->globals);
    Py_VISIT(op->builtins);

    /* Visit each of our children */
    PyJITFunctionObject *cursor = op->deps_head;
    while (cursor != NULL) {
        Py_VISIT(cursor);
        cursor = cursor->next;
    }
    return 0;
}

static int jf_clear(PyJITFunctionObject *op) {
    /* Unlink children */
    while (op->deps_head != NULL) {
        PyJITFunction_SetParent(op->deps_head, NULL);
    }

    PyJIT_CodeGenCleanup(op);

    if (op->trampoline) {
        PyJIT_DCall_ClearFunctionTrampoline(op->trampoline);
        op->trampoline = NULL;
    }

    Py_CLEAR(op->code);
    Py_CLEAR(op->globals);
    Py_CLEAR(op->builtins);
    return 0;
}

static void jf_dealloc(PyJITFunctionObject *op) {
    PyObject_GC_UnTrack(op);
    Py_TRASHCAN_SAFE_BEGIN(op)

    /* Unlink children */
    while (op->deps_head != NULL) {
        PyJITFunction_SetParent(op->deps_head, NULL);
    }

    /* This must be done first, since it may use code/globals/builtins. */
    PyJIT_CodeGenCleanup(op);

    if (op->trampoline)
        PyJIT_DCall_ClearFunctionTrampoline(op->trampoline);

    Py_XDECREF(op->code);
    Py_XDECREF(op->globals);
    Py_XDECREF(op->builtins);
    PyObject_GC_Del(op);
    Py_TRASHCAN_SAFE_END(op)
}

static PyObject* jf_repr(PyJITFunctionObject *op) {
    return PyUnicode_FromFormat("<jit_function at %p>", op);
}

PyTypeObject PyJITFunction_Type = {
    PyVarObject_HEAD_INIT(&PyType_Type, 0)
    "jit_function",
    sizeof(PyJITFunctionObject),
    0,
    (destructor)jf_dealloc,                     /* tp_dealloc */
    0,                                          /* tp_print */
    0,                                          /* tp_getattr */
    0,                                          /* tp_setattr */
    0,                                          /* tp_reserved */
    (reprfunc)jf_repr,                          /* tp_repr */
    0,                                          /* tp_as_number */
    0,                                          /* tp_as_sequence */
    0,                                          /* tp_as_mapping */
    0,                                          /* tp_hash */
    0,                                          /* tp_call */
    0,                                          /* tp_str */
    PyObject_GenericGetAttr,                    /* tp_getattro */
    0,                                          /* tp_setattro */
    0,                                          /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,    /* tp_flags */
    0,                                          /* tp_doc */
    (traverseproc)jf_traverse,                  /* tp_traverse */
    (inquiry)jf_clear,                          /* tp_clear */
    0,                                          /* tp_richcompare */
    0,                                          /* tp_weaklistoffset */
    0,                                          /* tp_iter */
    0,                                          /* tp_iternext */
    0,                                          /* tp_methods */
    0,                                          /* tp_members */
    0,                                          /* tp_getset */
};

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
