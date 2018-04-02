#include "Python.h"
#include "internal/pystate.h"
#include "frameobject.h"
#include "ir.h"
#include "jit_eval_entrypoint.h"

/* CallSiteSig map */
static adt_hashmap csmap;

int csmap_key_eq(void *pa, void *pb) {
    PyJITCallSiteSig *a = *(PyJITCallSiteSig**)pa;
    PyJITCallSiteSig *b = *(PyJITCallSiteSig**)pb;
    if (a->hash != b->hash) return 0;
    if (a->argcount != b->argcount) return 0;
    if (a->kwnames && b->kwnames) {
        int k = PyObject_RichCompareBool(a->kwnames, b->kwnames, Py_EQ);
        assert(k >= 0);
        if (!k) return 0;
    } else if (a->kwnames || b->kwnames) {
        return 0;
    }
    return 1;
}

size_t csmap_key_hash(void *pa) {
    PyJITCallSiteSig *a = *(PyJITCallSiteSig**)pa;
    return (size_t)a->hash;
}

PyJITCallSiteSig*
PyJIT_CallSiteSig_GetOrCreate(int argcount, PyObject *kwnames) {
    assert(argcount >= 0);
    if (kwnames != NULL) {
        assert(PyTuple_CheckExact(kwnames));
        assert(PyTuple_GET_SIZE(kwnames) > 0);
        assert(argcount >= PyTuple_GET_SIZE(kwnames));
    }
    PyJITCallSiteSig key;
    key.hash = argcount + (kwnames ? PyObject_Hash(kwnames) : 0);
    key.argcount = argcount;
    key.kwnames = kwnames;

    if (csmap == NULL)
        csmap = adt_hashmap_new(sizeof(PyJITCallSiteSig*), sizeof(PyJITCallSiteSig*),
                                csmap_key_eq, csmap_key_hash);

    PyJITCallSiteSig *pkey = &key;
    PyJITCallSiteSig *ret;
    if (adt_hashmap_get(csmap, &pkey, &ret)) {
        return ret;
    }
    ret = PyMem_RawMalloc(sizeof(PyJITCallSiteSig));
    if (ret == NULL)
        return NULL;
    *ret = key;
    Py_XINCREF(ret->kwnames);
    adt_hashmap_insert(csmap, &ret, &ret);
    return ret;
}

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

    _jeval_cleanup(op);

    if (op->trampoline) {
        PyJIT_CallTrampoline_Free(op->trampoline);
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
    _jeval_cleanup(op);

    if (op->trampoline)
        PyJIT_CallTrampoline_Free(op->trampoline);

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
