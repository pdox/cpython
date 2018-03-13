#include "Python.h"
#include "internal/pystate.h"
#include "code.h"
#include "frameobject.h"

PyFrameObject*
PyRunFrame_ToFrame(PyRunFrame *rf)
{
    if (rf == NULL)
        return NULL;

    /* Materialize 'rf' and all frames under it */
    PyRunFrame *cur = rf;
    PyFrameObject *next = NULL; /* for linking f_back */
    while (!PyRunFrame_IsMaterialized(cur)) {
        PyJITFunctionObject *jf = PyRunFrame_JITFunctionRef(cur);
        PyFrameObject *f = _PyFrame_New_Raw(NULL, (PyCodeObject*)jf->code, jf->globals, rf->f_locals);
        assert(f->f_builtins == jf->builtins);
        if (next != NULL) {
            assert(next->f_back == NULL);
            Py_INCREF(f);
            next->f_back = f;
        }
        /* f takes the reference to jf (previous owned by 'cur') */
        f->f_jit_function = (PyObject*)jf;
        if (rf->f_locals != NULL) {
            Py_INCREF(rf->f_locals);
            Py_XDECREF(f->f_locals);
            f->f_locals = rf->f_locals;
        }
        f->f_lasti = rf->f_lasti;
        /* 'cur' takes our reference to f */
        PyRunFrame_SetFrameRef(cur, f);
        next = f;
        cur = cur->prev;
    }
    if (next != NULL) {
        PyFrameObject *f = PyRunFrame_FrameRef(cur);
        Py_INCREF(f);
        next->f_back = f;
    }
    assert(PyRunFrame_IsMaterialized(rf));
    return PyRunFrame_FrameRef(rf);
}

PyFrameObject*
PyRunFrame_TopFrame(PyThreadState *tstate) {
    return PyRunFrame_ToFrame(tstate->runframe);
}

void
PyRunFrame_WipeThread(PyThreadState *tstate) {
    PyRunFrame *cur = tstate->runframe;
    while (cur != NULL) {
        if (cur->prev != NULL) {
            Py_DECREF(PyRunFrame_StripTag(cur));
        }
        Py_XDECREF(cur->f_locals);
        cur = cur->prev;
    }
    tstate->runframe = NULL;
}
