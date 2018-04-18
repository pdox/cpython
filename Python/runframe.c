#include "Python.h"
#include "internal/pystate.h"
#include "code.h"
#include "frameobject.h"

static int world_is_stopped = 0;

int PyRunFrame_GetLasti(PyRunFrame *rf) {
    if (PyRunFrame_IsMaterialized(rf)) {
        PyFrameObject *f = PyRunFrame_FrameRef(rf);
        if (f->f_jit_function == NULL ||
            PyJITFunction_UsesFrameObject(f->f_jit_function)) {
            return f->f_lasti_ceval;
        }
    }
    return rf->f_lasti;
}

int PyRunFrame_GetLineNumber(PyRunFrame *rf) {
    if (PyRunFrame_IsMaterialized(rf)) {
        PyFrameObject *f = PyRunFrame_FrameRef(rf);
        return PyFrame_GetLineNumber(f);
    } else {
        PyJITFunctionObject *jf = PyRunFrame_JITFunctionRef(rf);
        return PyCode_Addr2Line((PyCodeObject*)jf->code, rf->f_lasti);
    }
}

PyCodeObject* PyRunFrame_GetCode(PyRunFrame *rf) {
    if (PyRunFrame_IsMaterialized(rf)) {
        PyFrameObject *f = PyRunFrame_FrameRef(rf);
        return f->f_code;
    } else {
        PyJITFunctionObject *jf = PyRunFrame_JITFunctionRef(rf);
        return (PyCodeObject*)jf->code;
    }
}

PyFrameObject*
_PyRunFrame_MaterializeOne(PyRunFrame *rf, PyFrameObject *next)
{
#ifndef NDEBUG
    uintptr_t entry_ref = rf->ref;
#endif
    PyJITFunctionObject *jf = PyRunFrame_JITFunctionRef(rf);
    assert(jf->globals);
    assert(jf->builtins);
    Py_XINCREF(rf->f_locals);
    PyFrameObject *f = _PyFrame_New_Partial((PyCodeObject*)jf->code, jf->globals, jf->builtins, rf->f_locals);
    assert(f);
    if (next != NULL) {
        assert(next->f_back == NULL);
        Py_INCREF(f);
        next->f_back = f;
    }
    f->f_executing = 1;
    f->f_stacktop = NULL;
    assert(f->f_runframe == NULL);
    f->f_runframe = rf;

    /* f takes the run frame's reference to the jit function */
    assert(f->f_jit_function == NULL);
    f->f_jit_function = (PyObject*)jf;

    /* rf takes our reference to f */
    assert(rf->ref == entry_ref);
    rf->ref = (uintptr_t)f;
    return f;
}

PyFrameObject*
PyRunFrame_TopFrame(PyThreadState *tstate) {
    return PyRunFrame_ToFrame(tstate->runframe);
}

PyFrameObject*
PyRunFrame_ToFrame(PyRunFrame *rf)
{
    /* Return the PyFrameObject corresponding to PyRunFrame 'rf', possibly triggering materialization.

       This is literally a "stop-the-world" operation. During the execution of this function:

         * The GIL must be held without being released
         * Python user code cannot run (this can release the GIL)
         * No DECREFs that may trigger a finalizer
         * Garbage collection cannot run. (this can call finalizers / release the GIL)
         * It cannot be entered again (not reentrant)

       This is necessary because PyRunFrame entities are not reference counted, and thus it is
       not possible to hold a reference to them which survives releasing the GIL.
    */
    if (rf == NULL)
        return NULL;

    if (PyRunFrame_IsMaterialized(rf))
        return PyRunFrame_FrameRef(rf);

    /* Pause garbage collection */
    int gc_state = _PyGC_Pause();

    /* Mark the world as stopped to detect re-entry */
    if (world_is_stopped) {
        Py_FatalError("PyRunFrame_TopFrame already running");
    }
    world_is_stopped = 1;

    /* Materialize 'rf' and everything behind it, in reverse order.
       TODO: Make it possible to materialize frames independently. */
    PyRunFrame *cur = rf;
    PyFrameObject *next = NULL; /* for linking f_back */
    while (cur && !PyRunFrame_IsMaterialized(cur)) {
        next = _PyRunFrame_MaterializeOne(cur, next);
        cur = cur->prev;
    }
    if (cur && next != NULL) {
        assert(next->f_back == NULL);
        PyFrameObject *f = PyRunFrame_FrameRef(cur);
        Py_INCREF(f);
        next->f_back = f;
    }
    world_is_stopped = 0;
    _PyGC_Resume(gc_state);

    assert(PyRunFrame_IsMaterialized(rf));
    return PyRunFrame_FrameRef(rf);
}

void
PyRunFrame_WipeThread(PyThreadState *tstate) {
    PyRunFrame *cur = tstate->runframe;
    while (cur != NULL) {
        Py_DECREF(PyRunFrame_StripTag(cur));
        Py_XDECREF(cur->f_locals);
        cur = cur->prev;
    }
    tstate->runframe = NULL;
}
