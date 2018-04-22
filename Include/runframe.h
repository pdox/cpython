#ifndef Py_LIMITED_API
#ifndef Py_RUNFRAME_H
#define Py_RUNFRAME_H
#ifdef __cplusplus
extern "C" {
#endif

/* "RunFrames" are frames that exist while a function is running.
   They are created by both ceval and JIT functions, and in both
   cases exist on the C stack. (not heap)

   ceval always creates PyFrameObjects as well, because they re
   needed for the interpreter.

   jeval only creates RunFrames. A PyFrameObject will be lazily
   created ("materialized") if needed. When a frame is materialized,
   all frames below it are also materialized. Thus, at any given
   moment, the Python call stack will contain 0 or more
   materialized frames, followed by 0 or more unmaterialized
   frames.
 */
typedef struct _runframe {
    struct _runframe *prev;
    uintptr_t ref; /* This field is a tagged pointer:
                      If the LSB is 0, this is a PyFrameObject*. (strong reference)
                      If the LSB is 1, this is a PyJITFunction*. (strong reference)
                    */
    /* These are only used when this is a JIT frame */
    PyObject *f_locals;
    int f_lasti; /* TODO: Eliminate this by using return
                          addresses on the call stack */
} PyRunFrame;

#define PY_RUNFRAME_TAG_MASK           1
#define PY_RUNFRAME_TAG_FRAME          0
#define PY_RUNFRAME_TAG_JIT_FUNCTION   1

#define PyRunFrame_Tag(rf)             (((rf)->ref) & PY_RUNFRAME_TAG_MASK)
#define PyRunFrame_StripTag(rf)        (((rf)->ref) & ~PY_RUNFRAME_TAG_MASK)

/* Push a PyRunFrame corresponding to a frame (for use by ceval) */
#define PyRunFrame_Push(rf, tstate, f) do { \
    assert((f)->f_runframe == NULL); \
    (f)->f_runframe = (rf); \
    (rf)->prev = (tstate)->runframe; \
    Py_INCREF(f); \
    (rf)->ref = (uintptr_t)(f); \
    (rf)->f_locals = NULL; \
    (rf)->f_lasti = -1; \
    (tstate)->runframe = (rf); \
} while (0)

/* Pop a PyRunFrame corresponding to a frame (for use by ceval) */
#define PyRunFrame_Pop(rf, tstate, f) do { \
    assert((rf)->ref == (uintptr_t)(f)); \
    assert((tstate)->runframe == (rf)); \
    assert((f)->f_runframe == (rf)); \
    Py_DECREF(f); \
    Py_XDECREF((rf)->f_locals); \
    (tstate)->runframe = (rf)->prev; \
    (f)->f_runframe = NULL; \
} while (0)

#define PyRunFrame_IsMaterialized(rf) \
    (PyRunFrame_Tag(rf) == PY_RUNFRAME_TAG_FRAME)

#define PyRunFrame_JITFunctionRef(rf) \
    (assert(PyRunFrame_Tag(rf) == PY_RUNFRAME_TAG_JIT_FUNCTION), \
     (PyJITFunctionObject*)PyRunFrame_StripTag(rf))

#define PyRunFrame_FrameRef(rf) \
    (assert(PyRunFrame_Tag(rf) == PY_RUNFRAME_TAG_FRAME), \
     (PyFrameObject*)PyRunFrame_StripTag(rf))

#define PyRunFrame_IsJIT(rf) \
    (PyRunFrame_Tag(rf) == PY_RUNFRAME_TAG_JIT_FUNCTION || \
     PyRunFrame_FrameRef(rf)->f_jit_function != NULL)

/* Materialize a PyFrameObject for 'rf', and return it. Note that this
   also forces materialization of all runframes below 'rf'.

   The returned reference is borrowed.
 */
PyFrameObject*
PyRunFrame_ToFrame(PyRunFrame *rf);

/* Materialize all runframes for 'tstate' and return the topmost PyFrameObject.
   This is functionally equivalent to "tstate->frame" in upstream cPython.

   The returned reference is borrowed. (the actual reference is held by the PyRunFrame)
   If 'tstate' is for the current thread, it is safe to assume the reference will survive
   until the frame exits. If 'tstate' is for a different thread, the reference should be
   considered invalid after anything which triggers GC or code execution.
 */
PyFrameObject*
PyRunFrame_TopFrame(PyThreadState *tstate);

void
PyRunFrame_WipeThread(PyThreadState *tstate);

int PyRunFrame_GetLasti(PyRunFrame *rf);
int PyRunFrame_GetLineNumber(PyRunFrame *rf);
PyCodeObject* PyRunFrame_GetCode(PyRunFrame *rf);

int PyRunFrame_CanIntrospect(PyRunFrame *rf);

#ifdef __cplusplus
}
#endif
#endif /* !Py_RUNFRAME_H */
#endif /* Py_LIMITED_API */
