
/* Frame object interface */

#ifndef Py_LIMITED_API
#ifndef Py_FRAMEOBJECT_H
#define Py_FRAMEOBJECT_H
#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    int b_type;                 /* what kind of block this is */
    int b_handler;              /* where to jump to find handler */
    int b_level;                /* value stack level to pop to */
} PyTryBlock;

/* Forward reference */
struct _runframe;

typedef struct _frame {
    PyObject_VAR_HEAD
    struct _frame *f_back;      /* previous frame, or NULL */
    PyCodeObject *f_code;       /* code segment */
    PyObject *f_jit_function;   /* jit function */
    struct _runframe *f_runframe; /* if this frame is executing, this will point
                                     to the PyRunFrame */
    PyObject *f_builtins;       /* builtin symbol table (PyDictObject) */
    PyObject *f_globals;        /* global symbol table (PyDictObject) */
    PyObject *f_locals;         /* local symbol table (any mapping) */
    PyObject **f_valuestack;    /* points after the last local */
    /* Next free slot in f_valuestack.  Frame creation sets to f_valuestack.
       Frame evaluation usually NULLs it, but a frame that yields sets it
       to the current stack top. */
    PyObject **f_stacktop;
    PyObject *f_trace;          /* Trace function */
    char f_trace_lines;         /* Emit per-line trace events? */
    char f_trace_opcodes;       /* Emit per-opcode trace events? */

    /* Borrowed reference to a generator, or NULL */
    PyObject *f_gen;

    /* For PYJIT, this has been renamed, because it is not the source of
       truth for f_lasti for jeval frames. All fetches should use the helper
       function, PyFrame_GetLasti. */
    int f_lasti_ceval;           /* Last instruction if called */
    /* Call PyFrame_GetLineNumber() instead of reading this field
       directly.  As of 2.3 f_lineno is only valid when tracing is
       active (i.e. when f_trace is set).  At other times we use
       PyCode_Addr2Line to calculate the line from the current
       bytecode index. */
    int f_lineno;               /* Current line number */
    int f_iblock;               /* index in f_blockstack */
    char f_executing;           /* whether the frame is still executing */
    char f_partial;             /* whether this is a partial frame. A partial
                                   frame ends here, and does not have space
                                   allocated for f_blockstack and f_localsplus
                                   below. */

    /* ----------------------------------------------------------------
       Always use macros to access fields below this point, since they are
       conditionally allocated
       --------------------------------------------------------------- */

    PyTryBlock f_blockstack_[CO_MAXBLOCKS]; /* for try and loop blocks */
    PyObject *f_localsplus_[1];  /* locals+stack, dynamically sized */
} PyFrameObject;

#define PyFrame_GET_BLOCKSTACK(f)  (assert(!(f)->f_partial), (f)->f_blockstack_)
#define PyFrame_GET_LOCALSPLUS(f)  (assert(!(f)->f_partial), (f)->f_localsplus_)

/* Standard object interface */

PyAPI_DATA(PyTypeObject) PyFrame_Type;

#define PyFrame_Check(op) (Py_TYPE(op) == &PyFrame_Type)

PyAPI_FUNC(PyFrameObject *) PyFrame_New(PyThreadState *, PyCodeObject *,
                                        PyObject *, PyObject *);

/* only internal use */
PyFrameObject* _PyFrame_New_Partial(PyCodeObject *, PyObject *, PyObject *, PyObject *);

/* only internal use */
PyFrameObject* _PyFrame_New_NoTrack(PyThreadState *, PyCodeObject *,
                                    PyObject *, PyObject *);


/* The rest of the interface is specific for frame objects */

/* Block management functions */

PyAPI_FUNC(void) PyFrame_BlockSetup(PyFrameObject *, int, int, int);
PyAPI_FUNC(PyTryBlock *) PyFrame_BlockPop(PyFrameObject *);

/* Extend the value stack */

PyAPI_FUNC(PyObject **) PyFrame_ExtendStack(PyFrameObject *, int, int);

/* Conversions between "fast locals" and locals in dictionary */

PyAPI_FUNC(void) PyFrame_LocalsToFast(PyFrameObject *, int);

PyAPI_FUNC(int) PyFrame_FastToLocalsWithError(PyFrameObject *f);
PyAPI_FUNC(void) PyFrame_FastToLocals(PyFrameObject *);

PyAPI_FUNC(int) PyFrame_ClearFreeList(void);

PyAPI_FUNC(void) _PyFrame_DebugMallocStats(FILE *out);

/* Return the line of code the frame is currently executing. */
PyAPI_FUNC(int) PyFrame_GetLineNumber(PyFrameObject *);

PyAPI_FUNC(int) PyFrame_GetLasti(PyFrameObject *);

PyAPI_FUNC(int) PyFrame_CanIntrospect(PyFrameObject *);

#ifdef __cplusplus
}
#endif
#endif /* !Py_FRAMEOBJECT_H */
#endif /* Py_LIMITED_API */
