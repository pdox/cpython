#ifdef __cplusplus
extern "C" {
#endif

#include "ir.h"

#define JIT_RC_FLOW            0
#define JIT_RC_JUMP            1
#define JIT_RC_FAST_YIELD      2
#define JIT_RC_FAST_BLOCK_END  3
#define JIT_RC_ERROR           4
#define JIT_RC_UNWIND_CLEANUP  5
#define JIT_RC_NEXT_OPCODE     6
#define JIT_RC_EXIT            7
// EXIT needs to be the highest value

typedef struct _EvalContext {
    int next_instr_index;  /* Next instruction index. Only used during jumps and special instructions. */
    PyObject **stack_pointer; /* Only used for subroutine calls */

    int why; /* Reason for block stack unwind */
    PyObject **fastlocals, **freevars;
    PyObject *retval;            /* Return value */
    PyThreadState *tstate;
    PyCodeObject *co;

    /* when tracing we set things up so that

           not (instr_lb <= current_bytecode_offset < instr_ub)

       is true when the line being executed has changed.  The
       initial values are such as to make this false the first
       time it is tested. */
    int instr_ub, instr_lb, instr_prev;

    const _Py_CODEUNIT *first_instr;
    PyObject *names;
    PyObject *consts;
#ifdef DXPAIRS
    int lastopcode;
#endif
} EvalContext;

/* JIT data attached to a PyCodeObject */
struct _JITData;
typedef struct _JITData JITData;
typedef void (*PyJITEntryFunction)(EvalContext *ctx, PyFrameObject *f, PyObject **sp);
typedef void (*PyJITEmitHandlerFunction)(JITData *jd, int opcode);

typedef struct move_entry {
    ir_label from_label;
    ir_label to_label;
    struct move_entry *next;
} move_entry;

typedef struct _JITData {
    PyJITEntryFunction entry;
    PyCodeObject *co; /* Borrowed reference */
    ir_context context;
    ir_func func;

    ir_value rv;
    ir_value ctx;
    ir_value f;
    ir_value stack_pointer;
    ir_value fastlocals;
    ir_value retval; /* corresponding to ctx->retval */
    ir_value why;    /* corresponding to ctx->why */

    /* Some common function signature types:

       v = void
       i = int
       z = Py_ssize_t
       o = PyObject*
       p = PyObject**

       The first letter indicates the return type.
    */
    ir_type sig_oo;
    ir_type sig_ooo;
    ir_type sig_oooo;
    ir_type sig_io;
    ir_type sig_ioo;
    ir_type sig_iooo;

    /* Blocks that will be moved to the end */
    move_entry *move_entry_list;

    /* Private storage for each opcode */
    void *priv[256];

    /* Code to emit exceptional handlers for each opcode */
    PyJITEmitHandlerFunction handlers[256];

    ir_label j_special[JIT_RC_EXIT + 1];
    ir_label j_special_internal[JIT_RC_EXIT + 1];
    ir_label jmptab[1];
} JITData;


typedef void (*PyJITEmitterFunction)(JITData *jd, int next_instr_index, int opcode, int oparg);
typedef void (*PyJITSpecialEmitterFunction)(JITData *jd);

int _PyJIT_Execute(EvalContext *ctx, PyFrameObject *f, PyObject **sp);

#ifdef __cplusplus
}
#endif
