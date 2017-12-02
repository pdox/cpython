#ifdef __cplusplus
extern "C" {
#endif

#include <jit/jit.h>

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
    void *jit_ret_addr; /* Used by the JIT internally. */
    PyObject **stack_pointer; /* Only used for subroutine calls */

    unsigned why; /* Reason for block stack unwind */
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
    jit_label_t from_label;
    jit_label_t to_label;
    struct move_entry *next;
} move_entry;

typedef struct _JITData {
    PyJITEntryFunction entry;
    PyCodeObject *co; /* Borrowed reference */
    jit_function_t func;
    jit_value_t rv;

    jit_value_t ctx;
    jit_value_t f;
    jit_value_t stack_pointer;
    jit_value_t fastlocals;

    /* Blocks that will be moved to the end */
    move_entry *move_entry_list;

    /* Private storage for each opcode */
    void *priv[256];

    /* Code to emit exceptional handlers for each opcode */
    PyJITEmitHandlerFunction handlers[256];

    jit_label_t j_special[JIT_RC_EXIT + 1];
    jit_label_t j_special_internal[JIT_RC_EXIT + 1];
    jit_label_t jmptab[1];
} JITData;


typedef void (*PyJITEmitterFunction)(JITData *jd, int next_instr_index, int opcode, int oparg);
typedef void (*PyJITSpecialEmitterFunction)(JITData *jd);

int _PyJIT_Execute(EvalContext *ctx, PyFrameObject *f, PyObject **sp);

#ifdef __cplusplus
}
#endif
