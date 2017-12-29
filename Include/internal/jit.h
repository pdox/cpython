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

struct _frame;
typedef struct _frame PyFrameObject;
int _PyJIT_Execute(EvalContext *ctx, PyFrameObject *f, PyObject **sp);

/* Opaque type */
struct _pyjit_callsite;
typedef struct _pyjit_callsite _pyjit_callsite;

_pyjit_callsite*
_pyjit_new_fastcall(size_t nargs, PyObject *kwnames);

ir_value
_pyjit_load_entrypoint(ir_func func, _pyjit_callsite *callsite);

#ifdef __cplusplus
}
#endif
