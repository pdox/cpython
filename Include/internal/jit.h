#ifdef __cplusplus
extern "C" {
#endif

/* JIT data attached to a PyCodeObject */
typedef void* CallTarget;
typedef void* JumpTarget;
typedef struct _JITData {
    CallTarget entry;
    JumpTarget j_error;
    JumpTarget j_dispatch_opcode;
    JumpTarget j_fast_yield;
    JumpTarget j_fast_block_end;
    JumpTarget j_unwind_cleanup;
    JumpTarget j_next_opcode;
    JumpTarget j_ret;
    JumpTarget jmptab[1]; // variable-size
} JITData;

typedef struct _EvalContext {
    const _Py_CODEUNIT *next_instr;
    PyFrameObject *f;
    int opcode;        /* Current opcode */
    int oparg;         /* Current opcode argument, if any */
    void *jit_ret_addr; /* Used by the JIT internally. */

    unsigned why; /* Reason for block stack unwind */
    PyObject **stack_pointer;  /* Next free slot in value stack */
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

#define set_return_address(addr) \
    (*(((void**)(ctx->jit_ret_addr) - 1)) = (void*)(addr))

int _PyJIT_Execute(EvalContext *ctx, PyFrameObject *f);

#ifdef __cplusplus
}
#endif
