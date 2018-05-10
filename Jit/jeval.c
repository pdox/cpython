/* This is required to get MAP_ANONYMOUS with std=c99 */
#define _DEFAULT_SOURCE
#include <sys/mman.h>
#include <stddef.h>

#include "Python.h"
#include "opcode.h"
#include "frameobject.h"
#include "Include/internal/ceval.h"
#include "Include/internal/pystate.h"

#include "Jit/attrcache.h"
#include "Jit/entrypoint.h"
#include "Jit/ir.h"
#include "Jit/jit_macros.h"
#include "Jit/jit_eval_entrypoint.h"
#include "Jit/jit_locals_hack.h"

/* JIT data attached to a PyCodeObject */
struct _JITData;
typedef struct _JITData JITData;
typedef void (*PyJITEmitHandlerFunction)(JITData *jd, int opcode);
typedef void (*PyJITEmitterFunction)(JITData *jd, int next_instr_index, int opcode, int oparg);
typedef void (*PyJITSpecialEmitterFunction)(JITData *jd);

/* Because trampolines need to have the callable and callsite signature as their first
   two arguments, it makes sense to ignore the first two arguments of the direct
   entrypoint to match the signature the trampoline. By doing this, we can avoid
   shifting arguments around for the most common calls. */
#define ARG_SHIFT 2

typedef struct resume_entry {
    int f_lasti;
    ir_label resume_point;
    struct resume_entry *next;
} resume_entry;

/* These are kept around inside the PyJITFunctionObject,
   so that they can be released when the function is
   deallocated. (see _jeval_cleanup) */
typedef struct {
    Py_ssize_t load_global_cache_size;
    PyObject **load_global_cache;
    PyDictListener *globals_dl;
    PyDictListener *builtins_dl;
} _JITState;

typedef struct _JITData {
    PyJITFunctionObject *jit_function;
    _JITState *jstate;
    PyObject *globals;
    PyObject *builtins;
    PyCodeObject *co; /* Borrowed reference */
    Py_ssize_t inst_count;

    /* Code info */
    int is_generator;
    int has_vararg;
    int has_kwdict;
    int has_closure;
    int has_locals;
    Py_ssize_t total_pyargs;
    Py_ssize_t total_direct_args;

    /* CodeGen settings */
    int use_frame_object;
    int use_virtual_locals;
    int update_blockstack; /* Keep f_blockstack in the state ceval expects.
                              Needed for generators, where execution can switch between
                              the JIT and the interpreter (e.g. if tracing is enabled
                              while yielding) */

    ir_context context;
    ir_type func_sig;
    ir_func func;

    /* Some common function signature types:

       v = void
       i = int
       z = Py_ssize_t
       o = PyObject*
       p = PyObject**
       f = PyFrameObject*

       The first letter indicates the return type.
    */
    ir_type sig_o;
    ir_type sig_oo;
    ir_type sig_ooo;
    ir_type sig_oooo;
    ir_type sig_ooooo;
    ir_type sig_ooooooo;
    ir_type sig_oov;
    ir_type sig_vp;
    ir_type sig_i;
    ir_type sig_io;
    ir_type sig_ioo;
    ir_type sig_iooo;
    ir_type sig_ioi; /* for _do_eval_break() helper */
    ir_type sig_if;  /* for PyTraceBack_Here */
    ir_type sig_ol;  /* for PyLong_FromLong */
    ir_type dealloc_sig;
    ir_type blocksetup_sig;
    ir_type blockpop_sig;
    ir_type exc_triple_sig;

    /* CONSTANT_PYOBJ(NULL) */
    ir_value null;

    ir_label body_start;
    ir_label error_exit;
    ir_label exit;
    ir_label unknown_handler;

    /* Temporary stack is used for call_function */
    ir_value tmpstack;
    size_t tmpstack_size;

    /* Bytecode instruction boundary labels */
    ir_label *jmptab;

    ir_value f;
    ir_value throwflag;
    ir_value retval;

    ir_value tstate;
    ir_value rf;
    ir_value fastlocals;

    /* Used to keep track of generator resumes */
    resume_entry *resume_entry_list;

    /* Used during stack lowering pass */
    int stack_size;
    ir_value *stack_values;

    /* Virtual locals allocations */
    size_t fastlocals_size;
    ir_value *fastlocals_values;
} JITData;

#define ADD_RESUME_ENTRY(_f_lasti, _resume_point) do { \
    resume_entry *e = (resume_entry*)PyMem_RawMalloc(sizeof(resume_entry)); \
    e->f_lasti = (_f_lasti); \
    e->resume_point = (_resume_point); \
    e->next = jd->resume_entry_list; \
    jd->resume_entry_list = e; \
} while (0)

#define GOTO_ERROR_IF(val) do { \
    IR_LABEL_INIT(skip_error); \
    BRANCH_IF_NOT((val), skip_error, IR_LIKELY); \
    ir_goto_error(jd->func, jd->unknown_handler); \
    LABEL(skip_error); \
} while (0)

#define GOTO_ERROR_IF_EX(val, handler, with_stack_level, with_pb) do { \
    ir_cursor_set_pyblock(jd->func, (with_stack_level), (with_pb)); \
    IR_LABEL_INIT(skip_error); \
    BRANCH_IF_NOT((val), skip_error, IR_LIKELY); \
    ir_goto_error(jd->func, (handler)); \
    LABEL(skip_error); \
    ir_cursor_clear_pyblock(jd->func); \
} while (0)

#define GOTO_ERROR_IF_NOT(val)   GOTO_ERROR_IF(NOTBOOL((val)))

#define GOTO_ERROR()             ir_goto_error(jd->func, jd->unknown_handler)

#define GOTO_ERROR_EX(handler, with_stack_level, with_pb) do { \
    ir_cursor_set_pyblock(jd->func, (with_stack_level), (with_pb)); \
    ir_goto_error(jd->func, (handler)); \
    ir_cursor_clear_pyblock(jd->func); \
} while (0)

#define GOTO_EXIT()              BRANCH(jd->exit)

#define GOTO_FAST_BLOCK_END(why) \
    ir_goto_fbe(jd->func, IR_WHY_ ## why, jd->unknown_handler, NULL)

#define GOTO_FAST_BLOCK_END_CONTINUE(continue_target) \
    ir_goto_fbe(jd->func, IR_WHY_CONTINUE, jd->unknown_handler, (continue_target))

/* Issue a fast_block_end, but with a pre-computed stack level and pyblock */
#define GOTO_FAST_BLOCK_END_EX(why, handler, with_stack_level, with_pb) do { \
    ir_cursor_set_pyblock(jd->func, (with_stack_level), (with_pb)); \
    ir_goto_fbe(jd->func, IR_WHY_ ## why, (handler), NULL); \
    ir_cursor_clear_pyblock(jd->func); \
} while (0)

#define GOTO_FAST_BLOCK_END_CONTINUE_EX(handler, continue_target, with_stack_level, with_pb) do { \
    ir_cursor_set_pyblock(jd->func, (with_stack_level), (with_pb)); \
    ir_goto_fbe(jd->func, IR_WHY_CONTINUE, (handler), (continue_target)); \
    ir_cursor_clear_pyblock(jd->func); \
} while (0)

ir_label _instr_index_to_label(JITData *jd, int instr_index) {
    assert(instr_index >= 0);
    assert(instr_index < jd->inst_count);
    return jd->jmptab[instr_index];
}

int _label_to_instr_index(JITData *jd, ir_label target) {
    Py_ssize_t i;
    for (i = 0; i < jd->inst_count; i++) {
        if (jd->jmptab[i] == target) {
            break;
        }
    }
    assert(i < jd->inst_count);
    return i;
}

/* Scan bytecode in order, automatically resolving EXTENDED_ARG.

   Usage:
       SCAN_BYTECODE(co, {
           ... code here ...
       });

   Inside the loop, the following variables are available:
       opcode - the opcode (never EXTENDED_ARG)
       oparg - the argument to the opcode
       opfirst - the index of the first code unit for this instruction
       oplast - the index of the last code unit for this instruction
 */

#define SCAN_BYTECODE(co, loop_body) do { \
    _Py_CODEUNIT *_code = (_Py_CODEUNIT*)PyBytes_AS_STRING((co)->co_code); \
    Py_ssize_t _inst_count = PyBytes_GET_SIZE((co)->co_code)/sizeof(_Py_CODEUNIT); \
    Py_ssize_t _inst_index; \
    for (_inst_index = 0; _inst_index < _inst_count; _inst_index++) { \
        int opcode = _Py_OPCODE(_code[_inst_index]); \
        int oparg = _Py_OPARG(_code[_inst_index]); \
        int opfirst = _inst_index; \
        int oplast; \
        (void)opfirst; \
        (void)oplast; \
        while (opcode == EXTENDED_ARG) { \
            ++_inst_index; \
            opcode = _Py_OPCODE(_code[_inst_index]); \
            oparg = (oparg << 8) | _Py_OPARG(_code[_inst_index]); \
        } \
        oplast = _inst_index; \
        { loop_body } \
    } \
} while (0)


/* Fetch the previous instruction.
   Places the values in 'opcode_ptr' and 'oparg_ptr' respectively. (both int*)
   This transparently handles EXTENDED_ARG.
 */
#define FETCH_PREV_INSTR(opcode_ptr, oparg_ptr) \
    _fetch_prev_instr(jd, next_instr_index, (opcode_ptr), (oparg_ptr))

static void _fetch_prev_instr(JITData *jd, int next_instr_index, int *opcode_ptr, int *oparg_ptr) {
    _Py_CODEUNIT *code = (_Py_CODEUNIT*)PyBytes_AS_STRING(jd->co->co_code);

    /* Skip EXTENDED_ARG for the current instruction */
    int i = next_instr_index - 2;
    while (i >= 0 && _Py_OPCODE(code[i]) == EXTENDED_ARG) --i;
    if (i < 0) {
        Py_FatalError("FETCH_PREV_INSTR called, but no previous instruction exists");
    }

    /* i is now the index of the previous instruction */
    *opcode_ptr = _Py_OPCODE(code[i]);

    /* The previous instruction may have EXTENDED_ARG. Find the first one. */
    while (i > 0 && _Py_OPCODE(code[i-1]) == EXTENDED_ARG) --i;

    int oparg = _Py_OPARG(code[i]);
    while (_Py_OPCODE(code[i]) == EXTENDED_ARG) {
        i++;
        oparg = (oparg << 8) | _Py_OPARG(code[i]);
    }
    assert(_Py_OPCODE(code[i]) == *opcode_ptr);
    *oparg_ptr = oparg;
}

#define DO_SETUP_BLOCK(irkind, arg) do { \
    int _instr_index = (arg) / sizeof(_Py_CODEUNIT); \
    ir_label _target = _instr_index_to_label(jd, _instr_index); \
    ir_setup_block(jd->func, IR_PYBLOCK_ ## irkind, _target); \
} while (0)

#define SETUP_EXCEPT_HANDLER_BLOCK() do { \
    ir_setup_block(jd->func, IR_PYBLOCK_EXCEPT_HANDLER, NULL); \
} while (0)

#define DO_POP_BLOCK(irkind) do { \
    ir_pop_block(jd->func, IR_PYBLOCK_ ## irkind); \
} while (0)

#define REMOTE_SECTION(codeblock) do { \
    IR_LABEL_INIT(over_remote_section); \
    BRANCH(over_remote_section); \
    { codeblock }; \
    LABEL(over_remote_section); \
} while (0)

/* Stack operations */

#define FRAMEPTR()    (assert(jd->use_frame_object), jd->f)
#define FRAMEMAT()    (jd->use_frame_object ? jd->f : _materialize_frame(jd))

static inline JVALUE _materialize_frame(JITData *jd) {
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyframeobject_ptr, ir_type_pyrunframe_ptr);
    return CALL_NATIVE(sig, PyRunFrame_ToFrame, jd->rf);
}

#define RUNFRAMEPTR() (jd->rf)

#define STACKADJ(n)   ir_stackadj(jd->func, (n))

#define PEEK(n)       ir_stack_peek(jd->func, (n))
#define PUT(n, v)     ir_stack_put(jd->func, (n), (v))

#define TOP()     PEEK(1)
#define SECOND()  PEEK(2)
#define THIRD()   PEEK(3)
#define FOURTH()  PEEK(4)

#define SET_TOP(v)     PUT(1, (v))
#define SET_SECOND(v)  PUT(2, (v))
#define SET_THIRD(v)   PUT(3, (v))
#define SET_FOURTH(v)  PUT(4, (v))

#define PUSH(objval) do { \
    PUT(0, (objval)); \
    STACKADJ(1); \
} while (0)

/* Warning: Swapping stackadj/peek here may break ir_yield value substitution */
#define POP()          (STACKADJ(-1), PEEK(0))

#define GETNAME(i)   PyTuple_GET_ITEM(jd->co->co_names, (i))
#define LOAD_FREEVAR(i) \
    (jd->use_virtual_locals ? \
        LOAD(jd->fastlocals_values[jd->co->co_nlocals + (i)]) \
        : LOAD_AT_INDEX(jd->fastlocals, CONSTANT_INT(jd->co->co_nlocals + (i))))

/* ------------------- Macros and include copied from ceval.c ------------------- */
#define GIL_REQUEST _Py_atomic_load_relaxed(&_PyRuntime.ceval.gil_drop_request)

/* This can set eval_breaker to 0 even though gil_drop_request became
   1.  We believe this is all right because the eval loop will release
   the GIL eventually anyway. */
#define COMPUTE_EVAL_BREAKER() \
    _Py_atomic_store_relaxed( \
        &_PyRuntime.ceval.eval_breaker, \
        GIL_REQUEST | \
        _Py_atomic_load_relaxed(&_PyRuntime.ceval.pending.calls_to_do) | \
        _PyRuntime.ceval.pending.async_exc)

#define UNSIGNAL_ASYNC_EXC() \
    do { \
        _PyRuntime.ceval.pending.async_exc = 0; \
        COMPUTE_EVAL_BREAKER(); \
    } while (0)

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#include "pythread.h"
/* ---------------------------- End copy from ceval.c ------------------------------- */

void take_gil(PyThreadState *);
void drop_gil(PyThreadState *);

/* Do eval break. Return 0 on success, -1 on exception. */
static int _do_eval_break(PyCodeObject *co, int next_instr_index) {
    PyThreadState *tstate = PyThreadState_GET();
    assert(!PyErr_Occurred());

    /* Two cases where we skip running signal handlers and other
       pending calls:
       - If we're about to enter the try: of a try/finally (not
         *very* useful, but might help in some cases and it's
         traditional)
       - If we're resuming a chain of nested 'yield from' or
         'await' calls, then each frame is parked with YIELD_FROM
         as its next opcode. If the user hit control-C we want to
         wait until we've reached the innermost frame before
         running the signal handler and raising KeyboardInterrupt
         (see bpo-30039).
    */
    _Py_CODEUNIT *code = (_Py_CODEUNIT*)PyBytes_AS_STRING(co->co_code);
    assert(next_instr_index >= 0);
    assert((size_t)next_instr_index < PyBytes_GET_SIZE(co->co_code) / sizeof(_Py_CODEUNIT));
    if (_Py_OPCODE(code[next_instr_index]) == SETUP_FINALLY ||
        _Py_OPCODE(code[next_instr_index]) == YIELD_FROM) {
        return 0;
    }
    /* Do periodic things.  Doing this every time through
       the loop would add too much overhead, so we do it
       only every Nth instruction.  We also do it if
       ``pendingcalls_to_do'' is set, i.e. when an asynchronous
       event needs attention (e.g. a signal handler or
       async I/O handler); see Py_AddPendingCall() and
       Py_MakePendingCalls() above. */

    if (_Py_atomic_load_relaxed(
                &_PyRuntime.ceval.pending.calls_to_do))
    {
        if (Py_MakePendingCalls() < 0)
            return -1;
    }
    if (_Py_atomic_load_relaxed(
                &_PyRuntime.ceval.gil_drop_request))
    {
        /* Give another thread a chance */
        if (PyThreadState_Swap(NULL) != tstate)
            Py_FatalError("ceval: tstate mix-up");
        drop_gil(tstate);

        /* Other threads may run now */

        take_gil(tstate);

        /* Check if we should make a quick exit. */
        if (_Py_IsFinalizing() &&
            !_Py_CURRENTLY_FINALIZING(tstate))
        {
            drop_gil(tstate);
            PyThread_exit_thread();
        }

        if (PyThreadState_Swap(tstate) != NULL)
            Py_FatalError("ceval: orphan tstate");
    }
    /* Check for asynchronous exceptions. */
    if (tstate->async_exc != NULL) {
        PyObject *exc = tstate->async_exc;
        tstate->async_exc = NULL;
        UNSIGNAL_ASYNC_EXC();
        PyErr_SetNone(exc);
        Py_DECREF(exc);
        return -1;
    }
    return 0;
}

// TODO: This needs to be adjusted depending on the configuration of _Py_atomic_int
#define LOAD_EVAL_BREAKER() \
    LOAD(CONSTANT_PTR(ir_type_int_ptr, &_PyRuntime.ceval.eval_breaker._value))

static inline void _emit_eval_break_check(JITData *jd, ir_value next_instr_index) {
    IF(LOAD_EVAL_BREAKER(), IR_UNLIKELY, {
        JVALUE res = CALL_NATIVE(jd->sig_ioi, _do_eval_break, CONSTANT_PYOBJ((PyObject*)jd->co), next_instr_index);
        if (!Py_JITNoExc) {
            GOTO_ERROR_IF(res);
        }
    });
}

/* Check eval breaker, and call handler if set. */
#define HARD_EVAL_BREAK_CHECK_EX(_next_instr_index)  _emit_eval_break_check(jd, CONSTANT_INT(_next_instr_index))
#define HARD_EVAL_BREAK_CHECK()                      HARD_EVAL_BREAK_CHECK_EX(next_instr_index)
#define SOFT_EVAL_BREAK_CHECK() do { \
    if (Py_JITEvalBreaks) { \
        HARD_EVAL_BREAK_CHECK(); \
    } \
} while (0)

#define IR_PyErr_ExceptionMatches(exc) \
    CALL_NATIVE(jd->sig_io, PyErr_ExceptionMatches, CONSTANT_PYOBJ(exc))

#define TYPE_CHECK(typeval, expected_type, branch_if_not, likelyhood) \
    BRANCH_IF(CMP_NE((typeval), CONSTANT_PTR(ir_type_pytypeobject_ptr, &(expected_type))), (branch_if_not), (likelyhood))

void format_exc_check_arg(PyObject *, const char *, PyObject *);

#define RTCALL_NZ(sig, native_func, ...)  _rtcall_nz(jd, CALL_NATIVE((sig), (native_func), ##__VA_ARGS__))
#define RTCALL_Z(sig, native_func, ...)   _rtcall_z(jd, CALL_NATIVE((sig), (native_func), ##__VA_ARGS__))
#define RTCALL_GE0(sig, native_func, ...) _rtcall_ge0(jd, CALL_NATIVE((sig), (native_func), ##__VA_ARGS__))

static inline ir_value _rtcall_nz(JITData *jd, ir_value v) {
    if (!Py_JITNoExc) {
        GOTO_ERROR_IF_NOT(v);
    }
    return v;
}

static inline ir_value _rtcall_z(JITData *jd, ir_value v) {
    if (!Py_JITNoExc) {
        GOTO_ERROR_IF(v);
    }
    return v;
}

static inline ir_value _rtcall_ge0(JITData *jd, ir_value v) {
    assert(ir_type_is_integral(ir_typeof(v)));
    if (!Py_JITNoExc) {
        ir_value zero = ir_constant_from_uint64(jd->func, ir_typeof(v), 0, NULL);
        GOTO_ERROR_IF_NOT(CMP_GE(v, zero));
    }
    return v;
}

#define RTCALL_PyDict_New()  RTCALL_NZ(jd->sig_o, PyDict_New)

#define CALL_format_exc_check_arg(exc, msg, name) do { \
    JTYPE _sig = CREATE_SIGNATURE(ir_type_void, ir_type_pyobject_ptr, ir_type_char_ptr, ir_type_pyobject_ptr); \
    CALL_NATIVE( \
        _sig, \
        format_exc_check_arg, \
        CONSTANT_PYOBJ(exc), \
        CONSTANT_PTR(ir_type_char_ptr, msg), \
        CONSTANT_PYOBJ(name)); \
} while (0)

extern void format_exc_unbound(PyCodeObject *co, int oparg);

#define CALL_format_exc_unbound(arg) do { \
    JTYPE _sig = CREATE_SIGNATURE(ir_type_void, ir_type_void_ptr, ir_type_int); \
    CALL_NATIVE(_sig, format_exc_unbound, CONSTANT_PTR(ir_type_void_ptr, jd->co), CONSTANT_INT(arg)); \
} while (0)

#define CALL_PyObject_CallNoArg(func) ({ \
    JTYPE _sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr_ptr, ir_type_pyssizet, ir_type_pyobject_ptr); \
    CALL_NATIVE(_sig, _PyObject_FastCallDict, (func), CONSTANT_PTR(ir_type_pyobject_ptr_ptr, NULL), CONSTANT_PYSSIZET(0), CONSTANT_PYOBJ(NULL)); \
})

#define CALL_PyLong_AsLong(objval) ({ \
    JVALUE _val = (objval); \
    JTYPE _sig = CREATE_SIGNATURE(ir_type_long, ir_type_pyobject_ptr); \
    CALL_NATIVE(_sig, PyLong_AsLong, _val); \
})

#define CALL_PyErr_Fetch(p_exc, p_val, p_tb) do { \
    CALL_NATIVE(jd->exc_triple_sig, PyErr_Fetch, (p_exc), (p_val), (p_tb)); \
} while (0)

#define CALL_PyErr_NormalizeException(p_exc, p_val, p_tb) do { \
    CALL_NATIVE(jd->exc_triple_sig, PyErr_NormalizeException, (p_exc), (p_val), (p_tb)); \
} while (0)

#define CALL_PyException_SetTraceback(val, tb) \
    CALL_NATIVE(jd->sig_ioo, PyException_SetTraceback, (val), (tb))

#define CALL_PyLong_FromLong(v) \
    CALL_NATIVE(jd->sig_ol, PyLong_FromLong, (v))

/* Functions borrowed from ceval */
int do_raise(PyObject *, PyObject *);

/* Defeat nested macro evaluation problem.
   TODO: Can this be done in a less hacky way?
 */
#define EMITTER_FOR(op) \
    EMITTER_FOR_BASE(_ ## op)

#define EMITTER_FOR_BASE(op) \
    void _PyJIT_EMIT_TARGET##op (JITData *jd, int next_instr_index, int opcode, int oparg)

#define EMIT_JUMP(check_eval_breaker) do { \
    if (check_eval_breaker) { \
        HARD_EVAL_BREAK_CHECK(); \
    } \
    BRANCH(jd->jmptab[next_instr_index]); \
} while (0)

/* Emits immediate jump. */
#define JUMPTO(x, check_eval_breaker) do { \
    next_instr_index = (x) / sizeof(_Py_CODEUNIT); \
    EMIT_JUMP(check_eval_breaker); \
} while (0)

/* Emits immediate jump. Does not check eval_breaker. */
#define JUMPBY(x, check_eval_breaker) do { \
    next_instr_index += (x) / sizeof(_Py_CODEUNIT); \
    EMIT_JUMP(check_eval_breaker); \
} while (0)

#define INSTR_OFFSET()  (next_instr_index * sizeof(_Py_CODEUNIT))

#define SET_RETVAL(val) \
    STORE(jd->retval, (val))

#define LOAD_F_GLOBALS() \
    (jd->use_frame_object ? \
     LOAD_FIELD(FRAMEPTR(), PyFrameObject, f_globals, ir_type_pyobject_ptr) : \
     CONSTANT_PYOBJ(jd->globals))

#define LOAD_F_BUILTINS() \
    (jd->use_frame_object ? \
     LOAD_FIELD(FRAMEPTR(), PyFrameObject, f_builtins, ir_type_pyobject_ptr) : \
     CONSTANT_PYOBJ(jd->builtins))

#define LOAD_F_LOCALS() \
    (jd->use_frame_object ? \
     LOAD_FIELD(FRAMEPTR(), PyFrameObject, f_locals, ir_type_pyobject_ptr) : \
     LOAD_FIELD(RUNFRAMEPTR(), PyRunFrame, f_locals, ir_type_pyobject_ptr))

#define LOAD_F_LASTI() \
    (jd->use_frame_object ? \
     LOAD_FIELD(FRAMEPTR(), PyFrameObject, f_lasti_ceval, ir_type_int) : \
     LOAD_FIELD(RUNFRAMEPTR(), PyRunFrame, f_lasti, ir_type_int))

#define STORE_F_LASTI(val) do { \
    JVALUE _val = (val); \
    if (jd->use_frame_object) { \
        STORE_FIELD(FRAMEPTR(), PyFrameObject, f_lasti_ceval, ir_type_int, _val); \
    } else { \
        STORE_FIELD(RUNFRAMEPTR(), PyRunFrame, f_lasti, ir_type_int, _val); \
    } \
} while (0)

#define LOAD_F_PARTIAL()  LOAD_FIELD(FRAMEPTR(), PyFrameObject, f_partial, ir_type_char)

#define LOAD_F_LOCALSPLUS() ({ \
    IR_ASSERT(CMP_EQ(LOAD_F_PARTIAL(), CONSTANT_CHAR(0))); \
    ir_get_element_ptr(jd->func, FRAMEPTR(), offsetof(PyFrameObject, f_localsplus_), ir_type_pyobject_ptr, "f_localsplus_"); \
})

/* Status code for main loop (reason for stack unwind) */
enum why_code {
        WHY_NOT =       0x0001, /* No error */
        WHY_EXCEPTION = 0x0002, /* Exception occurred */
        WHY_RETURN =    0x0008, /* 'return' statement */
        WHY_BREAK =     0x0010, /* 'break' statement */
        WHY_CONTINUE =  0x0020, /* 'continue' statement */
        WHY_YIELD =     0x0040, /* 'yield' operator */
        WHY_SILENCED =  0x0080  /* Exception silenced by 'with' */
};

#define UNWIND_TO_NEW(curstack, level) do { \
    assert((curstack) >= (level)); \
    for (int _i = (curstack); _i > (level); _i--) { \
        XDECREF(POP()); \
    } \
} while (0)

#define UNWIND_BLOCK_NEW(curstack, pb) do { \
    UNWIND_TO_NEW((curstack), (pb)->b_level); \
} while (0)

static void _unwind_except_helper(PyObject *s_type, PyObject *s_value, PyObject *s_traceback) {
    PyObject *type, *value, *traceback;
    _PyErr_StackItem *exc_info;

    exc_info = PyThreadState_GET()->exc_info;
    type = exc_info->exc_type;
    value = exc_info->exc_value;
    traceback = exc_info->exc_traceback;
    exc_info->exc_type = s_type;
    exc_info->exc_value = s_value;
    exc_info->exc_traceback = s_traceback;
    Py_XDECREF(type);
    Py_XDECREF(value);
    Py_XDECREF(traceback);
}

#define UNWIND_EXCEPT_ONLY_NEW(curstack, pb) do { \
    assert((pb)->b_type == IR_PYBLOCK_EXCEPT_HANDLER); \
    UNWIND_TO_NEW((curstack), (pb)->b_level + 3); \
    JVALUE s_type = POP(); \
    JVALUE s_value = POP(); \
    JVALUE s_traceback = POP(); \
    JTYPE _sig = CREATE_SIGNATURE(ir_type_void, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr); \
    CALL_NATIVE(_sig, _unwind_except_helper, s_type, s_value, s_traceback); \
} while (0)

#define UNWIND_EXCEPT_HANDLER_NEW(curstack, pb) do { \
    assert((pb)->b_type == IR_PYBLOCK_EXCEPT_HANDLER); \
    UNWIND_TO_NEW((curstack), (pb)->b_level + 6); \
    JVALUE status = POP(); \
    int _level = (pb)->b_level + 5; \
    IF_ELSE(IR_PyLong_Check(status), IR_SEMILIKELY, { \
        JVALUE status_int = CAST(ir_type_int, CALL_PyLong_AsLong(status)); \
        IF_ELSE(CMP_EQ(status_int, CONSTANT_INT(WHY_SILENCED)), IR_SEMILIKELY, { \
            UNWIND_EXCEPT_ONLY_NEW(_level, pb); \
        }, { \
            UNWIND_BLOCK_NEW(_level, pb); \
        }); \
    }, { \
        IF_ELSE(IR_PyExceptionClass_Check(status), IR_SEMILIKELY, { \
            UNWIND_EXCEPT_ONLY_NEW(_level, pb); \
        }, { \
            IF_ELSE(CMP_EQ(status, CONSTANT_PYOBJ(Py_None)), IR_SEMILIKELY, { \
                UNWIND_BLOCK_NEW(_level, pb); \
            }, { \
                CALL_Py_FatalError("UNWIND_EXCEPT_HANDLER pops bad exception"); \
                CRASH(); \
            }); \
        }); \
    }); \
    DECREF(status); \
} while (0)

/* --- BEGIN EMITTERS --- */

EMITTER_FOR(INVALID_OPCODE) {
    Py_UNREACHABLE();
}

EMITTER_FOR(NOP) {
}

#define NAME_ERROR_MSG \
    "name '%.200s' is not defined"

#define UNBOUNDLOCAL_ERROR_MSG \
    "local variable '%.200s' referenced before assignment"

EMITTER_FOR(LOAD_FAST) {
    JVALUE v = ir_getlocal(jd->func, oparg, 1, jd->unknown_handler);
    INCREF(v);
    PUSH(v);
}

EMITTER_FOR(LOAD_CONST) {
    PyObject *obj = PyTuple_GET_ITEM(jd->co->co_consts, oparg);
    assert(obj != NULL);
    JVALUE v = CONSTANT_PYOBJ(obj);
    INCREF(v);
    PUSH(v);
}

EMITTER_FOR(STORE_FAST) {
    JVALUE v = POP();
    JVALUE tmp = ir_getlocal(jd->func, oparg, 0, jd->unknown_handler);
    ir_setlocal(jd->func, oparg, v);
    XDECREF(tmp);
}

EMITTER_FOR(POP_TOP) {
    JVALUE v = POP();
    DECREF(v);
}

EMITTER_FOR(ROT_TWO) {
    JVALUE top = TOP();
    JVALUE second = SECOND();
    SET_TOP(second);
    SET_SECOND(top);
}

EMITTER_FOR(ROT_THREE) {
    JVALUE top = TOP();
    JVALUE second = SECOND();
    JVALUE third = THIRD();
    SET_TOP(second);
    SET_SECOND(third);
    SET_THIRD(top);
}

EMITTER_FOR(DUP_TOP) {
    JVALUE top = TOP();
    INCREF(top);
    PUSH(top);
}

EMITTER_FOR(DUP_TOP_TWO) {
    JVALUE top = TOP();
    JVALUE second = SECOND();
    INCREF(top);
    INCREF(second);
    STACKADJ(2);
    SET_TOP(top);
    SET_SECOND(second);
}

#define EMIT_AS_UNARY_OP(op, func) \
    EMITTER_FOR_BASE(_ ## op) { \
        JVALUE objval = TOP(); \
        JVALUE res = RTCALL_NZ(jd->sig_oo, func, objval); \
        STACKADJ(-1); \
        DECREF(objval); \
        PUSH(res); \
        SOFT_EVAL_BREAK_CHECK(); \
    }

EMIT_AS_UNARY_OP(UNARY_POSITIVE, PyNumber_Positive)
EMIT_AS_UNARY_OP(UNARY_NEGATIVE, PyNumber_Negative)
EMIT_AS_UNARY_OP(UNARY_INVERT, PyNumber_Invert)

EMITTER_FOR(UNARY_NOT) {
    JVALUE value = TOP();
    JVALUE err = RTCALL_GE0(jd->sig_io, PyObject_IsTrue, value);
    STACKADJ(-1);
    DECREF(value);

    /* err >= 0 */
    JVALUE obj = TERNARY(err, CONSTANT_PYOBJ(Py_False), CONSTANT_PYOBJ(Py_True));
    INCREF(obj);
    PUSH(obj);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(BINARY_POWER) {
    JVALUE exp = TOP();
    JVALUE base = SECOND();
    JVALUE res = RTCALL_NZ(jd->sig_oooo, PyNumber_Power, base, exp, CONSTANT_PYOBJ(Py_None));
    STACKADJ(-2);
    DECREF(base);
    DECREF(exp);
    PUSH(res);
    SOFT_EVAL_BREAK_CHECK();
}

#define EMIT_AS_BINARY_OP(op, func) \
    EMITTER_FOR_BASE(_ ## op) { \
        JVALUE right = TOP(); \
        JVALUE left = SECOND(); \
        JVALUE res = RTCALL_NZ(jd->sig_ooo, func, left, right); \
        STACKADJ(-2); \
        DECREF(left); \
        DECREF(right); \
        PUSH(res); \
        SOFT_EVAL_BREAK_CHECK(); \
    }

EMIT_AS_BINARY_OP(BINARY_MULTIPLY, PyNumber_Multiply)
EMIT_AS_BINARY_OP(BINARY_MATRIX_MULTIPLY, PyNumber_MatrixMultiply)
EMIT_AS_BINARY_OP(BINARY_TRUE_DIVIDE, PyNumber_TrueDivide)
EMIT_AS_BINARY_OP(BINARY_FLOOR_DIVIDE, PyNumber_FloorDivide)
EMIT_AS_BINARY_OP(BINARY_MODULO, PyNumber_Remainder) /* ceval has specialization for unicode */
EMIT_AS_BINARY_OP(BINARY_ADD, PyNumber_Add) /* ceval has specialization for unicode */
EMIT_AS_BINARY_OP(BINARY_SUBTRACT, PyNumber_Subtract)
EMIT_AS_BINARY_OP(BINARY_SUBSCR, PyObject_GetItem)
EMIT_AS_BINARY_OP(BINARY_LSHIFT, PyNumber_Lshift)
EMIT_AS_BINARY_OP(BINARY_RSHIFT, PyNumber_Rshift)
EMIT_AS_BINARY_OP(BINARY_AND, PyNumber_And)
EMIT_AS_BINARY_OP(BINARY_XOR, PyNumber_Xor)
EMIT_AS_BINARY_OP(BINARY_OR, PyNumber_Or)


EMITTER_FOR(LIST_APPEND) {
    JVALUE v = TOP();
    JVALUE list = PEEK(oparg + 1);
    RTCALL_Z(jd->sig_ioo, PyList_Append, list, v);
    STACKADJ(-1);
    DECREF(v);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(SET_ADD) {
    JVALUE v = TOP();
    JVALUE set = PEEK(oparg + 1);
    RTCALL_Z(jd->sig_ioo, PySet_Add, set, v);
    STACKADJ(-1);
    DECREF(v);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(INPLACE_POWER) {
    JVALUE exp = TOP();
    JVALUE base = SECOND();
    JVALUE res = RTCALL_NZ(jd->sig_oooo, PyNumber_InPlacePower, base, exp, CONSTANT_PYOBJ(Py_None));
    STACKADJ(-2);
    DECREF(base);
    DECREF(exp);
    PUSH(res);
    SOFT_EVAL_BREAK_CHECK();
}

EMIT_AS_BINARY_OP(INPLACE_MULTIPLY, PyNumber_InPlaceMultiply)
EMIT_AS_BINARY_OP(INPLACE_MATRIX_MULTIPLY, PyNumber_InPlaceMatrixMultiply)
EMIT_AS_BINARY_OP(INPLACE_TRUE_DIVIDE, PyNumber_InPlaceTrueDivide)
EMIT_AS_BINARY_OP(INPLACE_FLOOR_DIVIDE, PyNumber_InPlaceFloorDivide)
EMIT_AS_BINARY_OP(INPLACE_MODULO, PyNumber_InPlaceRemainder)
EMIT_AS_BINARY_OP(INPLACE_ADD, PyNumber_InPlaceAdd)  /* ceval specializes for unicode */
EMIT_AS_BINARY_OP(INPLACE_SUBTRACT, PyNumber_InPlaceSubtract)
EMIT_AS_BINARY_OP(INPLACE_LSHIFT, PyNumber_InPlaceLshift)
EMIT_AS_BINARY_OP(INPLACE_RSHIFT, PyNumber_InPlaceRshift)
EMIT_AS_BINARY_OP(INPLACE_AND, PyNumber_InPlaceAnd)
EMIT_AS_BINARY_OP(INPLACE_XOR, PyNumber_InPlaceXor)
EMIT_AS_BINARY_OP(INPLACE_OR, PyNumber_InPlaceOr)

EMITTER_FOR(STORE_SUBSCR) {
    JVALUE sub = TOP();
    JVALUE container = SECOND();
    JVALUE v = THIRD();
    /* container[sub] = v */
    RTCALL_Z(jd->sig_iooo, PyObject_SetItem, container, sub, v);
    STACKADJ(-3);
    DECREF(v);
    DECREF(container);
    DECREF(sub);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(DELETE_SUBSCR) {
    JVALUE sub = TOP();
    JVALUE container = SECOND();
    /* del container[sub] */
    RTCALL_Z(jd->sig_ioo, PyObject_DelItem, container, sub);
    STACKADJ(-2);
    DECREF(container);
    DECREF(sub);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(PRINT_EXPR) {
    _Py_IDENTIFIER(displayhook);
    JVALUE value = TOP();
    JTYPE sig1 = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_void_ptr);
    // CUSTOM SITE
    JVALUE hook = CALL_NATIVE(sig1, _PySys_GetObjectId, CONSTANT_VOID_PTR(&PyId_displayhook));
    IF_NOT(hook, IR_UNLIKELY, {
        CALL_PyErr_SetString(PyExc_RuntimeError, "lost sys.displayhook");
        GOTO_ERROR();
    });
    JVALUE res = RTCALL_NZ(jd->sig_oooo, PyObject_CallFunctionObjArgs, hook, value, jd->null);
    DECREF(res);
    STACKADJ(-1);
    DECREF(value);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(RAISE_VARARGS) {
    JVALUE cause = CONSTANT_PYOBJ(NULL);
    JVALUE exc = CONSTANT_PYOBJ(NULL);
    JLABEL fin = JLABEL_INIT("fin");
    switch (oparg) {
    case 2:
        cause = POP(); /* fall through */
    case 1:
        exc = POP();   /* fall through */
    case 0: {
        JVALUE ret = CALL_NATIVE(jd->sig_ioo, do_raise, exc, cause);
        BRANCH_IF_NOT(ret, fin, IR_UNLIKELY);
        GOTO_FAST_BLOCK_END(EXCEPTION);
        break;
    }
    default:
        CALL_PyErr_SetString(PyExc_SystemError, "bad RAISE_VARARGS oparg");
        break;
    }
    LABEL(fin);
    GOTO_ERROR();
}

EMITTER_FOR(RETURN_VALUE) {
    SET_RETVAL(POP());
    GOTO_FAST_BLOCK_END(RETURN);
}

EMITTER_FOR(GET_AITER) {
    IR_LABEL_INIT(invalid_iter);
    JVALUE obj = TOP();
    JVALUE obj_type = IR_Py_TYPE(obj);
    JVALUE getter = ALLOCA(jd->sig_oo);
    STORE(getter, CONSTANT_PTR(jd->sig_oo, NULL));

    JVALUE tp_as_async = LOAD_FIELD(obj_type, PyTypeObject, tp_as_async, ir_type_pyasyncmethods_ptr);
    IF(tp_as_async, IR_LIKELY, {
        STORE(getter, LOAD_FIELD(tp_as_async, PyAsyncMethods, am_aiter, jd->sig_oo));
    });

    IF_NOT(LOAD(getter), IR_UNLIKELY, {
        SET_TOP(CONSTANT_PYOBJ(NULL));
        CALL_PyErr_Format(
            PyExc_TypeError,
            "'async for' requires an object with "
            "__aiter__ method, got %.100s",
            LOAD_FIELD(obj_type, PyTypeObject, tp_name, ir_type_char_ptr));
        DECREF(obj);
        GOTO_ERROR();
    });

    JVALUE iter = CALL_INDIRECT(LOAD(getter), obj);
    DECREF(obj);
    IF_NOT(iter, IR_UNLIKELY, {
        SET_TOP(CONSTANT_PYOBJ(NULL));
        GOTO_ERROR();
    });

    JVALUE iter_type = IR_Py_TYPE(iter);
    JVALUE iter_tp_as_async = LOAD_FIELD(iter_type, PyTypeObject, tp_as_async, ir_type_pyasyncmethods_ptr);
    BRANCH_IF_NOT(iter_tp_as_async, invalid_iter, IR_UNLIKELY);
    JVALUE iter_am_anext = LOAD_FIELD(iter_tp_as_async, PyAsyncMethods, am_anext, jd->sig_oo);
    BRANCH_IF_NOT(iter_am_anext, invalid_iter, IR_UNLIKELY);

    /* Good iterator, normal dispatch */
    SET_TOP(iter);
    SOFT_EVAL_BREAK_CHECK();

    REMOTE_SECTION({
        LABEL(invalid_iter);
        SET_TOP(CONSTANT_PYOBJ(NULL));
        CALL_PyErr_Format(
            PyExc_TypeError,
            "'async for' received an object from __aiter__ "
            "that does not implement __anext__: %.100s",
            LOAD_FIELD(iter_type, PyTypeObject, tp_name, ir_type_char_ptr));
        DECREF(iter);
        GOTO_ERROR();
    });
}

/* Handle the generic (not common) case for GET_ANEXT */
static PyObject *
_get_anext_helper(PyTypeObject *type, PyObject *aiter) {
    PyObject *next_iter = NULL;
    unaryfunc getter = NULL;
    PyObject *awaitable;

    if (type->tp_as_async != NULL){
        getter = type->tp_as_async->am_anext;
    }

    if (getter != NULL) {
        next_iter = (*getter)(aiter);
        if (next_iter == NULL) {
            return NULL;
        }
    }
    else {
        PyErr_Format(
            PyExc_TypeError,
            "'async for' requires an iterator with "
            "__anext__ method, got %.100s",
            type->tp_name);
        return NULL;
    }

    awaitable = _PyCoro_GetAwaitableIter(next_iter);
    if (awaitable == NULL) {
        _PyErr_FormatFromCause(
            PyExc_TypeError,
            "'async for' received an invalid object "
            "from __anext__: %.100s",
            Py_TYPE(next_iter)->tp_name);
        /* Fall through to decref and return NULL */
    }
    Py_DECREF(next_iter);
    return awaitable;
}

EMITTER_FOR(GET_ANEXT) {
    JLABEL skip_helper = JLABEL_INIT("skip_helper");
    JLABEL fin = JLABEL_INIT("fin");
    JVALUE aiter = TOP();
    JVALUE type = IR_Py_TYPE(aiter);
    JVALUE awaitableptr = ALLOCA(ir_type_pyobject_ptr);

    /* Fast path. Really hacky. If aiter is PyAsyncGen_Type exactly, skip the
       NULL checks and the call to _PyCoro_GetAwaitableIter.
     */
    JVALUE is_async_gen = CMP_EQ(type, CONSTANT_PTR(ir_type_pytypeobject_ptr, &PyAsyncGen_Type));
    BRANCH_IF(is_async_gen, skip_helper, IR_LIKELY);

    /* Slow path: use the helper */
    JTYPE helper_sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pytypeobject_ptr, ir_type_pyobject_ptr);
    STORE(awaitableptr, CALL_NATIVE(helper_sig, _get_anext_helper, type, aiter));
    BRANCH(fin);

    LABEL(skip_helper);
    /* Fast path: PyAsyncGen_CheckExact(aiter) is true */
    JVALUE tp_as_async = LOAD_FIELD(type, PyTypeObject, tp_as_async, ir_type_pyasyncmethods_ptr);
    JVALUE am_anext = LOAD_FIELD(tp_as_async, PyAsyncMethods, am_anext, jd->sig_oo);
    STORE(awaitableptr, CALL_INDIRECT(am_anext, aiter));
    BRANCH(fin);

    LABEL(fin);
    JVALUE awaitable = LOAD(awaitableptr);
    GOTO_ERROR_IF_NOT(awaitable);
    PUSH(awaitable);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(GET_AWAITABLE) {
    JLABEL fin = JLABEL_INIT("fin");
    JVALUE iterable = TOP();

    // TODO: Inline _PyCoro_GetAwaitableIter
    JVALUE iterptr = ALLOCA(ir_type_pyobject_ptr);
    STORE(iterptr, CALL_NATIVE(jd->sig_oo, _PyCoro_GetAwaitableIter, iterable));
    DECREF(iterable);

    // If this is a coroutine, ensure it isn't inside a yield from.
    // TODO: Turn this into a flag check.
    BRANCH_IF_NOT(LOAD(iterptr), fin, IR_UNLIKELY);
    BRANCH_IF_NOT(IR_PyCoro_CheckExact(LOAD(iterptr)), fin, IR_UNLIKELY);

    JVALUE yf = CALL_NATIVE(jd->sig_oo, _PyGen_yf, LOAD(iterptr));
    BRANCH_IF_NOT(yf, fin, IR_LIKELY);
    DECREF(yf);
    DECREF(LOAD(iterptr));
    STORE(iterptr, CONSTANT_PYOBJ(NULL));
    CALL_PyErr_SetString(
            PyExc_RuntimeError,
            "coroutine is being awaited already");
    BRANCH(fin);

    LABEL(fin);
    JVALUE iter = LOAD(iterptr);
    SET_TOP(iter);
    GOTO_ERROR_IF_NOT(iter);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(YIELD_FROM) {
    _Py_IDENTIFIER(send);
    IR_LABEL_INIT(fast_send);
    IR_LABEL_INIT(v_is_none);
    IR_LABEL_INIT(after_getting_yieldval);
    IR_LABEL_INIT(handle_null_yieldval);
    JVALUE v = POP();
    JVALUE receiver = TOP();
    JVALUE yieldval = ALLOCA(ir_type_pyobject_ptr);

    BRANCH_IF(
        LOGICAL_OR(IR_PyGen_CheckExact(receiver), IR_PyCoro_CheckExact(receiver)),
        fast_send,
        IR_LIKELY);

    /* Handle generic case */
    BRANCH_IF(CMP_EQ(v, CONSTANT_PYOBJ(Py_None)), v_is_none, IR_SEMILIKELY);
    /* v is not None */
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_void_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    STORE(yieldval,
        CALL_NATIVE(sig, _PyObject_CallMethodIdObjArgs,
            receiver, CONSTANT_PTR(ir_type_void_ptr, &PyId_send), v, CONSTANT_PYOBJ(NULL)));
    BRANCH(after_getting_yieldval);
    LABEL(v_is_none);
    JVALUE receiver_type = IR_Py_TYPE(receiver);
    JVALUE tp_iternext = LOAD_FIELD(receiver_type, PyTypeObject, tp_iternext, jd->sig_oo);
    STORE(yieldval, CALL_INDIRECT(tp_iternext, receiver));
    BRANCH(after_getting_yieldval);

    LABEL(fast_send);
    STORE(yieldval, CALL_NATIVE(jd->sig_ooo, _PyGen_Send, receiver, v));
    BRANCH(after_getting_yieldval);

    LABEL(after_getting_yieldval);
    DECREF(v);
    BRANCH_IF_NOT(LOAD(yieldval), handle_null_yieldval, IR_SOMETIMES);

    /* Receiver remains on the stack */
    ir_yield(jd->func,
             next_instr_index - 1,
             LOAD(yieldval),
             jd->exit,
             jd->jmptab[next_instr_index - 1],
             jd->jmptab[next_instr_index]);

    /* Exiting from 'yield from' */
    LABEL(handle_null_yieldval);
    /* Use the first tmpstack slot to receive 'val' */
    assert(jd->tmpstack_size >= 1);
    JVALUE valptr = jd->tmpstack;
    STORE(valptr, CONSTANT_PYOBJ(NULL));
    JTYPE sig2 = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr_ptr);
    RTCALL_GE0(sig2, _PyGen_FetchStopIterationValue, valptr);
    DECREF(receiver);
    SET_TOP(LOAD(valptr));
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(YIELD_VALUE) {
    JVALUE v = TOP();
    if (jd->co->co_flags & CO_ASYNC_GENERATOR) {
        JVALUE wrapped = RTCALL_NZ(jd->sig_oo, _PyAsyncGenValueWrapperNew, v);
        STACKADJ(-1);
        DECREF(v);
        v = wrapped;
    } else {
        STACKADJ(-1);
    }
    ir_yield(jd->func, next_instr_index, v, jd->exit, jd->jmptab[next_instr_index], NULL);
}

EMITTER_FOR(POP_EXCEPT) {
    DO_POP_BLOCK(EXCEPT_HANDLER);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(POP_BLOCK) {
    DO_POP_BLOCK(ANY);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(ENTER_FINALLY) {
    SETUP_EXCEPT_HANDLER_BLOCK();
    JVALUE none = CONSTANT_PYOBJ(Py_None);
    for (int i = 0; i < 6; i++) {
        INCREF(none);
        PUSH(none);
    }
}

EMITTER_FOR(END_FINALLY) {
    IR_LABEL_INIT(fallthrough);
    ir_end_finally(jd->func, fallthrough, jd->unknown_handler);
    LABEL(fallthrough);
    SOFT_EVAL_BREAK_CHECK();
}

static
PyObject *
_load_build_class_helper(PyObject *builtins) {
    _Py_IDENTIFIER(__build_class__);
    PyObject *bc;
    if (PyDict_CheckExact(builtins)) {
        bc = _PyDict_GetItemId(builtins, &PyId___build_class__);
        if (bc == NULL) {
            PyErr_SetString(PyExc_NameError,
                            "__build_class__ not found");
            return NULL;
        }
        Py_INCREF(bc);
    }
    else {
        PyObject *build_class_str = _PyUnicode_FromId(&PyId___build_class__);
        if (build_class_str == NULL)
            return NULL;
        bc = PyObject_GetItem(builtins, build_class_str);
        if (bc == NULL) {
            if (PyErr_ExceptionMatches(PyExc_KeyError))
                PyErr_SetString(PyExc_NameError,
                                "__build_class__ not found");
            return NULL;
        }
    }
    return bc;
}

EMITTER_FOR(LOAD_BUILD_CLASS) {
    JVALUE builtins = LOAD_F_BUILTINS();
    JVALUE bc = RTCALL_NZ(jd->sig_oo, _load_build_class_helper, builtins);
    PUSH(bc);
    SOFT_EVAL_BREAK_CHECK();
}

static int _do_store_name(PyObject *ns, PyObject *name, PyObject *v) {
    if (ns == NULL) {
        PyErr_Format(PyExc_SystemError,
                     "no locals found when storing %R", name);
        return 1;
    }
    if (PyDict_CheckExact(ns)) {
        return PyDict_SetItem(ns, name, v);
    } else {
        return PyObject_SetItem(ns, name, v);
    }
}

EMITTER_FOR(STORE_NAME) {
    PyObject *name = GETNAME(oparg);
    JVALUE v = TOP();
    JVALUE ns = LOAD_F_LOCALS();
    RTCALL_Z(jd->sig_iooo, _do_store_name, ns, CONSTANT_PYOBJ(name), v);
    STACKADJ(-1);
    DECREF(v);
    SOFT_EVAL_BREAK_CHECK();
}

static int _do_delete_name(PyObject *ns, PyObject *name) {
    if (ns == NULL) {
        PyErr_Format(PyExc_SystemError,
                     "no locals when deleting %R", name);
        return 1;
    }
    int err = PyObject_DelItem(ns, name);
    if (err != 0) {
        format_exc_check_arg(PyExc_NameError,
                             NAME_ERROR_MSG,
                             name);
        return 1;
    }
    return 0;
}

EMITTER_FOR(DELETE_NAME) {
    PyObject *name = GETNAME(oparg);
    assert(name);
    JVALUE ns = LOAD_F_LOCALS();
    RTCALL_Z(jd->sig_ioo, _do_delete_name, ns, CONSTANT_PYOBJ(name));
    SOFT_EVAL_BREAK_CHECK();
}

/* Rewrite exception in the same manner as unpack_iterable() from ceval.c */
static PyObject* _getiter_wrapped(PyObject *v) {
    PyObject *it = PyObject_GetIter(v);
    if (it == NULL) {
        if (PyErr_ExceptionMatches(PyExc_TypeError) &&
            v->ob_type->tp_iter == NULL && !PySequence_Check(v))
        {
            PyErr_Format(PyExc_TypeError,
                         "cannot unpack non-iterable %.200s object",
                         v->ob_type->tp_name);
        }
        return NULL;
    }
    return it;
}

EMITTER_FOR(UNPACK_SEQUENCE) {
    JVALUE seq = POP();
    JVALUE type = IR_Py_TYPE(seq);
    JLABEL dispatch = JLABEL_INIT("dispatch");
    JLABEL not_tuple = JLABEL_INIT("not_tuple");
    JLABEL generic_case = JLABEL_INIT("generic_case");

    /* Handle Tuple case first */
    TYPE_CHECK(type, PyTuple_Type, not_tuple, IR_SEMILIKELY);
    BRANCH_IF(CMP_NE(IR_PyTuple_GET_SIZE(seq), CONSTANT_PYSSIZET(oparg)),
              generic_case, IR_UNLIKELY);
    JVALUE tup_ob_item = IR_PyTuple_OB_ITEM(seq);
    for (int i = oparg - 1; i >= 0; i--) {
        JVALUE item = LOAD_AT_INDEX(tup_ob_item, CONSTANT_INT(i));
        INCREF(item);
        PUSH(item);
    }
    DECREF(seq);
    BRANCH(dispatch);

    /* Handle List case */
    LABEL(not_tuple);
    TYPE_CHECK(type, PyList_Type, generic_case, IR_UNLIKELY);
    BRANCH_IF(CMP_NE(IR_PyList_GET_SIZE(seq), CONSTANT_PYSSIZET(oparg)),
              generic_case, IR_UNLIKELY);
    JVALUE list_ob_item = IR_PyList_OB_ITEM(seq);
    for (int i = oparg - 1; i >= 0; i--) {
        JVALUE item = LOAD_AT_INDEX(list_ob_item, CONSTANT_INT(i));
        INCREF(item);
        PUSH(item);
    }
    DECREF(seq);
    BRANCH(dispatch);

    /* Generic case, iterator protocol. */
    LABEL(generic_case);
    JVALUE it = CALL_NATIVE(jd->sig_oo, _getiter_wrapped, seq);
    DECREF(seq);
    GOTO_ERROR_IF_NOT(it);

    /* We need to push objects onto the stack in the opposite
       order the iterator emits them, i.e. the first item the
       iterator returns will be at the top when we are done.
       So reserve the stack space, set them to NULL, and then
       proceed with the iteration. */
    JLABEL exhausted_too_early = JLABEL_INIT("exhausted_too_early");
    STACKADJ(oparg);
    for (int i = 0; i < oparg; i++) {
        PUT(1 + i, CONSTANT_PYOBJ(NULL));
    }
    JVALUE counter = ALLOCA(ir_type_int);
    STORE(counter, CONSTANT_INT(0));
    for (int i = 0; i < oparg; i++) {
        JVALUE item = CALL_NATIVE(jd->sig_oo, PyIter_Next, it);
        PUT(1 + i, item);
        BRANCH_IF_NOT(item, exhausted_too_early, IR_UNLIKELY);
        STORE(counter, ADD(LOAD(counter), CONSTANT_INT(1)));
    }
    /* Check once more, to ensure the iterator is exhausted */
    JVALUE final = CALL_NATIVE(jd->sig_oo, PyIter_Next, it);
    JLABEL too_many_values = JLABEL_INIT("too_many_values");
    DECREF(it);
    BRANCH_IF(final, too_many_values, IR_UNLIKELY);
    GOTO_ERROR_IF(IR_PyErr_Occurred());
    BRANCH(dispatch);

    LABEL(exhausted_too_early);
    DECREF(it);
    GOTO_ERROR_IF(IR_PyErr_Occurred());
    CALL_PyErr_Format(PyExc_ValueError,
                      "not enough values to unpack (expected %d, got %d)",
                      CONSTANT_INT(oparg), LOAD(counter));
    GOTO_ERROR();

    LABEL(too_many_values);
    DECREF(final);
    CALL_PyErr_Format(PyExc_ValueError,
        "too many values to unpack (expected %d)",
        CONSTANT_INT(oparg));
    GOTO_ERROR();

    LABEL(dispatch);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(UNPACK_EX) {
    int before = oparg & 0xFF;
    int after = oparg >> 8;
    int total = before + 1 + after;
    JVALUE seq = POP();
    JLABEL dispatch = JLABEL_INIT("dispatch");

    /* TODO: This could be specialized for tuple/list. */
    JVALUE it = CALL_NATIVE(jd->sig_oo, _getiter_wrapped, seq);
    DECREF(seq);
    GOTO_ERROR_IF_NOT(it);

    /* We need to push objects onto the stack in the opposite
       order the iterator emits them, i.e. the first item the
       iterator returns will be at the top when we are done.
       So reserve the stack space, set them to NULL, and then
       proceed with the iteration. */
    JLABEL exhausted_too_early = JLABEL_INIT("exhausted_too_early");
    STACKADJ(total);
    for (int i = 0; i < total; i++) {
        PUT(1 + i, CONSTANT_PYOBJ(NULL));
    }
    JVALUE counter = ALLOCA(ir_type_int);
    STORE(counter, CONSTANT_INT(0));
    for (int i = 0; i < before; i++) {
        JVALUE item = CALL_NATIVE(jd->sig_oo, PyIter_Next, it);
        PUT(1 + i, item);
        BRANCH_IF_NOT(item, exhausted_too_early, IR_UNLIKELY);
        STORE(counter, ADD(LOAD(counter), CONSTANT_INT(1)));
    }

    /* Convert the remaining items to a list */
    JVALUE l = CALL_NATIVE(jd->sig_oo, PySequence_List, it);
    DECREF(it);
    GOTO_ERROR_IF_NOT(l);
    PUT(1 + before, l);

    /* Verify we have enough items */
    JLABEL not_enough_values = JLABEL_INIT("not_enough_values");
    JVALUE ll = IR_PyList_GET_SIZE(l);
    STORE(counter, ADD(LOAD(counter), CAST(ir_type_int, ll)));
    BRANCH_IF(CMP_LT(ll, CONSTANT_PYSSIZET(after)), not_enough_values, IR_UNLIKELY);

    /* Move the "after-variable" args off the list */
    JVALUE list_ob_item = IR_PyList_OB_ITEM(l);
    for (int i = 0; i < after; i++) {
        JVALUE item = LOAD_AT_INDEX(list_ob_item, SUBTRACT(ll, CONSTANT_PYSSIZET(after - i)));
        PUT(2 + before + i, item);
    }
    /* Resize the list */
    IR_SET_Py_SIZE(l, SUBTRACT(ll, CONSTANT_PYSSIZET(after)));
    BRANCH(dispatch);

    LABEL(exhausted_too_early);
    DECREF(it);
    GOTO_ERROR_IF(IR_PyErr_Occurred());
    BRANCH(not_enough_values);

    LABEL(not_enough_values);
    CALL_PyErr_Format(PyExc_ValueError,
                      "not enough values to unpack "
                      "(expected at least %d, got %d)",
                      CONSTANT_INT(before + after), LOAD(counter));
    GOTO_ERROR();

    LABEL(dispatch);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(STORE_ATTR) {
    PyObject *name = GETNAME(oparg);
    JVALUE owner = TOP();
    JVALUE v = SECOND();
    RTCALL_Z(jd->sig_iooo, PyObject_SetAttr, owner, CONSTANT_PYOBJ(name), v);
    STACKADJ(-2);
    DECREF(v);
    DECREF(owner);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(DELETE_ATTR) {
    PyObject *name = GETNAME(oparg);
    JVALUE owner = TOP();
    RTCALL_Z(jd->sig_iooo, PyObject_SetAttr, owner, CONSTANT_PYOBJ(name), CONSTANT_PYOBJ(NULL));
    STACKADJ(-1);
    DECREF(owner);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(STORE_GLOBAL) {
    PyObject *name = GETNAME(oparg);
    JVALUE v = TOP();
    JVALUE globals = LOAD_F_GLOBALS();
    RTCALL_Z(jd->sig_iooo, PyDict_SetItem, globals, CONSTANT_PYOBJ(name), v);
    STACKADJ(-1);
    DECREF(v);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(DELETE_GLOBAL) {
    PyObject *name = GETNAME(oparg);
    JVALUE globals = LOAD_F_GLOBALS();
    // CUSTOM SITE
    JVALUE err = CALL_NATIVE(jd->sig_ioo, PyDict_DelItem, globals, CONSTANT_PYOBJ(name));
    IF(err, IR_UNLIKELY, {
        CALL_format_exc_check_arg(PyExc_NameError, NAME_ERROR_MSG, name);
        GOTO_ERROR();
    });
    SOFT_EVAL_BREAK_CHECK();
}

static PyObject *
_load_name_helper(PyObject *name, PyObject *globals, PyObject *builtins, PyObject *locals) {
    PyObject *v;
    if (locals == NULL) {
        PyErr_Format(PyExc_SystemError,
                     "no locals when loading %R", name);
        return NULL;
    }
    if (PyDict_CheckExact(locals)) {
        v = PyDict_GetItem(locals, name);
        Py_XINCREF(v);
    }
    else {
        v = PyObject_GetItem(locals, name);
        if (v == NULL) {
            if (!PyErr_ExceptionMatches(PyExc_KeyError))
                return NULL;
            PyErr_Clear();
        }
    }
    if (v == NULL) {
        v = PyDict_GetItem(globals, name);
        Py_XINCREF(v);
        if (v == NULL) {
            if (PyDict_CheckExact(builtins)) {
                v = PyDict_GetItem(builtins, name);
                if (v == NULL) {
                    format_exc_check_arg(
                                PyExc_NameError,
                                NAME_ERROR_MSG, name);
                    return NULL;
                }
                Py_INCREF(v);
            }
            else {
                v = PyObject_GetItem(builtins, name);
                if (v == NULL) {
                    if (PyErr_ExceptionMatches(PyExc_KeyError))
                        format_exc_check_arg(
                                    PyExc_NameError,
                                    NAME_ERROR_MSG, name);
                    return NULL;
                }
            }
        }
    }
    return v;
}

EMITTER_FOR(LOAD_NAME) {
    PyObject *name = GETNAME(oparg);
    JVALUE globals = LOAD_F_GLOBALS();
    JVALUE builtins = LOAD_F_BUILTINS();
    JVALUE locals = LOAD_F_LOCALS();
    JVALUE v = RTCALL_NZ(jd->sig_ooooo, _load_name_helper, CONSTANT_PYOBJ(name), globals, builtins, locals);
    PUSH(v);
    SOFT_EVAL_BREAK_CHECK();
}

static PyObject* _load_global_helper(PyObject *globals, PyObject *builtins, PyObject *name) {
    PyObject *v;
    if (PyDict_CheckExact(globals)
        && PyDict_CheckExact(builtins))
    {
        v = _PyDict_LoadGlobal((PyDictObject *)globals,
                               (PyDictObject *)builtins,
                               name);
        if (v == NULL) {
            if (!_PyErr_OCCURRED()) {
                /* _PyDict_LoadGlobal() returns NULL without raising
                 * an exception if the key doesn't exist */
                format_exc_check_arg(PyExc_NameError,
                                     NAME_ERROR_MSG, name);
            }
            return NULL;
        }
        Py_INCREF(v);
    }
    else {
        /* Slow-path if globals or builtins is not a dict */

        /* namespace 1: globals */
        v = PyObject_GetItem(globals, name);
        if (v == NULL) {
            if (!PyErr_ExceptionMatches(PyExc_KeyError))
                return NULL;
            PyErr_Clear();

            /* namespace 2: builtins */
            v = PyObject_GetItem(builtins, name);
            if (v == NULL) {
                if (PyErr_ExceptionMatches(PyExc_KeyError))
                    format_exc_check_arg(
                                PyExc_NameError,
                                NAME_ERROR_MSG, name);
                return NULL;
            }
        }
    }
    return v;
}

/* If either globals or builtins is updated, invalidate our cache. */
void _invalidate_cache(void *arg, PyDictObject *d) {
    _JITState *js = (_JITState*)arg;
    memset(js->load_global_cache, 0, sizeof(PyObject*) * js->load_global_cache_size);
}

static inline PyObject** _get_globals_inline_cache(JITData *jd, int oparg) {
    _JITState *js = jd->jstate;
    if (js->load_global_cache == NULL) {
        /* TODO: Many of these slots will be unused because they belong to attribute names
                 rather than global names. Splitting them up would save memory. */
        js->load_global_cache_size = PyTuple_GET_SIZE(jd->co->co_names);
        /* TODO: When it becomes possible to patch constants in the code, place the
                 cached values directly inline. */
        js->load_global_cache = calloc(sizeof(PyObject*) * js->load_global_cache_size, 1);
        js->globals_dl = _PyDict_AddListener((PyDictObject*)jd->globals, _invalidate_cache, js);
        js->builtins_dl = _PyDict_AddListener((PyDictObject*)jd->builtins, _invalidate_cache, js);
        if (!js->globals_dl || !js->builtins_dl) {
            Py_FatalError("Out of memory in _init_globals_inline_cache");
        }
    }
    assert(oparg >= 0 && oparg < js->load_global_cache_size);
    return &js->load_global_cache[oparg];
}

EMITTER_FOR(LOAD_GLOBAL) {
    PyObject *name = GETNAME(oparg);
    if (jd->globals && jd->builtins) {
        PyObject **slot = _get_globals_inline_cache(jd, oparg);
        JVALUE slotval = CONSTANT_PTR(ir_type_pyobject_ptr_ptr, slot);
        JVALUE v = LOAD(slotval);
        IF_ELSE(v, IR_LIKELY, {
            INCREF(v);
            PUSH(v);
        }, {
            JVALUE globals = LOAD_F_GLOBALS();
            JVALUE builtins = LOAD_F_BUILTINS();
            JVALUE rv = RTCALL_NZ(jd->sig_oooo, _load_global_helper, globals, builtins, CONSTANT_PYOBJ(name));
            PUSH(rv);
            STORE(slotval, rv);
        });
    } else {
        JVALUE globals = LOAD_F_GLOBALS();
        JVALUE builtins = LOAD_F_BUILTINS();
        JVALUE null = CONSTANT_PTR(ir_type_pyobject_ptr_ptr, NULL);
        JVALUE v = RTCALL_NZ(jd->sig_oooo, _load_global_helper, globals, builtins, CONSTANT_PYOBJ(name), null);
        PUSH(v);
    }
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(DELETE_FAST) {
    JVALUE tmp = ir_getlocal(jd->func, oparg, 1, jd->unknown_handler);
    ir_dellocal(jd->func, oparg);
    DECREF(tmp);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(DELETE_DEREF) {
    JLABEL fast_dispatch = JLABEL_INIT("fast_dispatch");
    JLABEL cell_empty = JLABEL_INIT("cell_empty");
    JVALUE cell = LOAD_FREEVAR(oparg);
    JVALUE oldobj = IR_PyCell_GET(cell);
    BRANCH_IF_NOT(oldobj, cell_empty, IR_UNLIKELY);
    IR_PyCell_SET(cell, CONSTANT_PYOBJ(NULL));
    DECREF(oldobj);
    SOFT_EVAL_BREAK_CHECK();
    BRANCH(fast_dispatch);

    LABEL(cell_empty);
    CALL_format_exc_unbound(oparg);
    GOTO_ERROR();

    LABEL(fast_dispatch);
}

EMITTER_FOR(LOAD_CLOSURE) {
    JVALUE cell = LOAD_FREEVAR(oparg);
    INCREF(cell);
    PUSH(cell);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(LOAD_CLASSDEREF) {
    assert(oparg >= PyTuple_GET_SIZE(jd->co->co_cellvars));
    Py_ssize_t idx = oparg - PyTuple_GET_SIZE(jd->co->co_cellvars);
    assert(idx >= 0 && idx < PyTuple_GET_SIZE(jd->co->co_freevars));
    PyObject *name = PyTuple_GET_ITEM(jd->co->co_freevars, idx);

    JVALUE value = ALLOCA(ir_type_pyobject_ptr);
    JVALUE locals = LOAD_F_LOCALS();
    IR_ASSERT(locals);

    IF_ELSE(
        IR_PyDict_CheckExact(locals), IR_LIKELY,
        {
            STORE(value, CALL_NATIVE(jd->sig_ooo, PyDict_GetItem, locals, CONSTANT_PYOBJ(name)));
            XINCREF(LOAD(value));
        },
        {
            STORE(value, CALL_NATIVE(jd->sig_ooo, PyObject_GetItem, locals, CONSTANT_PYOBJ(name)));
            IF_NOT(
                LOAD(value), IR_SEMILIKELY,
                {
                    GOTO_ERROR_IF_NOT(IR_PyErr_ExceptionMatches(PyExc_KeyError));
                    CALL_PyErr_Clear();
                });
        });
    IF_NOT(LOAD(value), IR_SEMILIKELY,
    {
        JVALUE cell = LOAD_FREEVAR(oparg);
        STORE(value, IR_PyCell_GET(cell));
        IF_NOT(LOAD(value), IR_UNLIKELY,
        {
            CALL_format_exc_unbound(oparg);
            GOTO_ERROR();
        });
        INCREF(LOAD(value));
    });
    PUSH(LOAD(value));
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(LOAD_DEREF) {
    JLABEL fast_dispatch = JLABEL_INIT("fast_dispatch");
    JLABEL unbound_value = JLABEL_INIT("unbound_value");
    JVALUE cell = LOAD_FREEVAR(oparg);
    JVALUE value = IR_PyCell_GET(cell);
    BRANCH_IF_NOT(value, unbound_value, IR_UNLIKELY);
    INCREF(value);
    PUSH(value);
    SOFT_EVAL_BREAK_CHECK();
    BRANCH(fast_dispatch);

    LABEL(unbound_value);
    CALL_format_exc_unbound(oparg);
    GOTO_ERROR();

    LABEL(fast_dispatch);
}

EMITTER_FOR(STORE_DEREF) {
    JVALUE v = POP();
    JVALUE cell = LOAD_FREEVAR(oparg);
    JVALUE oldobj = IR_PyCell_GET(cell);
    IR_PyCell_SET(cell, v);
    XDECREF(oldobj);
    SOFT_EVAL_BREAK_CHECK();
}

static PyObject* _build_string_helper0(PyObject *empty) {
    PyObject *stack[] = {};
    return _PyUnicode_JoinArray(empty, stack, 0);
}

static PyObject* _build_string_helper1(PyObject *empty, PyObject *arg1) {
    PyObject *stack[] = {arg1};
    return _PyUnicode_JoinArray(empty, stack, 1);
}

static PyObject* _build_string_helper2(PyObject *empty, PyObject *arg1, PyObject *arg2) {
    PyObject *stack[] = {arg1, arg2};
    return _PyUnicode_JoinArray(empty, stack, 2);
}

static PyObject* _build_string_helper3(PyObject *empty, PyObject *arg1, PyObject *arg2, PyObject *arg3) {
    PyObject *stack[] = {arg1, arg2, arg3};
    return _PyUnicode_JoinArray(empty, stack, 3);
}

static PyObject* _build_string_helper4(PyObject *empty, PyObject *arg1, PyObject *arg2, PyObject *arg3, PyObject *arg4) {
    PyObject *stack[] = {arg1, arg2, arg3, arg4};
    return _PyUnicode_JoinArray(empty, stack, 4);
}

EMITTER_FOR(BUILD_STRING) {
    // TODO: The extra call and stack allocation makes this slower than the
    // interpreted version. Consider inlining _PyUnicode_JoinArray.
    JTYPE sig1 = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyssizet, ir_type_uint);
    JVALUE empty = RTCALL_NZ(sig1, PyUnicode_New, CONSTANT_PYSSIZET(0), CONSTANT_UINT32(0));

    JVALUE result;
    switch (oparg) {
    case 0: {
        result = CALL_NATIVE(jd->sig_oo, _build_string_helper0, empty);
        break;
    }
    case 1: {
        result = CALL_NATIVE(jd->sig_ooo, _build_string_helper1, empty, PEEK(1));
        break;
    }
    case 2: {
        result = CALL_NATIVE(jd->sig_oooo, _build_string_helper2, empty, PEEK(2), PEEK(1));
        break;
    }
    case 3: {
        JTYPE sig = CREATE_SIGNATURE(
            ir_type_pyobject_ptr,
            ir_type_pyobject_ptr,
            ir_type_pyobject_ptr,
            ir_type_pyobject_ptr,
            ir_type_pyobject_ptr);
        result = CALL_NATIVE(sig, _build_string_helper3, empty, PEEK(3), PEEK(2), PEEK(1));
        break;
    }
    case 4: {
        JTYPE sig = CREATE_SIGNATURE(
            ir_type_pyobject_ptr,
            ir_type_pyobject_ptr,
            ir_type_pyobject_ptr,
            ir_type_pyobject_ptr,
            ir_type_pyobject_ptr,
            ir_type_pyobject_ptr);
        result = CALL_NATIVE(sig, _build_string_helper4, empty, PEEK(4), PEEK(3), PEEK(2), PEEK(1));
        break;
    }
    default: {
        JLABEL malloc_ok = JLABEL_INIT("malloc_ok");
        JTYPE sig1 = CREATE_SIGNATURE(ir_type_void_ptr, ir_type_sizet);
        JVALUE stack = CALL_NATIVE(sig1, PyMem_Malloc, CONSTANT_SIZET(sizeof(PyObject*) * oparg));
        BRANCH_IF(stack, malloc_ok, IR_LIKELY);
        CALL_PyErr_NoMemory();
        GOTO_ERROR();
        LABEL(malloc_ok);
        stack = CAST(ir_type_pyobject_ptr_ptr, stack);
        for (int i = 0; i < oparg; i++) {
            STORE_AT_INDEX(stack, CONSTANT_INT(i), PEEK(oparg - i));
        }
        JTYPE sig2 = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr_ptr, ir_type_pyssizet);
        result = CALL_NATIVE(sig2, _PyUnicode_JoinArray, empty, stack, CONSTANT_PYSSIZET(oparg));
        JTYPE sig3 = CREATE_SIGNATURE(ir_type_void, ir_type_void_ptr);
        CALL_NATIVE(sig3, PyMem_Free, CAST(ir_type_void_ptr, stack));
        break;
    }
    } // select

    DECREF(empty);
    for (int i = 0; i < oparg; i++) {
        DECREF(PEEK(1+i));
    }
    STACKADJ(-oparg);
    GOTO_ERROR_IF_NOT(result);
    PUSH(result);
    SOFT_EVAL_BREAK_CHECK();
}

/* TODO: Replace with a new opcode which directly places items into
   a tuple, instead of maintaining a large number of items on the stack.
   This is difficult to compile, and produces inefficient code.
 */
EMITTER_FOR(BUILD_TUPLE) {
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyssizet);
    JVALUE tup = RTCALL_NZ(sig, PyTuple_New, CONSTANT_PYSSIZET(oparg));
    JVALUE ob_item = IR_PyTuple_OB_ITEM(tup);
    for (int i = 0; i < oparg; i++) {
        STORE_AT_INDEX(ob_item, CONSTANT_INT(i), PEEK(oparg - i));
    }
    STACKADJ(-oparg);
    PUSH(tup);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(BUILD_LIST) {
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyssizet);
    JVALUE list = RTCALL_NZ(sig, PyList_New, CONSTANT_PYSSIZET(oparg));
    JVALUE ob_item = IR_PyList_OB_ITEM(list);
    for (int i = 0; i < oparg; i++) {
        STORE_AT_INDEX(ob_item, CONSTANT_INT(i), PEEK(oparg - i));
    }
    STACKADJ(-oparg);
    PUSH(list);
    SOFT_EVAL_BREAK_CHECK();
}

/* from ceval.c */
extern int check_args_iterable(PyObject *, PyObject *);

static void _build_unpack_common(JITData *jd, int opcode, int oparg, int next_instr_index) {
    int convert_to_tuple = opcode != BUILD_LIST_UNPACK;
    JLABEL handle_error = JLABEL_INIT("handle_error");
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyssizet);
    JVALUE sum = RTCALL_NZ(sig, PyList_New, CONSTANT_PYSSIZET(0));
    JVALUE badobj = ALLOCA(ir_type_pyobject_ptr);
    for (int i = oparg; i > 0; i--) {
        JVALUE item = PEEK(i);
        STORE(badobj, item);
        JVALUE none_val = CALL_NATIVE(jd->sig_ooo, _PyList_Extend, sum, item);
        BRANCH_IF_NOT(none_val, handle_error, IR_UNLIKELY);
        DECREF(none_val);
    }
    JVALUE result;
    if (convert_to_tuple) {
        JVALUE tup = CALL_NATIVE(jd->sig_oo, PyList_AsTuple, sum);
        DECREF(sum);
        GOTO_ERROR_IF_NOT(tup);
        result = tup;
    } else {
        result = sum;
    }
    for (int i = oparg; i > 0; i--) {
        DECREF(PEEK(i));
    }
    STACKADJ(-oparg);
    PUSH(result);
    SOFT_EVAL_BREAK_CHECK();

    REMOTE_SECTION({
        LABEL(handle_error);
        DECREF(sum);
        if (opcode == BUILD_TUPLE_UNPACK_WITH_CALL) {
            JLABEL skip = JLABEL_INIT("skip");
            BRANCH_IF_NOT(IR_PyErr_ExceptionMatches(PyExc_TypeError), skip, IR_SEMILIKELY);
            CALL_NATIVE(jd->sig_ioo, check_args_iterable, PEEK(1 + oparg), LOAD(badobj));
            LABEL(skip);
        }
        GOTO_ERROR();
    });
}

EMITTER_FOR(BUILD_TUPLE_UNPACK_WITH_CALL) {
    _build_unpack_common(jd, opcode, oparg, next_instr_index);
}

EMITTER_FOR(BUILD_TUPLE_UNPACK) {
    _build_unpack_common(jd, opcode, oparg, next_instr_index);
}

EMITTER_FOR(BUILD_LIST_UNPACK) {
    _build_unpack_common(jd, opcode, oparg, next_instr_index);
}

EMITTER_FOR(BUILD_SET) {
    JLABEL handle_error = JLABEL_INIT("handle_error");
    JVALUE set = RTCALL_NZ(jd->sig_oo, PySet_New, CONSTANT_PYOBJ(NULL));
    for (int i = 0; i < oparg; i++) {
        JVALUE item = PEEK(oparg - i);
        JVALUE err = CALL_NATIVE(jd->sig_ioo, PySet_Add, set, item);
        BRANCH_IF(err, handle_error, IR_UNLIKELY);
    }
    for (int i = 0; i < oparg; i++) {
        JVALUE item = PEEK(oparg - i);
        DECREF(item);
    }
    STACKADJ(-oparg);
    PUSH(set);
    SOFT_EVAL_BREAK_CHECK();

    REMOTE_SECTION({
        LABEL(handle_error);
        DECREF(set);
        GOTO_ERROR();
    });
}

EMITTER_FOR(BUILD_SET_UNPACK) {
    JLABEL handle_error = JLABEL_INIT("handle_error");
    JVALUE sum = RTCALL_NZ(jd->sig_oo, PySet_New, CONSTANT_PYOBJ(NULL));
    for (int i = 0; i < oparg; i++) {
        JVALUE err = CALL_NATIVE(jd->sig_ioo, _PySet_Update, sum, PEEK(oparg - i));
        BRANCH_IF(CMP_LT(err, CONSTANT_INT(0)), handle_error, IR_UNLIKELY);
    }
    for (int i = 0; i < oparg; i++) {
        DECREF(PEEK(oparg - i));
    }
    STACKADJ(-oparg);
    PUSH(sum);
    SOFT_EVAL_BREAK_CHECK();

    REMOTE_SECTION({
        LABEL(handle_error);
        DECREF(sum);
        GOTO_ERROR();
    });
}

EMITTER_FOR(BUILD_MAP) {
    JLABEL handle_error = JLABEL_INIT("handle_error");
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyssizet);
    JVALUE map = RTCALL_NZ(sig, _PyDict_NewPresized, CONSTANT_PYSSIZET(oparg));
    for (int i = oparg; i > 0; i--) {
        JVALUE key = PEEK(2*i);
        JVALUE value = PEEK(2*i - 1);
        JVALUE err = CALL_NATIVE(jd->sig_iooo, PyDict_SetItem, map, key, value);
        BRANCH_IF(err, handle_error, IR_UNLIKELY);
    }
    for (int i = oparg; i > 0; i--) {
        DECREF(PEEK(2*i));
        DECREF(PEEK(2*i - 1));
    }
    STACKADJ(-2*oparg);
    PUSH(map);

    REMOTE_SECTION({
        LABEL(handle_error);
        DECREF(map);
        GOTO_ERROR();
    });
}

/* This is copied from ceval.c */
int
_setup_annotations_helper(PyObject *f_locals) {
    _Py_IDENTIFIER(__annotations__);
    int err;
    PyObject *ann_dict;
    if (f_locals == NULL) {
        PyErr_Format(PyExc_SystemError,
                     "no locals found when setting up annotations");
        return -1;
    }
    /* check if __annotations__ in locals()... */
    if (PyDict_CheckExact(f_locals)) {
        ann_dict = _PyDict_GetItemId(f_locals,
                                     &PyId___annotations__);
        if (ann_dict == NULL) {
            /* ...if not, create a new one */
            ann_dict = PyDict_New();
            if (ann_dict == NULL) {
                return -1;
            }
            err = _PyDict_SetItemId(f_locals,
                                    &PyId___annotations__, ann_dict);
            Py_DECREF(ann_dict);
            if (err != 0) {
                return -1;
            }
        }
    }
    else {
        /* do the same if locals() is not a dict */
        PyObject *ann_str = _PyUnicode_FromId(&PyId___annotations__);
        if (ann_str == NULL) {
            return -1;
        }
        ann_dict = PyObject_GetItem(f_locals, ann_str);
        if (ann_dict == NULL) {
            if (!PyErr_ExceptionMatches(PyExc_KeyError)) {
                return -1;
            }
            PyErr_Clear();
            ann_dict = PyDict_New();
            if (ann_dict == NULL) {
                return -1;
            }
            err = PyObject_SetItem(f_locals, ann_str, ann_dict);
            Py_DECREF(ann_dict);
            if (err != 0) {
                return -1;
            }
        }
        else {
            Py_DECREF(ann_dict);
        }
    }
    return 0;
}

EMITTER_FOR(SETUP_ANNOTATIONS) {
    RTCALL_Z(jd->sig_io, _setup_annotations_helper, LOAD_F_LOCALS());
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(BUILD_CONST_KEY_MAP) {
    IR_LABEL_INIT(bad_keys_arg);
    IR_LABEL_INIT(handle_setitem_error);
    JVALUE keys = TOP();
    BRANCH_IF_NOT(IR_PyTuple_CheckExact(keys), bad_keys_arg, IR_UNLIKELY);
    BRANCH_IF_NOT(CMP_EQ(IR_PyTuple_GET_SIZE(keys), CONSTANT_PYSSIZET(oparg)), bad_keys_arg, IR_UNLIKELY);
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyssizet);
    JVALUE map = RTCALL_NZ(sig, _PyDict_NewPresized, CONSTANT_PYSSIZET(oparg));
    JVALUE ob_item = IR_PyTuple_OB_ITEM(keys);
    for (int i = oparg; i > 0; i--) {
        JVALUE key = LOAD_AT_INDEX(ob_item, CONSTANT_INT(oparg - i));
        JVALUE value = PEEK(i + 1);
        JVALUE err = CALL_NATIVE(jd->sig_iooo, PyDict_SetItem, map, key, value);
        BRANCH_IF(err, handle_setitem_error, IR_UNLIKELY);
    }
    DECREF(POP());
    for (int i = oparg; i > 0; i--) {
        DECREF(POP());
    }
    PUSH(map);
    SOFT_EVAL_BREAK_CHECK();

    REMOTE_SECTION({
        LABEL(handle_setitem_error);
        DECREF(map);
        GOTO_ERROR();
    });

    REMOTE_SECTION({
        LABEL(bad_keys_arg);
        CALL_PyErr_SetString(PyExc_SystemError,
                             "bad BUILD_CONST_KEY_MAP keys argument");
        GOTO_ERROR();
    });
}

EMITTER_FOR(BUILD_MAP_UNPACK) {
    IR_LABEL_INIT(handle_error);
    JVALUE sum = RTCALL_NZ(jd->sig_o, PyDict_New);
    JVALUE err_arg = ALLOCA(ir_type_pyobject_ptr);
    for (int i = oparg; i > 0; i--) {
        JVALUE arg = PEEK(i);
        STORE(err_arg, arg);
        JVALUE err = CALL_NATIVE(jd->sig_ioo, PyDict_Update, sum, arg);
        BRANCH_IF(CMP_LT(err, CONSTANT_INT(0)), handle_error, IR_UNLIKELY);
    }

    for (int i = oparg; i > 0; i--) {
        DECREF(POP());
    }
    PUSH(sum);
    SOFT_EVAL_BREAK_CHECK();

    REMOTE_SECTION({
        LABEL(handle_error);
        IF(IR_PyErr_ExceptionMatches(PyExc_AttributeError), IR_SEMILIKELY, {
            CALL_PyErr_Format(PyExc_TypeError,
                              "'%.200s' object is not a mapping",
                              LOAD_FIELD(IR_Py_TYPE(LOAD(err_arg)), PyTypeObject, tp_name, ir_type_char_ptr));
        });
        DECREF(sum);
        GOTO_ERROR();
    });
}

extern void format_kwargs_mapping_error(PyObject *func, PyObject *kwargs);

void _build_map_unpack_with_call_format_error(PyObject *func, PyObject *arg) {
    if (PyErr_ExceptionMatches(PyExc_AttributeError)) {
        format_kwargs_mapping_error(func, arg);
    }
    else if (PyErr_ExceptionMatches(PyExc_KeyError)) {
        PyObject *exc, *val, *tb;
        PyErr_Fetch(&exc, &val, &tb);
        if (val && PyTuple_Check(val) && PyTuple_GET_SIZE(val) == 1) {
            PyObject *key = PyTuple_GET_ITEM(val, 0);
            if (!PyUnicode_Check(key)) {
                PyErr_Format(PyExc_TypeError,
                        "%.200s%.200s keywords must be strings",
                        PyEval_GetFuncName(func),
                        PyEval_GetFuncDesc(func));
            } else {
                PyErr_Format(PyExc_TypeError,
                        "%.200s%.200s got multiple "
                        "values for keyword argument '%U'",
                        PyEval_GetFuncName(func),
                        PyEval_GetFuncDesc(func),
                        key);
            }
            Py_XDECREF(exc);
            Py_XDECREF(val);
            Py_XDECREF(tb);
        }
        else {
            PyErr_Restore(exc, val, tb);
        }
    }
}

EMITTER_FOR(BUILD_MAP_UNPACK_WITH_CALL) {
    IR_LABEL_INIT(handle_error);
    JVALUE sum = RTCALL_PyDict_New();
    JTYPE sig = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_int);
    JVALUE err_arg = ALLOCA(ir_type_pyobject_ptr);
    for (int i = oparg; i > 0; i--) {
        JVALUE arg = PEEK(i);
        STORE(err_arg, arg);
        JVALUE err = CALL_NATIVE(sig, _PyDict_MergeEx, sum, arg, CONSTANT_INT(2));
        BRANCH_IF(CMP_LT(err, CONSTANT_INT(0)), handle_error, IR_UNLIKELY);
    }
    for (int i = oparg; i > 0; i--) {
        DECREF(POP());
    }
    PUSH(sum);
    SOFT_EVAL_BREAK_CHECK();

    REMOTE_SECTION({
        LABEL(handle_error);
        JVALUE func = PEEK(2 + oparg);
        JTYPE sig2 = CREATE_SIGNATURE(ir_type_void, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
        CALL_NATIVE(sig2, _build_map_unpack_with_call_format_error, func, LOAD(err_arg));
        DECREF(sum);
        GOTO_ERROR();
    });
}

EMITTER_FOR(MAP_ADD) {
    JVALUE key = TOP();
    JVALUE value = SECOND();
    JVALUE map = PEEK(oparg + 2);
    IR_ASSERT(IR_PyDict_CheckExact(map));
    RTCALL_Z(jd->sig_iooo, PyDict_SetItem, map, key, value);
    STACKADJ(-2);
    DECREF(value);
    DECREF(key);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(LOAD_ATTR) {
    JVALUE owner = TOP();
    PyObject *name = GETNAME(oparg);
    JVALUE res;
    if (Py_JITAttrCache == 0) {
        res = RTCALL_NZ(jd->sig_ooo, PyObject_GetAttr, owner, CONSTANT_PYOBJ(name));
    } else {
        PyJITAttrCache *ic = PyJITAttrCache_New(name);
        if (!ic) {
            Py_FatalError("Failed to create attribute inline cache");
        }
        JVALUE icvalue = CONSTANT_PTR(ir_type_void_ptr, ic);
        res = RTCALL_NZ(jd->sig_oov, PyJITAttrCache_GetAttrFunc(), owner, icvalue);
    }
    STACKADJ(-1);
    DECREF(owner);
    PUSH(res);
    SOFT_EVAL_BREAK_CHECK();
}

/* Copied from ceval cmp_outcome */
#define CANNOT_CATCH_MSG "catching classes that do not inherit from "\
                         "BaseException is not allowed"

int
_compare_op_exc_match_helper(PyObject *v, PyObject *w) {
    Py_ssize_t i, length;
    length = PyTuple_Size(w);
    for (i = 0; i < length; i += 1) {
        PyObject *exc = PyTuple_GET_ITEM(w, i);
        if (!PyExceptionClass_Check(exc)) {
            PyErr_SetString(PyExc_TypeError,
                            CANNOT_CATCH_MSG "1");
            return 0;
        }
    }
    return 1;
}

EMITTER_FOR(COMPARE_OP) {
    JVALUE right = TOP();
    JVALUE left = SECOND();
    JVALUE res;

    /* Each of these must set 'res' */
    switch (oparg) {
    case PyCmp_IS: {
        res = TERNARY(CMP_EQ(left, right), CONSTANT_PYOBJ(Py_True), CONSTANT_PYOBJ(Py_False));
        INCREF(res);
        break;
    }
    case PyCmp_IS_NOT: {
        res = TERNARY(CMP_NE(left, right), CONSTANT_PYOBJ(Py_True), CONSTANT_PYOBJ(Py_False));
        INCREF(res);
        break;
    }
    case PyCmp_IN:
    case PyCmp_NOT_IN: {
        JVALUE tmp = RTCALL_GE0(jd->sig_ioo, PySequence_Contains, right, left);
        if (oparg == PyCmp_IN)
            res = TERNARY(tmp, CONSTANT_PYOBJ(Py_True), CONSTANT_PYOBJ(Py_False));
        else
            res = TERNARY(tmp, CONSTANT_PYOBJ(Py_False), CONSTANT_PYOBJ(Py_True));
        INCREF(res);
        break;
    }
    case PyCmp_EXC_MATCH: {
        JLABEL do_exc_match = JLABEL_INIT("do_exc_match");
        JLABEL handle_tuple = JLABEL_INIT("handle_tuple");

        /* Handle common case (exception class) first */
        BRANCH_IF(IR_PyExceptionClass_Check(right), do_exc_match, IR_LIKELY);
        BRANCH_IF(IR_PyTuple_Check(right), handle_tuple, IR_LIKELY);

        /* Error case: neither exception class nor tuple */
        CALL_PyErr_SetString(PyExc_TypeError, CANNOT_CATCH_MSG "2");
        if (!Py_JITNoExc) {
            GOTO_ERROR();
        }

        /* Tuple case */
        LABEL(handle_tuple);
        RTCALL_NZ(jd->sig_ioo, _compare_op_exc_match_helper, left, right);

        /* Finally, run PyErr_GivenExceptionMatches */
        LABEL(do_exc_match);
        JVALUE exc_matches = CALL_NATIVE(jd->sig_ioo, PyErr_GivenExceptionMatches, left, right);
        res = TERNARY(exc_matches, CONSTANT_PYOBJ(Py_True), CONSTANT_PYOBJ(Py_False));
        INCREF(res);
        break;
    }
    default: {
        JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_int);
        res = RTCALL_NZ(sig, PyObject_RichCompare, left, right, CONSTANT_INT(oparg));
        break;
    }
    }

    STACKADJ(-2);
    DECREF(left);
    DECREF(right);
    PUSH(res);
    SOFT_EVAL_BREAK_CHECK();
}

static PyObject *
_import_name(PyObject *globals, PyObject *builtins, PyObject *locals,
             PyObject *name, PyObject *fromlist, PyObject *level)
{
    _Py_IDENTIFIER(__import__);
    PyObject *import_func, *res;
    PyObject* stack[5];

    import_func = _PyDict_GetItemId(builtins, &PyId___import__);
    if (import_func == NULL) {
        PyErr_SetString(PyExc_ImportError, "__import__ not found");
        return NULL;
    }

    /* Fast path for not overloaded __import__. */
    if (import_func == PyThreadState_GET()->interp->import_func) {
        int ilevel = _PyLong_AsInt(level);
        if (ilevel == -1 && PyErr_Occurred()) {
            return NULL;
        }
        res = PyImport_ImportModuleLevelObject(
                        name,
                        globals,
                        locals == NULL ? Py_None : locals,
                        fromlist,
                        ilevel);
        return res;
    }

    Py_INCREF(import_func);

    stack[0] = name;
    stack[1] = globals;
    stack[2] = locals == NULL ? Py_None : locals;
    stack[3] = fromlist;
    stack[4] = level;
    res = _PyObject_FastCall(import_func, stack, 5);
    Py_DECREF(import_func);
    return res;
}

EMITTER_FOR(IMPORT_NAME) {
    PyObject *name = GETNAME(oparg);
    JVALUE fromlist = TOP();
    JVALUE level = SECOND();
    JVALUE globals = LOAD_F_GLOBALS();
    JVALUE builtins = LOAD_F_BUILTINS();
    JVALUE locals = LOAD_F_LOCALS();
    JVALUE res = RTCALL_NZ(jd->sig_ooooooo, _import_name, globals, builtins, locals, CONSTANT_PYOBJ(name), fromlist, level);
    STACKADJ(-2);
    DECREF(level);
    DECREF(fromlist);
    PUSH(res);
    SOFT_EVAL_BREAK_CHECK();
}

int import_all_from(PyObject *, PyObject *);

EMITTER_FOR(IMPORT_STAR) {
    JVALUE from = POP();

    JTYPE sig1 = CREATE_SIGNATURE(ir_type_int, ir_type_pyframeobject_ptr);
    IF(
        CMP_LT(
            CALL_NATIVE(sig1, PyFrame_FastToLocalsWithError, FRAMEPTR()),
            CONSTANT_INT(0)),
        IR_UNLIKELY,
        {
            DECREF(from);
            GOTO_ERROR();
         }
    );

    JVALUE locals = LOAD_F_LOCALS();
    IF_NOT(locals, IR_UNLIKELY, {
        CALL_PyErr_SetString(PyExc_SystemError,
            "no locals found during 'import *'");
        DECREF(from);
        GOTO_ERROR();
    });
    JVALUE err = CALL_NATIVE(jd->sig_ioo, import_all_from, locals, from);
    JTYPE sig2 = CREATE_SIGNATURE(ir_type_void, ir_type_pyframeobject_ptr, ir_type_int);
    CALL_NATIVE(sig2, PyFrame_LocalsToFast, FRAMEPTR(), CONSTANT_INT(0));
    DECREF(from);
    GOTO_ERROR_IF(err);
    SOFT_EVAL_BREAK_CHECK();
}

PyObject * import_from(PyObject *, PyObject *);

EMITTER_FOR(IMPORT_FROM) {
    PyObject *name = GETNAME(oparg);
    JVALUE from = TOP();
    JVALUE res = CALL_NATIVE(jd->sig_ooo, import_from, from, CONSTANT_PYOBJ(name));
    GOTO_ERROR_IF_NOT(res);
    PUSH(res);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(JUMP_FORWARD) {
    JUMPBY(oparg, 0);
}

EMITTER_FOR(POP_JUMP_IF_FALSE) {
    JLABEL skip1 = JLABEL_INIT("skip1");
    JLABEL skip2 = JLABEL_INIT("skip2");
    JLABEL fin = JLABEL_INIT("fin");
    JVALUE cond = TOP();
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_True)), skip1, IR_SEMILIKELY);
    /* Py_True case */
    STACKADJ(-1);
    DECREF(cond);
    BRANCH(fin);
    LABEL(skip1);
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_False)), skip2, IR_SEMILIKELY);
    /* Py_False case */
    STACKADJ(-1);
    DECREF(cond);
    JUMPTO(oparg, 0);
    LABEL(skip2);
    /* Generic case */
    JVALUE err = RTCALL_GE0(jd->sig_io, PyObject_IsTrue, cond);
    STACKADJ(-1);
    DECREF(cond);
    BRANCH_IF(CMP_GT(err, CONSTANT_INT(0)), fin, IR_SEMILIKELY);
    /* err == 0 case */
    JUMPTO(oparg, 1);
    LABEL(fin);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(POP_JUMP_IF_TRUE) {
    JLABEL skip1 = JLABEL_INIT("skip1");
    JLABEL skip2 = JLABEL_INIT("skip2");
    JLABEL fin = JLABEL_INIT("fin");
    JVALUE cond = POP();
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_False)), skip1, IR_SEMILIKELY);
    /* Py_False case */
    DECREF(cond);
    BRANCH(fin);
    LABEL(skip1);
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_True)), skip2, IR_SEMILIKELY);
    /* Py_True case */
    DECREF(cond);
    JUMPTO(oparg, 0);
    LABEL(skip2);
    /* Generic case */
    JVALUE err = CALL_NATIVE(jd->sig_io, PyObject_IsTrue, cond);
    DECREF(cond);
    BRANCH_IF(CMP_EQ(err, CONSTANT_INT(0)), fin, IR_SEMILIKELY);
    GOTO_ERROR_IF(CMP_LT(err, CONSTANT_INT(0)));
    /* err > 0 case */
    JUMPTO(oparg, 1);
    LABEL(fin);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(JUMP_IF_FALSE_OR_POP) {
    JLABEL skip1 = JLABEL_INIT("skip1");
    JLABEL skip2 = JLABEL_INIT("skip2");
    JLABEL dispatch = JLABEL_INIT("dispatch");
    JLABEL fast_dispatch = JLABEL_INIT("fast_dispatch");
    JLABEL do_jump = JLABEL_INIT("do_jump");
    JVALUE cond = TOP();
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_True)), skip1, IR_SEMILIKELY);
    /* Py_True case */
    STACKADJ(-1);
    DECREF(cond);
    BRANCH(fast_dispatch);
    LABEL(skip1);
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_False)), skip2, IR_SEMILIKELY);
    /* Py_False case */
    JUMPTO(oparg, 0);
    /* Generic case */
    LABEL(skip2);
    JVALUE err = CALL_NATIVE(jd->sig_io, PyObject_IsTrue, cond);
    BRANCH_IF(CMP_EQ(err, CONSTANT_INT(0)), do_jump, IR_SEMILIKELY);
    GOTO_ERROR_IF(CMP_LT(err, CONSTANT_INT(0)));
    /* err > 0 case */
    STACKADJ(-1);
    DECREF(cond);
    BRANCH(dispatch);
    /* err == 0 case */
    LABEL(do_jump);
    JUMPTO(oparg, 1);
    LABEL(dispatch);
    SOFT_EVAL_BREAK_CHECK();
    LABEL(fast_dispatch);
}

EMITTER_FOR(JUMP_IF_TRUE_OR_POP) {
    JLABEL skip1 = JLABEL_INIT("skip1");
    JLABEL skip2 = JLABEL_INIT("skip2");
    JLABEL dispatch = JLABEL_INIT("dispatch");
    JLABEL fast_dispatch = JLABEL_INIT("fast_dispatch");
    JLABEL do_jump = JLABEL_INIT("do_jump");
    JVALUE cond = TOP();
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_False)), skip1, IR_SEMILIKELY);
    /* Py_False case */
    STACKADJ(-1);
    DECREF(cond);
    BRANCH(fast_dispatch);
    LABEL(skip1);
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_True)), skip2, IR_SEMILIKELY);
    /* Py_True case */
    JUMPTO(oparg, 0);
    /* Generic case */
    LABEL(skip2);
    JVALUE err = CALL_NATIVE(jd->sig_io, PyObject_IsTrue, cond);
    BRANCH_IF(CMP_GT(err, CONSTANT_INT(0)), do_jump, IR_SEMILIKELY);
    GOTO_ERROR_IF(CMP_LT(err, CONSTANT_INT(0)));
    /* err == 0 case */
    STACKADJ(-1);
    DECREF(cond);
    BRANCH(dispatch);
    /* err > 0 case */
    LABEL(do_jump);
    JUMPTO(oparg, 1);
    LABEL(dispatch);
    SOFT_EVAL_BREAK_CHECK();
    LABEL(fast_dispatch);
}


EMITTER_FOR(JUMP_ABSOLUTE) {
    JUMPTO(oparg, 1);
}

EMITTER_FOR(GET_ITER) {
    JVALUE iterable = POP();
    JVALUE iter = CALL_NATIVE(jd->sig_oo, PyObject_GetIter, iterable);
    DECREF(iterable);
    if (!Py_JITNoExc) {
        GOTO_ERROR_IF_NOT(iter);
    }
    PUSH(iter);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(GET_YIELD_FROM_ITER) {
    JLABEL exact_coro = JLABEL_INIT("exact_coro");
    JLABEL exact_gen = JLABEL_INIT("exact_gen");
    JLABEL dispatch = JLABEL_INIT("dispatch");
    int is_coroutine = (jd->co->co_flags & (CO_COROUTINE | CO_ITERABLE_COROUTINE));
    JVALUE iterable = POP();
    BRANCH_IF(IR_PyCoro_CheckExact(iterable), exact_coro, IR_SEMILIKELY);
    BRANCH_IF(IR_PyGen_CheckExact(iterable), exact_gen, IR_SEMILIKELY);

    /* Generic case */
    JVALUE iter = CALL_NATIVE(jd->sig_oo, PyObject_GetIter, iterable);
    DECREF(iterable);
    GOTO_ERROR_IF_NOT(iter);
    PUSH(iter);
    BRANCH(dispatch);

    LABEL(exact_coro);
    if (!is_coroutine) {
        DECREF(iterable);
        CALL_PyErr_SetString(PyExc_TypeError,
                             "cannot 'yield from' a coroutine object "
                             "in a non-coroutine generator");
        GOTO_ERROR();
    }
    LABEL(exact_gen);
    PUSH(iterable);
    LABEL(dispatch);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(FOR_ITER) {
    JLABEL handle_null = JLABEL_INIT("handle_null");
    JLABEL cleanup = JLABEL_INIT("cleanup");
    JLABEL next_instruction = JLABEL_INIT("next_instruction");
    JVALUE iter_obj = TOP();
    JVALUE type_obj = IR_Py_TYPE(iter_obj);
    JVALUE tp_iternext = LOAD_FIELD(type_obj, PyTypeObject, tp_iternext, jd->sig_oo);
    JVALUE next = CALL_INDIRECT(tp_iternext, iter_obj);
    BRANCH_IF_NOT(next, handle_null, IR_SOMETIMES);
    PUSH(next);
    BRANCH(next_instruction);

    /* Handle NULL case */
    LABEL(handle_null);
    BRANCH_IF_NOT(IR_PyErr_Occurred(), cleanup, IR_LIKELY);
    JVALUE ret = CALL_NATIVE(jd->sig_io, PyErr_ExceptionMatches, CONSTANT_PYOBJ(PyExc_StopIteration));
    if (!Py_JITNoExc) {
        GOTO_ERROR_IF_NOT(ret); /* This may actually be likely? */
    }
    CALL_PyErr_Clear();

    LABEL(cleanup);
    STACKADJ(-1);
    DECREF(iter_obj);
    JUMPBY(oparg, 1);
    LABEL(next_instruction);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(BREAK_LOOP) {
    GOTO_FAST_BLOCK_END(BREAK);
}

EMITTER_FOR(CONTINUE_LOOP) {
    assert(oparg % sizeof(_Py_CODEUNIT) == 0);
    ir_label continue_target = _instr_index_to_label(jd, oparg / sizeof(_Py_CODEUNIT));
    GOTO_FAST_BLOCK_END_CONTINUE(continue_target);
}

EMITTER_FOR(SETUP_LOOP) {
    DO_SETUP_BLOCK(LOOP, INSTR_OFFSET() + oparg);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(SETUP_EXCEPT) {
    DO_SETUP_BLOCK(EXCEPT, INSTR_OFFSET() + oparg);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(SETUP_FINALLY) {
    DO_SETUP_BLOCK(FINALLY_TRY, INSTR_OFFSET() + oparg);
    SOFT_EVAL_BREAK_CHECK();
}

PyObject * special_lookup(PyObject *, _Py_Identifier *);

EMITTER_FOR(BEFORE_ASYNC_WITH) {
    _Py_IDENTIFIER(__aexit__);
    _Py_IDENTIFIER(__aenter__);

    JVALUE mgr = TOP();
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_void_ptr);
    JVALUE exit = CALL_NATIVE(sig, special_lookup, mgr, CONSTANT_VOID_PTR(&PyId___aexit__));
    GOTO_ERROR_IF_NOT(exit);
    SET_TOP(exit);
    JVALUE enter = CALL_NATIVE(sig, special_lookup, mgr, CONSTANT_VOID_PTR(&PyId___aenter__));
    DECREF(mgr);
    GOTO_ERROR_IF_NOT(enter);
    JVALUE res = CALL_PyObject_CallNoArg(enter);
    DECREF(enter);
    GOTO_ERROR_IF_NOT(res);
    PUSH(res);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(SETUP_ASYNC_WITH) {
    JVALUE res = POP();
    DO_SETUP_BLOCK(FINALLY_TRY, INSTR_OFFSET() + oparg);
    PUSH(res);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(SETUP_WITH) {
    _Py_IDENTIFIER(__exit__);
    _Py_IDENTIFIER(__enter__);

    JVALUE mgr = TOP();
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_void_ptr);
    JVALUE enter = CALL_NATIVE(sig, special_lookup, mgr, CONSTANT_VOID_PTR(&PyId___enter__));
    GOTO_ERROR_IF_NOT(enter);
    JVALUE exit = CALL_NATIVE(sig, special_lookup, mgr, CONSTANT_VOID_PTR(&PyId___exit__));
    IF_NOT(exit, IR_UNLIKELY, {
        DECREF(enter);
        GOTO_ERROR();
    });
    SET_TOP(exit);
    DECREF(mgr);
    JVALUE res = CALL_PyObject_CallNoArg(enter);
    DECREF(enter);
    GOTO_ERROR_IF_NOT(res);

    /* Setup the finally block before pushing the result
       of __enter__ on the stack. */
    DO_SETUP_BLOCK(FINALLY_TRY, INSTR_OFFSET() + oparg);
    PUSH(res);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(WITH_CLEANUP_START) {
    JVALUE exit_func = PEEK(7);

    assert(jd->tmpstack_size >= 3);
    JVALUE excptr = jd->tmpstack;
    JVALUE valptr = ir_get_index_ptr(jd->func, jd->tmpstack, CONSTANT_INT(1));
    JVALUE tbptr = ir_get_index_ptr(jd->func, jd->tmpstack, CONSTANT_INT(2));
    STORE(excptr, TOP());
    STORE(valptr, CONSTANT_PYOBJ(Py_None));
    STORE(tbptr, CONSTANT_PYOBJ(Py_None));

    IR_LABEL_INIT(proceed);
    BRANCH_IF(CMP_EQ(LOAD(excptr), CONSTANT_PYOBJ(Py_None)), proceed, IR_SEMILIKELY);
    IF_ELSE(IR_PyLong_Check(LOAD(excptr)), IR_SEMILIKELY, {
        STORE(excptr, CONSTANT_PYOBJ(Py_None));
    }, {
        STORE(valptr, SECOND());
        STORE(tbptr, THIRD());
    });

    LABEL(proceed);
    JTYPE sig2 = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr_ptr, ir_type_pyssizet, ir_type_pyobject_ptr);
    JVALUE res = CALL_NATIVE(sig2, _PyObject_FastCallDict, exit_func, jd->tmpstack, CONSTANT_PYSSIZET(3), CONSTANT_PYOBJ(NULL));
    GOTO_ERROR_IF_NOT(res);
    /* Duplicating the exception on the stack */
    INCREF(LOAD(excptr));
    PUSH(LOAD(excptr));
    PUSH(res);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(WITH_CLEANUP_FINISH) {
    JVALUE res = POP();
    JVALUE exc = POP();
    JVALUE err = ALLOCA(ir_type_int);
    STORE(err, CONSTANT_INT(0));
    IF(CMP_NE(exc, CONSTANT_PYOBJ(Py_None)), IR_SEMILIKELY, {
        STORE(err, CALL_NATIVE(jd->sig_io, PyObject_IsTrue, res));
    });
    DECREF(res);
    DECREF(exc);
    GOTO_ERROR_IF(CMP_LT(LOAD(err), CONSTANT_INT(0)));
    IF(CMP_GT(LOAD(err), CONSTANT_INT(0)), IR_SOMETIMES, {
        /* There was an exception and a True return */
        DECREF(POP());
        JVALUE why_silenced = CALL_PyLong_FromLong(CONSTANT_LONG(WHY_SILENCED));
        PUSH(why_silenced);
    });
    SOFT_EVAL_BREAK_CHECK();
}

extern int _PyObject_GetMethod(PyObject *, PyObject *, PyObject **);

EMITTER_FOR(LOAD_METHOD) {
    JVALUE obj = TOP();
    PyObject *name = GETNAME(oparg);
    JVALUE meth;
    JVALUE meth_found;
    if (Py_JITAttrCache == 0) {
        assert(jd->tmpstack_size >= 1);
        JVALUE meth_ptr = jd->tmpstack;
        STORE(meth_ptr, CONSTANT_PYOBJ(NULL));
        JTYPE sig = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr_ptr);
        meth_found = CALL_NATIVE(sig, _PyObject_GetMethod, obj, CONSTANT_PYOBJ(name), meth_ptr);
        meth = LOAD(meth_ptr);
        GOTO_ERROR_IF_NOT(meth);
    } else {
        PyJITAttrCache *ic = PyJITAttrCache_New(name);
        if (!ic) {
            Py_FatalError("Failed to create attribute inline cache");
        }
        JVALUE icvalue = CONSTANT_PTR(ir_type_void_ptr, ic);
        JVALUE meth_found_ptr = ALLOCA(ir_type_int);
        STORE(meth_found_ptr, CONSTANT_INT(0));
        JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_void_ptr, ir_type_int_ptr);
        meth = RTCALL_NZ(sig, PyJITAttrCache_GetMethodFunc(), obj, icvalue, meth_found_ptr);
        meth_found = LOAD(meth_found_ptr);
    }
    IF_ELSE(meth_found, IR_LIKELY, {
        SET_TOP(meth);
        PUSH(obj);
    }, {
        SET_TOP(CONSTANT_PYOBJ(NULL));
        DECREF(obj);
        PUSH(meth);
    });
    SOFT_EVAL_BREAK_CHECK();
}

static
ir_type _create_direct_signature(ir_context context, int argcount) {
    ir_type ret;
    /* PyObject* (PyObject* callable, PyJIT_CallSiteSig*, pyarg1, ..., pyargN) */
    ir_type *argtypes = (ir_type*)malloc((2 + argcount) * sizeof(ir_type));
    argtypes[0] = ir_type_pyobject_ptr;
    argtypes[1] = ir_type_pyjitcallsitesig_ptr;
    for (int i = 0; i < argcount; i++) {
        argtypes[2 + i] = ir_type_pyobject_ptr;
    }
    ret = ir_create_function_type(context, ir_type_pyobject_ptr, 2 + argcount, argtypes);
    free(argtypes);
    return ret;
}

static ir_type _create_eval_signature(ir_context context) {
    /* Signature: PyObject* func(f, throwflag); */
    ir_type argtypes[] = { ir_type_pyframeobject_ptr, ir_type_int };
    return ir_create_function_type(context, ir_type_pyobject_ptr, sizeof(argtypes)/sizeof(argtypes[0]), argtypes);
}

ir_value _emit_call_function(JITData *jd, ir_value callable, int oparg, PyObject *kwnames) {
    JTYPE sig = _create_direct_signature(jd->context, oparg);
    JVALUE trampoline = ALLOCA(sig);
    IF_ELSE(IR_PyFunction_Check(callable), IR_SEMILIKELY, {
        JVALUE callable_casted = CAST(ir_type_pyfunctionobject_ptr, callable);
        STORE(trampoline, LOAD_FIELD(callable_casted, PyFunctionObject, func_jit_call, sig));
    }, {
        IF_ELSE(IR_PyCFunction_Check(callable), IR_SEMILIKELY, {
            JVALUE callable_casted = CAST(ir_type_pycfunctionobject_ptr, callable);
            STORE(trampoline, LOAD_FIELD(callable_casted, PyCFunctionObject, m_jit_call, sig));
        }, {
            STORE(trampoline, CONSTANT_PTR(sig, _PyJIT_GenericTrampoline));
        });
    });

    PyJITCallSiteSig* css = PyJIT_CallSiteSig_GetOrCreate(oparg, kwnames);
    ir_value *args = (ir_value*)malloc((2 + oparg) * sizeof(ir_value));
    args[0] = callable;
    args[1] = CONSTANT_PTR(ir_type_pyjitcallsitesig_ptr, css);
    for (int i = 0; i < oparg; i++) {
        args[2 + i] = PEEK(oparg - i);
    }
    JVALUE res = ir_call(jd->func, LOAD(trampoline), 2 + oparg, args);
    free(args);
    return res;
}

EMITTER_FOR(CALL_METHOD) {
    JVALUE meth = PEEK(oparg + 2);
    JVALUE res = ALLOCA(ir_type_pyobject_ptr);
    IF_ELSE(NOTBOOL(meth), IR_SOMETIMES, {
        STORE(res, _emit_call_function(jd, PEEK(oparg+1), oparg, NULL));
    }, {
        STORE(res, _emit_call_function(jd, meth, oparg + 1, NULL));
    });
    for (int i = 0; i < oparg; i++) {
        DECREF(POP());
    }
    XDECREF(POP());
    XDECREF(POP());
    if (!Py_JITNoExc) {
        GOTO_ERROR_IF_NOT(LOAD(res));
    }
    PUSH(LOAD(res));
    HARD_EVAL_BREAK_CHECK();
}

EMITTER_FOR(CALL_FUNCTION) {
    JVALUE res = _emit_call_function(jd, PEEK(oparg+1), oparg, NULL);
    for (int i = 0; i < oparg; i++) {
        DECREF(POP());
    }
    DECREF(POP()); /* callable */
    if (!Py_JITNoExc) {
        GOTO_ERROR_IF_NOT(res);
    }
    PUSH(res);
    HARD_EVAL_BREAK_CHECK();
}

EMITTER_FOR(CALL_FUNCTION_KW) {
    JVALUE names = POP();
    IR_ASSERT(IR_PyTuple_CheckExact(names));
    IR_ASSERT(CMP_LE(IR_PyTuple_GET_SIZE(names), CONSTANT_PYSSIZET(oparg)));

    /* CALL_FUNCTION_KW is always immediately preceded by LOAD_CONST to fetch
       kwnames. We want kwnames to be available at compile time, so grab it
       directly here. */
    int prev_opcode;
    int prev_oparg;
    FETCH_PREV_INSTR(&prev_opcode, &prev_oparg);
    if (prev_opcode != LOAD_CONST) {
        Py_FatalError("Expected LOAD_CONST before CALL_FUNCTION_KW");
    }
    PyObject *kwnames = PyTuple_GET_ITEM(jd->co->co_consts, prev_oparg);
    IR_ASSERT(CMP_EQ(names, CONSTANT_PYOBJ(kwnames)));

    JVALUE res = _emit_call_function(jd, PEEK(oparg+1), oparg, kwnames);
    for (int i = 0; i < oparg; i++) {
        DECREF(POP());
    }
    DECREF(POP()); /* callable */
    DECREF(names);
    GOTO_ERROR_IF_NOT(res);
    PUSH(res);
    HARD_EVAL_BREAK_CHECK();
}

/* Helper to convert "kwargs" to a dict, when it is not already an exact dict */
PyObject *
_call_function_ex_make_dict(PyObject *func, PyObject *kwargs) {
    PyObject *d = PyDict_New();
    if (d == NULL)
        return NULL;
    if (PyDict_Update(d, kwargs) != 0) {
        Py_DECREF(d);
        /* PyDict_Update raises attribute
         * error (percolated from an attempt
         * to get 'keys' attribute) instead of
         * a type error if its second argument
         * is not a mapping.
         */
        if (PyErr_ExceptionMatches(PyExc_AttributeError)) {
            format_kwargs_mapping_error(func, kwargs);
        }
        return NULL;
    }
    return d;
}

PyObject *
_call_function_ex_make_tuple(PyObject *func, PyObject *callargs) {
    if (check_args_iterable(func, callargs) < 0) {
        return NULL;
    }
    return PySequence_Tuple(callargs);
}

PyObject * do_call_core(PyObject *, PyObject *, PyObject *);

EMITTER_FOR(CALL_FUNCTION_EX) {
    /* Stack on entry: func | callargs [| kwargs] */
    JVALUE kwargs = ALLOCA(ir_type_pyobject_ptr);
    if (oparg & 0x01) {
        STORE(kwargs, POP());
        IF_NOT(IR_PyDict_CheckExact(LOAD(kwargs)), IR_UNLIKELY, {
            JVALUE d = CALL_NATIVE(jd->sig_ooo, _call_function_ex_make_dict, SECOND(), LOAD(kwargs));
            DECREF(LOAD(kwargs));
            GOTO_ERROR_IF_NOT(d);
            STORE(kwargs, d);
        });
        IR_ASSERT(IR_PyDict_CheckExact(LOAD(kwargs)));
    } else {
        STORE(kwargs, CONSTANT_PYOBJ(NULL));
    }
    JVALUE callargs = ALLOCA(ir_type_pyobject_ptr);
    STORE(callargs, POP());
    IF_NOT(IR_PyTuple_CheckExact(LOAD(callargs)), IR_UNLIKELY, {
        JVALUE t = CALL_NATIVE(jd->sig_ooo, _call_function_ex_make_tuple, TOP(), LOAD(callargs));
        DECREF(LOAD(callargs));
        GOTO_ERROR_IF_NOT(t);
        STORE(callargs, t);
    });
    IR_ASSERT(IR_PyTuple_CheckExact(LOAD(callargs)));
    JVALUE func = POP();
    JVALUE result = CALL_NATIVE(jd->sig_oooo, do_call_core, func, LOAD(callargs), LOAD(kwargs));
    DECREF(func);
    DECREF(LOAD(callargs));
    if (oparg & 0x01) {
        DECREF(LOAD(kwargs));
    }
    GOTO_ERROR_IF_NOT(result);
    PUSH(result);
    HARD_EVAL_BREAK_CHECK();
}

EMITTER_FOR(MAKE_FUNCTION) {
    JVALUE qualname = POP();
    JVALUE codeobj = POP();
    JVALUE globals = LOAD_F_GLOBALS();
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyfunctionobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    JVALUE func = CALL_NATIVE(sig, PyFunction_NewWithQualName, codeobj, globals, qualname);
    DECREF(codeobj);
    DECREF(qualname);
    GOTO_ERROR_IF_NOT(func);

    /* Set func_jit_parent */
    JVALUE parentjf = CONSTANT_PYOBJ((PyObject*)jd->jit_function);
    INCREF(parentjf);
    STORE_FIELD(func, PyFunctionObject, func_jit_parent, ir_type_pyobject_ptr, parentjf);

    if (oparg & 0x08) {
        JVALUE closure = POP();
        IR_ASSERT(IR_PyTuple_CheckExact(closure));
        STORE_FIELD(func, PyFunctionObject, func_closure, ir_type_pyobject_ptr, closure);
    }
    if (oparg & 0x04) {
        JVALUE annotations = POP();
        IR_ASSERT(IR_PyDict_CheckExact(annotations));
        STORE_FIELD(func, PyFunctionObject, func_annotations, ir_type_pyobject_ptr, annotations);
    }
    if (oparg & 0x02) {
        JVALUE kwdefaults = POP();
        IR_ASSERT(IR_PyDict_CheckExact(kwdefaults));
        STORE_FIELD(func, PyFunctionObject, func_kwdefaults, ir_type_pyobject_ptr, kwdefaults);
    }
    if (oparg & 0x01) {
        JVALUE defaults = POP();
        IR_ASSERT(IR_PyTuple_CheckExact(defaults));
        STORE_FIELD(func, PyFunctionObject, func_defaults, ir_type_pyobject_ptr, defaults);
    }
    PUSH(CAST(ir_type_pyobject_ptr, func));
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(BUILD_SLICE) {
    JVALUE step = (oparg == 3) ? POP() : CONSTANT_PYOBJ(NULL);
    JVALUE stop = POP();
    JVALUE start = POP();
    JVALUE slice = CALL_NATIVE(jd->sig_oooo, PySlice_New, start, stop, step);
    DECREF(start);
    DECREF(stop);
    if (oparg == 3) {
        DECREF(step);
    }
    GOTO_ERROR_IF_NOT(slice);
    PUSH(slice);
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(FORMAT_VALUE) {
    PyObject *(*conv_fn)(PyObject *);
    int which_conversion = oparg & FVC_MASK;
    int have_fmt_spec = (oparg & FVS_MASK) == FVS_HAVE_SPEC;

    JVALUE fmt_spec = have_fmt_spec ? POP() : NULL;
    JVALUE valueptr = ALLOCA(ir_type_pyobject_ptr);
    STORE(valueptr, POP());

    /* See if any conversion is specified. */
    switch (which_conversion) {
    case FVC_STR:   conv_fn = PyObject_Str;   break;
    case FVC_REPR:  conv_fn = PyObject_Repr;  break;
    case FVC_ASCII: conv_fn = PyObject_ASCII; break;

    /* Must be 0 (meaning no conversion), since only four
       values are allowed by (oparg & FVC_MASK). */
    default:        conv_fn = NULL;           break;
    }

    /* If there's a conversion function, call it and replace
       value with that result. Otherwise, just use value,
       without conversion. */
    if (conv_fn != NULL) {
        JLABEL conv_ok = JLABEL_INIT("conv_ok");
        JVALUE conv_value = CALL_NATIVE(jd->sig_oo, conv_fn, LOAD(valueptr));
        DECREF(LOAD(valueptr));
        BRANCH_IF(conv_value, conv_ok, IR_LIKELY);
        /* Handle error (conv_fn returns NULL) */
        if (have_fmt_spec) {
            DECREF(fmt_spec);
        }
        GOTO_ERROR();
        LABEL(conv_ok);
        STORE(valueptr, conv_value);
    }

    JLABEL skip_format = JLABEL_INIT("skip_format");
    if (!have_fmt_spec) {
        BRANCH_IF(IR_PyUnicode_CheckExact(LOAD(valueptr)), skip_format, IR_LIKELY);
    }

    /* Actually call format() */
    JVALUE result = CALL_NATIVE(
         jd->sig_ooo, PyObject_Format, LOAD(valueptr), fmt_spec ? fmt_spec : CONSTANT_PYOBJ(NULL));
    DECREF(LOAD(valueptr));
    if (have_fmt_spec) {
        DECREF(fmt_spec);
    }
    GOTO_ERROR_IF_NOT(result);
    STORE(valueptr, result);

    /* Done */
    LABEL(skip_format);
    PUSH(LOAD(valueptr));
    SOFT_EVAL_BREAK_CHECK();
}

EMITTER_FOR(EXTENDED_ARG) {
    abort(); /* EXTENDED_ARG is handled by jit.c */
}

/* --- END EMITTERS --- */

#include "Python/opcode_emitter_table.h"
#include "Python/opcode_names.h"

#define IR_DEBUG_DUMP(filename) do { \
    if (Py_JITDebugFlag > 1) { \
        ir_func_dump_file(jd->func, (filename)); \
    } \
} while (0)

static void
_initialize_virtual_locals(JITData *jd)
{
    assert(!jd->is_generator);
    PyCodeObject *co = jd->co;

    /* Precompute which slots are copied from args, which are copied into cells,
       and which are initialized as empty cells */
    int ncellvars = PyTuple_GET_SIZE(co->co_cellvars);
    int nfreevars = PyTuple_GET_SIZE(co->co_freevars);
    int *slotmap = (int*)malloc((co->co_nlocals + ncellvars) * sizeof(int));
    int i;
    /* Arguments are copied by default */
    for (i = 0; i < jd->total_pyargs; i++) {
        slotmap[i] = i;
    }
    /* Remaining locals are initialized to NULL */
    while (i < co->co_nlocals) {
        slotmap[i++] = -1;
    }
    assert(i == co->co_nlocals);

    /* Initialize cell vars. Some are initialized from arguments,
       all others are initialized to NULL. */
    for (int j = 0; j < ncellvars; j++, i++) {
        Py_ssize_t arg;
        if (co->co_cell2arg != NULL &&
            (arg = co->co_cell2arg[j]) != CO_CELL_NOT_AN_ARG) {
            slotmap[i] = arg;
            /* Clear the local copy */
            assert(arg >= 0 && arg < jd->total_pyargs);
            slotmap[arg] = -1;
        } else {
            slotmap[i] = -1;
        }
    }
    assert(i == co->co_nlocals + ncellvars);

    /* Now actually initialize the locals */
    for (i = 0; i < co->co_nlocals; i++) {
        if (slotmap[i] == -1) {
            ir_dellocal(jd->func, i);
        } else {
            ir_value value = ir_func_get_argument(jd->func, ARG_SHIFT + slotmap[i]);
            INCREF(value);
            ir_setlocal(jd->func, i, value);
        }
    }

    /* Initialize cells */
    i = co->co_nlocals;
    for (int j = 0; j < ncellvars; j++, i++) {
        int src = slotmap[i];
        ir_value value;
        if (src == -1) {
            value = jd->null;
        } else {
            value = ir_func_get_argument(jd->func, ARG_SHIFT + slotmap[i]);
        }
        ir_value cell = CALL_PyCell_New(value);
        STORE(jd->fastlocals_values[i], cell);
    }
    assert(i == co->co_nlocals + ncellvars);

    /* Copy closure cells */
    if (jd->has_closure) {
        ir_value closure = ir_func_get_argument(jd->func, ARG_SHIFT + jd->total_pyargs);
        ir_value ob_item = IR_PyTuple_OB_ITEM(closure);
        i = co->co_nlocals + ncellvars;
        for (int j = 0; j < nfreevars; j++, i++) {
            ir_value value = LOAD_AT_INDEX(ob_item, CONSTANT_INT(j));
            INCREF(value);
            STORE(jd->fastlocals_values[i], value);
        }
    }
    assert(i == co->co_nlocals + ncellvars + nfreevars);
    assert(i == (int)jd->fastlocals_size);
    free(slotmap);
}

static void
_clear_virtual_locals(JITData *jd) {
    assert(!jd->is_generator);
    for (size_t i = 0; i < jd->fastlocals_size; i++) {
        XDECREF(LOAD(jd->fastlocals_values[i]));
    }
}

static int _uint64_cmp(const void *ap, const void *bp) {
    uint64_t a = *((const uint64_t*)ap);
    uint64_t b = *((const uint64_t*)bp);
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
}

uint64_t djb2_hash(const unsigned char *in, Py_ssize_t inlen) {
    uint64_t hash = 5381;
    for (Py_ssize_t i = 0; i < inlen; i++) {
        hash = ((hash << 5) + hash) + in[i];
    }
    return hash;
}

uint64_t djb2_code_hash(PyCodeObject *co) {
    return djb2_hash((unsigned char*)PyBytes_AS_STRING(co->co_code), PyBytes_GET_SIZE(co->co_code));
}

#ifndef NDEBUG
/* Verify that jit_locals_hack is sorted in ascending order for bsearch */
static int _verified_jit_locals = 0;
static void _verify_jit_locals(void) {
    if (_verified_jit_locals) return;
    _verified_jit_locals = 1;
    for (size_t i = 0; i+1 < sizeof(jit_locals_hack)/sizeof(uint64_t); i++) {
        uint64_t a = jit_locals_hack[i];
        uint64_t b = jit_locals_hack[i + 1];
        if (a >= b) {
            fprintf(stderr, "jit_locals_hack error: %"PRIu64" >= %"PRIu64"\n", a, b);
            abort();
        }
    }
}
#endif

static int _needs_frame_introspection(PyCodeObject *co) {
#ifndef NDEBUG
    _verify_jit_locals();
#endif
    /* If the function's hash is in jit_locals_hack, force real locals. */
    if (!Py_JITNoSuper) {
        uint64_t hash = djb2_code_hash(co);
        void *match = bsearch(&hash, jit_locals_hack, sizeof(jit_locals_hack)/sizeof(uint64_t), sizeof(uint64_t), _uint64_cmp);
        if (match)
            return 1;
    }

    /* IMPORT_STAR uses PyFrame_FastToLocals */
    SCAN_BYTECODE(co, {
        if (opcode == IMPORT_STAR)
            return 1;
    });
    return 0;
}

static void _init_jitdata(JITData *jd, PyCodeObject *co, PyObject *globals, PyObject *builtins) {
    jd->jit_function = (PyJITFunctionObject*)PyJITFunction_New((PyObject*)co, globals, builtins);
    jd->jstate = calloc(sizeof(_JITState), 1);
    jd->globals = globals;
    jd->builtins = builtins;
    jd->co = co;
    jd->inst_count = PyBytes_GET_SIZE(co->co_code)/sizeof(_Py_CODEUNIT);

    jd->is_generator = (co->co_flags & (CO_GENERATOR | CO_COROUTINE | CO_ASYNC_GENERATOR)) ? 1 : 0;
    jd->has_vararg = (co->co_flags & CO_VARARGS) ? 1 : 0;
    jd->has_kwdict = (co->co_flags & CO_VARKEYWORDS) ? 1 : 0;
    jd->has_closure = (PyTuple_GET_SIZE(co->co_freevars) > 0) ? 1 : 0;
    jd->has_locals = !(co->co_flags & CO_NEWLOCALS);
    jd->total_pyargs = co->co_argcount + co->co_kwonlyargcount + jd->has_vararg + jd->has_kwdict;
    jd->total_direct_args = jd->total_pyargs + jd->has_closure + jd->has_locals;

    /* CodeGen settings */
    jd->use_frame_object =
        globals == NULL ||
        builtins == NULL ||
        jd->is_generator ||
        jd->has_locals ||
        _needs_frame_introspection(co);
    jd->use_virtual_locals = !jd->use_frame_object;
    jd->update_blockstack = jd->use_frame_object;

    if (Py_JITDebugFlag > 1) {
        fprintf(stderr, "    code = %p\n", jd->co);
        fprintf(stderr, "    globals = %p\n", jd->globals);
        fprintf(stderr, "    builtins = %p\n", jd->builtins);
        fprintf(stderr, "    is_generator = %d\n", jd->is_generator);
        fprintf(stderr, "    has_vararg = %d\n", jd->has_vararg);
        fprintf(stderr, "    has_kwdict = %d\n", jd->has_kwdict);
        fprintf(stderr, "    has_closure = %d\n", jd->has_closure);
        fprintf(stderr, "    has_locals = %d\n", jd->has_locals);
        fprintf(stderr, "    total_pyargs = %ld\n", (long)jd->total_pyargs);
        fprintf(stderr, "    use_frame_object = %d\n", jd->use_frame_object);
        fprintf(stderr, "    use_virtual_locals = %d\n", jd->use_virtual_locals);
        fprintf(stderr, "    update_blockstack = %d\n", jd->update_blockstack);
    }

    /* Initialize IR */
    jd->context = ir_context_new();
    jd->func_sig = jd->use_frame_object ?
        _create_eval_signature(jd->context) :
        _create_direct_signature(jd->context, jd->total_direct_args);
    jd->func = ir_func_new(jd->context, PyUnicode_AsUTF8(co->co_name), jd->func_sig);

    /* Common function signatures (TODO: make these shared and static) */
    jd->sig_o = CREATE_SIGNATURE(ir_type_pyobject_ptr);
    jd->sig_oo = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    jd->sig_ooo = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    jd->sig_oooo = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    jd->sig_ooooo = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    jd->sig_ooooooo = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    jd->sig_oov = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_void_ptr);
    jd->sig_vp = CREATE_SIGNATURE(ir_type_void, ir_type_void_ptr);
    jd->sig_i = CREATE_SIGNATURE(ir_type_int);
    jd->sig_io = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr);
    jd->sig_ioo = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    jd->sig_iooo = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    jd->sig_ioi = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr, ir_type_int);
    jd->sig_if = CREATE_SIGNATURE(ir_type_int, ir_type_pyframeobject_ptr);
    jd->sig_ol = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_long);
    jd->dealloc_sig = CREATE_SIGNATURE(ir_type_void, ir_type_pyobject_ptr);
    jd->blocksetup_sig = CREATE_SIGNATURE(ir_type_void, ir_type_pyframeobject_ptr, ir_type_int, ir_type_int, ir_type_int);
    jd->blockpop_sig = CREATE_SIGNATURE(ir_type_pytryblock_ptr, ir_type_pyframeobject_ptr);
    jd->exc_triple_sig = CREATE_SIGNATURE(ir_type_void, ir_type_pyobject_ptr_ptr, ir_type_pyobject_ptr_ptr, ir_type_pyobject_ptr_ptr);

    jd->null = CONSTANT_PYOBJ(NULL);

    jd->body_start = ir_label_new(jd->func, "body_start");
    jd->error_exit = NULL;
    jd->exit = ir_label_new(jd->func, "exit");
    jd->unknown_handler = ir_label_new(jd->func, "unknown_handler");

    jd->tmpstack_size = 6;
    jd->tmpstack = ir_alloca(jd->func, ir_type_pyobject_ptr, jd->tmpstack_size);

    /* TODO: Only create labels at instruction starts */
    jd->jmptab = (ir_label*)malloc(sizeof(ir_label) * jd->inst_count);
    for (Py_ssize_t i = 0; i < jd->inst_count; i++) {
        char namebuf[64];
        sprintf(namebuf, "inst_%ld", (long)i);
        jd->jmptab[i] = ir_label_new(jd->func, namebuf);
    }

    if (jd->use_frame_object) {
        /* Arguments: f, throwflag */
        jd->f = ir_func_get_argument(jd->func, 0);
        jd->throwflag = ir_func_get_argument(jd->func, 1);
    } else {
        /* Arguments: ...pyargs..., closure?, locals? */
        jd->f = NULL;
        jd->throwflag = NULL;
    }
    jd->retval = ALLOCA(ir_type_pyobject_ptr);
}

static void
_emit_function_body(JITData *jd)
{
    PyCodeObject *co = jd->co;

    /* Initialization sequence */
    jd->tstate = IR_PyThreadState_GET();

    /* push frame */
    IF(IR_Py_EnterRecursiveCall(""), IR_UNLIKELY, {
        ir_retval(jd->func, CONSTANT_PYOBJ(NULL));
    });

    /* Initialize and push PyRunFrame */
    jd->rf = ir_alloca(jd->func, ir_type_pyrunframe, 1);
    if (jd->use_frame_object) {
        IR_PyRunFrame_Push(jd->rf, jd->tstate, jd->f);
    } else {
        JVALUE jf = CONSTANT_PYOBJ((PyObject*)jd->jit_function);
        JVALUE rflocals;
        if ((co->co_flags & (CO_NEWLOCALS | CO_OPTIMIZED)) ==
            (CO_NEWLOCALS | CO_OPTIMIZED)) {
            rflocals = CONSTANT_PYOBJ(NULL);
        } else if (co->co_flags & CO_NEWLOCALS) {
            rflocals = RTCALL_PyDict_New();
        } else {
            assert(jd->has_locals);
            rflocals = ir_func_get_argument(jd->func, ARG_SHIFT + jd->total_direct_args - 1);
            INCREF(rflocals);
        }
        IR_PyRunFrame_PushNoFrame(jd->rf, jd->tstate, jf, rflocals);
    }

    /* Initialize arguments */
    if (jd->use_virtual_locals) {
        jd->fastlocals = NULL;
        jd->fastlocals_size = co->co_nlocals + PyTuple_GET_SIZE(co->co_cellvars) + PyTuple_GET_SIZE(co->co_freevars);
        jd->fastlocals_values = (ir_value*)malloc(sizeof(ir_value) * jd->fastlocals_size);
        for (int i = 0; i < (int)jd->fastlocals_size; i++) {
            jd->fastlocals_values[i] = ALLOCA(ir_type_pyobject_ptr);
        }
        _initialize_virtual_locals(jd);
    } else {
        jd->fastlocals = LOAD_F_LOCALSPLUS();
        jd->fastlocals_size = 0;
        jd->fastlocals_values = NULL;
    }

    /* Setup retval */
    SET_RETVAL(CONSTANT_PYOBJ(NULL));

    if (jd->use_frame_object) {
        /* Set f_executing = 1 */
        STORE_FIELD(FRAMEPTR(), PyFrameObject, f_executing, ir_type_char, CONSTANT_CHAR(1));

        /* assert f->f_lasti >= -1 */
        IR_ASSERT(CMP_GE(LOAD_F_LASTI(), CONSTANT_INT(-1)));
    }

    if (jd->is_generator) {
        /* To be filled in by ir_lower() */
        ir_yield_dispatch(jd->func, jd->body_start);
    }

    /* Normal function entry (this is also the
       first yield entry point for generators) */
    LABEL(jd->body_start);

    /* Make sure the stack is setup correctly for function start */
    if (jd->use_frame_object) {
#ifndef NDEBUG
        JVALUE f_valuestack = LOAD_FIELD(FRAMEPTR(), PyFrameObject, f_valuestack, ir_type_pyobject_ptr_ptr);
        JVALUE f_stacktop = LOAD_FIELD(FRAMEPTR(), PyFrameObject, f_stacktop, ir_type_pyobject_ptr_ptr);
        IR_ASSERT(CMP_EQ(f_stacktop, f_valuestack));
#endif

        /* We must clear f_stacktop */
        STORE_FIELD(FRAMEPTR(), PyFrameObject, f_stacktop, ir_type_pyobject_ptr_ptr, CONSTANT_PTR(ir_type_pyobject_ptr_ptr, NULL));
    }

    if (jd->is_generator) {
        GOTO_ERROR_IF(jd->throwflag);
    }

#ifdef Py_DEBUG
    /* PyEval_EvalFrameEx() must not be called with an exception set,
       because it can clear it (directly or indirectly) and so the
       caller loses its exception */
    IR_ASSERT(NOTBOOL(IR_PyErr_Occurred()));
#endif

    HARD_EVAL_BREAK_CHECK_EX(0);

    SCAN_BYTECODE(co, {
        LABEL(jd->jmptab[opfirst]);

        char namebuf[64];
        sprintf(namebuf, "inst_%ld.%s", (long)opfirst, opcode_names[opcode]);
        ir_label_push_prefix(jd->func, namebuf);

        // Emit instruction
        // Set f->f_lasti
        STORE_F_LASTI(CONSTANT_INT(oplast * sizeof(_Py_CODEUNIT)));
        opcode_emitter_table[opcode](jd, oplast + 1, opcode, oparg);

        ir_label_pop_prefix(jd->func);
    });

    /* Shouldn't ever fall through to this area */
    CRASH();

    /* Special sections */
    LABEL(jd->exit);
    if (jd->use_virtual_locals) {
        _clear_virtual_locals(jd);
    }

    /* Pop PyRunFrame */
    IR_PyRunFrame_Pop(jd->rf, jd->tstate, jd->use_frame_object);

    /* Leave call */
    IR_Py_LeaveRecursiveCall();

    /* Check return result */
    JVALUE ret = IR_Py_CheckFunctionResult(NULL, LOAD(jd->retval), "PyJIT_EvalFrame");
    ir_retval(jd->func, ret);

    /* This is a placeholder used when pyblock-based control flow instructions are
       created (goto_fbe, goto_error, end_finally, etc). It should be dead
       after the pyblock analysis pass runs, because it replaces the placeholders
       with the actual handlers. */
    LABEL(jd->unknown_handler);
    BRANCH(jd->unknown_handler);

    /* Close the cursor */
    ir_cursor_close(jd->func);
}

static void ir_lower(JITData *jd);

PyJITFunctionObject*
_PyJIT_CodeGen(PyCodeObject *co, PyObject *globals, PyObject *builtins) {
    assert(PyCode_Check(co));
    assert(PyBytes_Check(co->co_code));
    assert(PyBytes_GET_SIZE(co->co_code) <= INT_MAX);
    assert(PyBytes_GET_SIZE(co->co_code) % sizeof(_Py_CODEUNIT) == 0);
    assert(_Py_IS_ALIGNED(PyBytes_AS_STRING(co->co_code), sizeof(_Py_CODEUNIT)));

    if (globals == NULL || !PyDict_CheckExact(globals) ||
        builtins == NULL || !PyDict_CheckExact(builtins)) {
        /* Force generic code */
        globals = NULL;
        builtins = NULL;
    }
    if (Py_JITDebugFlag > 0) {
        fprintf(stderr, "Entering CodeGen for %s:%s\n",
            PyUnicode_AsUTF8(co->co_filename),
            PyUnicode_AsUTF8(co->co_name));
    }

    JITData *jd = PyMem_RawMalloc(sizeof(JITData));
    if (jd == NULL) {
        PyErr_NoMemory();
        return NULL;
    }
    memset(jd, 0, sizeof(JITData));
    _init_jitdata(jd, co, globals, builtins);
    _emit_function_body(jd);

    IR_DEBUG_DUMP("/tmp/00-initial.ir");
    if (Py_JITDebugFlag > 0)
        ir_func_verify(jd->func);

    /* Lower all Python-specific instructions */
    ir_lower(jd);

    /* Compile IR to machine code object */
    ir_object object;
    if (Py_JITFlag == 1) {
        object = ir_libjit_compile(jd->func);
    } else if (Py_JITFlag == 2) {
        object = ir_llvm_compile(jd->func);
    } else {
        Py_FatalError("invalid PYJIT value");
        Py_UNREACHABLE();
    }

    /* Populate JIT object for return */
    PyJITFunctionObject *jf = jd->jit_function;
    jf->uses_frame_object = jd->use_frame_object;
    jf->eval_entrypoint = NULL;
    jf->direct_entrypoint = NULL;
    jf->object = object;
    jf->eval_entrypoint_object = NULL;
    jf->jstate = (void*)jd->jstate;
    if (jd->use_frame_object) {
        jf->eval_entrypoint = (PyJIT_EvalEntryPoint)object->entrypoint;
    } else {
        jf->direct_entrypoint = (PyJIT_DirectEntryPoint)object->entrypoint;
    }
    ir_context_destroy(jd->context);
    if (jd->fastlocals_values) {
        free(jd->fastlocals_values);
    }
    free(jd->jmptab);
    PyMem_RawFree(jd);
    return jf;
}

void _jeval_cleanup(PyJITFunctionObject *jf) {
    if (jf->object != NULL) {
        ir_object_free((ir_object)jf->object);
        jf->object = NULL;
    }
    if (jf->eval_entrypoint_object != NULL) {
        ir_object_free((ir_object)jf->eval_entrypoint_object);
        jf->eval_entrypoint_object = NULL;
    }
    if (jf->jstate != NULL) {
        _JITState *js = (_JITState*)jf->jstate;
        if (js->globals_dl)
            _PyDict_DeleteListener((PyDictObject*)jf->globals, js->globals_dl);
        if (js->builtins_dl)
            _PyDict_DeleteListener((PyDictObject*)jf->builtins, js->builtins_dl);
        if (js->load_global_cache != NULL)
            free(js->load_global_cache);
        free(js);
        jf->jstate = NULL;
    }
}

static inline
int _pyblock_type_to_b_type(ir_pyblock_type type) {
    switch (type) {
    case IR_PYBLOCK_LOOP: return SETUP_LOOP;
    case IR_PYBLOCK_EXCEPT: return SETUP_EXCEPT;
    case IR_PYBLOCK_FINALLY_TRY: return SETUP_FINALLY;
    case IR_PYBLOCK_EXCEPT_HANDLER: return EXCEPT_HANDLER;
    default: break;
    }
    abort(); /* Unexpected case */
}

static int _compute_pyblock_depth(ir_pyblock pb) {
    int count = 0;
    ir_pyblock cur = pb;
    while (cur != NULL) {
        count++;
        cur = cur->prev;
    }
    return count;
}

static void _push_except_handler_to_blockstack(JITData *jd, int b_level) {
    if (!jd->update_blockstack) return;

    CALL_NATIVE(jd->blocksetup_sig, PyFrame_BlockSetup,
         FRAMEPTR(), CONSTANT_INT(EXCEPT_HANDLER), CONSTANT_INT(-1), CONSTANT_INT(b_level));
}

static void _push_pyblock_to_blockstack(JITData *jd, ir_pyblock pb) {
    if (!jd->update_blockstack) return;

    int b_type = _pyblock_type_to_b_type(pb->b_type);
    int b_handler;
    if (b_type == EXCEPT_HANDLER) {
        assert(pb->b_handler == NULL);
        b_handler = -1;
    } else {
        assert(pb->b_handler != NULL);
        b_handler = sizeof(_Py_CODEUNIT) * _label_to_instr_index(jd, pb->b_handler);
    }

    /* Verify the depth */
    IR_ASSERT(CMP_EQ(LOAD_FIELD(FRAMEPTR(), PyFrameObject, f_iblock, ir_type_int),
                     CONSTANT_INT(_compute_pyblock_depth(pb->prev))));

    CALL_NATIVE(jd->blocksetup_sig, PyFrame_BlockSetup,
                FRAMEPTR(),
                CONSTANT_INT(b_type),
                CONSTANT_INT(b_handler),
                CONSTANT_INT(pb->b_level));
}

static void _pop_pyblock_from_blockstack(JITData *jd, ir_pyblock pb) {
    if (!jd->update_blockstack) return;

    /* Verify the depth */
    IR_ASSERT(CMP_EQ(LOAD_FIELD(FRAMEPTR(), PyFrameObject, f_iblock, ir_type_int),
                     CONSTANT_INT(_compute_pyblock_depth(pb))));
    JVALUE b = CALL_NATIVE(jd->blockpop_sig, PyFrame_BlockPop, FRAMEPTR());
#ifdef Py_DEBUG
    /* Verify the fields */
    int b_type = _pyblock_type_to_b_type(pb->b_type);
    int b_handler;
    if (b_type == EXCEPT_HANDLER) {
        assert(pb->b_handler == NULL);
        b_handler = -1;
    } else {
        assert(pb->b_handler != NULL);
        b_handler = sizeof(_Py_CODEUNIT) * _label_to_instr_index(jd, pb->b_handler);
    }
    IR_ASSERT(CMP_EQ(LOAD_FIELD(b, PyTryBlock, b_type, ir_type_int), CONSTANT_INT(b_type)));
    IR_ASSERT(CMP_EQ(LOAD_FIELD(b, PyTryBlock, b_handler, ir_type_int), CONSTANT_INT(b_handler)));
    IR_ASSERT(CMP_EQ(LOAD_FIELD(b, PyTryBlock, b_level, ir_type_int), CONSTANT_INT(pb->b_level)));
#else
    (void)b;
#endif
}

static
void _emit_end_finally(JITData *jd, ir_instr_end_finally instr) {
    IR_LABEL_INIT(try_case_2);
    IR_LABEL_INIT(try_case_3);
    IR_LABEL_INIT(normal_exit);
    ir_pyblock pb = instr->pb;
    int estack = instr->entry_stack_level;
    ir_instr _instr = (ir_instr)instr;
    ir_label fallthrough = IR_INSTR_OPERAND(_instr, 0);
    ir_label b_handler_exception = IR_INSTR_OPERAND(_instr, 1);
    ir_label b_handler_return = IR_INSTR_OPERAND(_instr, 2);
    ir_label b_handler_break = IR_INSTR_OPERAND(_instr, 3);
    ir_label b_handler_continue = IR_INSTR_OPERAND(_instr, 4);

    _pop_pyblock_from_blockstack(jd, pb);

    JVALUE status = POP();

    /* Case 1: status is PyLong (WHY code) */
    BRANCH_IF_NOT(IR_PyLong_Check(status), try_case_2, IR_SEMILIKELY);
    JVALUE why = CAST(ir_type_int, CALL_PyLong_AsLong(status));
    DECREF(status);

    IF(CMP_EQ(why, CONSTANT_INT(WHY_RETURN)), IR_SEMILIKELY, {
        STORE(jd->retval, POP());
        UNWIND_BLOCK_NEW(estack-2, pb);
        GOTO_FAST_BLOCK_END_EX(RETURN, b_handler_return, estack-6, pb->prev);
    });

    /* Only emit WHY_CONTINUE if there's a loop with a continue target above us */
    if (b_handler_continue != jd->unknown_handler) {
        ir_pyblock cur = pb;
        while (cur && cur->b_type != IR_PYBLOCK_LOOP) cur = cur->prev;
        assert(cur && cur->b_type == IR_PYBLOCK_LOOP && cur->b_continue != NULL);
        IF(CMP_EQ(why, CONSTANT_INT(WHY_CONTINUE)), IR_SEMILIKELY, {
            UNWIND_BLOCK_NEW(estack-1, pb);
            GOTO_FAST_BLOCK_END_CONTINUE_EX(b_handler_continue, cur->b_continue, estack-6, pb->prev);
        });
    } else {
        IR_ASSERT(CMP_NE(why, CONSTANT_INT(WHY_CONTINUE)));
    }

    /* Only emit WHY_BREAK if there's a loop above us */
    if (b_handler_break != jd->unknown_handler) {
        IF(CMP_EQ(why, CONSTANT_INT(WHY_BREAK)), IR_SEMILIKELY, {
            UNWIND_BLOCK_NEW(estack-1, pb);
            GOTO_FAST_BLOCK_END_EX(BREAK, b_handler_break, estack-6, pb->prev);
        });
    } else {
        IR_ASSERT(CMP_NE(why, CONSTANT_INT(WHY_BREAK)));
    }

    IF(CMP_EQ(why, CONSTANT_INT(WHY_SILENCED)), IR_SEMILIKELY, {
        /* An exception was silenced by 'with', we must
        manually unwind the EXCEPT_HANDLER block which was
        created when the exception was caught, otherwise
        the stack will be in an inconsistent state. */
        UNWIND_EXCEPT_ONLY_NEW(estack-1, pb);
        BRANCH(fallthrough);
    });
    CRASH(); /* Shouldn't get here. Invalid why value? */

    LABEL(try_case_2);
    BRANCH_IF_NOT(IR_PyExceptionClass_Check(status), try_case_3, IR_SEMILIKELY);
    {
        JVALUE exc = POP();
        JVALUE tb = POP();
        JTYPE sig2 = CREATE_SIGNATURE(ir_type_void, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
        CALL_NATIVE(sig2, PyErr_Restore, status, exc, tb);
        UNWIND_EXCEPT_ONLY_NEW(estack-3, pb);
        GOTO_FAST_BLOCK_END_EX(EXCEPTION, b_handler_exception, estack-6, pb->prev);
    }

    LABEL(try_case_3);
    BRANCH_IF(CMP_EQ(status, CONSTANT_PYOBJ(Py_None)), normal_exit, IR_LIKELY);
    {
        CALL_Py_FatalError("'finally' pops bad exception");
        CRASH();
    }

    LABEL(normal_exit);
    UNWIND_BLOCK_NEW(estack-1, pb);
    DECREF(status);
    BRANCH(fallthrough);
}

static inline
int _ir_why_to_why(ir_why why) {
    switch (why) {
    case IR_WHY_NOT: return WHY_NOT;
    case IR_WHY_EXCEPTION: return WHY_EXCEPTION;
    case IR_WHY_RETURN: return WHY_RETURN;
    case IR_WHY_BREAK: return WHY_BREAK;
    case IR_WHY_CONTINUE: return WHY_CONTINUE;
    case IR_WHY_SILENCED: return WHY_SILENCED;
    }
    abort();
}

static inline
void _emit_resume_stub(JITData *jd, ir_label resume_label, ir_label b_handler_exception, int stack_level, ir_pyblock pb, int target_inst_index, ir_label target_inst_label) {
    LABEL(resume_label);
    JVALUE f_valuestack = LOAD_FIELD(FRAMEPTR(), PyFrameObject, f_valuestack, ir_type_pyobject_ptr_ptr);
#ifndef NDEBUG
    JVALUE expected_stack_pointer = ir_get_index_ptr(jd->func, f_valuestack, CONSTANT_INT(stack_level));
    JVALUE f_stacktop = LOAD_FIELD(FRAMEPTR(), PyFrameObject, f_stacktop, ir_type_pyobject_ptr_ptr);
    IR_ASSERT(CMP_EQ(f_stacktop, expected_stack_pointer));
#endif
    STORE_FIELD(FRAMEPTR(), PyFrameObject, f_stacktop, ir_type_pyobject_ptr_ptr, CONSTANT_PTR(ir_type_pyobject_ptr_ptr, NULL));

    /* Restore stack */
    STACKADJ(stack_level);
    for (int i = 0; i < stack_level; i++) {
        JVALUE obj = LOAD_AT_INDEX(f_valuestack, CONSTANT_INT(i));
        PUT(stack_level - i, obj);
    }

    GOTO_ERROR_IF_EX(jd->throwflag, b_handler_exception, stack_level, pb);
    IR_ASSERT(NOTBOOL(IR_PyErr_Occurred()));
    ir_cursor_set_pyblock(jd->func, stack_level, pb);
    HARD_EVAL_BREAK_CHECK_EX(target_inst_index);
    ir_cursor_clear_pyblock(jd->func);
    BRANCH(target_inst_label);
}

static
void _emit_patchpoint_error_handler(JITData *jd, ir_pyblock pb, int entry_stack_level);

/* The error exit is where we jump when an error occurs, and after the stack has
   been unwound. It is similar to the precursor, except instead of pushing the
   exception onto the stack, it just exits the function. */
void _emit_error_exit(JITData *jd) {
    if (jd->error_exit != NULL) {
        BRANCH(jd->error_exit);
    } else {
        jd->error_exit = ir_label_new(jd->func, "error_exit");
        LABEL(jd->error_exit);
        IR_ASSERT(IR_PyErr_Occurred());
        /* Insert the current frame into the traceback */
        CALL_NATIVE(jd->sig_if, PyTraceBack_Here, FRAMEMAT());
        SET_RETVAL(CONSTANT_PYOBJ(NULL));
        BRANCH(jd->exit);
    }
}

/* A precursor is the code that executes when an exception occurs,
   after the stack has unwound, and when the exception is going to be
   handled by a pyblock. There is one precursor per pyblock of type
   EXCEPT or FINALLY_TRY. (since the stack level at each precursor may be different).
 */
void _emit_precursor(JITData *jd, ir_pyblock pb) {
    LABEL(pb->b_handler_precursor);
    IR_ASSERT(IR_PyErr_Occurred());

    /* Insert the current frame into the traceback */
    CALL_NATIVE(jd->sig_if, PyTraceBack_Here, FRAMEMAT());

    /* The except handler has the same stack level as the EXCEPT
       or TRY_FINALLY block which caused it. */
    _push_except_handler_to_blockstack(jd, pb->b_level);

    /* Push the old exc_info onto the stack */
    JVALUE exc_info = LOAD_FIELD(jd->tstate, PyThreadState, exc_info, ir_type_pyerr_stackitem_ptr);
    PUSH(LOAD_FIELD(exc_info, _PyErr_StackItem, exc_traceback, ir_type_pyobject_ptr));
    PUSH(LOAD_FIELD(exc_info, _PyErr_StackItem, exc_value, ir_type_pyobject_ptr));
    JVALUE exc_type = LOAD_FIELD(exc_info, _PyErr_StackItem, exc_type, ir_type_pyobject_ptr);
    IF_ELSE(exc_type, IR_SEMILIKELY, {
        PUSH(exc_type);
    }, {
        JVALUE none = CONSTANT_PYOBJ(Py_None);
        INCREF(none);
        PUSH(none);
    });

    /* Fetch and normalie the new exception */
    JVALUE exc_ptr = jd->tmpstack;
    JVALUE val_ptr = ir_get_index_ptr(jd->func, jd->tmpstack, CONSTANT_INT(1));
    JVALUE tb_ptr = ir_get_index_ptr(jd->func, jd->tmpstack, CONSTANT_INT(2));
    CALL_PyErr_Fetch(exc_ptr, val_ptr, tb_ptr);
    /* Make the raw exception data
       available to the handler,
       so a program can emulate the
       Python main loop. */
    CALL_PyErr_NormalizeException(exc_ptr, val_ptr, tb_ptr);
    JVALUE exc = LOAD(exc_ptr);
    JVALUE val = LOAD(val_ptr);
    JVALUE tb = LOAD(tb_ptr);
    IF_ELSE(tb, IR_SEMILIKELY, {
        CALL_PyException_SetTraceback(val, tb);
    }, {
        CALL_PyException_SetTraceback(val, CONSTANT_PYOBJ(Py_None));
    });

    /* Push the new exception onto the stack, and also store in exc_info */
    INCREF(exc);
    STORE_FIELD(exc_info, _PyErr_StackItem, exc_type, ir_type_pyobject_ptr, exc);
    INCREF(val);
    STORE_FIELD(exc_info, _PyErr_StackItem, exc_value, ir_type_pyobject_ptr, val);
    STORE_FIELD(exc_info, _PyErr_StackItem, exc_traceback, ir_type_pyobject_ptr, tb);
    JVALUE tb_or_none = TERNARY(tb, tb, CONSTANT_PYOBJ(Py_None));
    INCREF(tb_or_none);
    PUSH(tb_or_none);
    PUSH(val);
    PUSH(exc);
    //SOFT_EVAL_BREAK_CHECK?
    BRANCH(pb->b_handler);
}

static inline
int _ir_lower_control_flow(JITData *jd, ir_instr _instr) {
    switch (IR_INSTR_OPCODE(_instr)) {
    case ir_opcode_setup_block: {
        IR_INSTR_AS(setup_block)
        assert(instr->pb != NULL && instr->pb != INVALID_PYBLOCK);
        assert(instr->pb->b_type == instr->b_type);
        assert(instr->entry_stack_level == instr->pb->b_level);

        /* Each error handling target has an precursor, which saves the exception to the stack */
        if (instr->pb->b_type == IR_PYBLOCK_EXCEPT ||
            instr->pb->b_type == IR_PYBLOCK_FINALLY_TRY) {
            IR_LABEL_INIT(skip_over)
            BRANCH(skip_over);
            if (instr->pb->b_handler_precursor == NULL) {
                instr->pb->b_handler_precursor = ir_label_new(jd->func, "precursor");
            }
            _emit_precursor(jd, instr->pb);
            LABEL(skip_over);
        }
        _push_pyblock_to_blockstack(jd, instr->pb);
        return 1;
    }
    case ir_opcode_pop_block: {
        IR_INSTR_AS(pop_block)
        assert(instr->pb != NULL && instr->pb != INVALID_PYBLOCK);
        _pop_pyblock_from_blockstack(jd, instr->pb);
        if (instr->b_type == IR_PYBLOCK_EXCEPT_HANDLER) {
            UNWIND_EXCEPT_HANDLER_NEW(instr->entry_stack_level, instr->pb);
        } else {
            assert(instr->b_type == IR_PYBLOCK_ANY);
            UNWIND_BLOCK_NEW(instr->entry_stack_level, instr->pb);
        }
        return 1;
    }
    case ir_opcode_goto_error: {
        IR_INSTR_AS(goto_error)
        assert(instr->pb != INVALID_PYBLOCK);
        if (Py_JITPatchpoint) {
            /* This will unwind the stack and branch to the precursor
               (same as the code below) */
            _emit_patchpoint_error_handler(jd, instr->pb, instr->entry_stack_level);
            return 1;
        }

        /* Unwind until we hit the pyblock which handles this error */
        ir_pyblock cur = instr->pb;
        int curlevel = instr->entry_stack_level;
        while (cur) {
            _pop_pyblock_from_blockstack(jd, cur);
            if (cur->b_type == IR_PYBLOCK_EXCEPT_HANDLER) {
                UNWIND_EXCEPT_HANDLER_NEW(curlevel, cur);
                curlevel = cur->b_level;
                cur = cur->prev;
                continue;
            }
            UNWIND_BLOCK_NEW(curlevel, cur);
            curlevel = cur->b_level;

            if (cur->b_type == IR_PYBLOCK_EXCEPT ||
                cur->b_type == IR_PYBLOCK_FINALLY_TRY) {
                break;
            }
            cur = cur->prev;
        }

        /* Unwind the stack, then branch or exit */
        if (cur) {
            if (cur->b_handler_precursor == NULL)
                cur->b_handler_precursor = ir_label_new(jd->func, "precursor");
            BRANCH(cur->b_handler_precursor);
        } else {
            UNWIND_TO_NEW(curlevel, 0);
            _emit_error_exit(jd);
        }
        return 1;
    }
    case ir_opcode_goto_fbe: {
        IR_INSTR_AS(goto_fbe)
        assert(instr->pb != INVALID_PYBLOCK);
        ir_why why = instr->why;
        ir_pyblock cur = instr->pb;
        int curlevel = instr->entry_stack_level;
#ifndef NDEBUG
        ir_label expected_handler = IR_INSTR_OPERAND(_instr, 0);
#endif
        while (why != IR_WHY_NOT && cur) {
            if (cur->b_type == IR_PYBLOCK_LOOP && why == IR_WHY_CONTINUE) {
                /* Unwind stack without popping block. 'iter' remains on TOS */
                UNWIND_TO_NEW(curlevel, cur->b_level + 1);
                //SOFT_EVAL_BREAK_CHECK?
                assert(expected_handler == cur->b_continue);
                BRANCH(cur->b_continue);
                why = IR_WHY_NOT;
                break;
            }

            _pop_pyblock_from_blockstack(jd, cur);

            if (cur->b_type == IR_PYBLOCK_EXCEPT_HANDLER) {
                UNWIND_EXCEPT_HANDLER_NEW(curlevel, cur);
                curlevel = cur->b_level;
                cur = cur->prev;
                continue;
            }

            UNWIND_BLOCK_NEW(curlevel, cur);
            curlevel = cur->b_level;

            if (cur->b_type == IR_PYBLOCK_LOOP && why == IR_WHY_BREAK) {
                //SOFT_EVAL_BREAK_CHECK?
                assert(expected_handler == cur->b_handler);
                BRANCH(cur->b_handler);
                why = IR_WHY_NOT;
                break;
            }

            if (why == IR_WHY_EXCEPTION && (cur->b_type == IR_PYBLOCK_EXCEPT ||
                                            cur->b_type == IR_PYBLOCK_FINALLY_TRY)) {
                if (cur->b_handler_precursor == NULL) {
                   cur->b_handler_precursor = ir_label_new(jd->func, "precursor");
                }
                assert(expected_handler == cur->b_handler);
                BRANCH(cur->b_handler_precursor);
                why = IR_WHY_NOT;
                break;
            }
            if (cur->b_type == IR_PYBLOCK_FINALLY_TRY) {
                /* Entering finally: due to return/continue/break */
                _push_except_handler_to_blockstack(jd, cur->b_level);
                JVALUE none = CONSTANT_PYOBJ(Py_None);
                for (int i = 0; i < 4; i++) {
                    INCREF(none);
                    PUSH(none);
                }
                if (why == IR_WHY_RETURN) {
                    PUSH(LOAD(jd->retval));
                } else {
                    INCREF(none);
                    PUSH(none);
                }
                PUSH(CALL_PyLong_FromLong(CONSTANT_LONG((long)_ir_why_to_why(why))));
                //SOFT_EVAL_BREAK_CHECK?
                assert(expected_handler == cur->b_handler);
                BRANCH(cur->b_handler);
                why = IR_WHY_NOT;
                break;
            }
            cur = cur->prev;
        }
        if (why != IR_WHY_NOT) {
            /* We're leaving the function. Do the final unwind. */
            assert(cur == NULL);
            UNWIND_TO_NEW(curlevel, 0);
            if (why != IR_WHY_RETURN) {
                SET_RETVAL(CONSTANT_PYOBJ(NULL));
            }
            assert(expected_handler == jd->exit);
            BRANCH(jd->exit);
        }
        /* We should have branched before this point. Unreachable. */
        CRASH();
        return 1;
    }
    case ir_opcode_yield: {
        IR_INSTR_AS(yield)
        assert(instr->pb != INVALID_PYBLOCK);
        assert(instr->entry_stack_level != -1);
        IR_LABEL_INIT(resume);
        int resume_instr_index = instr->resume_instr_index;
        int yield_f_lasti = sizeof(_Py_CODEUNIT) * (resume_instr_index - 1);
        assert(yield_f_lasti >= 0);

        ir_value yieldval = IR_INSTR_OPERAND(_instr, 0);
        ir_label b_handler_exception = IR_INSTR_OPERAND(_instr, 1);
        ir_label resume_inst_label = IR_INSTR_OPERAND(_instr, 2);

        SET_RETVAL(yieldval);
        JVALUE f_valuestack = LOAD_FIELD(FRAMEPTR(), PyFrameObject, f_valuestack, ir_type_pyobject_ptr_ptr);
        JVALUE stack_pointer = ir_get_index_ptr(jd->func, f_valuestack, CONSTANT_INT(instr->entry_stack_level));
        STORE_FIELD(FRAMEPTR(), PyFrameObject, f_stacktop, ir_type_pyobject_ptr_ptr, stack_pointer);
        STORE_F_LASTI(CONSTANT_INT(yield_f_lasti));

        /* Save stack */
        for (int i = 0; i < instr->entry_stack_level; i++) {
            STORE_AT_INDEX(f_valuestack, CONSTANT_INT(i), PEEK(instr->entry_stack_level - i));
        }
        STACKADJ(-instr->entry_stack_level);
        BRANCH(jd->exit);

        /* ~~~ Magic happens ~~~ */
        ADD_RESUME_ENTRY(yield_f_lasti, resume);
        _emit_resume_stub(jd, resume, b_handler_exception, instr->entry_stack_level + 1, instr->pb, resume_instr_index, resume_inst_label);

        if (IR_INSTR_NUM_OPERANDS(_instr) > 3) {
            /* There is terrible code in genobject.c which increments f_lasti
               when an exception is thrown on a YIELD FROM. Handle this case */
            ir_label throw_inst_label = IR_INSTR_OPERAND(_instr, 3);
            IR_LABEL_INIT(resume_extra);
            ADD_RESUME_ENTRY(yield_f_lasti + sizeof(_Py_CODEUNIT), resume_extra);
            _emit_resume_stub(jd, resume_extra, b_handler_exception, instr->entry_stack_level, instr->pb, resume_instr_index + 1, throw_inst_label);
        }
        return 1;
    }
    case ir_opcode_end_finally: {
        IR_INSTR_AS(end_finally)
        assert(instr->pb != NULL && instr->pb != INVALID_PYBLOCK);
        _emit_end_finally(jd, instr);
        return 1;
    }
    default: break;
    } // switch
    return 0;
}

static inline
int _ir_lower_refcounting(JITData *jd, ir_instr _instr) {
    ir_func func = jd->func;
    switch (IR_INSTR_OPCODE(_instr)) {
    case ir_opcode_incref: {
        IR_INSTR_AS(incref)
        ir_value obj = IR_INSTR_OPERAND(_instr, 0);
        ir_label skip_incref = NULL;
        if (instr->is_xincref) {
            skip_incref = ir_label_new(func, "x_skip_incref");
            ir_branch_if_not(func, obj, skip_incref, IR_UNLIKELY);
        }
#ifdef Py_REF_DEBUG
        ir_value total_addr = ir_constant_pyssizet_ptr(func, &_Py_RefTotal, "&_Py_RefTotal");
        ir_value old_total = ir_load(func, total_addr);
        ir_value new_total = ir_add(func, old_total, ir_constant_pyssizet(func, 1, NULL));
        ir_store(func, total_addr, new_total);
#endif
        ir_value addr = ir_get_element_ptr(func, obj, offsetof(PyObject, ob_refcnt), ir_type_pyssizet, "ob_refcnt");
        ir_value old_value = ir_load(func, addr);
        ir_value new_value = ir_add(func, old_value, ir_constant_pyssizet(func, 1, NULL));
        ir_store(func, addr, new_value);
        if (skip_incref) {
            ir_label_here(func, skip_incref);
        }
        return 1;
    }
    case ir_opcode_decref: {
        IR_INSTR_AS(decref)
        ir_label skip_dealloc = ir_label_new(func, "decref_skip_dealloc");
        ir_value obj = IR_INSTR_OPERAND(_instr, 0);
        if (instr->is_xdecref) {
            ir_branch_if_not(func, obj, skip_dealloc, IR_UNLIKELY);
        }
#ifdef Py_REF_DEBUG
        ir_value total_addr = ir_constant_pyssizet_ptr(func, &_Py_RefTotal, "&_Py_RefTotal");
        ir_value old_total = ir_load(func, total_addr);
        ir_value new_total = ir_sub(func, old_total, ir_constant_pyssizet(func, 1, NULL));
        ir_store(func, total_addr, new_total);
#endif
        ir_value old_value = IR_LOAD_FIELD(func, obj, PyObject, ob_refcnt, ir_type_pyssizet);
        ir_value new_value = ir_sub(func, old_value, ir_constant_pyssizet(func, 1, NULL));
        IR_STORE_FIELD(func, obj, PyObject, ob_refcnt, ir_type_pyssizet, new_value);
        ir_branch_if(func, new_value, skip_dealloc, IR_SEMILIKELY);
        ir_value dealloc_func;
#if defined(Py_DEBUG) || defined(Py_TRACE_REFS)
        dealloc_func = ir_constant_from_ptr(func, jd->dealloc_sig, _Py_Dealloc, "_Py_Dealloc");
#else
        ir_value typeobj = IR_LOAD_FIELD(func, obj, PyObject, ob_type, ir_type_pytypeobject_ptr);
        dealloc_func = IR_LOAD_FIELD(func, typeobj, PyTypeObject, tp_dealloc, jd->dealloc_sig);
#endif
        ir_value args[] = {obj};
        ir_call(func, dealloc_func, 1, args);
        ir_label_here(func, skip_dealloc);
        return 1;
    }
    default: break;
    } // switch
    return 0;
}

static inline
int _ir_lower_stack_ops(JITData *jd, ir_instr _instr) {
    switch (IR_INSTR_OPCODE(_instr)) {
    case ir_opcode_stackadj: {
        return 1;
    }
    case ir_opcode_stack_peek: {
        IR_INSTR_AS(stack_peek)
        assert(instr->abs_offset >= 0);
        assert(instr->abs_offset < jd->stack_size);
        ir_value tmp = LOAD(jd->stack_values[instr->abs_offset]);
        ir_replace_all_uses(jd->func, IR_INSTR_DEST(_instr), tmp);
        return 1;
    }
    case ir_opcode_stack_put: {
        IR_INSTR_AS(stack_put)
        assert(instr->abs_offset >= 0);
        assert(instr->abs_offset < jd->stack_size);
        STORE(jd->stack_values[instr->abs_offset], IR_INSTR_OPERAND(_instr, 0));
        return 1;
    }
    default: break;
    } // switch
    return 0;
}

static inline
int _ir_lower_local_ops(JITData *jd, ir_instr _instr) {
    switch (IR_INSTR_OPCODE(_instr)) {
    case ir_opcode_getlocal: {
        IR_INSTR_AS(getlocal)
        ir_label fallthrough = IR_INSTR_OPERAND(_instr, 0);
        ir_label b_handler = IR_INSTR_OPERAND(_instr, 1);
        ir_value addr;
        if (jd->use_virtual_locals) {
            assert(instr->index < jd->fastlocals_size);
            addr = jd->fastlocals_values[instr->index];
        } else {
            addr = ir_get_index_ptr(jd->func, jd->fastlocals, CONSTANT_INT(instr->index));
        }
        ir_value val = LOAD(addr);
        ir_replace_all_uses(jd->func, IR_INSTR_DEST(_instr), val);
        if (instr->undef_check && !instr->known_defined) {
            IF_NOT(val, IR_UNLIKELY, {
                CALL_format_exc_check_arg(PyExc_UnboundLocalError,
                                          UNBOUNDLOCAL_ERROR_MSG,
                                          PyTuple_GetItem(jd->co->co_varnames, instr->index));
                GOTO_ERROR_EX(b_handler, instr->entry_stack_level, instr->pb);
            });
        }
        BRANCH(fallthrough);
        return 1;
    }
    case ir_opcode_setlocal: {
        IR_INSTR_AS(setlocal)
        ir_value addr;
        if (jd->use_virtual_locals) {
            assert(instr->index < jd->fastlocals_size);
            addr = jd->fastlocals_values[instr->index];
        } else {
            addr = ir_get_index_ptr(jd->func, jd->fastlocals, CONSTANT_INT(instr->index));
        }
        STORE(addr, IR_INSTR_OPERAND(_instr, 0));
        return 1;
    }
    case ir_opcode_dellocal: {
        IR_INSTR_AS(dellocal)
        ir_value addr;
        if (jd->use_virtual_locals) {
            assert(instr->index < jd->fastlocals_size);
            addr = jd->fastlocals_values[instr->index];
        } else {
            addr = ir_get_index_ptr(jd->func, jd->fastlocals, CONSTANT_INT(instr->index));
        }
        STORE(addr, jd->null);
        return 1;
    }
    default: break;
    } // switch
    return 0;
}

int _ir_lower_yield_dispatch(JITData *jd, ir_instr _instr) {
    if (IR_INSTR_OPCODE(_instr) != ir_opcode_yield_dispatch) {
        return 0;
    }
    /* TODO: Find a less quadratic way to do this dispatch */
    JVALUE f_lasti = LOAD_F_LASTI();

    /* Handle generator start */
    ir_label body_start = IR_INSTR_OPERAND(_instr, 0);
    BRANCH_IF(CMP_LT(f_lasti, CONSTANT_INT(0)), body_start, IR_SEMILIKELY);

    /* Handle each resume point */
    resume_entry *re = jd->resume_entry_list;
    resume_entry *re_next;
    while (re != NULL) {
        BRANCH_IF(CMP_EQ(f_lasti, CONSTANT_INT(re->f_lasti)), re->resume_point, IR_SEMILIKELY);
        re_next = re->next;
        PyMem_RawFree(re);
        re = re_next;
    }
    jd->resume_entry_list = NULL;

    /* Should be unreachable */
    CRASH();
    return 1;
}

typedef int (*lowerfunc_t)(JITData*, ir_instr);

static inline
void _lower_pass(JITData *jd, lowerfunc_t lowerfunc) {
    ir_func func = jd->func;
    ir_block b;
    ir_instr _instr;
    ir_instr _instr_next;
    ir_instr _instr_prev;

    for (b = func->first_block; b != NULL; b = b->next) {
        for (_instr = b->first_instr; _instr != NULL; _instr = _instr_next) {
            _instr_next = _instr->next;
            _instr_prev = _instr->prev;

            if (!ir_opcode_is_python_specific(IR_INSTR_OPCODE(_instr)))
                continue;

            /* Set our position to just before instruction we may replace */
            ir_cursor_open(func, b, _instr->prev);

            /* Insert lowered version */
            int did_lower = lowerfunc(jd, _instr);

            if (did_lower) {
                /* Unlink the instruction we lowered */
                assert(func->current_block->current_instr->next == _instr);
                ir_cursor_remove_next(func);

                /* Resume scan at beginning of lowered instructions */
                _instr_next = _instr_prev->next;
                assert(_instr_next != NULL);
            }

            /* Close cursor */
            ir_cursor_close(func);
        }
    }
}

static void compute_pyblock_pass(JITData *jd) {
    ir_compute_pyblock_info(jd->func, jd->unknown_handler, jd->exit);
}

static void remove_dead_blocks_pass(JITData *jd) {
    ir_remove_dead_blocks(jd->func);
}

static void mark_defined_locals_pass(JITData *jd) {
    ir_mark_defined_locals(jd->func, jd->co->co_nlocals);
}

static void lower_local_ops_pass(JITData *jd) {
    _lower_pass(jd, _ir_lower_local_ops);
}

static void lower_control_flow_pass(JITData *jd) {
    _lower_pass(jd, _ir_lower_control_flow);
}

static void lower_yield_dispatch(JITData *jd) {
    if (jd->is_generator) {
        _lower_pass(jd, _ir_lower_yield_dispatch);
    }
}

static void lower_stack_ops_pass(JITData *jd) {
    jd->stack_size = ir_compute_stack_positions(jd->func);
    assert(jd->stack_size <= jd->co->co_stacksize); /* TODO: Why aren't these always equal? */
    jd->stack_values = (ir_value*)malloc(sizeof(ir_value) * jd->stack_size);
    for (int i = 0; i < jd->stack_size; i++) {
        jd->stack_values[i] = ALLOCA(ir_type_pyobject_ptr);
    }
    _lower_pass(jd, _ir_lower_stack_ops);
    free(jd->stack_values);
}

static void lower_refcount_ops_pass(JITData *jd) {
    _lower_pass(jd, _ir_lower_refcounting);
}

static void lower_into_ssa_form(JITData *jd) {
    if (Py_JITDebugFlag > 1) {
        ir_domtree_verify(jd->func);
    }
    ir_domtree dt = ir_compute_domtree_semi_nca(jd->func);
    ir_domtree_delete(dt);
}

typedef void (*passfunc)(JITData*);

typedef struct {
  const char *name;
  passfunc run;
} _pass_info;

static _pass_info lower_passes[] = {
  { "compute_pyblock_info", compute_pyblock_pass },
  { "remove_dead_blocks", remove_dead_blocks_pass },
  { "mark_defined_locals", mark_defined_locals_pass },
  { "lower_local_ops", lower_local_ops_pass },
  { "lower_control_flow", lower_control_flow_pass },
  { "yield_dispatch", lower_yield_dispatch },
  { "remove_dead_blocks", remove_dead_blocks_pass },
  { "lower_stack_ops", lower_stack_ops_pass },
  { "remove_dead_blocks", remove_dead_blocks_pass },
  { "ssa_form", lower_into_ssa_form },
  { "lower_refcount_ops", lower_refcount_ops_pass },
  { NULL, NULL },
};

static void ir_lower(JITData *jd) {
    char filename[128];
    for (size_t i = 0; lower_passes[i].name != NULL; i++) {
        _pass_info *pass = &lower_passes[i];
        pass->run(jd);
        sprintf(filename, "/tmp/%02zu-%s.ir", i+1, pass->name);
        IR_DEBUG_DUMP(filename);
    }
    if (Py_JITDebugFlag > 0)
        ir_func_verify(jd->func);
}

/* Stack Record flags */
#define SR_FLAG_TERMINATOR          0x0  /* Ends the list */
#define SR_FLAG_VALID               0x1  /* Alway set for valid entries */
#define SR_FLAG_DECREF              0x2  /* Slot should be XDECREF'd */
#define SR_FLAG_EXCEPTION_START     0x4  /* Slot begins EXCEPT_HANDLER tuple (length 6) */

typedef struct {
    uint8_t flags;
} _stack_record;

/* _error_info records information at the error site required for
   jeval_error_handler to unwind the Python stack. */
typedef struct {
    ir_stackmap_info stackmap; /* This pointer gets filled in by the IR compiler */
    _stack_record sr[1];
} _error_info;

extern void jeval_error_handler_trampoline(void);

void jeval_error_handler(_error_info *info, void **save_area) {
    ir_stackmap_info stackmap = info->stackmap;

    /* Retrieve the PyRunFrame */
#ifndef NDEBUG
    PyRunFrame *rf = (PyRunFrame*)ir_stackmap_read_value(stackmap, 0, save_area);
    assert(rf != NULL);
#endif

    /* Unwind the stack */
    size_t next_except_handler = 0;
    PyObject *typ = NULL;
    PyObject *val = NULL;
    PyObject *tb = NULL;
    size_t i;
    for (i = 1; i < stackmap->num_values; i++) {
        uint8_t flags = info->sr[i-1].flags;
        assert(flags & SR_FLAG_VALID);
        PyObject *obj = (PyObject*)ir_stackmap_read_value(stackmap, i, save_area);
        if (next_except_handler && i >= next_except_handler) {
            assert(flags == (SR_FLAG_VALID | SR_FLAG_DECREF));
            if (i == next_except_handler) {
                typ = obj;
            } else if (i == next_except_handler + 1) {
                val = obj;
            } else if (i == next_except_handler + 2) {
                tb = obj;
                _unwind_except_helper(typ, val, tb);
                typ = val = tb = NULL;
                next_except_handler = 0;
            }
        } else if (flags & SR_FLAG_EXCEPTION_START) {
            if ((PyLong_Check(obj) && PyLong_AsLong(obj) == WHY_SILENCED) ||
                PyExceptionClass_Check(obj)) {
                /* The following 6 entries form an EXCEPT_HANDLER block. Mark the
                   except part for unwind. */
                assert(next_except_handler == 0);
                next_except_handler = i + 3;
                assert(next_except_handler + 2 < stackmap->num_values);
            }
        } else if (flags & SR_FLAG_DECREF) {
            Py_XDECREF(obj);
        }
    }
    assert(next_except_handler == 0);
    assert(i == stackmap->num_values);
    assert(info->sr[i-1].flags == SR_FLAG_TERMINATOR);
}

static
void _emit_patchpoint_error_handler(JITData *jd, ir_pyblock pb, int entry_stack_level) {
    /* First, compute the total unwind depth, so we know how large to
       make the _error_info structure. */
    ir_pyblock cur = pb;
    int level = entry_stack_level;
    while (cur) {
        assert(level >= cur->b_level);
        if (cur->b_type == IR_PYBLOCK_EXCEPT_HANDLER) {
            level = cur->b_level;
            cur = cur->prev;
            continue;
        }
        level = cur->b_level;
        if (cur->b_type == IR_PYBLOCK_EXCEPT ||
            cur->b_type == IR_PYBLOCK_FINALLY_TRY) {
            break;
        }
        cur = cur->prev;
    }
    if (cur == NULL)
        level = 0;
    int total_unwind = entry_stack_level - level;

    /* Attach error information to handle */
    _error_info *info = (_error_info*)malloc(sizeof(_error_info) + sizeof(_stack_record) * (total_unwind + 1));
    info->stackmap = NULL; /* This will be filled in by the IR compiler */

    /* Prepare argument list for patchpoint. The first argument is a real argument containing
       the pointer to the _error_info for this error. The remaining arguments are map-only.
       The second argument is the PyRunFrame, and later arguments are the stack positions to pop, in
       reverse order. */
    ir_value *args = (ir_value*)malloc((total_unwind + 2) * sizeof(ir_value));
    args[0] = CONSTANT_PTR(ir_type_void_ptr, info);
    args[1] = jd->rf;

    /* Loop again, this time filling in the stack records and arg. */
    cur = pb;
    level = entry_stack_level;
    int i = 0;
#define ADD_RECORD(extra_flags) do { \
    assert(level > 0); \
    assert(i < total_unwind); \
    args[i+2] = PEEK(1+i); \
    info->sr[i].flags = SR_FLAG_VALID | SR_FLAG_DECREF | (extra_flags); \
    i++; \
    level--; \
} while (0)
    while (cur) {
        assert(level >= cur->b_level);
        if (cur->b_type == IR_PYBLOCK_EXCEPT_HANDLER) {
            assert(level >= cur->b_level + 6);
            while (level > cur->b_level + 6) { ADD_RECORD(0); }
            ADD_RECORD(SR_FLAG_EXCEPTION_START);
        }
        while (level > cur->b_level) {
            ADD_RECORD(0);
        }
        if (cur->b_type == IR_PYBLOCK_EXCEPT ||
            cur->b_type == IR_PYBLOCK_FINALLY_TRY) {
            break;
        }
        cur = cur->prev;
    }
    if (cur == NULL) {
        while (level > 0) {
            ADD_RECORD(0);
        }
    }
    assert(i == total_unwind);
    assert(level == entry_stack_level - total_unwind);
    info->sr[i].flags = SR_FLAG_TERMINATOR;
    ir_value trampoline = CONSTANT_PTR(jd->sig_vp, jeval_error_handler_trampoline);
    ir_patchpoint(jd->func, info, trampoline, 1, total_unwind + 2, args);
    free(args);

    STACKADJ(-total_unwind);

    /* Branch or exit */
    if (cur) {
        if (cur->b_handler_precursor == NULL)
            cur->b_handler_precursor = ir_label_new(jd->func, "precursor");
        BRANCH(cur->b_handler_precursor);
    } else {
        _emit_error_exit(jd);
    }
}
