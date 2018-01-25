/* This is required to get MAP_ANONYMOUS with std=c99 */
#define _BSD_SOURCE
#include <sys/mman.h>
#include <stddef.h>

#include "Python.h"
#include "opcode.h"
#include "frameobject.h"
#include "ir.h"
#include "Include/internal/ceval.h"
#include "Include/internal/pystate.h"

#include "Include/jit_macros.h"
#include "Include/jit.h"

/* JIT data attached to a PyCodeObject */
struct _JITData;
typedef struct _JITData JITData;
typedef void (*PyJITEmitHandlerFunction)(JITData *jd, int opcode);
typedef void (*PyJITEmitterFunction)(JITData *jd, int next_instr_index, int opcode, int oparg);
typedef void (*PyJITSpecialEmitterFunction)(JITData *jd);

typedef struct move_entry {
    ir_label from_label;
    ir_label to_label;
    struct move_entry *next;
} move_entry;

typedef struct resume_entry {
    int f_lasti;
    ir_label resume_point;
    struct resume_entry *next;
} resume_entry;

typedef struct _JITData {
    PyCodeObject *co; /* Borrowed reference */
    ir_context context;
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
    ir_type sig_vp;
    ir_type sig_i;
    ir_type sig_io;
    ir_type sig_ioo;
    ir_type sig_iooo;
    ir_type sig_ifi; /* for _do_eval_breaker() helper */
    ir_type sig_ol;  /* for PyLong_FromLong */
    ir_type dealloc_sig;
    ir_type blocksetup_sig;
    ir_type blockpop_sig;
    ir_type exc_triple_sig;

    /* Blocks that will be moved to the end */
    move_entry *move_entry_list;

    int is_gen;
    int update_blockstack; /* Keep f_blockstack in the state ceval expects.
                              Needed for generators, where execution can switch between
                              the JIT and the interpreter (e.g. if tracing is enabled
                              while yielding) */
    int use_patchpoint_error_handler;

    ir_label body_start;

    ir_label jump;
    ir_label jump_int;
    ir_label error;
    ir_label error_int;
    ir_label fast_block_end;
    ir_label fast_block_end_int;
    ir_label error_exit;
    ir_label exit;

    resume_entry *resume_entry_list;

    ir_value tstate;
    ir_value f;
    ir_value throwflag;
    ir_value next_instr_index; /* only set during special control flow */
    ir_value fastlocals;
    ir_value retval;

    /* Temporary stack is used for call_function */
    ir_value tmpstack;
    size_t tmpstack_size;

    /* Used during stack lowering pass */
    int stack_size;
    ir_value *stack_values;

    ir_label jmptab[1];
} JITData;

#define ADD_RESUME_ENTRY(_f_lasti, _resume_point) do { \
    resume_entry *e = (resume_entry*)PyMem_RawMalloc(sizeof(resume_entry)); \
    e->f_lasti = (_f_lasti); \
    e->resume_point = (_resume_point); \
    e->next = jd->resume_entry_list; \
    jd->resume_entry_list = e; \
} while (0)

#define GOTO_ERROR_IF(val) do { \
    IR_LABEL_INIT(fallthrough); \
    ir_goto_error(jd->func, (val), fallthrough, jd->error_exit); \
    LABEL(fallthrough); \
} while (0)

#define GOTO_ERROR_IF_EX(val, with_stack_level, with_pb) do { \
    ir_cursor_set_pyblock(jd->func, (with_stack_level), (with_pb)); \
    GOTO_ERROR_IF((val)); \
    ir_cursor_clear_pyblock(jd->func); \
} while (0)

#define GOTO_ERROR_IF_NOT(val)   GOTO_ERROR_IF(NOTBOOL((val)))

#define GOTO_ERROR()             ir_goto_error(jd->func, NULL, NULL, jd->error_exit)

#define GOTO_ERROR_EX(with_stack_level, with_pb) do { \
    ir_cursor_set_pyblock(jd->func, (with_stack_level), (with_pb)); \
    GOTO_ERROR(); \
    ir_cursor_clear_pyblock(jd->func); \
} while (0)

#define GOTO_EXIT()              BRANCH(jd->exit)

#define GOTO_FAST_BLOCK_END(why) \
    ir_goto_fbe(jd->func, IR_WHY_ ## why, NULL, jd->exit)

/* Issue a fast_block_end, but with a pre-computed stack level and pyblock */
#define GOTO_FAST_BLOCK_END_EX(why, with_stack_level, with_pb) do { \
    ir_cursor_set_pyblock(jd->func, (with_stack_level), (with_pb)); \
    ir_goto_fbe(jd->func, IR_WHY_ ## why, NULL, jd->exit); \
    ir_cursor_clear_pyblock(jd->func); \
} while (0)

#define GOTO_FAST_BLOCK_END_CONTINUE(continue_target) \
    ir_goto_fbe(jd->func, IR_WHY_CONTINUE, continue_target, jd->exit)

ir_label _instr_index_to_label(JITData *jd, int instr_index) {
    Py_ssize_t inst_count = PyBytes_GET_SIZE(jd->co->co_code)/sizeof(_Py_CODEUNIT);
    assert(instr_index >= 0);
    assert(instr_index < inst_count);
    return jd->jmptab[instr_index];
}

int _label_to_instr_index(JITData *jd, ir_label target) {
    Py_ssize_t inst_count = PyBytes_GET_SIZE(jd->co->co_code)/sizeof(_Py_CODEUNIT);
    Py_ssize_t i;
    for (i = 0; i < inst_count; i++) {
        if (jd->jmptab[i] == target) {
            break;
        }
    }
    assert(i < inst_count);
    return i;
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

#define MOVE_TO_END(_from_label, _to_label) do { \
    move_entry *m = PyMem_RawMalloc(sizeof(move_entry)); \
    assert(m); \
    m->from_label = _from_label; \
    m->to_label = _to_label; \
    m->next = jd->move_entry_list; \
    jd->move_entry_list = m; \
} while (0)

#define BEGIN_REMOTE_SECTION(label) { \
    JLABEL remote_end_label = JLABEL_INIT(#label ".remote_end"); \
    BRANCH(remote_end_label); \
    LABEL(label);

#define END_REMOTE_SECTION(label) \
    LABEL(remote_end_label); \
    MOVE_TO_END(label, remote_end_label); \
} while (0)

/* Stack operations */

#define FRAMEPTR()    (jd->f)

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

#define LOAD_VALUE_STACK()  LOAD_FIELD(FRAMEPTR(), PyFrameObject, f_valuestack, ir_type_pyobject_ptr_ptr)

#define GETNAME(i)   PyTuple_GET_ITEM(jd->co->co_names, (i));
#define LOAD_FREEVAR(i) \
    LOAD_AT_INDEX(jd->fastlocals, CONSTANT_INT(jd->co->co_nlocals + (i)))

// TODO: This needs to be adjusted depending on the configuration of _Py_atomic_int
#define LOAD_EVAL_BREAKER() \
    LOAD(CONSTANT_PTR(ir_type_int_ptr, &_PyRuntime.ceval.eval_breaker._value))

/* Check eval breaker, and call handler if set. This can only be used
   in instructions with no jumping, since it assumes the next instruction
   is computed using f_lasti.
 */
#define CHECK_EVAL_BREAKER()         _emit_check_eval_breaker(jd, CONSTANT_INT(next_instr_index))
#define CHECK_EVAL_BREAKER_SPECIAL() _emit_check_eval_breaker(jd, jd->next_instr_index)
#define CHECK_EVAL_BREAKER_EX(_next_instr_index) \
    _emit_check_eval_breaker(jd, CONSTANT_INT(_next_instr_index))

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

#define SET_GIL_DROP_REQUEST() \
    do { \
        _Py_atomic_store_relaxed(&_PyRuntime.ceval.gil_drop_request, 1); \
        _Py_atomic_store_relaxed(&_PyRuntime.ceval.eval_breaker, 1); \
    } while (0)

#define RESET_GIL_DROP_REQUEST() \
    do { \
        _Py_atomic_store_relaxed(&_PyRuntime.ceval.gil_drop_request, 0); \
        COMPUTE_EVAL_BREAKER(); \
    } while (0)

/* Pending calls are only modified under pending_lock */
#define SIGNAL_PENDING_CALLS() \
    do { \
        _Py_atomic_store_relaxed(&_PyRuntime.ceval.pending.calls_to_do, 1); \
        _Py_atomic_store_relaxed(&_PyRuntime.ceval.eval_breaker, 1); \
    } while (0)

#define UNSIGNAL_PENDING_CALLS() \
    do { \
        _Py_atomic_store_relaxed(&_PyRuntime.ceval.pending.calls_to_do, 0); \
        COMPUTE_EVAL_BREAKER(); \
    } while (0)

#define SIGNAL_ASYNC_EXC() \
    do { \
        _PyRuntime.ceval.pending.async_exc = 1; \
        _Py_atomic_store_relaxed(&_PyRuntime.ceval.eval_breaker, 1); \
    } while (0)

#define UNSIGNAL_ASYNC_EXC() \
    do { \
        _PyRuntime.ceval.pending.async_exc = 0; \
        COMPUTE_EVAL_BREAKER(); \
    } while (0)

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#include "pythread.h"
#include "ceval_gil.h"
/* ---------------------------- End copy from ceval.c ------------------------------- */

/* To make unused error warnings go away */
void *dummy2[] = {_gil_initialize, gil_created, destroy_gil, recreate_gil};

/* Do eval break. Return 0 on success, -1 on exception. */
static int _do_eval_breaker(PyFrameObject *f, int next_instr_index) {
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
    _Py_CODEUNIT *code = (_Py_CODEUNIT*)PyBytes_AS_STRING(f->f_code->co_code);
    assert(next_instr_index >= 0);
    assert((size_t)next_instr_index < PyBytes_GET_SIZE(f->f_code->co_code) / sizeof(_Py_CODEUNIT));
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

static inline void _emit_check_eval_breaker(JITData *jd, ir_value next_instr_index) {
    IR_LABEL_INIT(eval_breaker_fired);
    IR_LABEL_INIT(after_eval_break);
    BRANCH_IF(LOAD_EVAL_BREAKER(), eval_breaker_fired, IR_UNLIKELY);
    LABEL(after_eval_break);

    BEGIN_REMOTE_SECTION(eval_breaker_fired);
    JVALUE res = CALL_NATIVE(jd->sig_ifi, _do_eval_breaker, jd->f, next_instr_index);
    GOTO_ERROR_IF(res);
    BRANCH(after_eval_break);
    END_REMOTE_SECTION(eval_breaker_fired);
}

#define IR_PyErr_ExceptionMatches(exc) \
    CALL_NATIVE(jd->sig_io, PyErr_ExceptionMatches, CONSTANT_PYOBJ(exc))

#define TYPE_CHECK(typeval, expected_type, branch_if_not, likelyhood) \
    BRANCH_IF(CMP_NE((typeval), CONSTANT_PTR(ir_type_pytypeobject_ptr, &(expected_type))), (branch_if_not), (likelyhood))

void format_exc_check_arg(PyObject *, const char *, PyObject *);

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

/* Set next_instr_index based on the current f_lasti. This must be done
   when a regular instruction jumps to a special handler. */
#define COMPUTE_NEXT_INSTR_INDEX() do { \
        /* next_instr_index = (f->f_lasti / 2) + 1 */ \
        JVALUE f_lasti_val = LOAD_FIELD(jd->f, PyFrameObject, f_lasti, ir_type_int); \
        JVALUE computed_next_instr_index = ADD(SHIFT_RIGHT(f_lasti_val, CONSTANT_INT(1)), CONSTANT_INT(1)); \
        SET_VALUE(jd->next_instr_index, computed_next_instr_index); \
} while (0)

#define GETLOCAL(i)  ir_getlocal(jd->func, (i))

/* Notice that the SETLOCAL macro does more than just ir_setlocal! */
#define SETLOCAL(i, val) do { \
    JVALUE tmp = GETLOCAL(i); \
    ir_setlocal(jd->func, (i), (val)); \
    XDECREF(tmp); \
} while (0)

#define EMIT_JUMP(check_eval_breaker) do { \
    if (check_eval_breaker) { \
        CHECK_EVAL_BREAKER(); \
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
    SET_VALUE(jd->retval, (val))

#define LOAD_F_LOCALS() \
    LOAD_FIELD(jd->f, PyFrameObject, f_locals, ir_type_pyobject_ptr)

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
    JLABEL load_fast_error = JLABEL_INIT("load_fast_error");
    JVALUE v = GETLOCAL(oparg);
    BRANCH_IF_NOT(v, load_fast_error, IR_UNLIKELY);
    INCREF(v);
    PUSH(v);

    BEGIN_REMOTE_SECTION(load_fast_error);
    CALL_format_exc_check_arg(PyExc_UnboundLocalError,
                              UNBOUNDLOCAL_ERROR_MSG,
                              PyTuple_GetItem(jd->co->co_varnames, oparg));
    GOTO_ERROR();
    END_REMOTE_SECTION(load_fast_error);
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
    SETLOCAL(oparg, v);
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
        JVALUE res = CALL_NATIVE(jd->sig_oo, func, objval); \
        DECREF(objval); \
        SET_TOP(res); \
        GOTO_ERROR_IF_NOT(res); \
        CHECK_EVAL_BREAKER(); \
    }

EMIT_AS_UNARY_OP(UNARY_POSITIVE, PyNumber_Positive)
EMIT_AS_UNARY_OP(UNARY_NEGATIVE, PyNumber_Negative)
EMIT_AS_UNARY_OP(UNARY_INVERT, PyNumber_Invert)

EMITTER_FOR(UNARY_NOT) {
    JLABEL real_error = JLABEL_INIT("real_error");
    JVALUE value = TOP();
    JVALUE err = CALL_NATIVE(jd->sig_io, PyObject_IsTrue, value);
    DECREF(value);

    /* Jump if err < 0 (unlikely) */
    BRANCH_IF(CMP_LT(err, CONSTANT_INT(0)), real_error, IR_UNLIKELY);

    /* Handle err > 0 case */
    JVALUE obj = TERNARY(err, CONSTANT_PYOBJ(Py_False), CONSTANT_PYOBJ(Py_True));
    INCREF(obj);
    SET_TOP(obj);
    CHECK_EVAL_BREAKER();

    /* Handle err < 0 case */
    BEGIN_REMOTE_SECTION(real_error);
    STACKADJ(-1);
    GOTO_ERROR();
    END_REMOTE_SECTION(real_error);
}

EMITTER_FOR(BINARY_POWER) {
    JVALUE exp = POP();
    JVALUE base = TOP();
    JVALUE res = CALL_NATIVE(jd->sig_oooo, PyNumber_Power, base, exp, CONSTANT_PYOBJ(Py_None));
    DECREF(base);
    DECREF(exp);
    SET_TOP(res);
    GOTO_ERROR_IF_NOT(res);
    CHECK_EVAL_BREAKER();
}

#define EMIT_AS_BINARY_OP(op, func) \
    EMITTER_FOR_BASE(_ ## op) { \
        JVALUE right = POP(); \
        JVALUE left = TOP(); \
        JVALUE res = CALL_NATIVE(jd->sig_ooo, func, left, right); \
        DECREF(left); \
        DECREF(right); \
        SET_TOP(res); \
        GOTO_ERROR_IF_NOT(res); \
        CHECK_EVAL_BREAKER(); \
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
    JVALUE v = POP();
    JVALUE list = PEEK(oparg);
    JVALUE err = CALL_NATIVE(jd->sig_ioo, PyList_Append, list, v);
    DECREF(v);
    GOTO_ERROR_IF(err);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(SET_ADD) {
    JVALUE v = POP();
    JVALUE set = PEEK(oparg);
    JVALUE err = CALL_NATIVE(jd->sig_ioo, PySet_Add, set, v);
    DECREF(v);
    GOTO_ERROR_IF(err);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(INPLACE_POWER) {
    JVALUE exp = POP();
    JVALUE base = TOP();
    JVALUE res = CALL_NATIVE(jd->sig_oooo, PyNumber_InPlacePower, base, exp, CONSTANT_PYOBJ(Py_None));
    DECREF(base);
    DECREF(exp);
    SET_TOP(res);
    GOTO_ERROR_IF_NOT(res);
    CHECK_EVAL_BREAKER();
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
    STACKADJ(-3);
    /* container[sub] = v */
    JVALUE err = CALL_NATIVE(jd->sig_iooo, PyObject_SetItem, container, sub, v);
    DECREF(v);
    DECREF(container);
    DECREF(sub);
    GOTO_ERROR_IF(err);
    CHECK_EVAL_BREAKER();
}

/* This is most of the work of STORE_ANNOTATION, taken from ceval,
   but modified so that it:
   i) returns 1 on error, 0 on success
   ii) doesn't decref "ann".
   */
int _store_annotation_helper(PyFrameObject *f, PyObject *name, PyObject *ann) {
    _Py_IDENTIFIER(__annotations__);
    PyObject *ann_dict;
    int err;
    if (f->f_locals == NULL) {
        PyErr_Format(PyExc_SystemError,
                     "no locals found when storing annotation");
        return 1;
    }
    /* first try to get __annotations__ from locals... */
    if (PyDict_CheckExact(f->f_locals)) {
        ann_dict = _PyDict_GetItemId(f->f_locals,
                                     &PyId___annotations__);
        if (ann_dict == NULL) {
            PyErr_SetString(PyExc_NameError,
                            "__annotations__ not found");
            return 1;
        }
        Py_INCREF(ann_dict);
    }
    else {
        PyObject *ann_str = _PyUnicode_FromId(&PyId___annotations__);
        if (ann_str == NULL) {
            return 1;
        }
        ann_dict = PyObject_GetItem(f->f_locals, ann_str);
        if (ann_dict == NULL) {
            if (PyErr_ExceptionMatches(PyExc_KeyError)) {
                PyErr_SetString(PyExc_NameError,
                                "__annotations__ not found");
            }
            return 1;
        }
    }
    /* ...if succeeded, __annotations__[name] = ann */
    if (PyDict_CheckExact(ann_dict)) {
        err = PyDict_SetItem(ann_dict, name, ann);
    }
    else {
        err = PyObject_SetItem(ann_dict, name, ann);
    }
    Py_DECREF(ann_dict);
    if (err != 0) {
        return 1;
    }
    return 0;
}

EMITTER_FOR(STORE_ANNOTATION) {
    PyObject *name = GETNAME(oparg);
    JVALUE ann = POP();
    JTYPE sig = CREATE_SIGNATURE(ir_type_int, ir_type_pyframeobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    JVALUE err = CALL_NATIVE(sig, _store_annotation_helper, jd->f, CONSTANT_PYOBJ(name), ann);
    DECREF(ann);
    GOTO_ERROR_IF(err);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(DELETE_SUBSCR) {
    JVALUE sub = TOP();
    JVALUE container = SECOND();
    STACKADJ(-2);
    /* del container[sub] */
    JVALUE err = CALL_NATIVE(jd->sig_ioo, PyObject_DelItem, container, sub);
    DECREF(container);
    DECREF(sub);
    GOTO_ERROR_IF(err);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(PRINT_EXPR) {
    _Py_IDENTIFIER(displayhook);
    JVALUE value = POP();
    JLABEL hook_null = JLABEL_INIT("hook_null");
    JTYPE sig1 = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_void_ptr);
    JVALUE hook = CALL_NATIVE(sig1, _PySys_GetObjectId, CONSTANT_VOID_PTR(&PyId_displayhook));
    BRANCH_IF_NOT(hook, hook_null, IR_UNLIKELY);
    JVALUE res = CALL_NATIVE(jd->sig_oooo, PyObject_CallFunctionObjArgs, hook, value, CONSTANT_PYOBJ(NULL));
    DECREF(value);
    GOTO_ERROR_IF_NOT(res);
    DECREF(res);
    CHECK_EVAL_BREAKER();

    BEGIN_REMOTE_SECTION(hook_null);
    CALL_PyErr_SetString(PyExc_RuntimeError, "lost sys.displayhook");
    DECREF(value);
    GOTO_ERROR();
    END_REMOTE_SECTION(hook_null);
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
    JLABEL getter_is_null = JLABEL_INIT("getter_is_null");
    JLABEL invalid_iter = JLABEL_INIT("invalid_iter");
    JLABEL iter_is_null = JLABEL_INIT("iter_is_null");
    JVALUE obj = TOP();
    JVALUE obj_type = IR_Py_TYPE(obj);
    JVALUE tp_as_async = LOAD_FIELD(obj_type, PyTypeObject, tp_as_async, ir_type_pyasyncmethods_ptr);
    BRANCH_IF_NOT(tp_as_async, getter_is_null, IR_UNLIKELY);
    JVALUE am_aiter = LOAD_FIELD(tp_as_async, PyAsyncMethods, am_aiter, jd->sig_oo);
    BRANCH_IF_NOT(am_aiter, getter_is_null, IR_UNLIKELY);
    JVALUE iter = CALL_INDIRECT(am_aiter, obj);
    DECREF(obj);
    BRANCH_IF_NOT(iter, iter_is_null, IR_UNLIKELY);
    JVALUE iter_type = IR_Py_TYPE(iter);
    JVALUE iter_tp_as_async = LOAD_FIELD(iter_type, PyTypeObject, tp_as_async, ir_type_pyasyncmethods_ptr);
    BRANCH_IF_NOT(iter_tp_as_async, invalid_iter, IR_UNLIKELY);
    JVALUE iter_am_anext = LOAD_FIELD(iter_tp_as_async, PyAsyncMethods, am_anext, jd->sig_oo);
    BRANCH_IF_NOT(iter_am_anext, invalid_iter, IR_UNLIKELY);

    /* Good iterator, normal dispatch */
    SET_TOP(iter);
    CHECK_EVAL_BREAKER();

    BEGIN_REMOTE_SECTION(iter_is_null);
    SET_TOP(CONSTANT_PYOBJ(NULL));
    GOTO_ERROR();
    END_REMOTE_SECTION(iter_is_null);

    BEGIN_REMOTE_SECTION(getter_is_null);
    SET_TOP(CONSTANT_PYOBJ(NULL));
    CALL_PyErr_Format(
        PyExc_TypeError,
        "'async for' requires an object with "
        "__aiter__ method, got %.100s",
        LOAD_FIELD(obj_type, PyTypeObject, tp_name, ir_type_char_ptr));
    DECREF(obj);
    GOTO_ERROR();
    END_REMOTE_SECTION(getter_is_null);

    BEGIN_REMOTE_SECTION(invalid_iter);
    SET_TOP(CONSTANT_PYOBJ(NULL));
    CALL_PyErr_Format(
        PyExc_TypeError,
        "'async for' received an object from __aiter__ "
        "that does not implement __anext__: %.100s",
        LOAD_FIELD(iter_type, PyTypeObject, tp_name, ir_type_char_ptr));
    DECREF(iter);
    GOTO_ERROR();
    END_REMOTE_SECTION(invalid_iter);
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
    JVALUE awaitable = JVALUE_CREATE(ir_type_pyobject_ptr);

    /* Fast path. Really hacky. If aiter is PyAsyncGen_Type exactly, skip the
       NULL checks and the call to _PyCoro_GetAwaitableIter.
     */
    JVALUE is_async_gen = CMP_EQ(type, CONSTANT_PTR(ir_type_pytypeobject_ptr, &PyAsyncGen_Type));
    BRANCH_IF(is_async_gen, skip_helper, IR_LIKELY);

    /* Slow path: use the helper */
    JTYPE helper_sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pytypeobject_ptr, ir_type_pyobject_ptr);
    SET_VALUE(awaitable, CALL_NATIVE(helper_sig, _get_anext_helper, type, aiter));
    BRANCH(fin);

    LABEL(skip_helper);
    /* Fast path: PyAsyncGen_CheckExact(aiter) is true */
    JVALUE tp_as_async = LOAD_FIELD(type, PyTypeObject, tp_as_async, ir_type_pyasyncmethods_ptr);
    JVALUE am_anext = LOAD_FIELD(tp_as_async, PyAsyncMethods, am_anext, jd->sig_oo);
    SET_VALUE(awaitable, CALL_INDIRECT(am_anext, aiter));
    BRANCH(fin);

    LABEL(fin);
    GOTO_ERROR_IF_NOT(awaitable);
    PUSH(awaitable);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(GET_AWAITABLE) {
    JLABEL fin = JLABEL_INIT("fin");
    JVALUE iterable = TOP();

    // TODO: Inline _PyCoro_GetAwaitableIter
    JVALUE iter = CALL_NATIVE(jd->sig_oo, _PyCoro_GetAwaitableIter, iterable);
    DECREF(iterable);

    // If this is a coroutine, ensure it isn't inside a yield from.
    // TODO: Turn this into a flag check.
    BRANCH_IF_NOT(iter, fin, IR_UNLIKELY);
    BRANCH_IF_NOT(IR_PyCoro_CheckExact(iter), fin, IR_UNLIKELY);

    JVALUE yf = CALL_NATIVE(jd->sig_oo, _PyGen_yf, iter);
    BRANCH_IF_NOT(yf, fin, IR_LIKELY);
    DECREF(yf);
    DECREF(iter);
    SET_VALUE(iter, CONSTANT_PYOBJ(NULL));
    CALL_PyErr_SetString(
            PyExc_RuntimeError,
            "coroutine is being awaited already");
    BRANCH(fin);

    LABEL(fin);
    SET_TOP(iter);
    GOTO_ERROR_IF_NOT(iter);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(YIELD_FROM) {
    _Py_IDENTIFIER(send);
    IR_LABEL_INIT(fast_send);
    IR_LABEL_INIT(v_is_none);
    IR_LABEL_INIT(after_getting_yieldval);
    IR_LABEL_INIT(handle_null_yieldval);
    JVALUE v = POP();
    JVALUE receiver = TOP();
    JVALUE yieldval = JVALUE_CREATE(ir_type_pyobject_ptr);

    BRANCH_IF(
        LOGICAL_OR(IR_PyGen_CheckExact(receiver), IR_PyCoro_CheckExact(receiver)),
        fast_send,
        IR_LIKELY);

    /* Handle generic case */
    BRANCH_IF(CMP_EQ(v, CONSTANT_PYOBJ(Py_None)), v_is_none, IR_SEMILIKELY);
    /* v is not None */
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_void_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    SET_VALUE(yieldval,
        CALL_NATIVE(sig, _PyObject_CallMethodIdObjArgs,
            receiver, CONSTANT_PTR(ir_type_void_ptr, &PyId_send), v, CONSTANT_PYOBJ(NULL)));
    BRANCH(after_getting_yieldval);
    LABEL(v_is_none);
    JVALUE receiver_type = IR_Py_TYPE(receiver);
    JVALUE tp_iternext = LOAD_FIELD(receiver_type, PyTypeObject, tp_iternext, jd->sig_oo);
    SET_VALUE(yieldval, CALL_INDIRECT(tp_iternext, receiver));
    BRANCH(after_getting_yieldval);

    LABEL(fast_send);
    SET_VALUE(yieldval, CALL_NATIVE(jd->sig_ooo, _PyGen_Send, receiver, v));
    BRANCH(after_getting_yieldval);

    LABEL(after_getting_yieldval);
    DECREF(v);
    BRANCH_IF_NOT(yieldval, handle_null_yieldval, IR_SOMETIMES);

    /* Receiver remains on the stack */
    ir_yield(jd->func,
             yieldval,
             next_instr_index - 1,
             jd->jmptab[next_instr_index - 1],
             jd->jmptab[next_instr_index],
             jd->exit);

    /* Exiting from 'yield from' */
    LABEL(handle_null_yieldval);
    JVALUE val = JVALUE_CREATE(ir_type_pyobject_ptr);
    SET_VALUE(val, CONSTANT_PYOBJ(NULL));
    JTYPE sig2 = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr_ptr);
    JVALUE err = CALL_NATIVE(sig2, _PyGen_FetchStopIterationValue, ADDRESS_OF(val));
    GOTO_ERROR_IF(CMP_LT(err, CONSTANT_INT(0)));
    DECREF(receiver);
    SET_TOP(val);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(YIELD_VALUE) {
    JVALUE v = POP();
    if (jd->co->co_flags & CO_ASYNC_GENERATOR) {
        JVALUE wrapped = CALL_NATIVE(jd->sig_oo, _PyAsyncGenValueWrapperNew, v);
        DECREF(v);
        GOTO_ERROR_IF_NOT(wrapped);
        v = wrapped;
    }
    ir_yield(jd->func, v, next_instr_index, jd->jmptab[next_instr_index], NULL, jd->exit);
}

EMITTER_FOR(POP_EXCEPT) {
    DO_POP_BLOCK(EXCEPT_HANDLER);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(POP_BLOCK) {
    DO_POP_BLOCK(ANY);
    CHECK_EVAL_BREAKER();
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
    ir_end_finally(jd->func, fallthrough);
    LABEL(fallthrough);
    CHECK_EVAL_BREAKER();
}

static
PyObject *
_load_build_class_helper(PyFrameObject *f) {
    _Py_IDENTIFIER(__build_class__);
    PyObject *bc;
    if (PyDict_CheckExact(f->f_builtins)) {
        bc = _PyDict_GetItemId(f->f_builtins, &PyId___build_class__);
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
        bc = PyObject_GetItem(f->f_builtins, build_class_str);
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
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyframeobject_ptr);
    JVALUE bc = CALL_NATIVE(sig, _load_build_class_helper, jd->f);
    GOTO_ERROR_IF_NOT(bc);
    PUSH(bc);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(STORE_NAME) {
    JLABEL after_setitem = JLABEL_INIT("after_setitem");
    JLABEL no_locals_found = JLABEL_INIT("no_locals_found");
    JLABEL object_case = JLABEL_INIT("object_case");
    PyObject *name = GETNAME(oparg);
    JVALUE v = POP();
    JVALUE ns = LOAD_F_LOCALS();
    BRANCH_IF_NOT(ns, no_locals_found, IR_UNLIKELY);
    BRANCH_IF_NOT(IR_PyDict_CheckExact(ns), object_case, IR_UNLIKELY);
    JVALUE err = JVALUE_CREATE(ir_type_int);
    SET_VALUE(err, CALL_NATIVE(jd->sig_iooo, PyDict_SetItem, ns, CONSTANT_PYOBJ(name), v));
    BRANCH(after_setitem);

    LABEL(no_locals_found);
    CALL_PyErr_Format(PyExc_SystemError,
                      "no locals found when storing %R", CONSTANT_PYOBJ(name));
    DECREF(v);
    GOTO_ERROR();

    LABEL(object_case);
    SET_VALUE(err, CALL_NATIVE(jd->sig_iooo, PyObject_SetItem, ns, CONSTANT_PYOBJ(name), v));
    BRANCH(after_setitem);

    LABEL(after_setitem);
    DECREF(v);
    GOTO_ERROR_IF(err);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(DELETE_NAME) {
    JLABEL no_locals = JLABEL_INIT("no_locals");
    JLABEL handle_err = JLABEL_INIT("handle_err");
    JLABEL fast_dispatch = JLABEL_INIT("fast_dispatch");
    PyObject *name = GETNAME(oparg);
    assert(name);
    JVALUE ns = LOAD_F_LOCALS();
    BRANCH_IF_NOT(ns, no_locals, IR_UNLIKELY);
    JVALUE err = CALL_NATIVE(jd->sig_ioo, PyObject_DelItem, ns, CONSTANT_PYOBJ(name));
    BRANCH_IF(err, handle_err, IR_UNLIKELY);
    CHECK_EVAL_BREAKER();
    BRANCH(fast_dispatch);

    LABEL(no_locals);
    CALL_PyErr_Format(PyExc_SystemError,
                      "no locals when deleting %R", CONSTANT_PYOBJ(name));
    GOTO_ERROR();

    LABEL(handle_err);
    CALL_format_exc_check_arg(PyExc_NameError, NAME_ERROR_MSG, name);
    GOTO_ERROR();

    LABEL(fast_dispatch);
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
    JVALUE it = CALL_NATIVE(jd->sig_oo, PyObject_GetIter, seq);
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
    JVALUE counter = JVALUE_CREATE(ir_type_int);
    SET_VALUE(counter, CONSTANT_INT(0));
    for (int i = 0; i < oparg; i++) {
        JVALUE item = CALL_NATIVE(jd->sig_oo, PyIter_Next, it);
        PUT(1 + i, item);
        BRANCH_IF_NOT(item, exhausted_too_early, IR_UNLIKELY);
        SET_VALUE(counter, ADD(counter, CONSTANT_INT(1)));
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
                      CONSTANT_INT(oparg), counter);
    GOTO_ERROR();

    LABEL(too_many_values);
    DECREF(final);
    CALL_PyErr_Format(PyExc_ValueError,
        "too many values to unpack (expected %d)",
        CONSTANT_INT(oparg));
    GOTO_ERROR();

    LABEL(dispatch);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(UNPACK_EX) {
    int before = oparg & 0xFF;
    int after = oparg >> 8;
    int total = before + 1 + after;
    JVALUE seq = POP();
    JLABEL dispatch = JLABEL_INIT("dispatch");

    /* TODO: This could be specialized for tuple/list. */
    JVALUE it = CALL_NATIVE(jd->sig_oo, PyObject_GetIter, seq);
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
    JVALUE counter = JVALUE_CREATE(ir_type_int);
    SET_VALUE(counter, CONSTANT_INT(0));
    for (int i = 0; i < before; i++) {
        JVALUE item = CALL_NATIVE(jd->sig_oo, PyIter_Next, it);
        PUT(1 + i, item);
        BRANCH_IF_NOT(item, exhausted_too_early, IR_UNLIKELY);
        SET_VALUE(counter, ADD(counter, CONSTANT_INT(1)));
    }

    /* Convert the remaining items to a list */
    JVALUE l = CALL_NATIVE(jd->sig_oo, PySequence_List, it);
    DECREF(it);
    GOTO_ERROR_IF_NOT(l);
    PUT(1 + before, l);

    /* Verify we have enough items */
    JLABEL not_enough_values = JLABEL_INIT("not_enough_values");
    JVALUE ll = IR_PyList_GET_SIZE(l);
    SET_VALUE(counter, ADD(counter, CAST(ir_type_int, ll)));
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
                      CONSTANT_INT(before + after), counter);
    GOTO_ERROR();

    LABEL(dispatch);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(STORE_ATTR) {
    PyObject *name = GETNAME(oparg)
    JVALUE owner = TOP();
    JVALUE v = SECOND();
    STACKADJ(-2);
    JVALUE err = CALL_NATIVE(jd->sig_iooo, PyObject_SetAttr, owner, CONSTANT_PYOBJ(name), v);
    DECREF(v);
    DECREF(owner);
    GOTO_ERROR_IF(err);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(DELETE_ATTR) {
    PyObject *name = GETNAME(oparg);
    JVALUE owner = POP();
    JVALUE err = CALL_NATIVE(jd->sig_iooo, PyObject_SetAttr, owner, CONSTANT_PYOBJ(name), CONSTANT_PYOBJ(NULL));
    DECREF(owner);
    GOTO_ERROR_IF(err);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(STORE_GLOBAL) {
    PyObject *name = GETNAME(oparg);
    JVALUE v = POP();
    JVALUE globals = LOAD_FIELD(jd->f, PyFrameObject, f_globals, ir_type_pyobject_ptr);
    JVALUE err = CALL_NATIVE(jd->sig_iooo, PyDict_SetItem, globals, CONSTANT_PYOBJ(name), v);
    DECREF(v);
    GOTO_ERROR_IF(err);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(DELETE_GLOBAL) {
    JLABEL handle_error = JLABEL_INIT("handle_error");
    PyObject *name = GETNAME(oparg);
    JVALUE globals = LOAD_FIELD(jd->f, PyFrameObject, f_globals, ir_type_pyobject_ptr);
    JVALUE err = CALL_NATIVE(jd->sig_ioo, PyDict_DelItem, globals, CONSTANT_PYOBJ(name));
    BRANCH_IF(err, handle_error, IR_UNLIKELY);
    CHECK_EVAL_BREAKER();

    BEGIN_REMOTE_SECTION(handle_error);
    CALL_format_exc_check_arg(PyExc_NameError, NAME_ERROR_MSG, name);
    GOTO_ERROR();
    END_REMOTE_SECTION(handle_error);
}

static PyObject *
_load_name_helper(PyObject *name, PyFrameObject *f) {
    PyObject *locals = f->f_locals;
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
        v = PyDict_GetItem(f->f_globals, name);
        Py_XINCREF(v);
        if (v == NULL) {
            if (PyDict_CheckExact(f->f_builtins)) {
                v = PyDict_GetItem(f->f_builtins, name);
                if (v == NULL) {
                    format_exc_check_arg(
                                PyExc_NameError,
                                NAME_ERROR_MSG, name);
                    return NULL;
                }
                Py_INCREF(v);
            }
            else {
                v = PyObject_GetItem(f->f_builtins, name);
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
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyframeobject_ptr);
    JVALUE v = CALL_NATIVE(sig, _load_name_helper, CONSTANT_PYOBJ(name), jd->f);
    GOTO_ERROR_IF_NOT(v);
    PUSH(v);
    CHECK_EVAL_BREAKER();
}

static PyObject* _load_global_helper(PyFrameObject *f, PyObject *name) {
    PyObject *v;
    if (PyDict_CheckExact(f->f_globals)
        && PyDict_CheckExact(f->f_builtins))
    {
        v = _PyDict_LoadGlobal((PyDictObject *)f->f_globals,
                               (PyDictObject *)f->f_builtins,
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
        v = PyObject_GetItem(f->f_globals, name);
        if (v == NULL) {
            if (!PyErr_ExceptionMatches(PyExc_KeyError))
                return NULL;
            PyErr_Clear();

            /* namespace 2: builtins */
            v = PyObject_GetItem(f->f_builtins, name);
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

EMITTER_FOR(LOAD_GLOBAL) {
    PyObject *name = GETNAME(oparg);
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyframeobject_ptr, ir_type_pyobject_ptr);
    JVALUE v = CALL_NATIVE(sig, _load_global_helper, jd->f, CONSTANT_PYOBJ(name));
    GOTO_ERROR_IF_NOT(v);
    PUSH(v);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(DELETE_FAST) {
    JLABEL no_error = JLABEL_INIT("no_error");
    JVALUE v = GETLOCAL(oparg);
    BRANCH_IF(v, no_error, IR_LIKELY);

    /* Handle error (unbound) */
    PyObject *varname = PyTuple_GetItem(jd->co->co_varnames, oparg);
    assert(varname);
    CALL_format_exc_check_arg(PyExc_UnboundLocalError, UNBOUNDLOCAL_ERROR_MSG, varname);
    GOTO_ERROR();

    LABEL(no_error);
    SETLOCAL(oparg, CONSTANT_PYOBJ(NULL));
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(DELETE_DEREF) {
    JLABEL fast_dispatch = JLABEL_INIT("fast_dispatch");
    JLABEL cell_empty = JLABEL_INIT("cell_empty");
    JVALUE cell = LOAD_FREEVAR(oparg);
    JVALUE oldobj = IR_PyCell_GET(cell);
    BRANCH_IF_NOT(oldobj, cell_empty, IR_UNLIKELY);
    IR_PyCell_SET(cell, CONSTANT_PYOBJ(NULL));
    DECREF(oldobj);
    CHECK_EVAL_BREAKER();
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
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(LOAD_CLASSDEREF) {
    assert(oparg >= PyTuple_GET_SIZE(jd->co->co_cellvars));
    Py_ssize_t idx = oparg - PyTuple_GET_SIZE(jd->co->co_cellvars);
    assert(idx >= 0 && idx < PyTuple_GET_SIZE(jd->co->co_freevars));
    PyObject *name = PyTuple_GET_ITEM(jd->co->co_freevars, idx);

    JVALUE value = JVALUE_CREATE(ir_type_pyobject_ptr);
    JVALUE locals = LOAD_F_LOCALS();
    IR_ASSERT(locals);

    IF_ELSE(
        IR_PyDict_CheckExact(locals), IR_LIKELY,
        {
            SET_VALUE(value, CALL_NATIVE(jd->sig_ooo, PyDict_GetItem, locals, CONSTANT_PYOBJ(name)));
            XINCREF(value);
        },
        {
            SET_VALUE(value, CALL_NATIVE(jd->sig_ooo, PyObject_GetItem, locals, CONSTANT_PYOBJ(name)));
            IF_NOT(
                value, IR_SEMILIKELY,
                {
                    GOTO_ERROR_IF_NOT(IR_PyErr_ExceptionMatches(PyExc_KeyError));
                    CALL_PyErr_Clear();
                });
        });
    IF_NOT(value, IR_SEMILIKELY,
    {
        JVALUE cell = LOAD_FREEVAR(oparg);
        SET_VALUE(value, IR_PyCell_GET(cell));
        IF_NOT(value, IR_UNLIKELY,
        {
            CALL_format_exc_unbound(oparg);
            GOTO_ERROR();
        });
        INCREF(value);
    });
    PUSH(value);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(LOAD_DEREF) {
    JLABEL fast_dispatch = JLABEL_INIT("fast_dispatch");
    JLABEL unbound_value = JLABEL_INIT("unbound_value");
    JVALUE cell = LOAD_FREEVAR(oparg);
    JVALUE value = IR_PyCell_GET(cell);
    BRANCH_IF_NOT(value, unbound_value, IR_UNLIKELY);
    INCREF(value);
    PUSH(value);
    CHECK_EVAL_BREAKER();
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
    CHECK_EVAL_BREAKER();
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
    JVALUE empty = CALL_NATIVE(sig1, PyUnicode_New, CONSTANT_PYSSIZET(0), CONSTANT_UINT32(0));
    GOTO_ERROR_IF_NOT(empty);

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
    CHECK_EVAL_BREAKER();
}

/* TODO: Replace with a new opcode which directly places items into
   a tuple, instead of maintaining a large number of items on the stack.
   This is difficult to compile, and produces inefficient code.
 */
EMITTER_FOR(BUILD_TUPLE) {
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyssizet);
    JVALUE tup = CALL_NATIVE(sig, PyTuple_New, CONSTANT_PYSSIZET(oparg));
    GOTO_ERROR_IF_NOT(tup);
    JVALUE ob_item = IR_PyTuple_OB_ITEM(tup);
    for (int i = 0; i < oparg; i++) {
        STORE_AT_INDEX(ob_item, CONSTANT_INT(i), PEEK(oparg - i));
    }
    STACKADJ(-oparg);
    PUSH(tup);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(BUILD_LIST) {
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyssizet);
    JVALUE list = CALL_NATIVE(sig, PyList_New, CONSTANT_PYSSIZET(oparg));
    GOTO_ERROR_IF_NOT(list);
    JVALUE ob_item = IR_PyList_OB_ITEM(list);
    for (int i = 0; i < oparg; i++) {
        STORE_AT_INDEX(ob_item, CONSTANT_INT(i), PEEK(oparg - i));
    }
    STACKADJ(-oparg);
    PUSH(list);
    CHECK_EVAL_BREAKER();
}

/* from ceval.c */
extern int check_args_iterable(PyObject *, PyObject *);

static void _build_unpack_common(JITData *jd, int opcode, int oparg, int next_instr_index) {
    int convert_to_tuple = opcode != BUILD_LIST_UNPACK;
    JLABEL handle_error = JLABEL_INIT("handle_error");
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyssizet);
    JVALUE sum = CALL_NATIVE(sig, PyList_New, CONSTANT_PYSSIZET(0));
    GOTO_ERROR_IF_NOT(sum);
    JVALUE badobj = JVALUE_CREATE(ir_type_pyobject_ptr);
    for (int i = oparg; i > 0; i--) {
        JVALUE item = PEEK(i);
        SET_VALUE(badobj, item);
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
    CHECK_EVAL_BREAKER();

    BEGIN_REMOTE_SECTION(handle_error);
    DECREF(sum);
    if (opcode == BUILD_TUPLE_UNPACK_WITH_CALL) {
        JLABEL skip = JLABEL_INIT("skip");
        BRANCH_IF_NOT(IR_PyErr_ExceptionMatches(PyExc_TypeError), skip, IR_SEMILIKELY);
        CALL_NATIVE(jd->sig_ioo, check_args_iterable, PEEK(1 + oparg), badobj);
        LABEL(skip);
    }
    GOTO_ERROR();
    END_REMOTE_SECTION(handle_error);
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
    JVALUE set = CALL_NATIVE(jd->sig_oo, PySet_New, CONSTANT_PYOBJ(NULL));
    GOTO_ERROR_IF_NOT(set);
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
    CHECK_EVAL_BREAKER();

    BEGIN_REMOTE_SECTION(handle_error);
    DECREF(set);
    GOTO_ERROR();
    END_REMOTE_SECTION(handle_error);
}

EMITTER_FOR(BUILD_SET_UNPACK) {
    JLABEL handle_error = JLABEL_INIT("handle_error");
    JVALUE sum = CALL_NATIVE(jd->sig_oo, PySet_New, CONSTANT_PYOBJ(NULL));
    GOTO_ERROR_IF_NOT(sum);
    for (int i = 0; i < oparg; i++) {
        JVALUE err = CALL_NATIVE(jd->sig_ioo, _PySet_Update, sum, PEEK(oparg - i));
        BRANCH_IF(CMP_LT(err, CONSTANT_INT(0)), handle_error, IR_UNLIKELY);
    }
    for (int i = 0; i < oparg; i++) {
        DECREF(PEEK(oparg - i));
    }
    STACKADJ(-oparg);
    PUSH(sum);
    CHECK_EVAL_BREAKER();

    BEGIN_REMOTE_SECTION(handle_error);
    DECREF(sum);
    GOTO_ERROR();
    END_REMOTE_SECTION(handle_error);
}

EMITTER_FOR(BUILD_MAP) {
    JLABEL handle_error = JLABEL_INIT("handle_error");
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyssizet);
    JVALUE map = CALL_NATIVE(sig, _PyDict_NewPresized, CONSTANT_PYSSIZET(oparg));
    GOTO_ERROR_IF_NOT(map);
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

    BEGIN_REMOTE_SECTION(handle_error);
    DECREF(map);
    GOTO_ERROR();
    END_REMOTE_SECTION(handle_error);
}

/* This is copied from ceval.c */
int
_setup_annotations_helper(PyFrameObject *f) {
    _Py_IDENTIFIER(__annotations__);
    int err;
    PyObject *ann_dict;
    if (f->f_locals == NULL) {
        PyErr_Format(PyExc_SystemError,
                     "no locals found when setting up annotations");
        return -1;
    }
    /* check if __annotations__ in locals()... */
    if (PyDict_CheckExact(f->f_locals)) {
        ann_dict = _PyDict_GetItemId(f->f_locals,
                                     &PyId___annotations__);
        if (ann_dict == NULL) {
            /* ...if not, create a new one */
            ann_dict = PyDict_New();
            if (ann_dict == NULL) {
                return -1;
            }
            err = _PyDict_SetItemId(f->f_locals,
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
        ann_dict = PyObject_GetItem(f->f_locals, ann_str);
        if (ann_dict == NULL) {
            if (!PyErr_ExceptionMatches(PyExc_KeyError)) {
                return -1;
            }
            PyErr_Clear();
            ann_dict = PyDict_New();
            if (ann_dict == NULL) {
                return -1;
            }
            err = PyObject_SetItem(f->f_locals, ann_str, ann_dict);
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
    JTYPE sig = CREATE_SIGNATURE(ir_type_int, ir_type_pyframeobject_ptr);
    JVALUE ret = CALL_NATIVE(sig, _setup_annotations_helper, jd->f);
    GOTO_ERROR_IF(ret);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(BUILD_CONST_KEY_MAP) {
    IR_LABEL_INIT(bad_keys_arg);
    IR_LABEL_INIT(handle_setitem_error);
    JVALUE keys = TOP();
    BRANCH_IF_NOT(IR_PyTuple_CheckExact(keys), bad_keys_arg, IR_UNLIKELY);
    BRANCH_IF_NOT(CMP_EQ(IR_PyTuple_GET_SIZE(keys), CONSTANT_PYSSIZET(oparg)), bad_keys_arg, IR_UNLIKELY);
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyssizet);
    JVALUE map = CALL_NATIVE(sig, _PyDict_NewPresized, CONSTANT_PYSSIZET(oparg));
    GOTO_ERROR_IF_NOT(map);
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
    CHECK_EVAL_BREAKER();

    BEGIN_REMOTE_SECTION(handle_setitem_error);
    DECREF(map);
    GOTO_ERROR();
    END_REMOTE_SECTION(handle_setitem_error);

    BEGIN_REMOTE_SECTION(bad_keys_arg);
    CALL_PyErr_SetString(PyExc_SystemError,
                         "bad BUILD_CONST_KEY_MAP keys argument");
    GOTO_ERROR();
    END_REMOTE_SECTION(bad_keys_arg);
}

EMITTER_FOR(BUILD_MAP_UNPACK) {
    IR_LABEL_INIT(handle_error);
    JVALUE sum = CALL_NATIVE(jd->sig_o, PyDict_New);
    GOTO_ERROR_IF_NOT(sum);
    JVALUE arg = JVALUE_CREATE(ir_type_pyobject_ptr);
    for (int i = oparg; i > 0; i--) {
        SET_VALUE(arg, PEEK(i));
        JVALUE err = CALL_NATIVE(jd->sig_ioo, PyDict_Update, sum, arg);
        BRANCH_IF(CMP_LT(err, CONSTANT_INT(0)), handle_error, IR_UNLIKELY);
    }

    for (int i = oparg; i > 0; i--) {
        DECREF(POP());
    }
    PUSH(sum);
    CHECK_EVAL_BREAKER();

    BEGIN_REMOTE_SECTION(handle_error);
    IF(IR_PyErr_ExceptionMatches(PyExc_AttributeError), IR_SEMILIKELY, {
        CALL_PyErr_Format(PyExc_TypeError,
                          "'%.200s' object is not a mapping",
                          LOAD_FIELD(IR_Py_TYPE(arg), PyTypeObject, tp_name, ir_type_char_ptr));
    });
    DECREF(sum);
    GOTO_ERROR();
    END_REMOTE_SECTION(handle_error);
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
    JVALUE sum = CALL_NATIVE(jd->sig_o, PyDict_New);
    GOTO_ERROR_IF_NOT(sum);

    JTYPE sig = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_int);
    JVALUE arg = JVALUE_CREATE(ir_type_pyobject_ptr);
    for (int i = oparg; i > 0; i--) {
        SET_VALUE(arg, PEEK(i));
        JVALUE err = CALL_NATIVE(sig, _PyDict_MergeEx, sum, arg, CONSTANT_INT(2));
        BRANCH_IF(CMP_LT(err, CONSTANT_INT(0)), handle_error, IR_UNLIKELY);
    }
    for (int i = oparg; i > 0; i--) {
        DECREF(POP());
    }
    PUSH(sum);
    CHECK_EVAL_BREAKER();

    BEGIN_REMOTE_SECTION(handle_error);
    JVALUE func = PEEK(2 + oparg);
    JTYPE sig2 = CREATE_SIGNATURE(ir_type_void, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    CALL_NATIVE(sig2, _build_map_unpack_with_call_format_error, func, arg);
    DECREF(sum);
    GOTO_ERROR();
    END_REMOTE_SECTION(handle_error);
}

EMITTER_FOR(MAP_ADD) {
    JVALUE key = POP();
    JVALUE value = POP();
    JVALUE map = PEEK(oparg);
    IR_ASSERT(IR_PyDict_CheckExact(map));
    JVALUE err = CALL_NATIVE(jd->sig_iooo, PyDict_SetItem, map, key, value);
    DECREF(value);
    DECREF(key);
    GOTO_ERROR_IF(err);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(LOAD_ATTR) {
    PyObject *name = GETNAME(oparg);
    JVALUE owner = TOP();
    JVALUE res = CALL_NATIVE(jd->sig_ooo, PyObject_GetAttr, owner, CONSTANT_PYOBJ(name));
    DECREF(owner);
    SET_TOP(res);
    GOTO_ERROR_IF_NOT(res);
    CHECK_EVAL_BREAKER();
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
    JVALUE right = POP();
    JVALUE left = TOP();
    JVALUE res = JVALUE_CREATE(ir_type_pyobject_ptr);

    /* Each of these must set 'res' */
    switch (oparg) {
    case PyCmp_IS: {
        SET_VALUE(res, TERNARY(CMP_EQ(left, right), CONSTANT_PYOBJ(Py_True), CONSTANT_PYOBJ(Py_False)));
        INCREF(res);
        break;
    }
    case PyCmp_IS_NOT: {
        SET_VALUE(res, TERNARY(CMP_NE(left, right), CONSTANT_PYOBJ(Py_True), CONSTANT_PYOBJ(Py_False)));
        INCREF(res);
        break;
    }
    case PyCmp_IN:
    case PyCmp_NOT_IN: {
        JLABEL no_exception = JLABEL_INIT("no_exception");
        JLABEL after_no_exception = JLABEL_INIT("after_no_exception");
        JVALUE tmp = CALL_NATIVE(jd->sig_ioo, PySequence_Contains, right, left);

        /* Check for < 0 */
        BRANCH_IF(CMP_GE(tmp, CONSTANT_INT(0)), no_exception, IR_LIKELY);

        /* Handle exception (tmp < 0) case */
        SET_VALUE(res, CONSTANT_PYOBJ(NULL));
        BRANCH(after_no_exception);

        /* Handle no exception case (tmp >= 0) */
        LABEL(no_exception);
        if (oparg == PyCmp_IN) {
            SET_VALUE(res, TERNARY(tmp, CONSTANT_PYOBJ(Py_True), CONSTANT_PYOBJ(Py_False)));
        } else {
            SET_VALUE(res, TERNARY(tmp, CONSTANT_PYOBJ(Py_False), CONSTANT_PYOBJ(Py_True)));
        }
        INCREF(res);
        LABEL(after_no_exception);
        break;
    }
    case PyCmp_EXC_MATCH: {
        JLABEL do_exc_match = JLABEL_INIT("do_exc_match");
        JLABEL after_exc_match = JLABEL_INIT("after_exc_match");
        JLABEL handle_tuple = JLABEL_INIT("handle_tuple");
        /* Handle common case (exception class) first */
        BRANCH_IF(IR_PyExceptionClass_Check(right), do_exc_match, IR_LIKELY);
        BRANCH_IF(IR_PyTuple_Check(right), handle_tuple, IR_LIKELY);

        /* Error case: neither exception class nor tuple */
        CALL_PyErr_SetString(PyExc_TypeError, CANNOT_CATCH_MSG "2");
        SET_VALUE(res, CONSTANT_PYOBJ(NULL));
        BRANCH(after_exc_match);

        /* Tuple case */
        LABEL(handle_tuple);
        JVALUE tmp = CALL_NATIVE(jd->sig_ioo, _compare_op_exc_match_helper, left, right);
        /* This returns 0 to indicate error, 1 to indicate OK */
        BRANCH_IF(tmp, do_exc_match, IR_LIKELY);
        SET_VALUE(res, CONSTANT_PYOBJ(NULL));
        BRANCH(after_exc_match);

        /* Finally, run PyErr_GivenExceptionMatches */
        LABEL(do_exc_match);
        JVALUE exc_matches = CALL_NATIVE(jd->sig_ioo, PyErr_GivenExceptionMatches, left, right);
        SET_VALUE(res, TERNARY(exc_matches, CONSTANT_PYOBJ(Py_True), CONSTANT_PYOBJ(Py_False)));
        INCREF(res);

        LABEL(after_exc_match);
        break;
    }
    default: {
        JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_int);
        SET_VALUE(res, CALL_NATIVE(sig, PyObject_RichCompare, left, right, CONSTANT_INT(oparg)));
        break;
    }
    }

    DECREF(left);
    DECREF(right);
    SET_TOP(res);
    GOTO_ERROR_IF_NOT(res);
    CHECK_EVAL_BREAKER();
}

PyObject * import_name(PyFrameObject *, PyObject *, PyObject *,
                       PyObject *);

EMITTER_FOR(IMPORT_NAME) {
    PyObject *name = GETNAME(oparg);
    JVALUE fromlist = POP();
    JVALUE level = POP();
    JTYPE sig = CREATE_SIGNATURE(
        ir_type_pyobject_ptr,
        ir_type_pyframeobject_ptr,
        ir_type_pyobject_ptr,
        ir_type_pyobject_ptr,
        ir_type_pyobject_ptr);
    JVALUE res = CALL_NATIVE(sig, import_name, jd->f, CONSTANT_PYOBJ(name), fromlist, level);
    DECREF(level);
    DECREF(fromlist);
    GOTO_ERROR_IF_NOT(res);
    PUSH(res);
    CHECK_EVAL_BREAKER();
}

int import_all_from(PyObject *, PyObject *);

EMITTER_FOR(IMPORT_STAR) {
    JVALUE from = POP();

    JTYPE sig1 = CREATE_SIGNATURE(ir_type_int, ir_type_pyframeobject_ptr);
    IF(
        CMP_LT(
            CALL_NATIVE(sig1, PyFrame_FastToLocalsWithError, jd->f),
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
    CALL_NATIVE(sig2, PyFrame_LocalsToFast, jd->f, CONSTANT_INT(0));
    DECREF(from);
    GOTO_ERROR_IF(err);
    CHECK_EVAL_BREAKER();
}

PyObject * import_from(PyObject *, PyObject *);

EMITTER_FOR(IMPORT_FROM) {
    PyObject *name = GETNAME(oparg);
    JVALUE from = TOP();
    JVALUE res = CALL_NATIVE(jd->sig_ooo, import_from, from, CONSTANT_PYOBJ(name));
    GOTO_ERROR_IF_NOT(res);
    PUSH(res);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(JUMP_FORWARD) {
    JUMPBY(oparg, 0);
}

EMITTER_FOR(POP_JUMP_IF_FALSE) {
    JLABEL skip1 = JLABEL_INIT("skip1");
    JLABEL skip2 = JLABEL_INIT("skip2");
    JLABEL fin = JLABEL_INIT("fin");
    JVALUE cond = POP();
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_True)), skip1, IR_SEMILIKELY);
    /* Py_True case */
    DECREF(cond);
    BRANCH(fin);
    LABEL(skip1);
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_False)), skip2, IR_SEMILIKELY);
    /* Py_False case */
    DECREF(cond);
    JUMPTO(oparg, 0);
    LABEL(skip2);
    /* Generic case */
    JVALUE err = CALL_NATIVE(jd->sig_io, PyObject_IsTrue, cond);
    DECREF(cond);
    BRANCH_IF(CMP_GT(err, CONSTANT_INT(0)), fin, IR_SEMILIKELY);
    GOTO_ERROR_IF(CMP_LT(err, CONSTANT_INT(0)));
    /* err == 0 case */
    JUMPTO(oparg, 1);
    LABEL(fin);
    CHECK_EVAL_BREAKER();
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
    CHECK_EVAL_BREAKER();
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
    CHECK_EVAL_BREAKER();
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
    CHECK_EVAL_BREAKER();
    LABEL(fast_dispatch);
}


EMITTER_FOR(JUMP_ABSOLUTE) {
    JUMPTO(oparg, 1);
}

EMITTER_FOR(GET_ITER) {
    JVALUE iterable = POP();
    JVALUE iter = CALL_NATIVE(jd->sig_oo, PyObject_GetIter, iterable);
    DECREF(iterable);
    GOTO_ERROR_IF_NOT(iter);
    PUSH(iter);
    CHECK_EVAL_BREAKER();
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
    CHECK_EVAL_BREAKER();
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
    GOTO_ERROR_IF_NOT(ret); /* This may actually be likely? */
    CALL_PyErr_Clear();

    LABEL(cleanup);
    STACKADJ(-1);
    DECREF(iter_obj);
    JUMPBY(oparg, 1);
    LABEL(next_instruction);
    CHECK_EVAL_BREAKER();
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
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(SETUP_EXCEPT) {
    DO_SETUP_BLOCK(EXCEPT, INSTR_OFFSET() + oparg);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(SETUP_FINALLY) {
    DO_SETUP_BLOCK(FINALLY_TRY, INSTR_OFFSET() + oparg);
    CHECK_EVAL_BREAKER();
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
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(SETUP_ASYNC_WITH) {
    JVALUE res = POP();
    DO_SETUP_BLOCK(FINALLY_TRY, INSTR_OFFSET() + oparg);
    PUSH(res);
    CHECK_EVAL_BREAKER();
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
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(WITH_CLEANUP_START) {
    JVALUE exit_func = PEEK(7);

    JVALUE exc = JVALUE_CREATE(ir_type_pyobject_ptr);
    JVALUE val = JVALUE_CREATE(ir_type_pyobject_ptr);
    JVALUE tb = JVALUE_CREATE(ir_type_pyobject_ptr);
    SET_VALUE(val, CONSTANT_PYOBJ(Py_None));
    SET_VALUE(tb, CONSTANT_PYOBJ(Py_None));
    SET_VALUE(exc, TOP());

    IR_LABEL_INIT(proceed);
    BRANCH_IF(CMP_EQ(exc, CONSTANT_PYOBJ(Py_None)), proceed, IR_SEMILIKELY);
    IF_ELSE(IR_PyLong_Check(exc), IR_SEMILIKELY, {
        SET_VALUE(exc, CONSTANT_PYOBJ(Py_None));
    }, {
        SET_VALUE(val, SECOND());
        SET_VALUE(tb, THIRD());
    });

    LABEL(proceed);
    assert(jd->tmpstack_size >= 3);
    STORE_AT_INDEX(jd->tmpstack, CONSTANT_INT(0), exc);
    STORE_AT_INDEX(jd->tmpstack, CONSTANT_INT(1), val);
    STORE_AT_INDEX(jd->tmpstack, CONSTANT_INT(2), tb);
    JTYPE sig2 = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr_ptr, ir_type_pyssizet, ir_type_pyobject_ptr);
    JVALUE res = CALL_NATIVE(sig2, _PyObject_FastCallDict, exit_func, jd->tmpstack, CONSTANT_PYSSIZET(3), CONSTANT_PYOBJ(NULL));
    GOTO_ERROR_IF_NOT(res);
    /* Duplicating the exception on the stack */
    INCREF(exc);
    PUSH(exc);
    PUSH(res);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(WITH_CLEANUP_FINISH) {
    JVALUE res = POP();
    JVALUE exc = POP();
    JVALUE err = JVALUE_CREATE(ir_type_int);
    SET_VALUE(err, CONSTANT_INT(0));
    IF(CMP_NE(exc, CONSTANT_PYOBJ(Py_None)), IR_SEMILIKELY, {
        SET_VALUE(err, CALL_NATIVE(jd->sig_io, PyObject_IsTrue, res));
    });
    DECREF(res);
    DECREF(exc);
    GOTO_ERROR_IF(CMP_LT(err, CONSTANT_INT(0)));
    IF(CMP_GT(err, CONSTANT_INT(0)), IR_SOMETIMES, {
        /* There was an exception and a True return */
        DECREF(POP());
        JVALUE why_silenced = CALL_PyLong_FromLong(CONSTANT_LONG(WHY_SILENCED));
        PUSH(why_silenced);
    });
    CHECK_EVAL_BREAKER();
}

/* Private API for the LOAD_METHOD opcode. */
extern int _PyObject_GetMethod(PyObject *, PyObject *, PyObject **);

EMITTER_FOR(LOAD_METHOD) {
    PyObject *name = GETNAME(oparg);
    JVALUE obj = TOP();
    JVALUE meth = JVALUE_CREATE(ir_type_pyobject_ptr);
    SET_VALUE(meth, CONSTANT_PYOBJ(NULL));
    JTYPE sig = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr_ptr);
    JVALUE meth_found = CALL_NATIVE(sig, _PyObject_GetMethod, obj, CONSTANT_PYOBJ(name), ADDRESS_OF(meth));

    /* If meth == NULL, most likely attribute wasn't found. */
    GOTO_ERROR_IF_NOT(meth);

    JLABEL fin = JLABEL_INIT("fin");
    JLABEL if_meth_found = JLABEL_INIT("if_meth_found");
    BRANCH_IF(meth_found, if_meth_found, IR_LIKELY);

    /* !meth_found case */
    SET_TOP(CONSTANT_PYOBJ(NULL));
    DECREF(obj);
    PUSH(meth);
    BRANCH(fin);

    /* meth_found case */
    LABEL(if_meth_found);
    SET_TOP(meth);
    PUSH(obj); // self

    LABEL(fin);
    CHECK_EVAL_BREAKER();
}

/* From ceval */
PyObject *call_function_extern(PyObject **, Py_ssize_t,
                               PyObject *);

ir_value _emit_call_function(JITData *jd, ir_value callable, int oparg, ir_value kwnames) {
    assert(jd->tmpstack_size >= (size_t)oparg + 1);

    /* This assumes the arguments are at the top of the "stack" */
    STORE_AT_INDEX(jd->tmpstack, CONSTANT_INT(0), callable);
    for (int i = 0; i < oparg; i++) {
        STORE_AT_INDEX(jd->tmpstack, CONSTANT_INT(i+1), PEEK(oparg-i));
    }
    JVALUE endptr = ir_get_index_ptr(jd->func, jd->tmpstack, CONSTANT_INT(oparg+1));
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr_ptr, ir_type_pyssizet, ir_type_pyobject_ptr);
    JVALUE res = CALL_NATIVE(sig, call_function_extern, endptr, CONSTANT_PYSSIZET(oparg), kwnames);
    return res;
}

EMITTER_FOR(CALL_METHOD) {
    JVALUE meth = PEEK(oparg + 2);
    JVALUE res = JVALUE_CREATE(ir_type_pyobject_ptr);
    IF_ELSE(NOTBOOL(meth), IR_SOMETIMES, {
        SET_VALUE(res, _emit_call_function(jd, PEEK(oparg+1), oparg, CONSTANT_PYOBJ(NULL)));
    }, {
        SET_VALUE(res, _emit_call_function(jd, meth, oparg + 1, CONSTANT_PYOBJ(NULL)));
    });
    for (int i = 0; i < oparg; i++) {
        DECREF(POP());
    }
    XDECREF(POP());
    XDECREF(POP());
    GOTO_ERROR_IF_NOT(res);
    PUSH(res);
    CHECK_EVAL_BREAKER();
}

#if 0
/* Emit a call to a function from the stack. The stack is expected to be arranged like so:

    callable, arg1, ..., argN, kwarg1, ..., kwargM

    where nargs = N, and M = (kwnames ? PyTuple_GET_SIZE(kwnames) : 0)

    Returns the return value (ir_value) of the function.
    Does not modify the stack. Does not check for NULL return.

 */
static ir_value
_call_function(JITData *jd, size_t nargs, PyObject *kwnames) {
    _pyjit_callsite *cs = _pyjit_new_fastcall(nargs, kwnames);
    assert(cs);

    /* Invoke the trampoline */
    ir_value entrypoint = _pyjit_load_entrypoint(jd->func, cs);

    /* Invoke the entry point: entrypoint(callable, args..., kwargs...) */
    size_t nkwargs = kwnames ? PyTuple_GET_SIZE(kwnames) : 0;
    size_t total_args = 1 + nargs + nkwargs;
    ir_value *args = (ir_value*)PyMem_RawMalloc(total_args * sizeof(ir_value));
    size_t j = 0;
    int k = nargs + nkwargs + 1;
    args[j++] = PEEK(k--); /* Callable */
    size_t stack_base = nargs + nkwargs;
    for (size_t i = 0; i < nargs; i++) {
        args[j++] = PEEK(k--);
    }
    for (size_t i = 0; i < nkwargs; i++) {
        args[j++] = PEEK(k--);
    }
    assert(j == total_args);
    assert(k == 0);
    ir_value ret = ir_call(jd->func, entrypoint, args);
    PyMem_RawFree(args);
    return ret;
}
#endif

EMITTER_FOR(CALL_FUNCTION) {
    JVALUE res = _emit_call_function(jd, PEEK(oparg+1), oparg, CONSTANT_PYOBJ(NULL));
    for (int i = 0; i < oparg; i++) {
        DECREF(POP());
    }
    DECREF(POP()); /* callable */
    GOTO_ERROR_IF_NOT(res);
    PUSH(res);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(CALL_FUNCTION_KW) {
    JVALUE names = POP();
    IR_ASSERT(IR_PyTuple_CheckExact(names));
    IR_ASSERT(CMP_LE(IR_PyTuple_GET_SIZE(names), CONSTANT_PYSSIZET(oparg)));

    JVALUE res = _emit_call_function(jd, PEEK(oparg+1), oparg, names);
    for (int i = 0; i < oparg; i++) {
        DECREF(POP());
    }
    DECREF(POP()); /* callable */
    DECREF(names);
    GOTO_ERROR_IF_NOT(res);
    PUSH(res);
    CHECK_EVAL_BREAKER();
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
    JVALUE kwargs;
    if (oparg & 0x01) {
        kwargs = POP();
        IF_NOT(IR_PyDict_CheckExact(kwargs), IR_UNLIKELY, {
            JVALUE d = CALL_NATIVE(jd->sig_ooo, _call_function_ex_make_dict, SECOND(), kwargs);
            DECREF(kwargs);
            GOTO_ERROR_IF_NOT(d);
            SET_VALUE(kwargs, d);
        });
        IR_ASSERT(IR_PyDict_CheckExact(kwargs));
    } else {
        kwargs = CONSTANT_PYOBJ(NULL);
    }
    JVALUE callargs = POP();
    IF_NOT(IR_PyTuple_CheckExact(callargs), IR_UNLIKELY, {
        JVALUE t = CALL_NATIVE(jd->sig_ooo, _call_function_ex_make_tuple, TOP(), callargs);
        DECREF(callargs);
        GOTO_ERROR_IF_NOT(t);
        SET_VALUE(callargs, t);
    });
    IR_ASSERT(IR_PyTuple_CheckExact(callargs));
    JVALUE func = POP();
    JVALUE result = CALL_NATIVE(jd->sig_oooo, do_call_core, func, callargs, kwargs);
    DECREF(func);
    DECREF(callargs);
    if (oparg & 0x01) {
        DECREF(kwargs);
    }
    GOTO_ERROR_IF_NOT(result);
    PUSH(result);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(MAKE_FUNCTION) {
    JVALUE qualname = POP();
    JVALUE codeobj = POP();
    JVALUE globals = LOAD_FIELD(jd->f, PyFrameObject, f_globals, ir_type_pyobject_ptr);
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyfunctionobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    JVALUE func = CALL_NATIVE(sig, PyFunction_NewWithQualName, codeobj, globals, qualname);
    DECREF(codeobj);
    DECREF(qualname);
    GOTO_ERROR_IF_NOT(func);

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
    CHECK_EVAL_BREAKER();
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
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(FORMAT_VALUE) {
    PyObject *(*conv_fn)(PyObject *);
    int which_conversion = oparg & FVC_MASK;
    int have_fmt_spec = (oparg & FVS_MASK) == FVS_HAVE_SPEC;

    JVALUE fmt_spec = have_fmt_spec ? POP() : NULL;
    JVALUE value = POP();

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
        JVALUE conv_value = CALL_NATIVE(jd->sig_oo, conv_fn, value);
        DECREF(value);
        BRANCH_IF(conv_value, conv_ok, IR_LIKELY);
        /* Handle error (conv_fn returns NULL) */
        if (have_fmt_spec) {
            DECREF(fmt_spec);
        }
        GOTO_ERROR();
        LABEL(conv_ok);
        SET_VALUE(value, conv_value);
    }

    JLABEL skip_format = JLABEL_INIT("skip_format");
    if (!have_fmt_spec) {
        BRANCH_IF(IR_PyUnicode_CheckExact(value), skip_format, IR_LIKELY);
    }

    /* Actually call format() */
    JVALUE result = CALL_NATIVE(
         jd->sig_ooo, PyObject_Format, value, fmt_spec ? fmt_spec : CONSTANT_PYOBJ(NULL));
    DECREF(value);
    if (have_fmt_spec) {
        DECREF(fmt_spec);
    }
    GOTO_ERROR_IF_NOT(result);
    SET_VALUE(value, result);

    /* Done */
    LABEL(skip_format);
    PUSH(value);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(EXTENDED_ARG) {
    abort(); /* EXTENDED_ARG is handled by jit.c */
}

/* --- END EMITTERS --- */

#include "opcode_emitter_table.h"
#include "opcode_names.h"

static void
translate_bytecode(JITData *jd, PyCodeObject *co)
{
    assert(PyBytes_Check(co->co_code));
    assert(PyBytes_GET_SIZE(co->co_code) <= INT_MAX);
    assert(PyBytes_GET_SIZE(co->co_code) % sizeof(_Py_CODEUNIT) == 0);
    assert(_Py_IS_ALIGNED(PyBytes_AS_STRING(co->co_code), sizeof(_Py_CODEUNIT)));
    _Py_CODEUNIT *code = (_Py_CODEUNIT*)PyBytes_AS_STRING(co->co_code);
    Py_ssize_t i;
    Py_ssize_t inst_count = PyBytes_GET_SIZE(co->co_code)/sizeof(_Py_CODEUNIT);

    jd->is_gen = (co->co_flags & (CO_GENERATOR | CO_COROUTINE | CO_ASYNC_GENERATOR));
    jd->update_blockstack = jd->is_gen;
    jd->use_patchpoint_error_handler = 0;

    jd->error_exit = ir_label_new(jd->func, "error_exit");
    jd->exit = ir_label_new(jd->func, "exit");
    jd->body_start = ir_label_new(jd->func, "body_start");

    char namebuf[64];
    for (i = 0; i < inst_count; i++) {
        sprintf(namebuf, "inst_%ld", (long)i);
        jd->jmptab[i] = ir_label_new(jd->func, namebuf);
    }

    /* Setup temporary stack */
    jd->tmpstack_size = 6; /* 6 needed by fast_block_unwind */
    for (i = 0; i < inst_count; i++) {
        int opcode = _Py_OPCODE(code[i]);
        int oparg = _Py_OPARG(code[i]);
        while (opcode == EXTENDED_ARG) {
            ++i;
            opcode = _Py_OPCODE(code[i]);
            oparg = (oparg << 8) | _Py_OPARG(code[i]);
        }
        if (opcode == CALL_FUNCTION || opcode == CALL_METHOD || opcode == CALL_FUNCTION_KW) {
            jd->tmpstack_size = Py_MAX(jd->tmpstack_size, (size_t)oparg + 2);
        }
        if (opcode == WITH_CLEANUP_START) {
            jd->tmpstack_size = Py_MAX(jd->tmpstack_size, 5);
        }
    }
    if (jd->tmpstack_size > 0) {
        jd->tmpstack = ir_alloca(jd->func, ir_type_pyobject_ptr, CONSTANT_SIZET(jd->tmpstack_size));
    } else {
        jd->tmpstack = NULL;
    }

    /* Common function signatures */
    jd->sig_o = CREATE_SIGNATURE(ir_type_pyobject_ptr);
    jd->sig_oo = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    jd->sig_ooo = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    jd->sig_oooo = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    jd->sig_vp = CREATE_SIGNATURE(ir_type_void, ir_type_void_ptr);
    jd->sig_i = CREATE_SIGNATURE(ir_type_int);
    jd->sig_ifi = CREATE_SIGNATURE(ir_type_int, ir_type_pyframeobject_ptr, ir_type_int);
    jd->sig_io = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr);
    jd->sig_ioo = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    jd->sig_iooo = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    jd->sig_ol = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_long);
    jd->dealloc_sig = CREATE_SIGNATURE(ir_type_void, ir_type_pyobject_ptr);
    jd->blocksetup_sig = CREATE_SIGNATURE(ir_type_void, ir_type_pyframeobject_ptr, ir_type_int, ir_type_int, ir_type_int);
    jd->blockpop_sig = CREATE_SIGNATURE(ir_type_pytryblock_ptr, ir_type_pyframeobject_ptr);
    jd->exc_triple_sig = CREATE_SIGNATURE(ir_type_void, ir_type_pyobject_ptr_ptr, ir_type_pyobject_ptr_ptr, ir_type_pyobject_ptr_ptr);

    /* Arguments: f, throwflag */
    jd->f = ir_func_get_argument(jd->func, 0);
    jd->throwflag = ir_func_get_argument(jd->func, 1);

    jd->retval = ir_value_new(jd->func, ir_type_pyobject_ptr);
    jd->next_instr_index = ir_value_new(jd->func, ir_type_int);
    jd->move_entry_list = NULL;

    /* Initialization sequence */
    jd->tstate = IR_PyThreadState_GET();
    jd->fastlocals = ir_get_element_ptr(jd->func, jd->f, offsetof(PyFrameObject, f_localsplus), ir_type_pyobject_ptr, "f_localsplus");

    /* push frame */
    IF(IR_Py_EnterRecursiveCall(""), IR_UNLIKELY, {
        ir_ret(jd->func, CONSTANT_PYOBJ(NULL));
    });
    STORE_FIELD(jd->tstate, PyThreadState, frame, ir_type_pyframeobject_ptr, jd->f);

    /* Setup retval and why */
    SET_RETVAL(CONSTANT_PYOBJ(NULL));

    /* Set f_executing = 1 */
    STORE_FIELD(jd->f, PyFrameObject, f_executing, ir_type_char, CONSTANT_CHAR(1));

    /* assert f->f_lasti >= -1 */
    IR_ASSERT(CMP_GE(LOAD_FIELD(jd->f, PyFrameObject, f_lasti, ir_type_int), CONSTANT_INT(-1)));

    if (jd->is_gen) {
        /* To be filled in by ir_lower() */
        ir_yield_dispatch(jd->func, jd->body_start);
    }

    /* Normal function entry (this is also the
       first yield entry point for generators) */
    LABEL(jd->body_start);

    /* Make sure the stack is setup correctly for function start */
    JVALUE f_valuestack = LOAD_FIELD(jd->f, PyFrameObject, f_valuestack, ir_type_pyobject_ptr_ptr);
    JVALUE f_stacktop = LOAD_FIELD(jd->f, PyFrameObject, f_stacktop, ir_type_pyobject_ptr_ptr);
    IR_ASSERT(CMP_EQ(f_stacktop, f_valuestack));

    /* We must clear f_stacktop */
    STORE_FIELD(jd->f, PyFrameObject, f_stacktop, ir_type_pyobject_ptr_ptr, CONSTANT_PTR(ir_type_pyobject_ptr_ptr, NULL));

    if (jd->is_gen) {
        GOTO_ERROR_IF(jd->throwflag);
    }

#ifdef Py_DEBUG
    /* PyEval_EvalFrameEx() must not be called with an exception set,
       because it can clear it (directly or indirectly) and so the
       caller loses its exception */
    IR_ASSERT(NOTBOOL(IR_PyErr_Occurred()));
#endif

    SET_VALUE(jd->next_instr_index, CONSTANT_INT(0));
    CHECK_EVAL_BREAKER_SPECIAL();

    for (i = 0; i < inst_count; i++) {
        int opcode = _Py_OPCODE(code[i]);
        int oparg = _Py_OPARG(code[i]);
        LABEL(jd->jmptab[i]);

        while (opcode == EXTENDED_ARG) {
            ++i;
            opcode = _Py_OPCODE(code[i]);
            oparg = (oparg << 8) | _Py_OPARG(code[i]);
            LABEL(jd->jmptab[i]);
        }

        char namebuf[64];
        sprintf(namebuf, "inst_%ld.%s", (long)i, opcode_names[opcode]);
        ir_label_push_prefix(jd->func, namebuf);

        // Emit instruction
        // Set f->f_lasti
        STORE_FIELD(jd->f, PyFrameObject, f_lasti, ir_type_int, CONSTANT_INT(i * sizeof(_Py_CODEUNIT)));
        opcode_emitter_table[opcode](jd, i + 1, opcode, oparg);

        ir_label_pop_prefix(jd->func);
    }

    /* Shouldn't ever fall through to this area */
    CRASH();

    /* Special sections. While in a special section, jd->next_instr_index must
       be valid and point to the next instruction to execute (for next dispatch).
       There are two entrypoints to each special section. One for instructions,
       and one for coming from another special section.
     */
    LABEL(jd->error_exit);
    IR_ASSERT(IR_PyErr_Occurred());
    /* Insert the current frame into the traceback */
    JTYPE sig = CREATE_SIGNATURE(ir_type_int, ir_type_pyframeobject_ptr);
    CALL_NATIVE(sig, PyTraceBack_Here, jd->f);
    SET_RETVAL(CONSTANT_PYOBJ(NULL));
    BRANCH(jd->exit);


    LABEL(jd->exit);
    IR_Py_LeaveRecursiveCall();
    STORE_FIELD(jd->f, PyFrameObject, f_executing, ir_type_char, CONSTANT_CHAR(0));
    JVALUE f_back = LOAD_FIELD(jd->f, PyFrameObject, f_back, ir_type_pyframeobject_ptr);
    STORE_FIELD(jd->tstate, PyThreadState, frame, ir_type_pyframeobject_ptr, f_back);
    JVALUE ret = IR_Py_CheckFunctionResult(NULL, jd->retval, "PyJIT_EvalFrame");
    ir_ret(jd->func, ret);

    /* Close the cursor */
    ir_cursor_close(jd->func);

    /* Move extra sections to end */
    {
        move_entry *m = jd->move_entry_list;
        move_entry *next;
        while (m != NULL) {
            next = m->next;
            ir_func_move_blocks_to_end(jd->func, m->from_label, m->to_label);
            PyMem_RawFree(m);
            m = next;
        }
        jd->move_entry_list = NULL;
    }
}

void ir_lower(JITData *jd);

int
_PyJIT_CodeGen(PyCodeObject *co) {
    ir_type sig;
    Py_ssize_t inst_count = PyBytes_GET_SIZE(co->co_code)/sizeof(_Py_CODEUNIT);
    size_t total_size = sizeof(JITData) + inst_count * sizeof(ir_label);
    JITData *jd = PyMem_RawMalloc(total_size);
    if (jd == NULL) {
        PyErr_NoMemory();
        return -1;
    }
    memset(jd, 0, total_size);
    jd->co = co;
    jd->context = ir_context_new();

    /* func(f, throwflag); */
    ir_type argtypes[] = { ir_type_pyframeobject_ptr, ir_type_int };
    sig = ir_create_function_type(jd->context, ir_type_pyobject_ptr, sizeof(argtypes)/sizeof(argtypes[0]), argtypes);
    jd->func = ir_func_new(jd->context, PyUnicode_AsUTF8(co->co_name), sig);

    translate_bytecode(jd, co);
    if (Py_JITDebugFlag > 1) {
        ir_func_dump_file(jd->func, "/tmp/before.ir", "Before lowering");
    }
#ifdef IR_DEBUG
    ir_func_verify(jd->func);
#endif
    //ir_verify_stack_effect(jd->func);

    if (Py_JITDebugFlag > 1) {
        ir_func_dump_file(jd->func, "/tmp/after_dead_blocks.ir", "After dead locks removal");
    }

    ir_lower(jd);
    if (Py_JITDebugFlag > 1) {
        ir_func_dump_file(jd->func, "/tmp/after.ir", "After lowering");
    }
#ifdef IR_DEBUG
    ir_func_verify(jd->func);
#endif
    PyJIT_Handle *handle = PyMem_RawMalloc(sizeof(PyJIT_Handle));
    ir_object object;
    if (Py_JITFlag == 1) {
        object = ir_libjit_compile(jd->func);
    } else if (Py_JITFlag == 2) {
        object = ir_llvm_compile(jd->func);
    } else {
        Py_FatalError("invalid PYJIT value");
        Py_UNREACHABLE();
    }
    handle->object = object;
    handle->entry = (PyJIT_EntryPoint)object->entrypoint;
    ir_context_destroy(jd->context);
    PyMem_RawFree(jd);
    co->co_jit_handle = handle;
    return 0;
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
    IR_ASSERT(CMP_EQ(LOAD_FIELD(jd->f, PyFrameObject, f_iblock, ir_type_int),
                     CONSTANT_INT(_compute_pyblock_depth(pb->prev))));

    CALL_NATIVE(jd->blocksetup_sig, PyFrame_BlockSetup,
                jd->f,
                CONSTANT_INT(b_type),
                CONSTANT_INT(b_handler),
                CONSTANT_INT(pb->b_level));
}

static void _pop_pyblock_from_blockstack(JITData *jd, ir_pyblock pb) {
    if (!jd->update_blockstack) return;

    /* Verify the depth */
    IR_ASSERT(CMP_EQ(LOAD_FIELD(jd->f, PyFrameObject, f_iblock, ir_type_int),
                     CONSTANT_INT(_compute_pyblock_depth(pb))));
    JVALUE b = CALL_NATIVE(jd->blockpop_sig, PyFrame_BlockPop, jd->f);
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

    _pop_pyblock_from_blockstack(jd, pb);

    JVALUE status = POP();

    /* Case 1: status is PyLong (WHY code) */
    BRANCH_IF_NOT(IR_PyLong_Check(status), try_case_2, IR_SEMILIKELY);
    JVALUE why = CAST(ir_type_int, CALL_PyLong_AsLong(status));
    DECREF(status);

    IF(CMP_EQ(why, CONSTANT_INT(WHY_RETURN)), IR_SEMILIKELY, {
        SET_VALUE(jd->retval, POP());
        UNWIND_BLOCK_NEW(estack-2, pb);
        GOTO_FAST_BLOCK_END_EX(RETURN, estack-6, pb->prev);
    });

    /* Only emit WHY_CONTINUE if there's a loop with a continue target above us */
    ir_pyblock cur = pb;
    while (cur && cur->b_type != IR_PYBLOCK_LOOP) cur = cur->prev;
    if (cur && cur->b_type == IR_PYBLOCK_LOOP && cur->b_continue != NULL) {
        IF(CMP_EQ(why, CONSTANT_INT(WHY_CONTINUE)), IR_SEMILIKELY, {
            UNWIND_BLOCK_NEW(estack-1, pb);
            GOTO_FAST_BLOCK_END_EX(CONTINUE, estack-6, pb->prev);
        });
    } else {
        IR_ASSERT(CMP_NE(why, CONSTANT_INT(WHY_CONTINUE)));
    }

    IF(CMP_EQ(why, CONSTANT_INT(WHY_BREAK)), IR_SEMILIKELY, {
        UNWIND_BLOCK_NEW(estack-1, pb);
        GOTO_FAST_BLOCK_END_EX(BREAK, estack-6, pb->prev);
    });
    IF(CMP_EQ(why, CONSTANT_INT(WHY_SILENCED)), IR_SEMILIKELY, {
        /* An exception was silenced by 'with', we must
        manually unwind the EXCEPT_HANDLER block which was
        created when the exception was caught, otherwise
        the stack will be in an inconsistent state. */
        UNWIND_EXCEPT_ONLY_NEW(estack-1, pb);
        BRANCH(instr->fallthrough);
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
        GOTO_FAST_BLOCK_END_EX(EXCEPTION, estack-6, pb->prev);
    }

    LABEL(try_case_3);
    BRANCH_IF(CMP_EQ(status, CONSTANT_PYOBJ(Py_None)), normal_exit, IR_LIKELY);
    {
        CALL_PyErr_SetString(PyExc_SystemError,
                    "'finally' pops bad exception");
        UNWIND_BLOCK_NEW(estack-1, pb);
        DECREF(status);
        GOTO_ERROR_EX(estack-6, pb->prev);
    }

    LABEL(normal_exit);
    UNWIND_BLOCK_NEW(estack-1, pb);
    DECREF(status);
    BRANCH(instr->fallthrough);
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
void _emit_resume_stub(JITData *jd, ir_label resume_label, int stack_level, ir_pyblock pb, int target_inst_index, ir_label target_inst_label) {
    LABEL(resume_label);
    JVALUE f_valuestack = LOAD_FIELD(jd->f, PyFrameObject, f_valuestack, ir_type_pyobject_ptr_ptr);
    JVALUE expected_stack_pointer = ir_get_index_ptr(jd->func, f_valuestack, CONSTANT_INT(stack_level));
    JVALUE f_stacktop = LOAD_FIELD(jd->f, PyFrameObject, f_stacktop, ir_type_pyobject_ptr_ptr);
    IR_ASSERT(CMP_EQ(f_stacktop, expected_stack_pointer));
    STORE_FIELD(jd->f, PyFrameObject, f_stacktop, ir_type_pyobject_ptr_ptr, CONSTANT_PTR(ir_type_pyobject_ptr_ptr, NULL));

    /* Restore stack */
    STACKADJ(stack_level);
    for (int i = 0; i < stack_level; i++) {
        JVALUE obj = LOAD_AT_INDEX(f_valuestack, CONSTANT_INT(i));
        PUT(stack_level - i, obj);
    }

    GOTO_ERROR_IF_EX(jd->throwflag, stack_level, pb);
    IR_ASSERT(NOTBOOL(IR_PyErr_Occurred()));
    ir_cursor_set_pyblock(jd->func, stack_level, pb);
    CHECK_EVAL_BREAKER_EX(target_inst_index);
    ir_cursor_clear_pyblock(jd->func);
    BRANCH(target_inst_label);
}

static
void _emit_patchpoint_error_handler(JITData *jd, ir_pyblock pb, int entry_stack_level);

/* A precursor is the code that executes when an exception occurs,
   after the stack has unwound, and when the exception is going to be
   handled by a pyblock. There is one precursor per pyblock of type
   EXCEPT or FINALLY_TRY. (since the stack level at each precursor may be different).
 */
void _emit_precursor(JITData *jd, ir_pyblock pb) {
    LABEL(pb->b_handler_precursor);
    IR_ASSERT(IR_PyErr_Occurred());

    /* Insert the current frame into the traceback */
    JTYPE sig = CREATE_SIGNATURE(ir_type_int, ir_type_pyframeobject_ptr);
    CALL_NATIVE(sig, PyTraceBack_Here, jd->f);

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
    //CHECK_EVAL_BREAKER?
    BRANCH(pb->b_handler);
}

static inline
int _ir_lower_control_flow(JITData *jd, ir_instr _instr, void *Py_UNUSED(arg)) {
    switch (_instr->opcode) {
    case ir_opcode_setup_block: {
        IR_INSTR_AS(setup_block)
        assert(instr->pb != NULL && instr->pb != INVALID_PYBLOCK);
        assert(instr->pb->b_type == instr->b_type);
        assert(instr->entry_stack_level == instr->pb->b_level);

        /* Each error handling target has an precursor, which saves the exception to the stack */
        /* TODO: Use remote section */
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

        if (instr->cond != NULL) {
            BRANCH_IF_NOT(instr->cond, instr->fallthrough, IR_LIKELY);
        }

        if (jd->use_patchpoint_error_handler) {
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
            BRANCH(jd->error_exit);
        }
        return 1;
    }
    case ir_opcode_goto_fbe: {
        IR_INSTR_AS(goto_fbe)
        assert(instr->pb != INVALID_PYBLOCK);
        ir_why why = instr->why;
        ir_pyblock cur = instr->pb;
        int curlevel = instr->entry_stack_level;
        while (why != IR_WHY_NOT && cur) {
            if (cur->b_type == IR_PYBLOCK_LOOP && why == IR_WHY_CONTINUE) {
                /* Unwind stack without popping block. 'iter' remains on TOS */
                UNWIND_TO_NEW(curlevel, cur->b_level + 1);
                //CHECK_EVAL_BREAKER?
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
                //CHECK_EVAL_BREAKER?
                BRANCH(cur->b_handler);
                why = IR_WHY_NOT;
                break;
            }

            if (why == IR_WHY_EXCEPTION && (cur->b_type == IR_PYBLOCK_EXCEPT ||
                                            cur->b_type == IR_PYBLOCK_FINALLY_TRY)) {
                if (cur->b_handler_precursor == NULL) {
                   cur->b_handler_precursor = ir_label_new(jd->func, "precursor");
                }
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
                    PUSH(jd->retval);
                } else {
                    INCREF(none);
                    PUSH(none);
                }
                PUSH(CALL_PyLong_FromLong(CONSTANT_LONG((long)_ir_why_to_why(why))));
                //CHECK_EVAL_BREAKER?
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

        SET_RETVAL(instr->value);
        JVALUE f_valuestack = LOAD_FIELD(jd->f, PyFrameObject, f_valuestack, ir_type_pyobject_ptr_ptr);
        JVALUE stack_pointer = ir_get_index_ptr(jd->func, f_valuestack, CONSTANT_INT(instr->entry_stack_level));
        STORE_FIELD(jd->f, PyFrameObject, f_stacktop, ir_type_pyobject_ptr_ptr, stack_pointer);
        STORE_FIELD(jd->f, PyFrameObject, f_lasti, ir_type_int, CONSTANT_INT(yield_f_lasti));

        /* Save stack */
        for (int i = 0; i < instr->entry_stack_level; i++) {
            STORE_AT_INDEX(f_valuestack, CONSTANT_INT(i), PEEK(instr->entry_stack_level - i));
        }
        STACKADJ(-instr->entry_stack_level);
        BRANCH(jd->exit);

        /* ~~~ Magic happens ~~~ */

        ADD_RESUME_ENTRY(yield_f_lasti, resume);
        _emit_resume_stub(jd, resume, instr->entry_stack_level + 1, instr->pb, resume_instr_index, instr->resume_inst_label);

        if (instr->throw_inst_label) {
            /* There is terrible code in genobject.c which increments f_lasti
               when an exception is thrown on a YIELD FROM. Handle this case */
            IR_LABEL_INIT(resume_extra);
            ADD_RESUME_ENTRY(yield_f_lasti + sizeof(_Py_CODEUNIT), resume_extra);
            _emit_resume_stub(jd, resume_extra, instr->entry_stack_level, instr->pb, resume_instr_index + 1, instr->throw_inst_label);
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
int _ir_lower_refcounting(JITData *jd, ir_instr _instr, void *Py_UNUSED(arg)) {
    ir_func func = jd->func;
    switch (_instr->opcode) {
    case ir_opcode_incref: {
        IR_INSTR_AS(incref)
        ir_value obj = instr->obj;
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
        ir_value obj = instr->obj;
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
int _ir_lower_stack_ops(JITData *jd, ir_instr _instr, void *Py_UNUSED(arg)) {
    switch (_instr->opcode) {
    case ir_opcode_stackadj: {
        return 1;
    }
    case ir_opcode_stack_peek: {
        IR_INSTR_AS(stack_peek)
        assert(instr->abs_offset >= 0);
        assert(instr->abs_offset < jd->stack_size);
        ir_set_value(jd->func, _instr->dest, jd->stack_values[instr->abs_offset]);
        return 1;
    }
    case ir_opcode_stack_put: {
        IR_INSTR_AS(stack_put)
        assert(instr->abs_offset >= 0);
        assert(instr->abs_offset < jd->stack_size);
        ir_set_value(jd->func, jd->stack_values[instr->abs_offset], instr->value);
        return 1;
    }
    default: break;
    } // switch
    return 0;
}

static inline
int _ir_lower_remaining(JITData *jd, ir_instr _instr, void *Py_UNUSED(arg)) {
    ir_func func = jd->func;
    switch (_instr->opcode) {
    case ir_opcode_getlocal: {
        IR_INSTR_AS(getlocal)
        ir_value addr = ir_get_index_ptr(func, jd->fastlocals, ir_constant_int(func, instr->index, NULL));
        ir_value tmp = ir_load(func, addr);
        _ir_instr_replace_dest(func, tmp, _instr->dest);
        return 1;
    }
    case ir_opcode_setlocal: {
        IR_INSTR_AS(setlocal)
        ir_value addr = ir_get_index_ptr(func, jd->fastlocals, ir_constant_int(func, instr->index, NULL));
        ir_store(func, addr, instr->value);
        return 1;
    }
    case ir_opcode_check_eval_breaker: {
        assert(0); /* Not yet impemented */
        return 1;
    }
    default: break;
    } // switch
    return 0;
}

int _ir_lower_yield_dispatch(JITData *jd, ir_instr _instr, void *Py_UNUSED(arg)) {
    if (_instr->opcode != ir_opcode_yield_dispatch) {
        return 0;
    }
    IR_INSTR_AS(yield_dispatch)

    /* TODO: Find a less quadratic way to do this dispatch */
    JVALUE f_lasti = LOAD_FIELD(jd->f, PyFrameObject, f_lasti, ir_type_int);

    /* Handle generator start */
    BRANCH_IF(CMP_LT(f_lasti, CONSTANT_INT(0)), instr->body_start, IR_SEMILIKELY);

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

typedef int (*lowerfunc_type)(JITData*, ir_instr, void*);

static inline
void _lower_pass(JITData *jd, lowerfunc_type lowerfunc, void *arg) {
    ir_func func = jd->func;
    ir_block b;
    ir_instr _instr;
    ir_instr _instr_next;
    ir_instr _instr_prev;

    for (b = func->first_block; b != NULL; b = b->next) {
        for (_instr = b->first_instr; _instr != NULL; _instr = _instr_next) {
            _instr_next = _instr->next;
            _instr_prev = _instr->prev;

            if (!ir_opcode_is_python_specific(_instr->opcode))
                continue;

            /* Set our position to just before instruction we may replace */
            ir_cursor_open(func, b, _instr->prev);

            /* Insert lowered version */
            int did_lower = lowerfunc(jd, _instr, arg);

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

void ir_lower(JITData *jd) {
    /* For lowering, we need to get the pyblock map */
    ir_remove_dead_blocks(jd->func);

    ir_label ignored[] = { jd->exit, jd->error_exit, NULL };
    ir_pyblock_map map = ir_compute_pyblock_map(jd->func, ignored);
    if (Py_JITDebugFlag > 1) {
        ir_dump_pyblock_map(map, "/tmp/pyblock_map.ir");
    }

    /* First-pass: Lower pyblock control flow */
    _lower_pass(jd, _ir_lower_control_flow, map);
    ir_free_pyblock_map(map);

    if (Py_JITDebugFlag > 1) {
        ir_func_dump_file(jd->func, "/tmp/after_lower_control_flow.ir", "After lowering control flow");
    }

    /* Second-pass: Lower yield dispatch */
    if (jd->is_gen) {
        _lower_pass(jd, _ir_lower_yield_dispatch, NULL);
        if (Py_JITDebugFlag > 1) {
            ir_func_dump_file(jd->func, "/tmp/after_lower_yield_dispatch.ir", "After lowering yield dispatch");
        }
    }

    ir_remove_dead_blocks(jd->func);

    if (Py_JITDebugFlag > 1) {
        ir_func_dump_file(jd->func, "/tmp/after_remove_dead_blocks_2.ir", "After remove dead blocks 2");
    }

    /* Fourth-pass: Lower stack operations */
    jd->stack_size = ir_compute_stack_positions(jd->func);
    //assert(jd->co->co_stacksize == jd->stack_size);
    jd->stack_values = (ir_value*)malloc(sizeof(ir_value) * jd->stack_size);
    for (int i = 0; i < jd->stack_size; i++) {
        jd->stack_values[i] = ir_value_new(jd->func, ir_type_pyobject_ptr);
    }
    _lower_pass(jd, _ir_lower_stack_ops, NULL);
    free(jd->stack_values);

    if (Py_JITDebugFlag > 1) {
        ir_func_dump_file(jd->func, "/tmp/after_lower_stack_ops.ir", "After lowering stack operations");
    }

    /* Third-pass: Lower incref/decref */
    _lower_pass(jd, _ir_lower_refcounting, NULL);

    if (Py_JITDebugFlag > 1) {
        ir_func_dump_file(jd->func, "/tmp/after_lower_refcounting.ir", "After lowering reference counting");
    }

    /* Fifth-pass: Lower everything else */
    _lower_pass(jd, _ir_lower_remaining, NULL);
}

void ir_verify_stack_effect(ir_func func) {
    ir_block b;
    ir_instr _instr;
    ssize_t num_blocks = ir_func_largest_block_index(func);
    int *stack_at_entry = (int*)malloc(num_blocks * sizeof(int));
    for (int i = 0; i < num_blocks; i++) {
        stack_at_entry[i] = -1;
    }
    stack_at_entry[func->first_block->index] = 0;

    int errors = 0;
    int rerun = 1;
    while (rerun) {
        rerun = 0;
        for (b = func->first_block; b != NULL; b = b->next) {
            int entry_stack_level = stack_at_entry[b->index];
            if (entry_stack_level == -1) {
                rerun = 1;
                continue;
            }
            int block_stack_effect = 0;
            for (_instr = b->first_instr; _instr != NULL; _instr = _instr->next) {
                if (_instr->opcode == ir_opcode_stackadj) {
                    IR_INSTR_AS(stackadj)
                    block_stack_effect += instr->amount;
                } else if (ir_instr_opcode_is_flow_control(_instr->opcode)) {
                    break;
                }
            }
            assert(_instr != NULL && _instr->next == NULL);
            int exit_stack_level = entry_stack_level + block_stack_effect;
#define SET_AND_CHECK(_label) do { \
    ir_label label = (_label); \
    size_t index = label->block->index; \
    if (stack_at_entry[index] == -1) { \
        stack_at_entry[index] = exit_stack_level; \
    } \
    if (stack_at_entry[index] != exit_stack_level) { \
        fprintf(stderr, "Stack level mismatch at branch from %p to %s (%p)\n", b, label->name ? label->name : "<NULL>", label->block); \
        errors = 1; \
    } \
} while (0)
            switch (_instr->opcode) {
            case ir_opcode_branch: {
                IR_INSTR_AS(branch)
                SET_AND_CHECK(instr->target);
                break;
            }
            case ir_opcode_branch_cond: {
                IR_INSTR_AS(branch_cond)
                SET_AND_CHECK(instr->if_true);
                SET_AND_CHECK(instr->if_false);
                break;
            }
            case ir_opcode_jumptable: {
                IR_INSTR_AS(jumptable)
                for (size_t i = 0; i < instr->table_size; i++) {
                    SET_AND_CHECK(instr->table[i]);
                }
                break;
            }
            case ir_opcode_ret: {
                assert(exit_stack_level == 0);
            }
            default: abort(); /* Unhandled flow control */
            } // switch
#undef SET_AND_CHECK
        }
    }
    assert(!errors);
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

    /* Retrieve the PyFrameObject */
    PyFrameObject *f = (PyFrameObject*)ir_stackmap_read_value(stackmap, 0, save_area);
    assert(Py_TYPE(f) == &PyFrame_Type);

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
       The second argument is jd->f, and later arguments are the stack positions to pop, in
       reverse order. */
    ir_value *args = (ir_value*)malloc((total_unwind + 2) * sizeof(ir_value));
    args[0] = CONSTANT_PTR(ir_type_void_ptr, info);
    args[1] = jd->f;

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
        BRANCH(jd->error_exit);
    }
}
