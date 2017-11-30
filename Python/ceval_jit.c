/* enable more aggressive intra-module optimizations, where available */
#define PY_LOCAL_AGGRESSIVE

#include "Python.h"
#include "internal/pystate.h"

#include "code.h"
#include "dictobject.h"
#include "frameobject.h"
#include "opcode.h"
#include "pydtrace.h"
#include "setobject.h"
#include "structmember.h"
#include "jit.h"
#include "internal/jit.h"

#include <ctype.h>

/* Private API for the LOAD_METHOD opcode. */
extern int _PyObject_GetMethod(PyObject *, PyObject *, PyObject **);

typedef PyObject *(*callproc)(PyObject *, PyObject *, PyObject *);

/* Forward declarations */
PyObject *
call_function_extern(PyObject ***, Py_ssize_t,
                     PyObject *);
PyObject * do_call_core(PyObject *, PyObject *, PyObject *);

#ifdef LLTRACE
extern int lltrace;
int prtrace(PyObject *, const char *);
#endif

int call_trace(Py_tracefunc, PyObject *,
                      PyThreadState *, PyFrameObject *,
                      int, PyObject *);
int call_trace_protected(Py_tracefunc, PyObject *,
                                PyThreadState *, PyFrameObject *,
                                int, PyObject *);
void call_exc_trace(Py_tracefunc, PyObject *,
                           PyThreadState *, PyFrameObject *);
int maybe_call_line_trace(Py_tracefunc, PyObject *,
                                 PyThreadState *, PyFrameObject *,
                                 int *, int *, int *);
void maybe_dtrace_line(PyFrameObject *, int *, int *, int *);
void dtrace_function_entry(PyFrameObject *);
void dtrace_function_return(PyFrameObject *);

PyObject * cmp_outcome(int, PyObject *, PyObject *);
PyObject * import_name(PyFrameObject *, PyObject *, PyObject *,
                              PyObject *);
PyObject * import_from(PyObject *, PyObject *);
int import_all_from(PyObject *, PyObject *);
void format_exc_check_arg(PyObject *, const char *, PyObject *);
void format_exc_unbound(PyCodeObject *co, int oparg);
PyObject * unicode_concatenate(PyObject *, PyObject *,
                                      PyFrameObject *, const _Py_CODEUNIT *);
PyObject * special_lookup(PyObject *, _Py_Identifier *);
int check_args_iterable(PyObject *func, PyObject *vararg);
void format_kwargs_mapping_error(PyObject *func, PyObject *kwargs);

#define NAME_ERROR_MSG \
    "name '%.200s' is not defined"
#define UNBOUNDLOCAL_ERROR_MSG \
    "local variable '%.200s' referenced before assignment"
#define UNBOUNDFREE_ERROR_MSG \
    "free variable '%.200s' referenced before assignment" \
    " in enclosing scope"

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

void *dummy[] = {_gil_initialize, gil_created, destroy_gil, recreate_gil};

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

int do_raise(PyObject *, PyObject *);
int unpack_iterable(PyObject *, int, int, PyObject **);

#define _Py_TracingPossible _PyRuntime.ceval.tracing_possible

/* Tuple access macros */

#ifndef Py_DEBUG
#define GETITEM(v, i) PyTuple_GET_ITEM((PyTupleObject *)(v), (i))
#else
#define GETITEM(v, i) PyTuple_GetItem((v), (i))
#endif

//----------------------------------------------------------------------------------

#define TARGET_FUNC(op) \
    int _PyEval_FUNC_JIT_TARGET_##op (EvalContext *ctx, PyFrameObject *f, int next_instr_index, int opcode, int oparg, int jumpev)

#define TARGET_SPECIAL(op) \
    int _PyEval_FUNC_JIT_TARGET_II_##op(EvalContext *ctx, PyFrameObject *f, int next_instr_index, int jumpev)

#define JIT_TARGET_EXPORT(f)

#undef PREDICT
#define PREDICT(a)

#undef DISPATCH
#undef FAST_DISPATCH


#define PREPARE_NEXT_INSTR_INDEX()  if (!jumpev) ctx->next_instr_index = next_instr_index;

#define DISPATCH() do { \
    if (_Py_atomic_load_relaxed(&_PyRuntime.ceval.eval_breaker)) { \
        PREPARE_NEXT_INSTR_INDEX(); \
        return JIT_RC_NEXT_OPCODE; \
    } else { \
        return jumpev ? JIT_RC_JUMP : JIT_RC_FLOW; \
    } \
} while (0)

#define FAST_DISPATCH()          do { return jumpev ? JIT_RC_JUMP : JIT_RC_FLOW; } while (0)
#define GOTO_FAST_NEXT_OPCODE()  do { return jumpev ? JIT_RC_JUMP : JIT_RC_FLOW; } while (0)
#define GOTO_FAST_YIELD()        do { PREPARE_NEXT_INSTR_INDEX(); return JIT_RC_FAST_YIELD; } while (0)
#define GOTO_FAST_BLOCK_END()    do { PREPARE_NEXT_INSTR_INDEX(); return JIT_RC_FAST_BLOCK_END; } while (0)
#define GOTO_ERROR()             do { PREPARE_NEXT_INSTR_INDEX(); return JIT_RC_ERROR; } while (0)
#define GOTO_UNWIND_CLEANUP()    do { PREPARE_NEXT_INSTR_INDEX(); return JIT_RC_UNWIND_CLEANUP; } while (0)
#define GOTO_NEXT_OPCODE()       do { PREPARE_NEXT_INSTR_INDEX(); return JIT_RC_NEXT_OPCODE; } while (0)
#define GOTO_EXIT_EVAL_FRAME()   do { PREPARE_NEXT_INSTR_INDEX(); return JIT_RC_EXIT; } while (0)


#undef INSTR_OFFSET
#define INSTR_OFFSET()          (sizeof(_Py_CODEUNIT) * next_instr_index)
#define NEXT_INSTR()            (ctx->first_instr + next_instr_index)

#undef NEXTOPARG
#undef JUMPTO
#define JUMPTO(x) do { \
    ctx->next_instr_index = (x)/sizeof(_Py_CODEUNIT); \
    jumpev = 1; \
} while (0)

#undef JUMPBY
#define JUMPBY(x) do { \
    ctx->next_instr_index = next_instr_index + (x)/sizeof(_Py_CODEUNIT); \
    jumpev = 1; \
} while (0)

#undef STACK_LEVEL
#undef EMPTY
#undef TOP
#undef SECOND
#undef THIRD
#undef FOURTH
#undef PEEK
#undef SET_TOP
#undef SET_SECOND
#undef SET_THIRD
#undef SET_FOURTH
#undef SET_VALUE
#undef BASIC_STACKADJ
#undef BASIC_PUSH
#undef BASIC_POP
#define FRAMEPTR()        (f)
#define STACKPTR()        (ctx->stack_pointer)

#define STACK_LEVEL()     ((int)(STACKPTR() - FRAMEPTR()->f_valuestack))
#define EMPTY()           (STACK_LEVEL() == 0)
#define TOP()             (STACKPTR()[-1])
#define SECOND()          (STACKPTR()[-2])
#define THIRD()           (STACKPTR()[-3])
#define FOURTH()          (STACKPTR()[-4])
#define PEEK(n)           (STACKPTR()[-(n)])
#define SET_TOP(v)        (STACKPTR()[-1] = (v))
#define SET_SECOND(v)     (STACKPTR()[-2] = (v))
#define SET_THIRD(v)      (STACKPTR()[-3] = (v))
#define SET_FOURTH(v)     (STACKPTR()[-4] = (v))
#define SET_VALUE(n, v)   (STACKPTR()[-(n)] = (v))
#define BASIC_STACKADJ(n) (STACKPTR() += n)
#define BASIC_PUSH(v)     (*STACKPTR()++ = (v))
#define BASIC_POP()       (*--STACKPTR())

#undef PUSH
#undef POP
#undef STACKADJ
#undef EXT_POP
#define PUSH(v)                BASIC_PUSH(v)
#define POP()                  BASIC_POP()
#define STACKADJ(n)            BASIC_STACKADJ(n)
#define EXT_POP(STACK_POINTER) (*--(STACK_POINTER))

#undef GETLOCAL
#undef SETLOCAL
#define GETLOCAL(i)     (ctx->fastlocals[i])
#define SETLOCAL(i, value)      do { PyObject *tmp = GETLOCAL(i); \
                                     GETLOCAL(i) = value; \
                                     Py_XDECREF(tmp); } while (0)

#undef UNWIND_BLOCK
#define UNWIND_BLOCK(b) \
    while (STACK_LEVEL() > (b)->b_level) { \
        PyObject *v = POP(); \
        Py_XDECREF(v); \
    }

#undef UNWIND_EXCEPT_HANDLER
#define UNWIND_EXCEPT_HANDLER(b) \
    do { \
        PyObject *type, *value, *traceback; \
        _PyErr_StackItem *exc_info; \
        assert(STACK_LEVEL() >= (b)->b_level + 3); \
        while (STACK_LEVEL() > (b)->b_level + 3) { \
            value = POP(); \
            Py_XDECREF(value); \
        } \
        exc_info = ctx->tstate->exc_info; \
        type = exc_info->exc_type; \
        value = exc_info->exc_value; \
        traceback = exc_info->exc_traceback; \
        exc_info->exc_type = POP(); \
        exc_info->exc_value = POP(); \
        exc_info->exc_traceback = POP(); \
        Py_XDECREF(type); \
        Py_XDECREF(value); \
        Py_XDECREF(traceback); \
    } while(0)


TARGET_SPECIAL(NEXT_OPCODE) {
    assert(STACKPTR() >= FRAMEPTR()->f_valuestack); /* else underflow */
    assert(STACK_LEVEL() <= ctx->co->co_stacksize);  /* else overflow */
    assert(!PyErr_Occurred());

    /* Do periodic things.  Doing this every time through
       the loop would add too much overhead, so we do it
       only every Nth instruction.  We also do it if
       ``pendingcalls_to_do'' is set, i.e. when an asynchronous
       event needs attention (e.g. a signal handler or
       async I/O handler); see Py_AddPendingCall() and
       Py_MakePendingCalls() above. */

    if (_Py_atomic_load_relaxed(&_PyRuntime.ceval.eval_breaker)) {
        if (_Py_OPCODE(*NEXT_INSTR()) == SETUP_FINALLY ||
            _Py_OPCODE(*NEXT_INSTR()) == YIELD_FROM) {
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
            GOTO_FAST_NEXT_OPCODE();
        }
        if (_Py_atomic_load_relaxed(
                    &_PyRuntime.ceval.pending.calls_to_do))
        {
            if (Py_MakePendingCalls() < 0)
                GOTO_ERROR();
        }
        if (_Py_atomic_load_relaxed(
                    &_PyRuntime.ceval.gil_drop_request))
        {
            /* Give another thread a chance */
            if (PyThreadState_Swap(NULL) != ctx->tstate)
                Py_FatalError("ceval: ctx->tstate mix-up");
            drop_gil(ctx->tstate);

            /* Other threads may run now */

            take_gil(ctx->tstate);

            /* Check if we should make a quick exit. */
            if (_Py_IsFinalizing() &&
                !_Py_CURRENTLY_FINALIZING(ctx->tstate))
            {
                drop_gil(ctx->tstate);
                PyThread_exit_thread();
            }

            if (PyThreadState_Swap(ctx->tstate) != NULL)
                Py_FatalError("ceval: orphan ctx->tstate");
        }
        /* Check for asynchronous exceptions. */
        if (ctx->tstate->async_exc != NULL) {
            PyObject *exc = ctx->tstate->async_exc;
            ctx->tstate->async_exc = NULL;
            UNSIGNAL_ASYNC_EXC();
            PyErr_SetNone(exc);
            Py_DECREF(exc);
            GOTO_ERROR();
        }
    }
    GOTO_FAST_NEXT_OPCODE();
}
JIT_TARGET_EXPORT(II_NEXT_OPCODE)

TARGET_SPECIAL(ERROR) {
    assert(ctx->why == WHY_NOT);
    ctx->why = WHY_EXCEPTION;

    /* Double-check exception status. */
#ifdef NDEBUG
    if (!PyErr_Occurred())
        PyErr_SetString(PyExc_SystemError,
                        "error return without exception set");
#else
    assert(PyErr_Occurred());
#endif

    /* Log traceback info. */
    PyTraceBack_Here(f);

    if (ctx->tstate->c_tracefunc != NULL)
        call_exc_trace(ctx->tstate->c_tracefunc, ctx->tstate->c_traceobj,
                       ctx->tstate, f);
    GOTO_FAST_BLOCK_END();
}
JIT_TARGET_EXPORT(II_ERROR)

TARGET_SPECIAL(FAST_BLOCK_END) {
    assert(ctx->why != WHY_NOT);

    /* Unwind stacks if a (pseudo) exception occurred */
    while (ctx->why != WHY_NOT && FRAMEPTR()->f_iblock > 0) {
        /* Peek at the current block. */
        PyTryBlock *b = &FRAMEPTR()->f_blockstack[FRAMEPTR()->f_iblock - 1];

        assert(ctx->why != WHY_YIELD);
        if (b->b_type == SETUP_LOOP && ctx->why == WHY_CONTINUE) {
            ctx->why = WHY_NOT;
            JUMPTO(PyLong_AS_LONG(ctx->retval));
            Py_DECREF(ctx->retval);
            break;
        }
        /* Now we have to pop the block. */
        FRAMEPTR()->f_iblock--;

        if (b->b_type == EXCEPT_HANDLER) {
            UNWIND_EXCEPT_HANDLER(b);
            continue;
        }
        UNWIND_BLOCK(b);
        if (b->b_type == SETUP_LOOP && ctx->why == WHY_BREAK) {
            ctx->why = WHY_NOT;
            JUMPTO(b->b_handler);
            break;
        }
        if (ctx->why == WHY_EXCEPTION && (b->b_type == SETUP_EXCEPT
            || b->b_type == SETUP_FINALLY)) {
            PyObject *exc, *val, *tb;
            int handler = b->b_handler;
            _PyErr_StackItem *exc_info = ctx->tstate->exc_info;
            /* Beware, this invalidates all b->b_* fields */
            PyFrame_BlockSetup(FRAMEPTR(), EXCEPT_HANDLER, -1, STACK_LEVEL());
            PUSH(exc_info->exc_traceback);
            PUSH(exc_info->exc_value);
            if (exc_info->exc_type != NULL) {
                PUSH(exc_info->exc_type);
            }
            else {
                Py_INCREF(Py_None);
                PUSH(Py_None);
            }
            PyErr_Fetch(&exc, &val, &tb);
            /* Make the raw exception data
               available to the handler,
               so a program can emulate the
               Python main loop. */
            PyErr_NormalizeException(
                &exc, &val, &tb);
            if (tb != NULL)
                PyException_SetTraceback(val, tb);
            else
                PyException_SetTraceback(val, Py_None);
            Py_INCREF(exc);
            exc_info->exc_type = exc;
            Py_INCREF(val);
            exc_info->exc_value = val;
            exc_info->exc_traceback = tb;
            if (tb == NULL)
                tb = Py_None;
            Py_INCREF(tb);
            PUSH(tb);
            PUSH(val);
            PUSH(exc);
            ctx->why = WHY_NOT;
            JUMPTO(handler);
            break;
        }
        if (b->b_type == SETUP_FINALLY) {
            if (ctx->why & (WHY_RETURN | WHY_CONTINUE))
                PUSH(ctx->retval);
            PUSH(PyLong_FromLong((long)ctx->why));
            ctx->why = WHY_NOT;
            JUMPTO(b->b_handler);
            break;
        }
    } /* unwind stack */

    /* End the loop if we still have an error (or return) */

    if (ctx->why != WHY_NOT)
        GOTO_UNWIND_CLEANUP();

    assert(!PyErr_Occurred());
    GOTO_NEXT_OPCODE();
}
JIT_TARGET_EXPORT(II_FAST_BLOCK_END)

TARGET_SPECIAL(FAST_YIELD) {
    if (ctx->tstate->use_tracing) {
        if (ctx->tstate->c_tracefunc) {
            if (ctx->why == WHY_RETURN || ctx->why == WHY_YIELD) {
                if (call_trace(ctx->tstate->c_tracefunc, ctx->tstate->c_traceobj,
                               ctx->tstate, FRAMEPTR(),
                               PyTrace_RETURN, ctx->retval)) {
                    Py_CLEAR(ctx->retval);
                    ctx->why = WHY_EXCEPTION;
                }
            }
            else if (ctx->why == WHY_EXCEPTION) {
                call_trace_protected(ctx->tstate->c_tracefunc, ctx->tstate->c_traceobj,
                                     ctx->tstate, FRAMEPTR(),
                                     PyTrace_RETURN, NULL);
            }
        }
        if (ctx->tstate->c_profilefunc) {
            if (ctx->why == WHY_EXCEPTION)
                call_trace_protected(ctx->tstate->c_profilefunc,
                                     ctx->tstate->c_profileobj,
                                     ctx->tstate, FRAMEPTR(),
                                     PyTrace_RETURN, NULL);
            else if (call_trace(ctx->tstate->c_profilefunc, ctx->tstate->c_profileobj,
                                ctx->tstate, FRAMEPTR(),
                                PyTrace_RETURN, ctx->retval)) {
                Py_CLEAR(ctx->retval);
                /* ctx->why = WHY_EXCEPTION; */
            }
        }
    }
    GOTO_EXIT_EVAL_FRAME();
}
JIT_TARGET_EXPORT(II_FAST_YIELD)

TARGET_SPECIAL(UNWIND_CLEANUP) {
    assert(ctx->why != WHY_YIELD);
    /* Pop remaining stack entries. */
    while (!EMPTY()) {
        PyObject *o = POP();
        Py_XDECREF(o);
    }

    if (ctx->why != WHY_RETURN)
        ctx->retval = NULL;

    assert((ctx->retval != NULL) ^ (PyErr_Occurred() != NULL));
    GOTO_FAST_YIELD();
}
JIT_TARGET_EXPORT(II_UNWIND_CLEANUP)

TARGET_FUNC(NOP) {
    FAST_DISPATCH();
}
JIT_TARGET_EXPORT(NOP)

TARGET_FUNC(LOAD_FAST) {
    PyObject *value = GETLOCAL(oparg);
    if (value == NULL) {
        format_exc_check_arg(PyExc_UnboundLocalError,
                             UNBOUNDLOCAL_ERROR_MSG,
                             PyTuple_GetItem(ctx->co->co_varnames, oparg));
        GOTO_ERROR();
    }
    Py_INCREF(value);
    PUSH(value);
    FAST_DISPATCH();
}
JIT_TARGET_EXPORT(LOAD_FAST)

TARGET_FUNC(LOAD_CONST) {
    PyObject *value = GETITEM(ctx->consts, oparg);
    Py_INCREF(value);
    PUSH(value);
    FAST_DISPATCH();
}
JIT_TARGET_EXPORT(LOAD_CONST)

TARGET_FUNC(STORE_FAST) {
    PyObject *value = POP();
    SETLOCAL(oparg, value);
    FAST_DISPATCH();
}
JIT_TARGET_EXPORT(STORE_FAST)

TARGET_FUNC(POP_TOP) {
    PyObject *value = POP();
    Py_DECREF(value);
    FAST_DISPATCH();
}
JIT_TARGET_EXPORT(POP_TOP)

TARGET_FUNC(ROT_TWO) {
    PyObject *top = TOP();
    PyObject *second = SECOND();
    SET_TOP(second);
    SET_SECOND(top);
    FAST_DISPATCH();
}
JIT_TARGET_EXPORT(ROT_TWO)

TARGET_FUNC(ROT_THREE) {
    PyObject *top = TOP();
    PyObject *second = SECOND();
    PyObject *third = THIRD();
    SET_TOP(second);
    SET_SECOND(third);
    SET_THIRD(top);
    FAST_DISPATCH();
}
JIT_TARGET_EXPORT(ROT_THREE)

TARGET_FUNC(DUP_TOP) {
    PyObject *top = TOP();
    Py_INCREF(top);
    PUSH(top);
    FAST_DISPATCH();
}
JIT_TARGET_EXPORT(DUP_TOP)

TARGET_FUNC(DUP_TOP_TWO) {
    PyObject *top = TOP();
    PyObject *second = SECOND();
    Py_INCREF(top);
    Py_INCREF(second);
    STACKADJ(2);
    SET_TOP(top);
    SET_SECOND(second);
    FAST_DISPATCH();
}
JIT_TARGET_EXPORT(DUP_TOP_TWO)

TARGET_FUNC(UNARY_POSITIVE) {
    PyObject *value = TOP();
    PyObject *res = PyNumber_Positive(value);
    Py_DECREF(value);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(UNARY_POSITIVE)

TARGET_FUNC(UNARY_NEGATIVE) {
    PyObject *value = TOP();
    PyObject *res = PyNumber_Negative(value);
    Py_DECREF(value);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(UNARY_NEGATIVE)

TARGET_FUNC(UNARY_NOT) {
    PyObject *value = TOP();
    int err = PyObject_IsTrue(value);
    Py_DECREF(value);
    if (err == 0) {
        Py_INCREF(Py_True);
        SET_TOP(Py_True);
        DISPATCH();
    }
    else if (err > 0) {
        Py_INCREF(Py_False);
        SET_TOP(Py_False);
        DISPATCH();
    }
    STACKADJ(-1);
    GOTO_ERROR();
}
JIT_TARGET_EXPORT(UNARY_NOT)

TARGET_FUNC(UNARY_INVERT) {
    PyObject *value = TOP();
    PyObject *res = PyNumber_Invert(value);
    Py_DECREF(value);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(UNARY_INVERT)

TARGET_FUNC(BINARY_POWER) {
    PyObject *exp = POP();
    PyObject *base = TOP();
    PyObject *res = PyNumber_Power(base, exp, Py_None);
    Py_DECREF(base);
    Py_DECREF(exp);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(BINARY_POWER)

TARGET_FUNC(BINARY_MULTIPLY) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *res = PyNumber_Multiply(left, right);
    Py_DECREF(left);
    Py_DECREF(right);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(BINARY_MULTIPLY)

TARGET_FUNC(BINARY_MATRIX_MULTIPLY) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *res = PyNumber_MatrixMultiply(left, right);
    Py_DECREF(left);
    Py_DECREF(right);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(BINARY_MATRIX_MULTIPLY)

TARGET_FUNC(BINARY_TRUE_DIVIDE) {
    PyObject *divisor = POP();
    PyObject *dividend = TOP();
    PyObject *quotient = PyNumber_TrueDivide(dividend, divisor);
    Py_DECREF(dividend);
    Py_DECREF(divisor);
    SET_TOP(quotient);
    if (quotient == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(BINARY_TRUE_DIVIDE)

TARGET_FUNC(BINARY_FLOOR_DIVIDE) {
    PyObject *divisor = POP();
    PyObject *dividend = TOP();
    PyObject *quotient = PyNumber_FloorDivide(dividend, divisor);
    Py_DECREF(dividend);
    Py_DECREF(divisor);
    SET_TOP(quotient);
    if (quotient == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(BINARY_FLOOR_DIVIDE)

TARGET_FUNC(BINARY_MODULO) {
    PyObject *divisor = POP();
    PyObject *dividend = TOP();
    PyObject *res;
    if (PyUnicode_CheckExact(dividend) && (
          !PyUnicode_Check(divisor) || PyUnicode_CheckExact(divisor))) {
      // fast path; string formatting, but not if the RHS is a str subclass
      // (see issue28598)
      res = PyUnicode_Format(dividend, divisor);
    } else {
      res = PyNumber_Remainder(dividend, divisor);
    }
    Py_DECREF(divisor);
    Py_DECREF(dividend);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(BINARY_MODULO)

TARGET_FUNC(BINARY_ADD) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *sum;
    /* NOTE(haypo): Please don't try to micro-optimize int+int on
       CPython using bytecode, it is simply worthless.
       See http://bugs.python.org/issue21955 and
       http://bugs.python.org/issue10044 for the discussion. In short,
       no patch shown any impact on a realistic benchmark, only a minor
       speedup on microbenchmarks. */
    if (PyUnicode_CheckExact(left) &&
             PyUnicode_CheckExact(right)) {
        sum = unicode_concatenate(left, right, FRAMEPTR(), NEXT_INSTR());
        /* unicode_concatenate consumed the ref to left */
    }
    else {
        sum = PyNumber_Add(left, right);
        Py_DECREF(left);
    }
    Py_DECREF(right);
    SET_TOP(sum);
    if (sum == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(BINARY_ADD)

TARGET_FUNC(BINARY_SUBTRACT) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *diff = PyNumber_Subtract(left, right);
    Py_DECREF(right);
    Py_DECREF(left);
    SET_TOP(diff);
    if (diff == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(BINARY_SUBTRACT)

TARGET_FUNC(BINARY_SUBSCR) {
    PyObject *sub = POP();
    PyObject *container = TOP();
    PyObject *res = PyObject_GetItem(container, sub);
    Py_DECREF(container);
    Py_DECREF(sub);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(BINARY_SUBSCR)

TARGET_FUNC(BINARY_LSHIFT) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *res = PyNumber_Lshift(left, right);
    Py_DECREF(left);
    Py_DECREF(right);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(BINARY_LSHIFT)

TARGET_FUNC(BINARY_RSHIFT) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *res = PyNumber_Rshift(left, right);
    Py_DECREF(left);
    Py_DECREF(right);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(BINARY_RSHIFT)

TARGET_FUNC(BINARY_AND) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *res = PyNumber_And(left, right);
    Py_DECREF(left);
    Py_DECREF(right);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(BINARY_AND)

TARGET_FUNC(BINARY_XOR) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *res = PyNumber_Xor(left, right);
    Py_DECREF(left);
    Py_DECREF(right);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(BINARY_XOR)

TARGET_FUNC(BINARY_OR) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *res = PyNumber_Or(left, right);
    Py_DECREF(left);
    Py_DECREF(right);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(BINARY_OR)

TARGET_FUNC(LIST_APPEND) {
    PyObject *v = POP();
    PyObject *list = PEEK(oparg);
    int err;
    err = PyList_Append(list, v);
    Py_DECREF(v);
    if (err != 0)
        GOTO_ERROR();
    PREDICT(JUMP_ABSOLUTE);
    DISPATCH();
}
JIT_TARGET_EXPORT(LIST_APPEND)

TARGET_FUNC(SET_ADD) {
    PyObject *v = POP();
    PyObject *set = PEEK(oparg);
    int err;
    err = PySet_Add(set, v);
    Py_DECREF(v);
    if (err != 0)
        GOTO_ERROR();
    PREDICT(JUMP_ABSOLUTE);
    DISPATCH();
}
JIT_TARGET_EXPORT(SET_ADD)

TARGET_FUNC(INPLACE_POWER) {
    PyObject *exp = POP();
    PyObject *base = TOP();
    PyObject *res = PyNumber_InPlacePower(base, exp, Py_None);
    Py_DECREF(base);
    Py_DECREF(exp);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(INPLACE_POWER)

TARGET_FUNC(INPLACE_MULTIPLY) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *res = PyNumber_InPlaceMultiply(left, right);
    Py_DECREF(left);
    Py_DECREF(right);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(INPLACE_MULTIPLY)

TARGET_FUNC(INPLACE_MATRIX_MULTIPLY) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *res = PyNumber_InPlaceMatrixMultiply(left, right);
    Py_DECREF(left);
    Py_DECREF(right);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(INPLACE_MATRIX_MULTIPLY)

TARGET_FUNC(INPLACE_TRUE_DIVIDE) {
    PyObject *divisor = POP();
    PyObject *dividend = TOP();
    PyObject *quotient = PyNumber_InPlaceTrueDivide(dividend, divisor);
    Py_DECREF(dividend);
    Py_DECREF(divisor);
    SET_TOP(quotient);
    if (quotient == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(INPLACE_TRUE_DIVIDE)

TARGET_FUNC(INPLACE_FLOOR_DIVIDE) {
    PyObject *divisor = POP();
    PyObject *dividend = TOP();
    PyObject *quotient = PyNumber_InPlaceFloorDivide(dividend, divisor);
    Py_DECREF(dividend);
    Py_DECREF(divisor);
    SET_TOP(quotient);
    if (quotient == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(INPLACE_FLOOR_DIVIDE)

TARGET_FUNC(INPLACE_MODULO) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *mod = PyNumber_InPlaceRemainder(left, right);
    Py_DECREF(left);
    Py_DECREF(right);
    SET_TOP(mod);
    if (mod == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(INPLACE_MODULO)

TARGET_FUNC(INPLACE_ADD) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *sum;
    if (PyUnicode_CheckExact(left) && PyUnicode_CheckExact(right)) {
        sum = unicode_concatenate(left, right, FRAMEPTR(), NEXT_INSTR());
        /* unicode_concatenate consumed the ref to left */
    }
    else {
        sum = PyNumber_InPlaceAdd(left, right);
        Py_DECREF(left);
    }
    Py_DECREF(right);
    SET_TOP(sum);
    if (sum == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(INPLACE_ADD)

TARGET_FUNC(INPLACE_SUBTRACT) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *diff = PyNumber_InPlaceSubtract(left, right);
    Py_DECREF(left);
    Py_DECREF(right);
    SET_TOP(diff);
    if (diff == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(INPLACE_SUBTRACT)

TARGET_FUNC(INPLACE_LSHIFT) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *res = PyNumber_InPlaceLshift(left, right);
    Py_DECREF(left);
    Py_DECREF(right);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(INPLACE_LSHIFT)

TARGET_FUNC(INPLACE_RSHIFT) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *res = PyNumber_InPlaceRshift(left, right);
    Py_DECREF(left);
    Py_DECREF(right);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(INPLACE_RSHIFT)

TARGET_FUNC(INPLACE_AND) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *res = PyNumber_InPlaceAnd(left, right);
    Py_DECREF(left);
    Py_DECREF(right);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(INPLACE_AND)

TARGET_FUNC(INPLACE_XOR) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *res = PyNumber_InPlaceXor(left, right);
    Py_DECREF(left);
    Py_DECREF(right);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(INPLACE_XOR)

TARGET_FUNC(INPLACE_OR) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *res = PyNumber_InPlaceOr(left, right);
    Py_DECREF(left);
    Py_DECREF(right);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(INPLACE_OR)

TARGET_FUNC(STORE_SUBSCR) {
    PyObject *sub = TOP();
    PyObject *container = SECOND();
    PyObject *v = THIRD();
    int err;
    STACKADJ(-3);
    /* container[sub] = v */
    err = PyObject_SetItem(container, sub, v);
    Py_DECREF(v);
    Py_DECREF(container);
    Py_DECREF(sub);
    if (err != 0)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(STORE_SUBSCR)

TARGET_FUNC(STORE_ANNOTATION) {
    _Py_IDENTIFIER(__annotations__);
    PyObject *ann_dict;
    PyObject *ann = POP();
    PyObject *name = GETITEM(ctx->names, oparg);
    int err;
    if (FRAMEPTR()->f_locals == NULL) {
        PyErr_Format(PyExc_SystemError,
                     "no locals found when storing annotation");
        Py_DECREF(ann);
        GOTO_ERROR();
    }
    /* first try to get __annotations__ from locals... */
    if (PyDict_CheckExact(FRAMEPTR()->f_locals)) {
        ann_dict = _PyDict_GetItemId(FRAMEPTR()->f_locals,
                                     &PyId___annotations__);
        if (ann_dict == NULL) {
            PyErr_SetString(PyExc_NameError,
                            "__annotations__ not found");
            Py_DECREF(ann);
            GOTO_ERROR();
        }
        Py_INCREF(ann_dict);
    }
    else {
        PyObject *ann_str = _PyUnicode_FromId(&PyId___annotations__);
        if (ann_str == NULL) {
            Py_DECREF(ann);
            GOTO_ERROR();
        }
        ann_dict = PyObject_GetItem(FRAMEPTR()->f_locals, ann_str);
        if (ann_dict == NULL) {
            if (PyErr_ExceptionMatches(PyExc_KeyError)) {
                PyErr_SetString(PyExc_NameError,
                                "__annotations__ not found");
            }
            Py_DECREF(ann);
            GOTO_ERROR();
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
    Py_DECREF(ann);
    if (err != 0) {
        GOTO_ERROR();
    }
    DISPATCH();
}
JIT_TARGET_EXPORT(STORE_ANNOTATION)

TARGET_FUNC(DELETE_SUBSCR) {
    PyObject *sub = TOP();
    PyObject *container = SECOND();
    int err;
    STACKADJ(-2);
    /* del container[sub] */
    err = PyObject_DelItem(container, sub);
    Py_DECREF(container);
    Py_DECREF(sub);
    if (err != 0)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(DELETE_SUBSCR)

TARGET_FUNC(PRINT_EXPR) {
    _Py_IDENTIFIER(displayhook);
    PyObject *value = POP();
    PyObject *hook = _PySys_GetObjectId(&PyId_displayhook);
    PyObject *res;
    if (hook == NULL) {
        PyErr_SetString(PyExc_RuntimeError,
                        "lost sys.displayhook");
        Py_DECREF(value);
        GOTO_ERROR();
    }
    res = PyObject_CallFunctionObjArgs(hook, value, NULL);
    Py_DECREF(value);
    if (res == NULL)
        GOTO_ERROR();
    Py_DECREF(res);
    DISPATCH();
}
JIT_TARGET_EXPORT(PRINT_EXPR)

TARGET_FUNC(RAISE_VARARGS) {
    PyObject *cause = NULL, *exc = NULL;
    switch (oparg) {
    case 2:
        cause = POP(); /* cause */
        /* fall through */
    case 1:
        exc = POP(); /* exc */
        /* fall through */
    case 0:
        if (do_raise(exc, cause)) {
            ctx->why = WHY_EXCEPTION;
            GOTO_FAST_BLOCK_END();
        }
        break;
    default:
        PyErr_SetString(PyExc_SystemError,
                   "bad RAISE_VARARGS oparg");
        break;
    }
    GOTO_ERROR();
}
JIT_TARGET_EXPORT(RAISE_VARARGS)

TARGET_FUNC(RETURN_VALUE) {
    ctx->retval = POP();
    ctx->why = WHY_RETURN;
    GOTO_FAST_BLOCK_END();
}
JIT_TARGET_EXPORT(RETURN_VALUE)

TARGET_FUNC(GET_AITER) {
    unaryfunc getter = NULL;
    PyObject *iter = NULL;
    PyObject *obj = TOP();
    PyTypeObject *type = Py_TYPE(obj);

    if (type->tp_as_async != NULL) {
        getter = type->tp_as_async->am_aiter;
    }

    if (getter != NULL) {
        iter = (*getter)(obj);
        Py_DECREF(obj);
        if (iter == NULL) {
            SET_TOP(NULL);
            GOTO_ERROR();
        }
    }
    else {
        SET_TOP(NULL);
        PyErr_Format(
            PyExc_TypeError,
            "'async for' requires an object with "
            "__aiter__ method, got %.100s",
            type->tp_name);
        Py_DECREF(obj);
        GOTO_ERROR();
    }

    if (Py_TYPE(iter)->tp_as_async == NULL ||
            Py_TYPE(iter)->tp_as_async->am_anext == NULL) {

        SET_TOP(NULL);
        PyErr_Format(
            PyExc_TypeError,
            "'async for' received an object from __aiter__ "
            "that does not implement __anext__: %.100s",
            Py_TYPE(iter)->tp_name);
        Py_DECREF(iter);
        GOTO_ERROR();
    }

    SET_TOP(iter);
    DISPATCH();
}
JIT_TARGET_EXPORT(GET_AITER)

TARGET_FUNC(GET_ANEXT) {
    unaryfunc getter = NULL;
    PyObject *next_iter = NULL;
    PyObject *awaitable = NULL;
    PyObject *aiter = TOP();
    PyTypeObject *type = Py_TYPE(aiter);

    if (PyAsyncGen_CheckExact(aiter)) {
        awaitable = type->tp_as_async->am_anext(aiter);
        if (awaitable == NULL) {
            GOTO_ERROR();
        }
    } else {
        if (type->tp_as_async != NULL){
            getter = type->tp_as_async->am_anext;
        }

        if (getter != NULL) {
            next_iter = (*getter)(aiter);
            if (next_iter == NULL) {
                GOTO_ERROR();
            }
        }
        else {
            PyErr_Format(
                PyExc_TypeError,
                "'async for' requires an iterator with "
                "__anext__ method, got %.100s",
                type->tp_name);
            GOTO_ERROR();
        }

        awaitable = _PyCoro_GetAwaitableIter(next_iter);
        if (awaitable == NULL) {
            _PyErr_FormatFromCause(
                PyExc_TypeError,
                "'async for' received an invalid object "
                "from __anext__: %.100s",
                Py_TYPE(next_iter)->tp_name);

            Py_DECREF(next_iter);
            GOTO_ERROR();
        } else {
            Py_DECREF(next_iter);
        }
    }

    PUSH(awaitable);
    PREDICT(LOAD_CONST);
    DISPATCH();
}
JIT_TARGET_EXPORT(GET_ANEXT)

TARGET_FUNC(GET_AWAITABLE) {
    PyObject *iterable = TOP();
    PyObject *iter = _PyCoro_GetAwaitableIter(iterable);

    Py_DECREF(iterable);

    if (iter != NULL && PyCoro_CheckExact(iter)) {
        PyObject *yf = _PyGen_yf((PyGenObject*)iter);
        if (yf != NULL) {
            /* `iter` is a coroutine object that is being
               awaited, `yf` is a pointer to the current awaitable
               being awaited on. */
            Py_DECREF(yf);
            Py_CLEAR(iter);
            PyErr_SetString(
                PyExc_RuntimeError,
                "coroutine is being awaited already");
            /* The code below jumps to `error` if `iter` is NULL. */
        }
    }

    SET_TOP(iter); /* Even if it's NULL */

    if (iter == NULL) {
        GOTO_ERROR();
    }

    PREDICT(LOAD_CONST);
    DISPATCH();
}
JIT_TARGET_EXPORT(GET_AWAITABLE)

TARGET_FUNC(YIELD_FROM) {
    PyObject *v = POP();
    PyObject *receiver = TOP();
    int err;
    if (PyGen_CheckExact(receiver) || PyCoro_CheckExact(receiver)) {
        ctx->retval = _PyGen_Send((PyGenObject *)receiver, v);
    } else {
        _Py_IDENTIFIER(send);
        if (v == Py_None)
            ctx->retval = Py_TYPE(receiver)->tp_iternext(receiver);
        else
            ctx->retval = _PyObject_CallMethodIdObjArgs(receiver, &PyId_send, v, NULL);
    }
    Py_DECREF(v);
    if (ctx->retval == NULL) {
        PyObject *val;
        if (ctx->tstate->c_tracefunc != NULL
                && PyErr_ExceptionMatches(PyExc_StopIteration))
            call_exc_trace(ctx->tstate->c_tracefunc, ctx->tstate->c_traceobj, ctx->tstate, f);
        err = _PyGen_FetchStopIterationValue(&val);
        if (err < 0)
            GOTO_ERROR();
        Py_DECREF(receiver);
        SET_TOP(val);
        DISPATCH();
    }
    /* receiver remains on stack, ctx->retval is value to be yielded */
    FRAMEPTR()->f_stacktop = STACKPTR();
    ctx->why = WHY_YIELD;
    /* and repeat... */
    assert(FRAMEPTR()->f_lasti >= (int)sizeof(_Py_CODEUNIT));
    FRAMEPTR()->f_lasti -= sizeof(_Py_CODEUNIT);
    GOTO_FAST_YIELD();
}
JIT_TARGET_EXPORT(YIELD_FROM)

TARGET_FUNC(YIELD_VALUE) {
    ctx->retval = POP();

    if (ctx->co->co_flags & CO_ASYNC_GENERATOR) {
        PyObject *w = _PyAsyncGenValueWrapperNew(ctx->retval);
        Py_DECREF(ctx->retval);
        if (w == NULL) {
            ctx->retval = NULL;
            GOTO_ERROR();
        }
        ctx->retval = w;
    }

    FRAMEPTR()->f_stacktop = STACKPTR();
    ctx->why = WHY_YIELD;
    GOTO_FAST_YIELD();
}
JIT_TARGET_EXPORT(YIELD_VALUE)

TARGET_FUNC(POP_EXCEPT) {
    PyTryBlock *b = PyFrame_BlockPop(f);
    if (b->b_type != EXCEPT_HANDLER) {
        PyErr_SetString(PyExc_SystemError,
                        "popped block is not an except handler");
        GOTO_ERROR();
    }
    UNWIND_EXCEPT_HANDLER(b);
    DISPATCH();
}
JIT_TARGET_EXPORT(POP_EXCEPT)

TARGET_FUNC(POP_BLOCK) {
    PyTryBlock *b = PyFrame_BlockPop(f);
    UNWIND_BLOCK(b);
    DISPATCH();
}
JIT_TARGET_EXPORT(POP_BLOCK)

TARGET_FUNC(END_FINALLY) {
    PyObject *status = POP();
    if (PyLong_Check(status)) {
        ctx->why = (enum why_code) PyLong_AS_LONG(status);
        assert(ctx->why != WHY_YIELD && ctx->why != WHY_EXCEPTION);
        if (ctx->why == WHY_RETURN ||
            ctx->why == WHY_CONTINUE)
            ctx->retval = POP();
        if (ctx->why == WHY_SILENCED) {
            /* An exception was silenced by 'with', we must
            manually unwind the EXCEPT_HANDLER block which was
            created when the exception was caught, otherwise
            the stack will be in an inconsistent state. */
            PyTryBlock *b = PyFrame_BlockPop(f);
            assert(b->b_type == EXCEPT_HANDLER);
            UNWIND_EXCEPT_HANDLER(b);
            ctx->why = WHY_NOT;
            Py_DECREF(status);
            DISPATCH();
        }
        Py_DECREF(status);
        GOTO_FAST_BLOCK_END();
    }
    else if (PyExceptionClass_Check(status)) {
        PyObject *exc = POP();
        PyObject *tb = POP();
        PyErr_Restore(status, exc, tb);
        ctx->why = WHY_EXCEPTION;
        GOTO_FAST_BLOCK_END();
    }
    else if (status != Py_None) {
        PyErr_SetString(PyExc_SystemError,
            "'finally' pops bad exception");
        Py_DECREF(status);
        GOTO_ERROR();
    }
    Py_DECREF(status);
    DISPATCH();
}
JIT_TARGET_EXPORT(END_FINALLY)

TARGET_FUNC(LOAD_BUILD_CLASS) {
    _Py_IDENTIFIER(__build_class__);

    PyObject *bc;
    if (PyDict_CheckExact(FRAMEPTR()->f_builtins)) {
        bc = _PyDict_GetItemId(FRAMEPTR()->f_builtins, &PyId___build_class__);
        if (bc == NULL) {
            PyErr_SetString(PyExc_NameError,
                            "__build_class__ not found");
            GOTO_ERROR();
        }
        Py_INCREF(bc);
    }
    else {
        PyObject *build_class_str = _PyUnicode_FromId(&PyId___build_class__);
        if (build_class_str == NULL)
            GOTO_ERROR();
        bc = PyObject_GetItem(FRAMEPTR()->f_builtins, build_class_str);
        if (bc == NULL) {
            if (PyErr_ExceptionMatches(PyExc_KeyError))
                PyErr_SetString(PyExc_NameError,
                                "__build_class__ not found");
            GOTO_ERROR();
        }
    }
    PUSH(bc);
    DISPATCH();
}
JIT_TARGET_EXPORT(LOAD_BUILD_CLASS)

TARGET_FUNC(STORE_NAME) {
    PyObject *name = GETITEM(ctx->names, oparg);
    PyObject *v = POP();
    PyObject *ns = FRAMEPTR()->f_locals;
    int err;
    if (ns == NULL) {
        PyErr_Format(PyExc_SystemError,
                     "no locals found when storing %R", name);
        Py_DECREF(v);
        GOTO_ERROR();
    }
    if (PyDict_CheckExact(ns))
        err = PyDict_SetItem(ns, name, v);
    else
        err = PyObject_SetItem(ns, name, v);
    Py_DECREF(v);
    if (err != 0)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(STORE_NAME)

TARGET_FUNC(DELETE_NAME) {
    PyObject *name = GETITEM(ctx->names, oparg);
    PyObject *ns = FRAMEPTR()->f_locals;
    int err;
    if (ns == NULL) {
        PyErr_Format(PyExc_SystemError,
                     "no locals when deleting %R", name);
        GOTO_ERROR();
    }
    err = PyObject_DelItem(ns, name);
    if (err != 0) {
        format_exc_check_arg(PyExc_NameError,
                             NAME_ERROR_MSG,
                             name);
        GOTO_ERROR();
    }
    DISPATCH();
}
JIT_TARGET_EXPORT(DELETE_NAME)

TARGET_FUNC(UNPACK_SEQUENCE) {
    PyObject *seq = POP(), *item, **items;
    if (PyTuple_CheckExact(seq) &&
        PyTuple_GET_SIZE(seq) == oparg) {
        items = ((PyTupleObject *)seq)->ob_item;
        while (oparg--) {
            item = items[oparg];
            Py_INCREF(item);
            PUSH(item);
        }
    } else if (PyList_CheckExact(seq) &&
               PyList_GET_SIZE(seq) == oparg) {
        items = ((PyListObject *)seq)->ob_item;
        while (oparg--) {
            item = items[oparg];
            Py_INCREF(item);
            PUSH(item);
        }
    } else if (unpack_iterable(seq, oparg, -1,
                               STACKPTR() + oparg)) {
        STACKADJ(oparg);
    } else {
        /* unpack_iterable() raised an exception */
        Py_DECREF(seq);
        GOTO_ERROR();
    }
    Py_DECREF(seq);
    DISPATCH();
}
JIT_TARGET_EXPORT(UNPACK_SEQUENCE)

TARGET_FUNC(UNPACK_EX) {
    int totalargs = 1 + (oparg & 0xFF) + (oparg >> 8);
    PyObject *seq = POP();

    if (unpack_iterable(seq, oparg & 0xFF, oparg >> 8,
                        STACKPTR() + totalargs)) {
        STACKPTR() += totalargs;
    } else {
        Py_DECREF(seq);
        GOTO_ERROR();
    }
    Py_DECREF(seq);
    DISPATCH();
}
JIT_TARGET_EXPORT(UNPACK_EX)

TARGET_FUNC(STORE_ATTR) {
    PyObject *name = GETITEM(ctx->names, oparg);
    PyObject *owner = TOP();
    PyObject *v = SECOND();
    int err;
    STACKADJ(-2);
    err = PyObject_SetAttr(owner, name, v);
    Py_DECREF(v);
    Py_DECREF(owner);
    if (err != 0)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(STORE_ATTR)

TARGET_FUNC(DELETE_ATTR) {
    PyObject *name = GETITEM(ctx->names, oparg);
    PyObject *owner = POP();
    int err;
    err = PyObject_SetAttr(owner, name, (PyObject *)NULL);
    Py_DECREF(owner);
    if (err != 0)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(DELETE_ATTR)

TARGET_FUNC(STORE_GLOBAL) {
    PyObject *name = GETITEM(ctx->names, oparg);
    PyObject *v = POP();
    int err;
    err = PyDict_SetItem(FRAMEPTR()->f_globals, name, v);
    Py_DECREF(v);
    if (err != 0)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(STORE_GLOBAL)

TARGET_FUNC(DELETE_GLOBAL) {
    PyObject *name = GETITEM(ctx->names, oparg);
    int err;
    err = PyDict_DelItem(FRAMEPTR()->f_globals, name);
    if (err != 0) {
        format_exc_check_arg(
            PyExc_NameError, NAME_ERROR_MSG, name);
        GOTO_ERROR();
    }
    DISPATCH();
}
JIT_TARGET_EXPORT(DELETE_GLOBAL)

TARGET_FUNC(LOAD_NAME) {
    PyObject *name = GETITEM(ctx->names, oparg);
    PyObject *locals = FRAMEPTR()->f_locals;
    PyObject *v;
    if (locals == NULL) {
        PyErr_Format(PyExc_SystemError,
                     "no locals when loading %R", name);
        GOTO_ERROR();
    }
    if (PyDict_CheckExact(locals)) {
        v = PyDict_GetItem(locals, name);
        Py_XINCREF(v);
    }
    else {
        v = PyObject_GetItem(locals, name);
        if (v == NULL) {
            if (!PyErr_ExceptionMatches(PyExc_KeyError))
                GOTO_ERROR();
            PyErr_Clear();
        }
    }
    if (v == NULL) {
        v = PyDict_GetItem(FRAMEPTR()->f_globals, name);
        Py_XINCREF(v);
        if (v == NULL) {
            if (PyDict_CheckExact(FRAMEPTR()->f_builtins)) {
                v = PyDict_GetItem(FRAMEPTR()->f_builtins, name);
                if (v == NULL) {
                    format_exc_check_arg(
                                PyExc_NameError,
                                NAME_ERROR_MSG, name);
                    GOTO_ERROR();
                }
                Py_INCREF(v);
            }
            else {
                v = PyObject_GetItem(FRAMEPTR()->f_builtins, name);
                if (v == NULL) {
                    if (PyErr_ExceptionMatches(PyExc_KeyError))
                        format_exc_check_arg(
                                    PyExc_NameError,
                                    NAME_ERROR_MSG, name);
                    GOTO_ERROR();
                }
            }
        }
    }
    PUSH(v);
    DISPATCH();
}
JIT_TARGET_EXPORT(LOAD_NAME)

TARGET_FUNC(LOAD_GLOBAL) {
    PyObject *name = GETITEM(ctx->names, oparg);
    PyObject *v;
    if (PyDict_CheckExact(FRAMEPTR()->f_globals)
        && PyDict_CheckExact(FRAMEPTR()->f_builtins))
    {
        v = _PyDict_LoadGlobal((PyDictObject *)FRAMEPTR()->f_globals,
                               (PyDictObject *)FRAMEPTR()->f_builtins,
                               name);
        if (v == NULL) {
            if (!_PyErr_OCCURRED()) {
                /* _PyDict_LoadGlobal() returns NULL without raising
                 * an exception if the key doesn't exist */
                format_exc_check_arg(PyExc_NameError,
                                     NAME_ERROR_MSG, name);
            }
            GOTO_ERROR();
        }
        Py_INCREF(v);
    }
    else {
        /* Slow-path if globals or builtins is not a dict */

        /* namespace 1: globals */
        v = PyObject_GetItem(FRAMEPTR()->f_globals, name);
        if (v == NULL) {
            if (!PyErr_ExceptionMatches(PyExc_KeyError))
                GOTO_ERROR();
            PyErr_Clear();

            /* namespace 2: builtins */
            v = PyObject_GetItem(FRAMEPTR()->f_builtins, name);
            if (v == NULL) {
                if (PyErr_ExceptionMatches(PyExc_KeyError))
                    format_exc_check_arg(
                                PyExc_NameError,
                                NAME_ERROR_MSG, name);
                GOTO_ERROR();
            }
        }
    }
    PUSH(v);
    DISPATCH();
}
JIT_TARGET_EXPORT(LOAD_GLOBAL)

TARGET_FUNC(DELETE_FAST) {
    PyObject *v = GETLOCAL(oparg);
    if (v != NULL) {
        SETLOCAL(oparg, NULL);
        DISPATCH();
    }
    format_exc_check_arg(
        PyExc_UnboundLocalError,
        UNBOUNDLOCAL_ERROR_MSG,
        PyTuple_GetItem(ctx->co->co_varnames, oparg)
        );
    GOTO_ERROR();
}
JIT_TARGET_EXPORT(DELETE_FAST)

TARGET_FUNC(DELETE_DEREF) {
    PyObject *cell = ctx->freevars[oparg];
    PyObject *oldobj = PyCell_GET(cell);
    if (oldobj != NULL) {
        PyCell_SET(cell, NULL);
        Py_DECREF(oldobj);
        DISPATCH();
    }
    format_exc_unbound(ctx->co, oparg);
    GOTO_ERROR();
}
JIT_TARGET_EXPORT(DELETE_DEREF)

TARGET_FUNC(LOAD_CLOSURE) {
    PyObject *cell = ctx->freevars[oparg];
    Py_INCREF(cell);
    PUSH(cell);
    DISPATCH();
}
JIT_TARGET_EXPORT(LOAD_CLOSURE)

TARGET_FUNC(LOAD_CLASSDEREF) {
    PyObject *name, *value, *locals = FRAMEPTR()->f_locals;
    Py_ssize_t idx;
    assert(locals);
    assert(oparg >= PyTuple_GET_SIZE(ctx->co->co_cellvars));
    idx = oparg - PyTuple_GET_SIZE(ctx->co->co_cellvars);
    assert(idx >= 0 && idx < PyTuple_GET_SIZE(ctx->co->co_freevars));
    name = PyTuple_GET_ITEM(ctx->co->co_freevars, idx);
    if (PyDict_CheckExact(locals)) {
        value = PyDict_GetItem(locals, name);
        Py_XINCREF(value);
    }
    else {
        value = PyObject_GetItem(locals, name);
        if (value == NULL) {
            if (!PyErr_ExceptionMatches(PyExc_KeyError))
                GOTO_ERROR();
            PyErr_Clear();
        }
    }
    if (!value) {
        PyObject *cell = ctx->freevars[oparg];
        value = PyCell_GET(cell);
        if (value == NULL) {
            format_exc_unbound(ctx->co, oparg);
            GOTO_ERROR();
        }
        Py_INCREF(value);
    }
    PUSH(value);
    DISPATCH();
}
JIT_TARGET_EXPORT(LOAD_CLASSDEREF)

TARGET_FUNC(LOAD_DEREF) {
    PyObject *cell = ctx->freevars[oparg];
    PyObject *value = PyCell_GET(cell);
    if (value == NULL) {
        format_exc_unbound(ctx->co, oparg);
        GOTO_ERROR();
    }
    Py_INCREF(value);
    PUSH(value);
    DISPATCH();
}
JIT_TARGET_EXPORT(LOAD_DEREF)

TARGET_FUNC(STORE_DEREF) {
    PyObject *v = POP();
    PyObject *cell = ctx->freevars[oparg];
    PyObject *oldobj = PyCell_GET(cell);
    PyCell_SET(cell, v);
    Py_XDECREF(oldobj);
    DISPATCH();
}
JIT_TARGET_EXPORT(STORE_DEREF)

TARGET_FUNC(BUILD_STRING) {
    PyObject *str;
    PyObject *empty = PyUnicode_New(0, 0);
    if (empty == NULL) {
        GOTO_ERROR();
    }
    str = _PyUnicode_JoinArray(empty, STACKPTR() - oparg, oparg);
    Py_DECREF(empty);
    if (str == NULL)
        GOTO_ERROR();
    while (--oparg >= 0) {
        PyObject *item = POP();
        Py_DECREF(item);
    }
    PUSH(str);
    DISPATCH();
}
JIT_TARGET_EXPORT(BUILD_STRING)

TARGET_FUNC(BUILD_TUPLE) {
    PyObject *tup = PyTuple_New(oparg);
    if (tup == NULL)
        GOTO_ERROR();
    while (--oparg >= 0) {
        PyObject *item = POP();
        PyTuple_SET_ITEM(tup, oparg, item);
    }
    PUSH(tup);
    DISPATCH();
}
JIT_TARGET_EXPORT(BUILD_TUPLE)

TARGET_FUNC(BUILD_LIST) {
    PyObject *list =  PyList_New(oparg);
    if (list == NULL)
        GOTO_ERROR();
    while (--oparg >= 0) {
        PyObject *item = POP();
        PyList_SET_ITEM(list, oparg, item);
    }
    PUSH(list);
    DISPATCH();
}
JIT_TARGET_EXPORT(BUILD_LIST)

TARGET_FUNC(BUILD_TUPLE_UNPACK_WITH_CALL) {
    int convert_to_tuple = opcode != BUILD_LIST_UNPACK;
    Py_ssize_t i;
    PyObject *sum = PyList_New(0);
    PyObject *return_value;

    if (sum == NULL)
        GOTO_ERROR();

    for (i = oparg; i > 0; i--) {
        PyObject *none_val;

        none_val = _PyList_Extend((PyListObject *)sum, PEEK(i));
        if (none_val == NULL) {
            if (opcode == BUILD_TUPLE_UNPACK_WITH_CALL &&
                PyErr_ExceptionMatches(PyExc_TypeError))
            {
                check_args_iterable(PEEK(1 + oparg), PEEK(i));
            }
            Py_DECREF(sum);
            GOTO_ERROR();
        }
        Py_DECREF(none_val);
    }

    if (convert_to_tuple) {
        return_value = PyList_AsTuple(sum);
        Py_DECREF(sum);
        if (return_value == NULL)
            GOTO_ERROR();
    }
    else {
        return_value = sum;
    }

    while (oparg--)
        Py_DECREF(POP());
    PUSH(return_value);
    DISPATCH();
}
JIT_TARGET_EXPORT(BUILD_TUPLE_UNPACK_WITH_CALL)

TARGET_FUNC(BUILD_TUPLE_UNPACK) {
    int convert_to_tuple = opcode != BUILD_LIST_UNPACK;
    Py_ssize_t i;
    PyObject *sum = PyList_New(0);
    PyObject *return_value;

    if (sum == NULL)
        GOTO_ERROR();

    for (i = oparg; i > 0; i--) {
        PyObject *none_val;

        none_val = _PyList_Extend((PyListObject *)sum, PEEK(i));
        if (none_val == NULL) {
            if (opcode == BUILD_TUPLE_UNPACK_WITH_CALL &&
                PyErr_ExceptionMatches(PyExc_TypeError))
            {
                check_args_iterable(PEEK(1 + oparg), PEEK(i));
            }
            Py_DECREF(sum);
            GOTO_ERROR();
        }
        Py_DECREF(none_val);
    }

    if (convert_to_tuple) {
        return_value = PyList_AsTuple(sum);
        Py_DECREF(sum);
        if (return_value == NULL)
            GOTO_ERROR();
    }
    else {
        return_value = sum;
    }

    while (oparg--)
        Py_DECREF(POP());
    PUSH(return_value);
    DISPATCH();
}
JIT_TARGET_EXPORT(BUILD_TUPLE_UNPACK)

TARGET_FUNC(BUILD_LIST_UNPACK) {
    int convert_to_tuple = opcode != BUILD_LIST_UNPACK;
    Py_ssize_t i;
    PyObject *sum = PyList_New(0);
    PyObject *return_value;

    if (sum == NULL)
        GOTO_ERROR();

    for (i = oparg; i > 0; i--) {
        PyObject *none_val;

        none_val = _PyList_Extend((PyListObject *)sum, PEEK(i));
        if (none_val == NULL) {
            if (opcode == BUILD_TUPLE_UNPACK_WITH_CALL &&
                PyErr_ExceptionMatches(PyExc_TypeError))
            {
                check_args_iterable(PEEK(1 + oparg), PEEK(i));
            }
            Py_DECREF(sum);
            GOTO_ERROR();
        }
        Py_DECREF(none_val);
    }

    if (convert_to_tuple) {
        return_value = PyList_AsTuple(sum);
        Py_DECREF(sum);
        if (return_value == NULL)
            GOTO_ERROR();
    }
    else {
        return_value = sum;
    }

    while (oparg--)
        Py_DECREF(POP());
    PUSH(return_value);
    DISPATCH();
}
JIT_TARGET_EXPORT(BUILD_LIST_UNPACK)

TARGET_FUNC(BUILD_SET) {
    PyObject *set = PySet_New(NULL);
    int err = 0;
    int i;
    if (set == NULL)
        GOTO_ERROR();
    for (i = oparg; i > 0; i--) {
        PyObject *item = PEEK(i);
        if (err == 0)
            err = PySet_Add(set, item);
        Py_DECREF(item);
    }
    STACKADJ(-oparg);
    if (err != 0) {
        Py_DECREF(set);
        GOTO_ERROR();
    }
    PUSH(set);
    DISPATCH();
}
JIT_TARGET_EXPORT(BUILD_SET)

TARGET_FUNC(BUILD_SET_UNPACK) {
    Py_ssize_t i;
    PyObject *sum = PySet_New(NULL);
    if (sum == NULL)
        GOTO_ERROR();

    for (i = oparg; i > 0; i--) {
        if (_PySet_Update(sum, PEEK(i)) < 0) {
            Py_DECREF(sum);
            GOTO_ERROR();
        }
    }

    while (oparg--)
        Py_DECREF(POP());
    PUSH(sum);
    DISPATCH();
}
JIT_TARGET_EXPORT(BUILD_SET_UNPACK)

TARGET_FUNC(BUILD_MAP) {
    Py_ssize_t i;
    PyObject *map = _PyDict_NewPresized((Py_ssize_t)oparg);
    if (map == NULL)
        GOTO_ERROR();
    for (i = oparg; i > 0; i--) {
        int err;
        PyObject *key = PEEK(2*i);
        PyObject *value = PEEK(2*i - 1);
        err = PyDict_SetItem(map, key, value);
        if (err != 0) {
            Py_DECREF(map);
            GOTO_ERROR();
        }
    }

    while (oparg--) {
        Py_DECREF(POP());
        Py_DECREF(POP());
    }
    PUSH(map);
    DISPATCH();
}
JIT_TARGET_EXPORT(BUILD_MAP)

TARGET_FUNC(SETUP_ANNOTATIONS) {
    _Py_IDENTIFIER(__annotations__);
    int err;
    PyObject *ann_dict;
    if (FRAMEPTR()->f_locals == NULL) {
        PyErr_Format(PyExc_SystemError,
                     "no locals found when setting up annotations");
        GOTO_ERROR();
    }
    /* check if __annotations__ in locals()... */
    if (PyDict_CheckExact(FRAMEPTR()->f_locals)) {
        ann_dict = _PyDict_GetItemId(FRAMEPTR()->f_locals,
                                     &PyId___annotations__);
        if (ann_dict == NULL) {
            /* ...if not, create a new one */
            ann_dict = PyDict_New();
            if (ann_dict == NULL) {
                GOTO_ERROR();
            }
            err = _PyDict_SetItemId(FRAMEPTR()->f_locals,
                                    &PyId___annotations__, ann_dict);
            Py_DECREF(ann_dict);
            if (err != 0) {
                GOTO_ERROR();
            }
        }
    }
    else {
        /* do the same if locals() is not a dict */
        PyObject *ann_str = _PyUnicode_FromId(&PyId___annotations__);
        if (ann_str == NULL) {
            GOTO_ERROR();
        }
        ann_dict = PyObject_GetItem(FRAMEPTR()->f_locals, ann_str);
        if (ann_dict == NULL) {
            if (!PyErr_ExceptionMatches(PyExc_KeyError)) {
                GOTO_ERROR();
            }
            PyErr_Clear();
            ann_dict = PyDict_New();
            if (ann_dict == NULL) {
                GOTO_ERROR();
            }
            err = PyObject_SetItem(FRAMEPTR()->f_locals, ann_str, ann_dict);
            Py_DECREF(ann_dict);
            if (err != 0) {
                GOTO_ERROR();
            }
        }
        else {
            Py_DECREF(ann_dict);
        }
    }
    DISPATCH();
}
JIT_TARGET_EXPORT(SETUP_ANNOTATIONS)

TARGET_FUNC(BUILD_CONST_KEY_MAP) {
    Py_ssize_t i;
    PyObject *map;
    PyObject *keys = TOP();
    if (!PyTuple_CheckExact(keys) ||
        PyTuple_GET_SIZE(keys) != (Py_ssize_t)oparg) {
        PyErr_SetString(PyExc_SystemError,
                        "bad BUILD_CONST_KEY_MAP keys argument");
        GOTO_ERROR();
    }
    map = _PyDict_NewPresized((Py_ssize_t)oparg);
    if (map == NULL) {
        GOTO_ERROR();
    }
    for (i = oparg; i > 0; i--) {
        int err;
        PyObject *key = PyTuple_GET_ITEM(keys, oparg - i);
        PyObject *value = PEEK(i + 1);
        err = PyDict_SetItem(map, key, value);
        if (err != 0) {
            Py_DECREF(map);
            GOTO_ERROR();
        }
    }

    Py_DECREF(POP());
    while (oparg--) {
        Py_DECREF(POP());
    }
    PUSH(map);
    DISPATCH();
}
JIT_TARGET_EXPORT(BUILD_CONST_KEY_MAP)

TARGET_FUNC(BUILD_MAP_UNPACK) {
    Py_ssize_t i;
    PyObject *sum = PyDict_New();
    if (sum == NULL)
        GOTO_ERROR();

    for (i = oparg; i > 0; i--) {
        PyObject *arg = PEEK(i);
        if (PyDict_Update(sum, arg) < 0) {
            if (PyErr_ExceptionMatches(PyExc_AttributeError)) {
                PyErr_Format(PyExc_TypeError,
                        "'%.200s' object is not a mapping",
                        arg->ob_type->tp_name);
            }
            Py_DECREF(sum);
            GOTO_ERROR();
        }
    }

    while (oparg--)
        Py_DECREF(POP());
    PUSH(sum);
    DISPATCH();
}
JIT_TARGET_EXPORT(BUILD_MAP_UNPACK)

TARGET_FUNC(BUILD_MAP_UNPACK_WITH_CALL) {
    Py_ssize_t i;
    PyObject *sum = PyDict_New();
    if (sum == NULL)
        GOTO_ERROR();

    for (i = oparg; i > 0; i--) {
        PyObject *arg = PEEK(i);
        if (_PyDict_MergeEx(sum, arg, 2) < 0) {
            PyObject *func = PEEK(2 + oparg);
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
            Py_DECREF(sum);
            GOTO_ERROR();
        }
    }

    while (oparg--)
        Py_DECREF(POP());
    PUSH(sum);
    DISPATCH();
}
JIT_TARGET_EXPORT(BUILD_MAP_UNPACK_WITH_CALL)

TARGET_FUNC(MAP_ADD) {
    PyObject *key = TOP();
    PyObject *value = SECOND();
    PyObject *map;
    int err;
    STACKADJ(-2);
    map = PEEK(oparg);                      /* dict */
    assert(PyDict_CheckExact(map));
    err = PyDict_SetItem(map, key, value);  /* map[key] = value */
    Py_DECREF(value);
    Py_DECREF(key);
    if (err != 0)
        GOTO_ERROR();
    PREDICT(JUMP_ABSOLUTE);
    DISPATCH();
}
JIT_TARGET_EXPORT(MAP_ADD)

TARGET_FUNC(LOAD_ATTR) {
    PyObject *name = GETITEM(ctx->names, oparg);
    PyObject *owner = TOP();
    PyObject *res = PyObject_GetAttr(owner, name);
    Py_DECREF(owner);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(LOAD_ATTR)

TARGET_FUNC(COMPARE_OP) {
    PyObject *right = POP();
    PyObject *left = TOP();
    PyObject *res = cmp_outcome(oparg, left, right);
    Py_DECREF(left);
    Py_DECREF(right);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    PREDICT(POP_JUMP_IF_FALSE);
    PREDICT(POP_JUMP_IF_TRUE);
    DISPATCH();
}
JIT_TARGET_EXPORT(COMPARE_OP)

TARGET_FUNC(IMPORT_NAME) {
    PyObject *name = GETITEM(ctx->names, oparg);
    PyObject *fromlist = POP();
    PyObject *level = TOP();
    PyObject *res;
    res = import_name(FRAMEPTR(), name, fromlist, level);
    Py_DECREF(level);
    Py_DECREF(fromlist);
    SET_TOP(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(IMPORT_NAME)

TARGET_FUNC(IMPORT_STAR) {
    PyObject *from = POP(), *locals;
    int err;
    if (PyFrame_FastToLocalsWithError(f) < 0) {
        Py_DECREF(from);
        GOTO_ERROR();
    }

    locals = FRAMEPTR()->f_locals;
    if (locals == NULL) {
        PyErr_SetString(PyExc_SystemError,
            "no locals found during 'import *'");
        Py_DECREF(from);
        GOTO_ERROR();
    }
    err = import_all_from(locals, from);
    PyFrame_LocalsToFast(FRAMEPTR(), 0);
    Py_DECREF(from);
    if (err != 0)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(IMPORT_STAR)

TARGET_FUNC(IMPORT_FROM) {
    PyObject *name = GETITEM(ctx->names, oparg);
    PyObject *from = TOP();
    PyObject *res;
    res = import_from(from, name);
    PUSH(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(IMPORT_FROM)

TARGET_FUNC(JUMP_FORWARD) {
    JUMPBY(oparg);
    FAST_DISPATCH();
}
JIT_TARGET_EXPORT(JUMP_FORWARD)

TARGET_FUNC(POP_JUMP_IF_FALSE) {
    PyObject *cond = POP();
    int err;
    if (cond == Py_True) {
        Py_DECREF(cond);
        FAST_DISPATCH();
    }
    if (cond == Py_False) {
        Py_DECREF(cond);
        JUMPTO(oparg);
        FAST_DISPATCH();
    }
    err = PyObject_IsTrue(cond);
    Py_DECREF(cond);
    if (err > 0)
        ;
    else if (err == 0)
        JUMPTO(oparg);
    else
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(POP_JUMP_IF_FALSE)

TARGET_FUNC(POP_JUMP_IF_TRUE) {
    PyObject *cond = POP();
    int err;
    if (cond == Py_False) {
        Py_DECREF(cond);
        FAST_DISPATCH();
    }
    if (cond == Py_True) {
        Py_DECREF(cond);
        JUMPTO(oparg);
        FAST_DISPATCH();
    }
    err = PyObject_IsTrue(cond);
    Py_DECREF(cond);
    if (err > 0) {
        JUMPTO(oparg);
    }
    else if (err == 0)
        ;
    else
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(POP_JUMP_IF_TRUE)

TARGET_FUNC(JUMP_IF_FALSE_OR_POP) {
    PyObject *cond = TOP();
    int err;
    if (cond == Py_True) {
        STACKADJ(-1);
        Py_DECREF(cond);
        FAST_DISPATCH();
    }
    if (cond == Py_False) {
        JUMPTO(oparg);
        FAST_DISPATCH();
    }
    err = PyObject_IsTrue(cond);
    if (err > 0) {
        STACKADJ(-1);
        Py_DECREF(cond);
    }
    else if (err == 0)
        JUMPTO(oparg);
    else
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(JUMP_IF_FALSE_OR_POP)

TARGET_FUNC(JUMP_IF_TRUE_OR_POP) {
    PyObject *cond = TOP();
    int err;
    if (cond == Py_False) {
        STACKADJ(-1);
        Py_DECREF(cond);
        FAST_DISPATCH();
    }
    if (cond == Py_True) {
        JUMPTO(oparg);
        FAST_DISPATCH();
    }
    err = PyObject_IsTrue(cond);
    if (err > 0) {
        JUMPTO(oparg);
    }
    else if (err == 0) {
        STACKADJ(-1);
        Py_DECREF(cond);
    }
    else
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(JUMP_IF_TRUE_OR_POP)

TARGET_FUNC(JUMP_ABSOLUTE) {
    JUMPTO(oparg);
#if FAST_LOOPS
    /* Enabling this path speeds-up all while and for-loops by bypassing
       the per-loop checks for signals.  By default, this should be turned-off
       because it prevents detection of a control-break in tight loops like
       "while 1: pass".  Compile with this option turned-on when you need
       the speed-up and do not need break checking inside tight loops (ones
       that contain only instructions ending with FAST_DISPATCH).
    */
    FAST_DISPATCH();
#else
    DISPATCH();
#endif
}
JIT_TARGET_EXPORT(JUMP_ABSOLUTE)

TARGET_FUNC(GET_ITER) {
    /* before: [obj]; after [getiter(obj)] */
    PyObject *iterable = TOP();
    PyObject *iter = PyObject_GetIter(iterable);
    Py_DECREF(iterable);
    SET_TOP(iter);
    if (iter == NULL)
        GOTO_ERROR();
    PREDICT(FOR_ITER);
    PREDICT(CALL_FUNCTION);
    DISPATCH();
}
JIT_TARGET_EXPORT(GET_ITER)

TARGET_FUNC(GET_YIELD_FROM_ITER) {
    /* before: [obj]; after [getiter(obj)] */
    PyObject *iterable = TOP();
    PyObject *iter;
    if (PyCoro_CheckExact(iterable)) {
        /* `iterable` is a coroutine */
        if (!(ctx->co->co_flags & (CO_COROUTINE | CO_ITERABLE_COROUTINE))) {
            /* and it is used in a 'yield from' expression of a
               regular generator. */
            Py_DECREF(iterable);
            SET_TOP(NULL);
            PyErr_SetString(PyExc_TypeError,
                            "cannot 'yield from' a coroutine object "
                            "in a non-coroutine generator");
            GOTO_ERROR();
        }
    }
    else if (!PyGen_CheckExact(iterable)) {
        /* `iterable` is not a generator. */
        iter = PyObject_GetIter(iterable);
        Py_DECREF(iterable);
        SET_TOP(iter);
        if (iter == NULL)
            GOTO_ERROR();
    }
    PREDICT(LOAD_CONST);
    DISPATCH();
}
JIT_TARGET_EXPORT(GET_YIELD_FROM_ITER)

TARGET_FUNC(FOR_ITER) {
    /* before: [iter]; after: [iter, iter()] *or* [] */
    PyObject *iter = TOP();
    PyObject *next = (*iter->ob_type->tp_iternext)(iter);
    if (next != NULL) {
        PUSH(next);
        PREDICT(STORE_FAST);
        PREDICT(UNPACK_SEQUENCE);
        DISPATCH();
    }
    if (PyErr_Occurred()) {
        if (!PyErr_ExceptionMatches(PyExc_StopIteration))
            GOTO_ERROR();
        else if (ctx->tstate->c_tracefunc != NULL)
            call_exc_trace(ctx->tstate->c_tracefunc, ctx->tstate->c_traceobj, ctx->tstate, f);
        PyErr_Clear();
    }
    /* iterator ended normally */
    STACKADJ(-1);
    Py_DECREF(iter);
    JUMPBY(oparg);
    PREDICT(POP_BLOCK);
    DISPATCH();
}
JIT_TARGET_EXPORT(FOR_ITER)

TARGET_FUNC(BREAK_LOOP) {
    ctx->why = WHY_BREAK;
    GOTO_FAST_BLOCK_END();
}
JIT_TARGET_EXPORT(BREAK_LOOP)

TARGET_FUNC(CONTINUE_LOOP) {
    ctx->retval = PyLong_FromLong(oparg);
    if (ctx->retval == NULL)
        GOTO_ERROR();
    ctx->why = WHY_CONTINUE;
    GOTO_FAST_BLOCK_END();
}
JIT_TARGET_EXPORT(CONTINUE_LOOP)

TARGET_FUNC(SETUP_LOOP) {
    /* NOTE: If you add any new block-setup opcodes that
       are not try/except/finally handlers, you may need
       to update the PyGen_NeedsFinalizing() function.
       */

    PyFrame_BlockSetup(FRAMEPTR(), opcode, INSTR_OFFSET() + oparg,
                       STACK_LEVEL());
    DISPATCH();
}
JIT_TARGET_EXPORT(SETUP_LOOP)

TARGET_FUNC(SETUP_EXCEPT) {
    /* NOTE: If you add any new block-setup opcodes that
       are not try/except/finally handlers, you may need
       to update the PyGen_NeedsFinalizing() function.
       */

    PyFrame_BlockSetup(FRAMEPTR(), opcode, INSTR_OFFSET() + oparg,
                       STACK_LEVEL());
    DISPATCH();
}
JIT_TARGET_EXPORT(SETUP_EXCEPT)

TARGET_FUNC(SETUP_FINALLY) {
    /* NOTE: If you add any new block-setup opcodes that
       are not try/except/finally handlers, you may need
       to update the PyGen_NeedsFinalizing() function.
       */

    PyFrame_BlockSetup(FRAMEPTR(), opcode, INSTR_OFFSET() + oparg,
                       STACK_LEVEL());
    DISPATCH();
}
JIT_TARGET_EXPORT(SETUP_FINALLY)

TARGET_FUNC(BEFORE_ASYNC_WITH) {
    _Py_IDENTIFIER(__aexit__);
    _Py_IDENTIFIER(__aenter__);

    PyObject *mgr = TOP();
    PyObject *exit = special_lookup(mgr, &PyId___aexit__),
             *enter;
    PyObject *res;
    if (exit == NULL)
        GOTO_ERROR();
    SET_TOP(exit);
    enter = special_lookup(mgr, &PyId___aenter__);
    Py_DECREF(mgr);
    if (enter == NULL)
        GOTO_ERROR();
    res = _PyObject_CallNoArg(enter);
    Py_DECREF(enter);
    if (res == NULL)
        GOTO_ERROR();
    PUSH(res);
    PREDICT(GET_AWAITABLE);
    DISPATCH();
}
JIT_TARGET_EXPORT(BEFORE_ASYNC_WITH)

TARGET_FUNC(SETUP_ASYNC_WITH) {
    PyObject *res = POP();
    /* Setup the finally block before pushing the result
       of __aenter__ on the stack. */
    PyFrame_BlockSetup(FRAMEPTR(), SETUP_FINALLY, INSTR_OFFSET() + oparg,
                       STACK_LEVEL());
    PUSH(res);
    DISPATCH();
}
JIT_TARGET_EXPORT(SETUP_ASYNC_WITH)

TARGET_FUNC(SETUP_WITH) {
    _Py_IDENTIFIER(__exit__);
    _Py_IDENTIFIER(__enter__);
    PyObject *mgr = TOP();
    PyObject *enter = special_lookup(mgr, &PyId___enter__), *exit;
    PyObject *res;
    if (enter == NULL)
        GOTO_ERROR();
    exit = special_lookup(mgr, &PyId___exit__);
    if (exit == NULL) {
        Py_DECREF(enter);
        GOTO_ERROR();
    }
    SET_TOP(exit);
    Py_DECREF(mgr);
    res = _PyObject_CallNoArg(enter);
    Py_DECREF(enter);
    if (res == NULL)
        GOTO_ERROR();
    /* Setup the finally block before pushing the result
       of __enter__ on the stack. */
    PyFrame_BlockSetup(FRAMEPTR(), SETUP_FINALLY, INSTR_OFFSET() + oparg,
                       STACK_LEVEL());

    PUSH(res);
    DISPATCH();
}
JIT_TARGET_EXPORT(SETUP_WITH)

TARGET_FUNC(WITH_CLEANUP_START) {
    /* At the top of the stack are 1-6 values indicating
       how/ctx->why we entered the finally clause:
       - TOP = None
       - (TOP, SECOND) = (WHY_{RETURN,CONTINUE}), ctx->retval
       - TOP = WHY_*; no ctx->retval below it
       - (TOP, SECOND, THIRD) = exc_info()
         (FOURTH, FITH, SIXTH) = previous exception for EXCEPT_HANDLER
       Below them is EXIT, the context.__exit__ bound method.
       In the last case, we must call
         EXIT(TOP, SECOND, THIRD)
       otherwise we must call
         EXIT(None, None, None)

       In the first three cases, we remove EXIT from the
       stack, leaving the rest in the same order.  In the
       fourth case, we shift the bottom 3 values of the
       stack down, and replace the empty spot with NULL.

       In addition, if the stack represents an exception,
       *and* the function call returns a 'true' value, we
       push WHY_SILENCED onto the stack.  END_FINALLY will
       then not re-raise the exception.  (But non-local
       gotos should still be resumed.)
    */

    PyObject* stack[3];
    PyObject *exit_func;
    PyObject *exc, *val, *tb, *res;

    val = tb = Py_None;
    exc = TOP();
    if (exc == Py_None) {
        (void)POP();
        exit_func = TOP();
        SET_TOP(exc);
    }
    else if (PyLong_Check(exc)) {
        STACKADJ(-1);
        switch (PyLong_AsLong(exc)) {
        case WHY_RETURN:
        case WHY_CONTINUE:
            /* Retval in TOP. */
            exit_func = SECOND();
            SET_SECOND(TOP());
            SET_TOP(exc);
            break;
        default:
            exit_func = TOP();
            SET_TOP(exc);
            break;
        }
        exc = Py_None;
    }
    else {
        PyObject *tp2, *exc2, *tb2;
        PyTryBlock *block;
        val = SECOND();
        tb = THIRD();
        tp2 = FOURTH();
        exc2 = PEEK(5);
        tb2 = PEEK(6);
        exit_func = PEEK(7);
        SET_VALUE(7, tb2);
        SET_VALUE(6, exc2);
        SET_VALUE(5, tp2);
        /* UNWIND_EXCEPT_HANDLER will pop this off. */
        SET_FOURTH(NULL);
        /* We just shifted the stack down, so we have
           to tell the except handler block that the
           values are lower than it expects. */
        block = &FRAMEPTR()->f_blockstack[FRAMEPTR()->f_iblock - 1];
        assert(block->b_type == EXCEPT_HANDLER);
        block->b_level--;
    }

    stack[0] = exc;
    stack[1] = val;
    stack[2] = tb;
    res = _PyObject_FastCall(exit_func, stack, 3);
    Py_DECREF(exit_func);
    if (res == NULL)
        GOTO_ERROR();

    Py_INCREF(exc); /* Duplicating the exception on the stack */
    PUSH(exc);
    PUSH(res);
    PREDICT(WITH_CLEANUP_FINISH);
    DISPATCH();
}
JIT_TARGET_EXPORT(WITH_CLEANUP_START)

TARGET_FUNC(WITH_CLEANUP_FINISH) {
    PyObject *res = POP();
    PyObject *exc = POP();
    int err;

    if (exc != Py_None)
        err = PyObject_IsTrue(res);
    else
        err = 0;

    Py_DECREF(res);
    Py_DECREF(exc);

    if (err < 0)
        GOTO_ERROR();
    else if (err > 0) {
        /* There was an exception and a True return */
        PUSH(PyLong_FromLong((long) WHY_SILENCED));
    }
    PREDICT(END_FINALLY);
    DISPATCH();
}
JIT_TARGET_EXPORT(WITH_CLEANUP_FINISH)

TARGET_FUNC(LOAD_METHOD) {
    /* Designed to work in tamdem with CALL_METHOD. */
    PyObject *name = GETITEM(ctx->names, oparg);
    PyObject *obj = TOP();
    PyObject *meth = NULL;

    int meth_found = _PyObject_GetMethod(obj, name, &meth);

    if (meth == NULL) {
        /* Most likely attribute wasn't found. */
        GOTO_ERROR();
    }

    if (meth_found) {
        /* We can bypass temporary bound method object.
           meth is unbound method and obj is self.

           meth | self | arg1 | ... | argN
         */
        SET_TOP(meth);
        PUSH(obj);  // self
    }
    else {
        /* meth is not an unbound method (but a regular attr, or
           something was returned by a descriptor protocol).  Set
           the second element of the stack to NULL, to signal
           CALL_METHOD that it's not a method call.

           NULL | meth | arg1 | ... | argN
        */
        SET_TOP(NULL);
        Py_DECREF(obj);
        PUSH(meth);
    }
    DISPATCH();
}
JIT_TARGET_EXPORT(LOAD_METHOD)

TARGET_FUNC(CALL_METHOD) {
    /* Designed to work in tamdem with LOAD_METHOD. */
    PyObject **sp, *res, *meth;

    sp = STACKPTR();

    meth = PEEK(oparg + 2);
    if (meth == NULL) {
        /* `meth` is NULL when LOAD_METHOD thinks that it's not
           a method call.

           Stack layout:

               ... | NULL | callable | arg1 | ... | argN
                                                    ^- TOP()
                                       ^- (-oparg)
                            ^- (-oparg-1)
                     ^- (-oparg-2)

           `callable` will be POPed by call_function.
           NULL will will be POPed manually later.
        */
        res = call_function_extern(&sp, oparg, NULL);
        STACKPTR() = sp;
        (void)POP(); /* POP the NULL. */
    }
    else {
        /* This is a method call.  Stack layout:

             ... | method | self | arg1 | ... | argN
                                                ^- TOP()
                                   ^- (-oparg)
                            ^- (-oparg-1)
                   ^- (-oparg-2)

          `self` and `method` will be POPed by call_function.
          We'll be passing `oparg + 1` to call_function, to
          make it accept the `self` as a first argument.
        */
        res = call_function_extern(&sp, oparg + 1, NULL);
        STACKPTR() = sp;
    }

    PUSH(res);
    if (res == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(CALL_METHOD)

TARGET_FUNC(CALL_FUNCTION) {
    PyObject **sp, *res;
    sp = STACKPTR();
    res = call_function_extern(&sp, oparg, NULL);
    STACKPTR() = sp;
    PUSH(res);
    if (res == NULL) {
        GOTO_ERROR();
    }
    DISPATCH();
}
JIT_TARGET_EXPORT(CALL_FUNCTION)

TARGET_FUNC(CALL_FUNCTION_KW) {
    PyObject **sp, *res, *kwnames;

    kwnames = POP();
    assert(PyTuple_CheckExact(kwnames) && PyTuple_GET_SIZE(kwnames) <= oparg);
    sp = STACKPTR();
    res = call_function_extern(&sp, oparg, kwnames);
    STACKPTR() = sp;
    PUSH(res);
    Py_DECREF(kwnames);

    if (res == NULL) {
        GOTO_ERROR();
    }
    DISPATCH();
}
JIT_TARGET_EXPORT(CALL_FUNCTION_KW)

TARGET_FUNC(CALL_FUNCTION_EX) {
    PyObject *func, *callargs, *kwargs = NULL, *result;
    if (oparg & 0x01) {
        kwargs = POP();
        if (!PyDict_CheckExact(kwargs)) {
            PyObject *d = PyDict_New();
            if (d == NULL)
                GOTO_ERROR();
            if (PyDict_Update(d, kwargs) != 0) {
                Py_DECREF(d);
                /* PyDict_Update raises attribute
                 * error (percolated from an attempt
                 * to get 'keys' attribute) instead of
                 * a type error if its second argument
                 * is not a mapping.
                 */
                if (PyErr_ExceptionMatches(PyExc_AttributeError)) {
                    format_kwargs_mapping_error(SECOND(), kwargs);
                }
                Py_DECREF(kwargs);
                GOTO_ERROR();
            }
            Py_DECREF(kwargs);
            kwargs = d;
        }
        assert(PyDict_CheckExact(kwargs));
    }
    callargs = POP();
    func = TOP();
    if (!PyTuple_CheckExact(callargs)) {
        if (check_args_iterable(func, callargs) < 0) {
            Py_DECREF(callargs);
            GOTO_ERROR();
        }
        Py_SETREF(callargs, PySequence_Tuple(callargs));
        if (callargs == NULL) {
            GOTO_ERROR();
        }
    }
    assert(PyTuple_CheckExact(callargs));

    result = do_call_core(func, callargs, kwargs);
    Py_DECREF(func);
    Py_DECREF(callargs);
    Py_XDECREF(kwargs);

    SET_TOP(result);
    if (result == NULL) {
        GOTO_ERROR();
    }
    DISPATCH();
}
JIT_TARGET_EXPORT(CALL_FUNCTION_EX)

TARGET_FUNC(MAKE_FUNCTION) {
    PyObject *qualname = POP();
    PyObject *codeobj = POP();
    PyFunctionObject *func = (PyFunctionObject *)
        PyFunction_NewWithQualName(codeobj, FRAMEPTR()->f_globals, qualname);

    Py_DECREF(codeobj);
    Py_DECREF(qualname);
    if (func == NULL) {
        GOTO_ERROR();
    }

    if (oparg & 0x08) {
        assert(PyTuple_CheckExact(TOP()));
        func ->func_closure = POP();
    }
    if (oparg & 0x04) {
        assert(PyDict_CheckExact(TOP()));
        func->func_annotations = POP();
    }
    if (oparg & 0x02) {
        assert(PyDict_CheckExact(TOP()));
        func->func_kwdefaults = POP();
    }
    if (oparg & 0x01) {
        assert(PyTuple_CheckExact(TOP()));
        func->func_defaults = POP();
    }

    PUSH((PyObject *)func);
    DISPATCH();
}
JIT_TARGET_EXPORT(MAKE_FUNCTION)

TARGET_FUNC(BUILD_SLICE) {
    PyObject *start, *stop, *step, *slice;
    if (oparg == 3)
        step = POP();
    else
        step = NULL;
    stop = POP();
    start = TOP();
    slice = PySlice_New(start, stop, step);
    Py_DECREF(start);
    Py_DECREF(stop);
    Py_XDECREF(step);
    SET_TOP(slice);
    if (slice == NULL)
        GOTO_ERROR();
    DISPATCH();
}
JIT_TARGET_EXPORT(BUILD_SLICE)

TARGET_FUNC(FORMAT_VALUE) {
    /* Handles ctx->f-string value formatting. */
    PyObject *result;
    PyObject *fmt_spec;
    PyObject *value;
    PyObject *(*conv_fn)(PyObject *);
    int which_conversion = oparg & FVC_MASK;
    int have_fmt_spec = (oparg & FVS_MASK) == FVS_HAVE_SPEC;

    fmt_spec = have_fmt_spec ? POP() : NULL;
    value = POP();

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
        result = conv_fn(value);
        Py_DECREF(value);
        if (result == NULL) {
            Py_XDECREF(fmt_spec);
            GOTO_ERROR();
        }
        value = result;
    }

    /* If value is a unicode object, and there's no fmt_spec,
       then we know the result of format(value) is value
       itself. In that case, skip calling format(). I plan to
       move this optimization in to PyObject_Format()
       itself. */
    if (PyUnicode_CheckExact(value) && fmt_spec == NULL) {
        /* Do nothing, just transfer ownership to result. */
        result = value;
    } else {
        /* Actually call format(). */
        result = PyObject_Format(value, fmt_spec);
        Py_DECREF(value);
        Py_XDECREF(fmt_spec);
        if (result == NULL) {
            GOTO_ERROR();
        }
    }

    PUSH(result);
    DISPATCH();
}
JIT_TARGET_EXPORT(FORMAT_VALUE)

TARGET_FUNC(EXTENDED_ARG) {
    Py_UNREACHABLE(); // Implemented in jit.c
}
JIT_TARGET_EXPORT(EXTENDED_ARG)

