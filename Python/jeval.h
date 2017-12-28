#include "Include/internal/ceval.h"
#include "Include/internal/pystate.h"

#include "Include/jit_macros.h"
#include "Include/jit.h"

/* Special pseudo-instructions */
#define DECLARE_SPECIAL(name)  int _PyEval_FUNC_JIT_TARGET_II_##name (EvalContext *ctx, PyFrameObject *f, int jumpev)

DECLARE_SPECIAL(ERROR);
DECLARE_SPECIAL(FAST_YIELD);
DECLARE_SPECIAL(FAST_BLOCK_END);
DECLARE_SPECIAL(UNWIND_CLEANUP);
DECLARE_SPECIAL(NEXT_OPCODE);

#define HANDLE_RV_INTERNAL(inval) do { \
    SET_VALUE(jd->rv, (inval)); \
    ir_branch_if(jd->func, jd->rv, jd->j_special_internal[0], IR_UNLIKELY); \
} while (0)

#define BRANCH_SPECIAL(name)     BRANCH(jd->j_special[JIT_RC_ ## name])

/* TODO: Deprecate these */
#define BRANCH_SPECIAL_IF(val, name) \
    BRANCH_IF((val), jd->j_special[JIT_RC_ ## name], IR_UNLIKELY)
#define BRANCH_SPECIAL_IF_NOT(val, name) \
    BRANCH_IF_NOT((val), jd->j_special[JIT_RC_ ## name], IR_UNLIKELY)

#define GOTO_ERROR()             BRANCH_SPECIAL(ERROR)
#define GOTO_ERROR_IF(val)       BRANCH_SPECIAL_IF((val), ERROR)
#define GOTO_ERROR_IF_NOT(val)   BRANCH_SPECIAL_IF_NOT((val), ERROR)

#define GOTO_FAST_YIELD()        BRANCH_SPECIAL(FAST_YIELD)
#define GOTO_FAST_BLOCK_END()    BRANCH_SPECIAL(FAST_BLOCK_END)

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

#define STACKPTR()    (jd->stack_pointer)
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

#define POP()          (STACKADJ(-1), PEEK(0))

#define LOAD_VALUE_STACK()  LOAD_FIELD(FRAMEPTR(), PyFrameObject, f_valuestack, ir_type_pyobject_ptr_ptr)

/* Computes: (int)(((uintptr_t)stack_pointer - (uintptr_t)f_valuestack)/sizeof(PyObject*))  */
#define STACK_LEVEL() \
    CAST(ir_type_int, \
      SHIFT_RIGHT( \
        SUBTRACT(CAST(ir_type_uintptr, STACKPTR()), \
                 CAST(ir_type_uintptr, LOAD_VALUE_STACK())), \
        CONSTANT_UINTPTR(3)))

#define GETNAME(i)   PyTuple_GET_ITEM(jd->co->co_names, (i));
#define LOAD_FREEVAR(i) \
    LOAD_AT_INDEX(jd->fastlocals, CONSTANT_INT(jd->co->co_nlocals + (i)))

// TODO: This needs to be adjusted depending on the configuration of _Py_atomic_int
#define LOAD_EVAL_BREAKER() \
    LOAD(CONSTANT_PTR(ir_type_int_ptr, &_PyRuntime.ceval.eval_breaker._value))

/* Check eval breaker, and jump to handler if set. This can only be used
   in instructions with no jumping, since it assumes the next instruction
   is computed using f_lasti.
 */
#define CHECK_EVAL_BREAKER() \
    BRANCH_IF(LOAD_EVAL_BREAKER(), jd->j_special[JIT_RC_NEXT_OPCODE], IR_UNLIKELY)

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

#define INIT_INFO(op) \
    op##_INFO *info = (op##_INFO *) jd->priv[opcode]; \
    int did_alloc_info = 0; \
    if (info == NULL) { \
        info = jd->priv[opcode] = PyMem_RawMalloc(sizeof(op##_INFO)); \
        if (info == NULL) { \
            Py_FatalError("Out of memory"); \
        } \
        did_alloc_info = 1; \
    } \
    if (did_alloc_info)

#define FREE_INFO(opcode) do { \
    if (jd->priv[opcode] != NULL) { \
        PyMem_RawFree(jd->priv[opcode]); \
    } \
} while (0)

#define FETCH_INFO(op) \
    op##_INFO *info = (op##_INFO *) jd->priv[opcode];

/* Functions borrowed from ceval */
int do_raise(PyObject *, PyObject *);

/* Defeat nested macro evaluation problem.
   TODO: Can this be done in a less hacky way?
 */
#define EMITTER_FOR(op) \
    EMITTER_FOR_BASE(_ ## op)

#define EMITTER_FOR_BASE(op) \
    void _PyJIT_EMIT_TARGET##op (JITData *jd, int next_instr_index, int opcode, int oparg)

#define EMIT_AS_SUBROUTINE(op) \
    void _PyJIT_EMIT_TARGET_##op (JITData *jd, int next_instr_index, int opcode, int oparg) { \
        STORE_FIELD(jd->ctx, EvalContext, stack_pointer, ir_type_pyobject_ptr_ptr, STACKPTR()); \
        STORE_FIELD(jd->ctx, EvalContext, retval, ir_type_pyobject_ptr, jd->retval); \
        STORE_FIELD(jd->ctx, EvalContext, why, ir_type_int, jd->why); \
        JTYPE instr_sig = CREATE_SIGNATURE(ir_type_int, ir_type_evalcontext_ptr, ir_type_pyframeobject_ptr, ir_type_int, ir_type_int, ir_type_int, ir_type_int); \
        JVALUE tmprv = CALL_NATIVE(instr_sig, opcode_function_table[opcode], jd->ctx, jd->f, CONSTANT_INT(next_instr_index), CONSTANT_INT(opcode), CONSTANT_INT(oparg), /*jumpev=*/ CONSTANT_INT(0)); \
        SET_VALUE(STACKPTR(), LOAD_FIELD(jd->ctx, EvalContext, stack_pointer, ir_type_pyobject_ptr_ptr)); \
        SET_VALUE(jd->retval, LOAD_FIELD(jd->ctx, EvalContext, retval, ir_type_pyobject_ptr)); \
        SET_VALUE(jd->why, LOAD_FIELD(jd->ctx, EvalContext, why, ir_type_int)); \
        HANDLE_RV_INTERNAL(tmprv); \
    }

/* Set next_instr_index based on the current f_lasti. This must be done
   when a regular instruction jumps to a special handler. */
#define SET_NEXT_INSTR_INDEX() do { \
        /* ctx->next_instr_index = (f->f_lasti / 2) + 1 */ \
        JVALUE f_lasti_val = LOAD_FIELD(jd->f, PyFrameObject, f_lasti, ir_type_int); \
        JVALUE computed_next_instr_index = ADD(SHIFT_RIGHT(f_lasti_val, CONSTANT_INT(1)), CONSTANT_INT(1)); \
        STORE_FIELD(jd->ctx, EvalContext, next_instr_index, ir_type_int, computed_next_instr_index); \
} while (0)

#define EMITTER_FOR_SPECIAL(op) \
    void _PyJIT_EMIT_SPECIAL_##op (JITData *jd)

#define GET_CTX_NEXT_INSTR_INDEX() \
    LOAD_FIELD(jd->ctx, EvalContext, next_instr_index, ir_type_int)

#define SET_CTX_NEXT_INSTR_INDEX(val) \
    STORE_FIELD(jd->ctx, EvalContext, next_instr_index, ir_type_int, (val))

#define EMIT_SPECIAL_AS_SUBROUTINE(op) \
    void _PyJIT_EMIT_SPECIAL_##op (JITData *jd) { \
        STORE_FIELD(jd->ctx, EvalContext, stack_pointer, ir_type_pyobject_ptr_ptr, STACKPTR()); \
        STORE_FIELD(jd->ctx, EvalContext, retval, ir_type_pyobject_ptr, jd->retval); \
        STORE_FIELD(jd->ctx, EvalContext, why, ir_type_int, jd->why); \
        JTYPE sig = CREATE_SIGNATURE(ir_type_int, ir_type_evalcontext_ptr, ir_type_pyframeobject_ptr, ir_type_int, ir_type_int); \
        JVALUE tmprv = CALL_NATIVE(sig, _PyEval_FUNC_JIT_TARGET_II_##op, jd->ctx, jd->f, GET_CTX_NEXT_INSTR_INDEX(), /*jumpev=*/ CONSTANT_INT(0)); \
        SET_VALUE(STACKPTR(), LOAD_FIELD(jd->ctx, EvalContext, stack_pointer, ir_type_pyobject_ptr_ptr)); \
        SET_VALUE(jd->retval, LOAD_FIELD(jd->ctx, EvalContext, retval, ir_type_pyobject_ptr)); \
        SET_VALUE(jd->why, LOAD_FIELD(jd->ctx, EvalContext, why, ir_type_int)); \
        HANDLE_RV_INTERNAL(tmprv); \
    }

#define GETLOCAL(i)  ir_getlocal(jd->func, (i))

/* Notice that the SETLOCAL macro does more than just ir_setlocal! */
#define SETLOCAL(i, val) do { \
    JVALUE tmp = GETLOCAL(i); \
    ir_setlocal(jd->func, (i), (val)); \
    XDECREF(tmp); \
} while (0)

#define EMIT_JUMP(check_eval_breaker) do { \
    if (check_eval_breaker) { \
        BRANCH_IF_NOT(LOAD_EVAL_BREAKER(), jd->jmptab[next_instr_index], IR_LIKELY); \
        SET_CTX_NEXT_INSTR_INDEX(CONSTANT_INT(next_instr_index)); \
        BRANCH(jd->j_special_internal[JIT_RC_NEXT_OPCODE]); \
    } else { \
        BRANCH(jd->jmptab[next_instr_index]); \
    } \
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

#define SET_WHY(n) \
    SET_VALUE(jd->why, CONSTANT_INT(n))

#define SET_RETVAL(val) \
    SET_VALUE(jd->retval, (val))

#define LOAD_F_LOCALS() \
    LOAD_FIELD(jd->f, PyFrameObject, f_locals, ir_type_pyobject_ptr)

#define HANDLERS_FOR(op) \
    void emit_exceptional_handlers_for_##op (JITData *jd, int opcode)

#define INSTALL_HANDLERS(op)   jd->handlers[opcode] = emit_exceptional_handlers_for_##op

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

#define UNWIND_TO(level) do { \
    JLABEL loop_entry = JLABEL_INIT("loop_entry"); \
    JLABEL loop_done = JLABEL_INIT("loop_done"); \
    JVALUE _level = (level); \
    IR_ASSERT(CMP_GE(STACK_LEVEL(), _level)); \
    LABEL(loop_entry); \
    BRANCH_IF(CMP_LE(STACK_LEVEL(), _level), loop_done, IR_SOMETIMES); \
    XDECREF(POP()); \
    BRANCH(loop_entry); \
    LABEL(loop_done); \
} while (0)

#define UNWIND_BLOCK(b) do { \
    JVALUE level = LOAD_FIELD((b), PyTryBlock, b_level, ir_type_int); \
    UNWIND_TO(level); \
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

#define UNWIND_EXCEPT_HANDLER(b) do { \
    JVALUE level = LOAD_FIELD((b), PyTryBlock, b_level, ir_type_int); \
    UNWIND_TO(ADD(level, CONSTANT_INT(3))); \
    JVALUE s_type = POP(); \
    JVALUE s_value = POP(); \
    JVALUE s_traceback = POP(); \
    JTYPE _sig = CREATE_SIGNATURE(ir_type_void, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr); \
    CALL_NATIVE(_sig, _unwind_except_helper, s_type, s_value, s_traceback); \
} while (0)

EMITTER_FOR_SPECIAL(FLOW) {
    // Index 0 is special, it dispatches to the other handlers based on 'rv'
    ir_jumptable(jd->func, jd->rv, jd->j_special_internal, JIT_RC_EXIT + 1);
}

EMITTER_FOR_SPECIAL(JUMP) {
    Py_ssize_t inst_count = PyBytes_GET_SIZE(jd->co->co_code)/sizeof(_Py_CODEUNIT);
    JVALUE index_val = GET_CTX_NEXT_INSTR_INDEX();
    ir_jumptable(jd->func, index_val, jd->jmptab, inst_count);
}

EMIT_SPECIAL_AS_SUBROUTINE(NEXT_OPCODE)
EMIT_SPECIAL_AS_SUBROUTINE(ERROR)
EMIT_SPECIAL_AS_SUBROUTINE(FAST_BLOCK_END)
EMIT_SPECIAL_AS_SUBROUTINE(FAST_YIELD)
EMIT_SPECIAL_AS_SUBROUTINE(UNWIND_CLEANUP)

EMITTER_FOR_SPECIAL(EXIT) {
    STORE_FIELD(jd->ctx, EvalContext, retval, ir_type_pyobject_ptr, jd->retval);
    STORE_FIELD(jd->ctx, EvalContext, why, ir_type_int, jd->why);
    ir_ret(jd->func, NULL);
}

EMITTER_FOR(INVALID_OPCODE) {
    Py_UNREACHABLE();
}

EMITTER_FOR(NOP) {
}

#define NAME_ERROR_MSG \
    "name '%.200s' is not defined"

#define UNBOUNDLOCAL_ERROR_MSG \
    "local variable '%.200s' referenced before assignment"

void handle_load_fast_unbound_local(EvalContext *ctx, int opcode, int oparg) {
    format_exc_check_arg(PyExc_UnboundLocalError,
                         UNBOUNDLOCAL_ERROR_MSG,
                         PyTuple_GetItem(ctx->co->co_varnames, oparg));
}

EMITTER_FOR(LOAD_FAST) {
    JLABEL load_fast_error = JLABEL_INIT("load_fast_error");
    JVALUE v = GETLOCAL(oparg);
    BRANCH_IF_NOT(v, load_fast_error, IR_UNLIKELY);
    INCREF(v);
    PUSH(v);

    BEGIN_REMOTE_SECTION(load_fast_error);
    JTYPE sig = CREATE_SIGNATURE(ir_type_void, ir_type_evalcontext_ptr, ir_type_int, ir_type_int);
    CALL_NATIVE(sig, handle_load_fast_unbound_local, jd->ctx, CONSTANT_INT(opcode), CONSTANT_INT(oparg));
    BRANCH(jd->j_special[JIT_RC_ERROR]);
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
    BRANCH(jd->j_special[JIT_RC_ERROR]);
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
        SET_WHY(WHY_EXCEPTION);
        BRANCH_SPECIAL(FAST_BLOCK_END);
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
    SET_WHY(WHY_RETURN);
    BRANCH_SPECIAL(FAST_BLOCK_END);
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
    BRANCH(jd->j_special[JIT_RC_ERROR]);
    END_REMOTE_SECTION(iter_is_null);

    BEGIN_REMOTE_SECTION(getter_is_null);
    SET_TOP(CONSTANT_PYOBJ(NULL));
    CALL_PyErr_Format(
        PyExc_TypeError,
        "'async for' requires an object with "
        "__aiter__ method, got %.100s",
        LOAD_FIELD(obj_type, PyTypeObject, tp_name, ir_type_char_ptr));
    DECREF(obj);
    BRANCH(jd->j_special[JIT_RC_ERROR]);
    END_REMOTE_SECTION(getter_is_null);

    BEGIN_REMOTE_SECTION(invalid_iter);
    SET_TOP(CONSTANT_PYOBJ(NULL));
    CALL_PyErr_Format(
        PyExc_TypeError,
        "'async for' received an object from __aiter__ "
        "that does not implement __anext__: %.100s",
        LOAD_FIELD(iter_type, PyTypeObject, tp_name, ir_type_char_ptr));
    DECREF(iter);
    BRANCH(jd->j_special[JIT_RC_ERROR]);
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
    JLABEL fast_send = JLABEL_INIT("fast_send");
    JLABEL v_is_none = JLABEL_INIT("v_is_none");
    JLABEL after_getting_retval = JLABEL_INIT("after_getting_retval");
    JLABEL handle_null_retval = JLABEL_INIT("handle_null_retval");
    JVALUE v = POP();
    JVALUE receiver = TOP();

    BRANCH_IF(
        LOGICAL_OR(IR_PyGen_CheckExact(receiver), IR_PyCoro_CheckExact(receiver)),
        fast_send,
        IR_LIKELY);

    /* Handle generic case */
    BRANCH_IF(CMP_EQ(v, CONSTANT_PYOBJ(Py_None)), v_is_none, IR_SEMILIKELY);
    /* v is not None */
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_void_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    SET_VALUE(jd->retval,
        CALL_NATIVE(sig, _PyObject_CallMethodIdObjArgs,
            receiver, CONSTANT_PTR(ir_type_void_ptr, &PyId_send), v, CONSTANT_PYOBJ(NULL)));
    BRANCH(after_getting_retval);
    LABEL(v_is_none);
    JVALUE receiver_type = IR_Py_TYPE(receiver);
    JVALUE tp_iternext = LOAD_FIELD(receiver_type, PyTypeObject, tp_iternext, jd->sig_oo);
    SET_VALUE(jd->retval, CALL_INDIRECT(tp_iternext, receiver));
    BRANCH(after_getting_retval);

    LABEL(fast_send);
    SET_VALUE(jd->retval, CALL_NATIVE(jd->sig_ooo, _PyGen_Send, receiver, v));
    BRANCH(after_getting_retval);

    LABEL(after_getting_retval);
    DECREF(v);
    BRANCH_IF_NOT(jd->retval, handle_null_retval, IR_SOMETIMES);

    /* Receiver remains on the stack, retval is value to be yielded */
    STORE_FIELD(jd->f, PyFrameObject, f_stacktop, ir_type_pyobject_ptr_ptr, STACKPTR());
    SET_WHY(WHY_YIELD);
    assert(next_instr_index >= 2);
    STORE_FIELD(jd->f, PyFrameObject, f_lasti, ir_type_int, CONSTANT_INT(2*(next_instr_index - 2)));
    GOTO_FAST_YIELD();

    /* Exiting from 'yield from' */
    LABEL(handle_null_retval);
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
    SET_RETVAL(POP());
    if (jd->co->co_flags & CO_ASYNC_GENERATOR) {
        JVALUE wrapped = CALL_NATIVE(jd->sig_oo, _PyAsyncGenValueWrapperNew, jd->retval);
        DECREF(jd->retval);
        SET_VALUE(jd->retval, wrapped);
        GOTO_ERROR_IF_NOT(wrapped);
    }
    STORE_FIELD(jd->f, PyFrameObject, f_stacktop, ir_type_pyobject_ptr_ptr, STACKPTR());
    SET_WHY(WHY_YIELD);
    GOTO_FAST_YIELD();
}

EMITTER_FOR(POP_EXCEPT) {
    JLABEL handle_error = JLABEL_INIT("handle_error");
    JTYPE sig = CREATE_SIGNATURE(ir_type_pytryblock_ptr, ir_type_pyframeobject_ptr);
    JVALUE b = CALL_NATIVE(sig, PyFrame_BlockPop, jd->f);
    BRANCH_IF(
        CMP_NE(
            LOAD_FIELD(b, PyTryBlock, b_type, ir_type_int),
            CONSTANT_INT(EXCEPT_HANDLER)),
        handle_error,
        IR_UNLIKELY);
    UNWIND_EXCEPT_HANDLER(b);
    CHECK_EVAL_BREAKER();

    BEGIN_REMOTE_SECTION(handle_error);
    CALL_PyErr_SetString(PyExc_SystemError,
                         "popped block is not an except handler");
    GOTO_ERROR();
    END_REMOTE_SECTION(handle_error);
}

EMITTER_FOR(POP_BLOCK) {
    JTYPE sig = CREATE_SIGNATURE(ir_type_pytryblock_ptr, ir_type_pyframeobject_ptr);
    JVALUE b = CALL_NATIVE(sig, PyFrame_BlockPop, jd->f);
    UNWIND_BLOCK(b);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(END_FINALLY) {
    IR_LABEL_INIT(try_case_2);
    IR_LABEL_INIT(skip_retval);
    IR_LABEL_INIT(skip_silenced);
    IR_LABEL_INIT(try_case_3);
    IR_LABEL_INIT(normal_exit);
    IR_LABEL_INIT(dispatch);

    JVALUE status = POP();
    BRANCH_IF_NOT(IR_PyLong_Check(status), try_case_2, IR_SEMILIKELY);
    /* Case 1: PyLong_Check(status) is true */
    JTYPE sig1 = CREATE_SIGNATURE(ir_type_long, ir_type_pyobject_ptr);
    SET_VALUE(jd->why, CAST(ir_type_int, CALL_NATIVE(sig1, PyLong_AsLong, status)));
    IR_ASSERT(
        LOGICAL_AND_NSC(
            CMP_NE(jd->why, CONSTANT_INT(WHY_YIELD)),
            CMP_NE(jd->why, CONSTANT_INT(WHY_EXCEPTION))));
    JVALUE is_return = CMP_EQ(jd->why, CONSTANT_INT(WHY_RETURN));
    JVALUE is_continue = CMP_EQ(jd->why, CONSTANT_INT(WHY_CONTINUE));
    BRANCH_IF_NOT(LOGICAL_OR(is_return, is_continue), skip_retval, IR_SEMILIKELY);
    SET_VALUE(jd->retval, POP());
    LABEL(skip_retval);
    BRANCH_IF_NOT(CMP_EQ(jd->why, CONSTANT_INT(WHY_SILENCED)), skip_silenced, IR_LIKELY);
    /* An exception was silenced by 'with', we must
    manually unwind the EXCEPT_HANDLER block which was
    created when the exception was caught, otherwise
    the stack will be in an inconsistent state. */
    {
        JTYPE sig2 = CREATE_SIGNATURE(ir_type_pytryblock_ptr, ir_type_pyframeobject_ptr);
        JVALUE b = CALL_NATIVE(sig2, PyFrame_BlockPop, jd->f);
        IR_ASSERT(CMP_EQ(LOAD_FIELD(b, PyTryBlock, b_type, ir_type_int), CONSTANT_INT(EXCEPT_HANDLER)));
        UNWIND_EXCEPT_HANDLER(b);
        SET_WHY(WHY_NOT);
        DECREF(status);
        BRANCH(dispatch);
    }
    LABEL(skip_silenced);
    DECREF(status);
    GOTO_FAST_BLOCK_END();

    LABEL(try_case_2);
    BRANCH_IF_NOT(IR_PyExceptionClass_Check(status), try_case_3, IR_SEMILIKELY);
    {
        JVALUE exc = POP();
        JVALUE tb = POP();
        JTYPE sig2 = CREATE_SIGNATURE(ir_type_void, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
        CALL_NATIVE(sig2, PyErr_Restore, status, exc, tb);
        SET_WHY(WHY_EXCEPTION);
        GOTO_FAST_BLOCK_END();
    }

    LABEL(try_case_3);
    BRANCH_IF(CMP_EQ(status, CONSTANT_PYOBJ(Py_None)), normal_exit, IR_LIKELY);
    {
        CALL_PyErr_SetString(PyExc_SystemError,
                    "'finally' pops bad exception");
        DECREF(status);
        GOTO_ERROR();
    }

    LABEL(normal_exit);
    DECREF(status);

    LABEL(dispatch);
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

static void _build_unpack_common(JITData *jd, int opcode, int oparg) {
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
    _build_unpack_common(jd, opcode, oparg);
}

EMITTER_FOR(BUILD_TUPLE_UNPACK) {
    _build_unpack_common(jd, opcode, oparg);
}

EMITTER_FOR(BUILD_LIST_UNPACK) {
    _build_unpack_common(jd, opcode, oparg);
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
    SET_WHY(WHY_BREAK);
    BRANCH_SPECIAL(FAST_BLOCK_END);
}

EMITTER_FOR(CONTINUE_LOOP) {
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_long);
    JVALUE tmp = CALL_NATIVE(sig, PyLong_FromLong, CONSTANT_LONG(oparg));
    SET_RETVAL(tmp);
    GOTO_ERROR_IF_NOT(tmp);
    SET_WHY(WHY_CONTINUE);
    BRANCH_SPECIAL(FAST_BLOCK_END);
}

/* SETUP_* are the same */
EMITTER_FOR(SETUP_LOOP) {
    JTYPE sig = CREATE_SIGNATURE(ir_type_void, ir_type_pyframeobject_ptr, ir_type_int, ir_type_int, ir_type_int);
    CALL_NATIVE(sig, PyFrame_BlockSetup,
        FRAMEPTR(), CONSTANT_INT(opcode), CONSTANT_INT(INSTR_OFFSET() + oparg), STACK_LEVEL());
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(SETUP_EXCEPT) {
    JTYPE sig = CREATE_SIGNATURE(ir_type_void, ir_type_pyframeobject_ptr, ir_type_int, ir_type_int, ir_type_int);
    CALL_NATIVE(sig, PyFrame_BlockSetup,
        FRAMEPTR(), CONSTANT_INT(opcode), CONSTANT_INT(INSTR_OFFSET() + oparg), STACK_LEVEL());
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(SETUP_FINALLY) {
    JTYPE sig = CREATE_SIGNATURE(ir_type_void, ir_type_pyframeobject_ptr, ir_type_int, ir_type_int, ir_type_int);
    CALL_NATIVE(sig, PyFrame_BlockSetup,
        FRAMEPTR(), CONSTANT_INT(opcode), CONSTANT_INT(INSTR_OFFSET() + oparg), STACK_LEVEL());
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
    JTYPE sig = CREATE_SIGNATURE(ir_type_void, ir_type_pyframeobject_ptr, ir_type_int, ir_type_int, ir_type_int);
    CALL_NATIVE(sig, PyFrame_BlockSetup,
        FRAMEPTR(), CONSTANT_INT(SETUP_FINALLY), CONSTANT_INT(INSTR_OFFSET() + oparg), STACK_LEVEL());
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
    JTYPE sig2 = CREATE_SIGNATURE(ir_type_void, ir_type_pyframeobject_ptr, ir_type_int, ir_type_int, ir_type_int);
    CALL_NATIVE(sig2, PyFrame_BlockSetup,
        FRAMEPTR(), CONSTANT_INT(SETUP_FINALLY), CONSTANT_INT(INSTR_OFFSET() + oparg), STACK_LEVEL());
    PUSH(res);
    CHECK_EVAL_BREAKER();
}

EMIT_AS_SUBROUTINE(WITH_CLEANUP_START)
EMIT_AS_SUBROUTINE(WITH_CLEANUP_FINISH)

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

EMIT_AS_SUBROUTINE(CALL_METHOD)

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

EMITTER_FOR(CALL_FUNCTION) {
    IR_LABEL_INIT(fast_dispatch);
    IR_LABEL_INIT(use_subroutine);
    JVALUE func = PEEK(oparg + 1);
    BRANCH_IF_NOT(IR_PyCFunction_Check(func), use_subroutine, IR_SEMILIKELY);
    JVALUE res = _call_function(jd, oparg, NULL);
    for (int i = 0; i < oparg; i++) {
        DECREF(POP());
    }
    DECREF(POP()); /* func */
    GOTO_ERROR_IF_NOT(res);
    PUSH(res);
    BRANCH(fast_dispatch);

    LABEL(use_subroutine);
    STORE_FIELD(jd->ctx, EvalContext, stack_pointer, ir_type_pyobject_ptr_ptr, STACKPTR());
    STORE_FIELD(jd->ctx, EvalContext, retval, ir_type_pyobject_ptr, jd->retval);
    STORE_FIELD(jd->ctx, EvalContext, why, ir_type_int, jd->why);
    JTYPE instr_sig = CREATE_SIGNATURE(ir_type_int, ir_type_evalcontext_ptr, ir_type_pyframeobject_ptr, ir_type_int, ir_type_int, ir_type_int, ir_type_int);
    JVALUE tmprv = CALL_NATIVE(instr_sig, opcode_function_table[opcode], jd->ctx, jd->f, CONSTANT_INT(next_instr_index), CONSTANT_INT(opcode), CONSTANT_INT(oparg), /*jumpev=*/ CONSTANT_INT(0));
    SET_VALUE(STACKPTR(), LOAD_FIELD(jd->ctx, EvalContext, stack_pointer, ir_type_pyobject_ptr_ptr));
    SET_VALUE(jd->retval, LOAD_FIELD(jd->ctx, EvalContext, retval, ir_type_pyobject_ptr));
    SET_VALUE(jd->why, LOAD_FIELD(jd->ctx, EvalContext, why, ir_type_int));
    HANDLE_RV_INTERNAL(tmprv);
    LABEL(fast_dispatch);
}
#endif

EMIT_AS_SUBROUTINE(CALL_FUNCTION)
EMIT_AS_SUBROUTINE(CALL_FUNCTION_KW)
EMIT_AS_SUBROUTINE(CALL_FUNCTION_EX)

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
