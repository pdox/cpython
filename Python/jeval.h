#include "Include/internal/ceval.h"
#include "Include/internal/pystate.h"

/* Special pseudo-instructions */
#define DECLARE_SPECIAL(name)  int _PyEval_FUNC_JIT_TARGET_II_##name (EvalContext *ctx, PyFrameObject *f, int jumpev)

DECLARE_SPECIAL(ERROR);
DECLARE_SPECIAL(FAST_YIELD);
DECLARE_SPECIAL(FAST_BLOCK_END);
DECLARE_SPECIAL(UNWIND_CLEANUP);
DECLARE_SPECIAL(NEXT_OPCODE);

#define JTYPE             ir_type
#define JVALUE            ir_value
#define JLABEL            ir_label
#define JLABEL_INIT(name) ir_label_new(jd->func, name)

#define JVALUE_CREATE(irtype)    ir_value_new(jd->func, (irtype))
#define ADDRESS_OF(val)          ir_address_of(jd->func, (val))

#define SET_VALUE(dest, src)     ir_set_value(jd->func, (dest), (src))

#define CREATE_SIGNATURE(sigvar, ret_type, ...) \
    JTYPE sigvar##_args[] = { __VA_ARGS__ }; \
    JTYPE sigvar = ir_create_function_type(jd->context, ret_type, sizeof(sigvar##_args)/sizeof(JTYPE), sigvar##_args);

#define CALL_INDIRECT(sig, funcval, ...) \
    do { \
        JVALUE call_args[] = { __VA_ARGS__ }; \
        JVALUE func_casted = ir_cast(jd->func, (sig), (funcval)); \
        ir_call(jd->func, func_casted, sizeof(call_args)/sizeof(JVALUE), call_args); \
    } while (0)

#define CALL_INDIRECT_WITH_RET(retvar, sig, funcval, ...) \
    JVALUE retvar; \
    do { \
        JVALUE call_args[] = { __VA_ARGS__ }; \
        JVALUE func_casted = ir_cast(jd->func, sig, (funcval)); \
        retvar = ir_call(jd->func, func_casted, sizeof(call_args)/sizeof(JVALUE), call_args); \
    } while (0)

#define CALL_NATIVE(sig, native_func, ...) do { \
    JVALUE call_args[] = { __VA_ARGS__ }; \
    JVALUE funcval = ir_constant_from_ptr(jd->func, (sig), native_func, #native_func); \
    ir_call(jd->func, funcval, sizeof(call_args)/sizeof(JVALUE), call_args); \
} while (0)

#define CALL_NATIVE_WITH_RET(retvar, sig, native_func, ...) \
    JVALUE retvar; \
    do { \
        JVALUE call_args[] = { __VA_ARGS__ }; \
        JVALUE funcval = ir_constant_from_ptr(jd->func, sig, native_func, #native_func); \
        retvar = ir_call(jd->func, funcval, sizeof(call_args)/sizeof(JVALUE), call_args); \
    } while (0)

#define HANDLE_RV(inval) do { \
    SET_VALUE(jd->rv, (inval)); \
    ir_branch_if(jd->func, jd->rv, jd->j_special[0]); \
} while (0)

#define HANDLE_RV_INTERNAL(inval) do { \
    SET_VALUE(jd->rv, (inval)); \
    ir_branch_if(jd->func, jd->rv, jd->j_special_internal[0]); \
} while (0)

#define LABEL(label)                ir_label_here(jd->func, (label))
#define BRANCH(label)               ir_branch(jd->func, (label))
#define BRANCH_IF_NOT(val, label)   ir_branch_if_not(jd->func, (val), (label))
#define BRANCH_IF(val, label)       ir_branch_if(jd->func, (val), (label))

#define BRANCH_SPECIAL(name) \
    BRANCH(jd->j_special[JIT_RC_ ## name])
#define BRANCH_SPECIAL_IF(val, name) \
    BRANCH_IF((val), jd->j_special[JIT_RC_ ## name])
#define BRANCH_SPECIAL_IF_ZERO(val, name) \
    BRANCH_IF_NOT((val), jd->j_special[JIT_RC_ ## name])

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

#define ADD(v1, v2)         ir_add(jd->func, (v1), (v2))
#define SUBTRACT(v1, v2)    ir_sub(jd->func, (v1), (v2))
#define SHIFT_RIGHT(v1, v2) ir_shr(jd->func, (v1), (v2))
#define CMP_LT(v1, v2)      ir_lt(jd->func, (v1), (v2))
#define CMP_EQ(v1, v2)      ir_eq(jd->func, (v1), (v2))
#define CMP_NE(v1, v2)      ir_ne(jd->func, (v1), (v2))
#define CMP_GT(v1, v2)      ir_gt(jd->func, (v1), (v2))
#define TERNARY(v, if_true, if_false)   ir_ternary(jd->func, (v), (if_true), (if_false))

/* Constant int value */
#define CONSTANT_INT(n)   ir_constant_int(jd->func, (n), NULL)
#define CONSTANT_UINT(n)  ir_constant_uint(jd->func, (n), NULL)

#define CONSTANT_LONG(n)  ir_constant_long(jd->func, (n), NULL)
#define CONSTANT_ULONG(n) ir_constant_ulong(jd->func, (n), NULL)

#define CONSTANT_PYSSIZET(n) ir_constant_pyssizet(jd->func, (n), NULL)

/* Constant uintptr_t value */
#define CONSTANT_NINT(n)  ir_constant_uintptr(jd->func, (n), NULL)

/* Constant pointer value */
#define CONSTANT_PTR(type, p)    ir_constant_from_ptr(jd->func, (type), (p), #p)
#define CONSTANT_PYOBJ(p)        ir_constant_pyobject_ptr(jd->func, (p), #p)
#define CONSTANT_VOID_PTR(p)     ir_constant_void_ptr(jd->func, (p), #p)

#define CAST(fieldtype, val) ir_cast(jd->func, (fieldtype), (val))

#define LOAD_FIELD(ptrval, structname, fieldname, fieldtype) \
    IR_LOAD_FIELD(jd->func, (ptrval), structname, fieldname, (fieldtype))

#define STORE_FIELD(ptrval, structname, fieldname, fieldtype, val) \
    IR_STORE_FIELD(jd->func, (ptrval), structname, fieldname, (fieldtype), (val))

#define LOAD_AT_INDEX(ptrval, indexval) \
    ir_load(jd->func, ir_get_index_ptr(jd->func, (ptrval), (indexval)))

#define STORE_AT_INDEX(ptrval, indexval, val) \
    ir_store(jd->func, ir_get_index_ptr(jd->func, (ptrval), (indexval)), (val))

#define LOAD(ptrval) \
    ir_load(jd->func, (ptrval))

#define STORE(ptrval, val) \
    ir_store(jd->func, (ptrval), (val))

#define CRASH() \
    STORE(CONSTANT_PTR(ir_type_int_ptr, 0), CONSTANT_INT(0))

/* High-level Python macros */

#define INCREF(_objval)    ir_incref(jd->func, (_objval), 0)
#define XINCREF(_objval)   ir_incref(jd->func, (_objval), 0)

#define DECREF(_objval)    ir_decref(jd->func, (_objval), 0)
#define XDECREF(_objval)   ir_decref(jd->func, (_objval), 1)


/* Stack operations */

#define STACKPTR()    (jd->stack_pointer)
#define FRAMEPTR()    (jd->f)

/* This does regular pointer arithmetic, equivalent to ptr + n in C.
   TODO: Figure out how to integrate this better into the IR
 */
#define POINTER_ADD(ptr, n) \
    ir_get_index_ptr(jd->func, ptr, CONSTANT_INT(n))

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
    CAST(ir_type_int, SHIFT_RIGHT(SUBTRACT(STACKPTR(), LOAD_VALUE_STACK()), CONSTANT_INT(3)))

#define GETNAME(i)   PyTuple_GET_ITEM(jd->co->co_names, (i));

// TODO: This needs to be adjusted depending on the configuration of _Py_atomic_int
#define LOAD_EVAL_BREAKER() \
    LOAD(CONSTANT_PTR(ir_type_int_ptr, &_PyRuntime.ceval.eval_breaker._value))

/* Check eval breaker, and jump to handler if set. This can only be used
   in instructions with no jumping, since it assumes the next instruction
   is computed using f_lasti.
 */
#define CHECK_EVAL_BREAKER() \
    BRANCH_IF(LOAD_EVAL_BREAKER(), jd->j_special[JIT_RC_NEXT_OPCODE]);

#define CALL_PyErr_SetString(exc, msg) do { \
    CREATE_SIGNATURE(sig, ir_type_void, ir_type_void_ptr, ir_type_void_ptr); \
    CALL_NATIVE(sig, PyErr_SetString, CONSTANT_VOID_PTR(exc), CONSTANT_VOID_PTR(msg)); \
} while (0)

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
        CREATE_SIGNATURE(instr_sig, ir_type_int, ir_type_evalcontext_ptr, ir_type_pyframeobject_ptr, ir_type_int, ir_type_int, ir_type_int, ir_type_int); \
        CALL_NATIVE_WITH_RET(tmprv, instr_sig, opcode_function_table[opcode], jd->ctx, jd->f, CONSTANT_INT(next_instr_index), CONSTANT_INT(opcode), CONSTANT_INT(oparg), /*jumpev=*/ CONSTANT_INT(0)); \
        SET_VALUE(STACKPTR(), LOAD_FIELD(jd->ctx, EvalContext, stack_pointer, ir_type_pyobject_ptr_ptr)); \
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
        CREATE_SIGNATURE(sig, ir_type_int, ir_type_evalcontext_ptr, ir_type_pyframeobject_ptr, ir_type_int, ir_type_int); \
        CALL_NATIVE_WITH_RET(tmprv, sig, _PyEval_FUNC_JIT_TARGET_II_##op, jd->ctx, jd->f, GET_CTX_NEXT_INSTR_INDEX(), /*jumpev=*/ CONSTANT_INT(0)); \
        SET_VALUE(STACKPTR(), LOAD_FIELD(jd->ctx, EvalContext, stack_pointer, ir_type_pyobject_ptr_ptr)); \
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
        BRANCH_IF_NOT(LOAD_EVAL_BREAKER(), jd->jmptab[next_instr_index]); \
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
    STORE_FIELD(jd->ctx, EvalContext, why, ir_type_uint, CONSTANT_UINT(n))

#define SET_RETVAL(val) \
    STORE_FIELD(jd->ctx, EvalContext, retval, ir_type_pyobject_ptr, (val))


#define HANDLERS_FOR(op) \
    void emit_exceptional_handlers_for_##op (JITData *jd, int opcode)

#define INSTALL_HANDLERS(op)   jd->handlers[opcode] = emit_exceptional_handlers_for_##op

/* Unlike PyErr_Occurred, this assumes tstate != NULL */
#define PYERR_OCCURRED()  LOAD_FIELD(TSTATE(), PyThreadState, curexc_type, ir_type_void_ptr)

/* This is equivalent to PyThreadState_GET() */
#define TSTATE() \
    CAST(ir_type_pythreadstate_ptr, \
         LOAD(CONSTANT_PTR(ir_type_uintptr_ptr, &_PyRuntime.gilstate.tstate_current._value)))

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
    ir_ret(jd->func, NULL);
}

EMITTER_FOR(INVALID_OPCODE) {
    Py_UNREACHABLE();
}

EMITTER_FOR(NOP) {
}

void format_exc_check_arg(PyObject *, const char *, PyObject *);
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
    BRANCH_IF_NOT(v, load_fast_error);
    INCREF(v);
    PUSH(v);

    BEGIN_REMOTE_SECTION(load_fast_error);
    CREATE_SIGNATURE(sig, ir_type_void, ir_type_evalcontext_ptr, ir_type_int, ir_type_int);
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
        CREATE_SIGNATURE(sig, ir_type_pyobject_ptr, ir_type_pyobject_ptr); \
        CALL_NATIVE_WITH_RET(res, sig, func, objval); \
        DECREF(objval); \
        SET_TOP(res); \
        BRANCH_IF_NOT(res, jd->j_special[JIT_RC_ERROR]); \
        CHECK_EVAL_BREAKER(); \
    }

EMIT_AS_UNARY_OP(UNARY_POSITIVE, PyNumber_Positive)
EMIT_AS_UNARY_OP(UNARY_NEGATIVE, PyNumber_Negative)
EMIT_AS_UNARY_OP(UNARY_INVERT, PyNumber_Invert)

EMITTER_FOR(UNARY_NOT) {
    JLABEL real_error = JLABEL_INIT("real_error");
    JVALUE value = TOP();
    CREATE_SIGNATURE(sig, ir_type_int, ir_type_pyobject_ptr);
    CALL_NATIVE_WITH_RET(err, sig, PyObject_IsTrue, value);
    DECREF(value);

    /* Jump if err < 0 (unlikely) */
    BRANCH_IF(CMP_LT(err, CONSTANT_INT(0)), real_error);

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
    CREATE_SIGNATURE(sig, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr); \
    CALL_NATIVE_WITH_RET(res, sig, PyNumber_Power, base, exp, CONSTANT_PYOBJ(Py_None));
    DECREF(base);
    DECREF(exp);
    SET_TOP(res);
    BRANCH_SPECIAL_IF_ZERO(res, ERROR);
    CHECK_EVAL_BREAKER();
}

#define EMIT_AS_BINARY_OP(op, func) \
    EMITTER_FOR_BASE(_ ## op) { \
        JVALUE right = POP(); \
        JVALUE left = TOP(); \
        CREATE_SIGNATURE(sig, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr); \
        CALL_NATIVE_WITH_RET(res, sig, func, left, right); \
        DECREF(left); \
        DECREF(right); \
        SET_TOP(res); \
        BRANCH_IF_NOT(res, jd->j_special[JIT_RC_ERROR]); \
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
    CREATE_SIGNATURE(sig, ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    CALL_NATIVE_WITH_RET(err, sig, PyList_Append, list, v);
    DECREF(v);
    BRANCH_SPECIAL_IF(err, ERROR);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(SET_ADD) {
    JVALUE v = POP();
    JVALUE set = PEEK(oparg);
    CREATE_SIGNATURE(sig, ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    CALL_NATIVE_WITH_RET(err, sig, PySet_Add, set, v);
    DECREF(v);
    BRANCH_SPECIAL_IF(err, ERROR);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(INPLACE_POWER) {
    JVALUE exp = POP();
    JVALUE base = TOP();
    CREATE_SIGNATURE(sig, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr); \
    CALL_NATIVE_WITH_RET(res, sig, PyNumber_InPlacePower, base, exp, CONSTANT_PYOBJ(Py_None));
    DECREF(base);
    DECREF(exp);
    SET_TOP(res);
    BRANCH_SPECIAL_IF_ZERO(res, ERROR);
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
    CREATE_SIGNATURE(sig, ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    CALL_NATIVE_WITH_RET(err, sig, PyObject_SetItem, container, sub, v);
    DECREF(v);
    DECREF(container);
    DECREF(sub);
    BRANCH_SPECIAL_IF(err, ERROR);
    CHECK_EVAL_BREAKER();
}

EMIT_AS_SUBROUTINE(STORE_ANNOTATION)

EMITTER_FOR(DELETE_SUBSCR) {
    JVALUE sub = TOP();
    JVALUE container = SECOND();
    STACKADJ(-2);
    /* del container[sub] */
    CREATE_SIGNATURE(sig, ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    CALL_NATIVE_WITH_RET(err, sig, PyObject_DelItem, container, sub);
    DECREF(container);
    DECREF(sub);
    BRANCH_SPECIAL_IF(err, ERROR);
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(PRINT_EXPR) {
    _Py_IDENTIFIER(displayhook);
    JVALUE value = POP();
    JLABEL hook_null = JLABEL_INIT("hook_null");
    CREATE_SIGNATURE(sig1, ir_type_pyobject_ptr, ir_type_void_ptr);
    CALL_NATIVE_WITH_RET(hook, sig1, _PySys_GetObjectId, CONSTANT_VOID_PTR(&PyId_displayhook));
    BRANCH_IF_NOT(hook, hook_null);
    CREATE_SIGNATURE(sig2, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    CALL_NATIVE_WITH_RET(res, sig2, PyObject_CallFunctionObjArgs, hook, value, CONSTANT_PYOBJ(NULL));
    DECREF(value);
    BRANCH_SPECIAL_IF_ZERO(res, ERROR);
    DECREF(res);
    CHECK_EVAL_BREAKER();

    BEGIN_REMOTE_SECTION(hook_null);
    CALL_PyErr_SetString(PyExc_RuntimeError, "lost sys.displayhook");
    DECREF(value);
    BRANCH_SPECIAL(ERROR);
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
        CREATE_SIGNATURE(sig, ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
        CALL_NATIVE_WITH_RET(ret, sig, do_raise, exc, cause);
        BRANCH_IF_NOT(ret, fin);
        SET_WHY(WHY_EXCEPTION);
        BRANCH_SPECIAL(FAST_BLOCK_END);
    }
    default:
        CALL_PyErr_SetString(PyExc_SystemError, "bad RAISE_VARARGS oparg");
        break;
    }
    LABEL(fin);
    BRANCH_SPECIAL(ERROR);
}

EMITTER_FOR(RETURN_VALUE) {
    SET_RETVAL(POP());
    SET_WHY(WHY_RETURN);
    BRANCH_SPECIAL(FAST_BLOCK_END);
}

EMIT_AS_SUBROUTINE(GET_AITER)
EMIT_AS_SUBROUTINE(GET_ANEXT)
EMIT_AS_SUBROUTINE(GET_AWAITABLE)
EMIT_AS_SUBROUTINE(YIELD_FROM)
EMIT_AS_SUBROUTINE(YIELD_VALUE)
EMIT_AS_SUBROUTINE(POP_EXCEPT)
EMIT_AS_SUBROUTINE(POP_BLOCK)
EMIT_AS_SUBROUTINE(END_FINALLY)
EMIT_AS_SUBROUTINE(LOAD_BUILD_CLASS)
EMIT_AS_SUBROUTINE(STORE_NAME)
EMIT_AS_SUBROUTINE(DELETE_NAME)
EMIT_AS_SUBROUTINE(UNPACK_SEQUENCE)
EMIT_AS_SUBROUTINE(UNPACK_EX)

EMITTER_FOR(STORE_ATTR) {
    PyObject *name = GETNAME(oparg)
    JVALUE owner = TOP();
    JVALUE v = SECOND();
    STACKADJ(-2);
    CREATE_SIGNATURE(sig, ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    CALL_NATIVE_WITH_RET(err, sig, PyObject_SetAttr, owner, CONSTANT_PYOBJ(name), v);
    DECREF(v);
    DECREF(owner);
    BRANCH_IF(err, jd->j_special[JIT_RC_ERROR]);
    CHECK_EVAL_BREAKER();
}

EMIT_AS_SUBROUTINE(DELETE_ATTR)
EMIT_AS_SUBROUTINE(STORE_GLOBAL)
EMIT_AS_SUBROUTINE(DELETE_GLOBAL)
EMIT_AS_SUBROUTINE(LOAD_NAME)
EMIT_AS_SUBROUTINE(LOAD_GLOBAL)
EMIT_AS_SUBROUTINE(DELETE_FAST)
EMIT_AS_SUBROUTINE(DELETE_DEREF)
EMIT_AS_SUBROUTINE(LOAD_CLOSURE)
EMIT_AS_SUBROUTINE(LOAD_CLASSDEREF)
EMIT_AS_SUBROUTINE(LOAD_DEREF)
EMIT_AS_SUBROUTINE(STORE_DEREF)
EMIT_AS_SUBROUTINE(BUILD_STRING)
EMIT_AS_SUBROUTINE(BUILD_TUPLE)
EMIT_AS_SUBROUTINE(BUILD_LIST)
EMIT_AS_SUBROUTINE(BUILD_TUPLE_UNPACK_WITH_CALL)
EMIT_AS_SUBROUTINE(BUILD_TUPLE_UNPACK)
EMIT_AS_SUBROUTINE(BUILD_LIST_UNPACK)
EMIT_AS_SUBROUTINE(BUILD_SET)
EMIT_AS_SUBROUTINE(BUILD_SET_UNPACK)
EMIT_AS_SUBROUTINE(BUILD_MAP)
EMIT_AS_SUBROUTINE(SETUP_ANNOTATIONS)
EMIT_AS_SUBROUTINE(BUILD_CONST_KEY_MAP)
EMIT_AS_SUBROUTINE(BUILD_MAP_UNPACK)
EMIT_AS_SUBROUTINE(BUILD_MAP_UNPACK_WITH_CALL)
EMIT_AS_SUBROUTINE(MAP_ADD)

EMITTER_FOR(LOAD_ATTR) {
    PyObject *name = GETNAME(oparg);
    JVALUE owner = TOP();
    CREATE_SIGNATURE(sig, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    CALL_NATIVE_WITH_RET(res, sig, PyObject_GetAttr, owner, CONSTANT_PYOBJ(name));
    DECREF(owner);
    SET_TOP(res);
    BRANCH_IF_NOT(res, jd->j_special[JIT_RC_ERROR]);
    CHECK_EVAL_BREAKER();
}

EMIT_AS_SUBROUTINE(COMPARE_OP)
EMIT_AS_SUBROUTINE(IMPORT_NAME)
EMIT_AS_SUBROUTINE(IMPORT_STAR)
EMIT_AS_SUBROUTINE(IMPORT_FROM)

EMITTER_FOR(JUMP_FORWARD) {
    JUMPBY(oparg, 0);
}

EMITTER_FOR(POP_JUMP_IF_FALSE) {
    JLABEL skip1 = JLABEL_INIT("skip1");
    JLABEL skip2 = JLABEL_INIT("skip2");
    JLABEL fin = JLABEL_INIT("fin");
    JVALUE cond = POP();
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_True)), skip1);
    /* Py_True case */
    DECREF(cond);
    BRANCH(fin);
    LABEL(skip1);
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_False)), skip2);
    /* Py_False case */
    DECREF(cond);
    JUMPTO(oparg, 0);
    LABEL(skip2);
    /* Generic case */
    CREATE_SIGNATURE(sig, ir_type_int, ir_type_pyobject_ptr);
    CALL_NATIVE_WITH_RET(err, sig, PyObject_IsTrue, cond);
    DECREF(cond);
    BRANCH_IF(CMP_GT(err, CONSTANT_INT(0)), fin);
    BRANCH_IF(CMP_LT(err, CONSTANT_INT(0)), jd->j_special[JIT_RC_ERROR]);
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
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_False)), skip1);
    /* Py_False case */
    DECREF(cond);
    BRANCH(fin);
    LABEL(skip1);
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_True)), skip2);
    /* Py_True case */
    DECREF(cond);
    JUMPTO(oparg, 0);
    LABEL(skip2);
    /* Generic case */
    CREATE_SIGNATURE(sig, ir_type_int, ir_type_pyobject_ptr);
    CALL_NATIVE_WITH_RET(err, sig, PyObject_IsTrue, cond);
    DECREF(cond);
    BRANCH_IF(CMP_EQ(err, CONSTANT_INT(0)), fin);
    BRANCH_IF(CMP_LT(err, CONSTANT_INT(0)), jd->j_special[JIT_RC_ERROR]);
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
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_True)), skip1);
    /* Py_True case */
    STACKADJ(-1);
    DECREF(cond);
    BRANCH(fast_dispatch);
    LABEL(skip1);
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_False)), skip2);
    /* Py_False case */
    JUMPTO(oparg, 0);
    /* Generic case */
    LABEL(skip2);
    CREATE_SIGNATURE(sig, ir_type_int, ir_type_pyobject_ptr);
    CALL_NATIVE_WITH_RET(err, sig, PyObject_IsTrue, cond);
    BRANCH_IF(CMP_EQ(err, CONSTANT_INT(0)), do_jump);
    BRANCH_IF(CMP_LT(err, CONSTANT_INT(0)), jd->j_special[JIT_RC_ERROR]);
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
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_False)), skip1);
    /* Py_False case */
    STACKADJ(-1);
    DECREF(cond);
    BRANCH(fast_dispatch);
    LABEL(skip1);
    BRANCH_IF(CMP_NE(cond, CONSTANT_PYOBJ(Py_True)), skip2);
    /* Py_True case */
    JUMPTO(oparg, 0);
    /* Generic case */
    LABEL(skip2);
    CREATE_SIGNATURE(sig, ir_type_int, ir_type_pyobject_ptr);
    CALL_NATIVE_WITH_RET(err, sig, PyObject_IsTrue, cond);
    BRANCH_IF(CMP_GT(err, CONSTANT_INT(0)), do_jump);
    BRANCH_IF(CMP_LT(err, CONSTANT_INT(0)), jd->j_special[JIT_RC_ERROR]);
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

EMIT_AS_SUBROUTINE(GET_ITER)
EMIT_AS_SUBROUTINE(GET_YIELD_FROM_ITER)


EMITTER_FOR(FOR_ITER) {
    JLABEL handle_null = JLABEL_INIT("handle_null");
    JLABEL cleanup = JLABEL_INIT("cleanup");
    JLABEL next_instruction = JLABEL_INIT("next_instruction");
    JVALUE iter_obj = TOP();
    JVALUE type_obj = LOAD_FIELD(iter_obj, PyObject, ob_type, ir_type_pytypeobject_ptr);
    JVALUE tp_iternext = LOAD_FIELD(type_obj, PyTypeObject, tp_iternext, ir_type_void_ptr);
    CREATE_SIGNATURE(sig, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    CALL_INDIRECT_WITH_RET(next, sig, tp_iternext, iter_obj);
    BRANCH_IF_NOT(next, handle_null);
    PUSH(next);
    BRANCH(next_instruction);

    /* Handle NULL case */
    LABEL(handle_null);
    BRANCH_IF_NOT(PYERR_OCCURRED(), cleanup);
    CREATE_SIGNATURE(sig2, ir_type_int, ir_type_pyobject_ptr);
    CALL_NATIVE_WITH_RET(ret, sig2, PyErr_ExceptionMatches, CONSTANT_PYOBJ(PyExc_StopIteration));
    BRANCH_IF_NOT(ret, jd->j_special[JIT_RC_ERROR]);
    CREATE_SIGNATURE(sig3, ir_type_void);
    CALL_NATIVE(sig3, PyErr_Clear);

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
    CREATE_SIGNATURE(sig, ir_type_pyobject_ptr, ir_type_long);
    CALL_NATIVE_WITH_RET(tmp, sig, PyLong_FromLong, CONSTANT_LONG(oparg));
    SET_RETVAL(tmp);
    BRANCH_SPECIAL_IF_ZERO(tmp, ERROR);
    SET_WHY(WHY_CONTINUE);
    BRANCH_SPECIAL(FAST_BLOCK_END);
}

/* SETUP_* are the same */
EMITTER_FOR(SETUP_LOOP) {
    CREATE_SIGNATURE(sig, ir_type_void, ir_type_pyframeobject_ptr, ir_type_int, ir_type_int, ir_type_int);
    CALL_NATIVE(sig, PyFrame_BlockSetup,
        FRAMEPTR(), CONSTANT_INT(opcode), CONSTANT_INT(INSTR_OFFSET() + oparg), STACK_LEVEL());
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(SETUP_EXCEPT) {
    CREATE_SIGNATURE(sig, ir_type_void, ir_type_pyframeobject_ptr, ir_type_int, ir_type_int, ir_type_int);
    CALL_NATIVE(sig, PyFrame_BlockSetup,
        FRAMEPTR(), CONSTANT_INT(opcode), CONSTANT_INT(INSTR_OFFSET() + oparg), STACK_LEVEL());
    CHECK_EVAL_BREAKER();
}

EMITTER_FOR(SETUP_FINALLY) {
    CREATE_SIGNATURE(sig, ir_type_void, ir_type_pyframeobject_ptr, ir_type_int, ir_type_int, ir_type_int);
    CALL_NATIVE(sig, PyFrame_BlockSetup,
        FRAMEPTR(), CONSTANT_INT(opcode), CONSTANT_INT(INSTR_OFFSET() + oparg), STACK_LEVEL());
    CHECK_EVAL_BREAKER();
}

EMIT_AS_SUBROUTINE(BEFORE_ASYNC_WITH)
EMIT_AS_SUBROUTINE(SETUP_ASYNC_WITH)
EMIT_AS_SUBROUTINE(SETUP_WITH)
EMIT_AS_SUBROUTINE(WITH_CLEANUP_START)
EMIT_AS_SUBROUTINE(WITH_CLEANUP_FINISH)

/* Private API for the LOAD_METHOD opcode. */
extern int _PyObject_GetMethod(PyObject *, PyObject *, PyObject **);

EMITTER_FOR(LOAD_METHOD) {
    PyObject *name = GETNAME(oparg);
    JVALUE obj = TOP();
    JVALUE meth = JVALUE_CREATE(ir_type_pyobject_ptr);
    SET_VALUE(meth, CONSTANT_PYOBJ(NULL));
    CREATE_SIGNATURE(sig, ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr_ptr);
    CALL_NATIVE_WITH_RET(meth_found, sig, _PyObject_GetMethod, obj, CONSTANT_PYOBJ(name), ADDRESS_OF(meth));

    /* If meth == NULL, most likely attribute wasn't found. */
    BRANCH_IF_NOT(meth, jd->j_special[JIT_RC_ERROR]);

    JLABEL fin = JLABEL_INIT("fin");
    JLABEL if_meth_found = JLABEL_INIT("if_meth_found");
    BRANCH_IF(meth_found, if_meth_found);

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
EMIT_AS_SUBROUTINE(CALL_FUNCTION)
EMIT_AS_SUBROUTINE(CALL_FUNCTION_KW)
EMIT_AS_SUBROUTINE(CALL_FUNCTION_EX)

EMIT_AS_SUBROUTINE(MAKE_FUNCTION)
EMIT_AS_SUBROUTINE(BUILD_SLICE)
EMIT_AS_SUBROUTINE(FORMAT_VALUE)
EMIT_AS_SUBROUTINE(EXTENDED_ARG)
