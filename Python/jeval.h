#include "Include/internal/ceval.h"
#include "Include/internal/pystate.h"

/* Special pseudo-instructions */
#define DECLARE_SPECIAL(name)  int _PyEval_FUNC_JIT_TARGET_II_##name (EvalContext *ctx, PyFrameObject *f, int jumpev)

DECLARE_SPECIAL(ERROR);
DECLARE_SPECIAL(FAST_YIELD);
DECLARE_SPECIAL(FAST_BLOCK_END);
DECLARE_SPECIAL(UNWIND_CLEANUP);
DECLARE_SPECIAL(NEXT_OPCODE);

#define JTYPE       jit_type_t
#define JVALUE      jit_value_t
#define JLABEL      jit_label_t
#define JLABEL_INIT jit_label_undefined

#define SET_VALUE(dest, src)     jit_insn_store(jd->func, (dest), (src))

#define CREATE_SIGNATURE(sigvar, ret_type, ...) \
    JTYPE sigvar##_args[] = { __VA_ARGS__ }; \
    JTYPE sigvar = jit_type_create_signature(jit_abi_cdecl, ret_type, sigvar##_args, sizeof(sigvar##_args)/sizeof(JTYPE), 1);

#define CALL_INDIRECT(sig, funcval, ...) \
    do { \
        JVALUE call_args[] = { __VA_ARGS__ }; \
        jit_insn_call_indirect(jd->func, (funcval), sig, call_args, sizeof(call_args)/sizeof(JVALUE), JIT_CALL_NOTHROW); \
    } while (0)

#define CALL_INDIRECT_WITH_RET(retvar, sig, funcval, ...) \
    JVALUE retvar; \
    do { \
        JVALUE call_args[] = { __VA_ARGS__ }; \
        retvar = jit_insn_call_indirect(jd->func, (funcval), sig, call_args, sizeof(call_args)/sizeof(JVALUE), JIT_CALL_NOTHROW); \
    } while (0)

#define CALL_NATIVE(sig, nfunc, ...) do { \
    JVALUE call_args[] = { __VA_ARGS__ }; \
    jit_insn_call_native(jd->func, NULL, nfunc, sig, call_args, sizeof(call_args)/sizeof(JVALUE), JIT_CALL_NOTHROW); \
} while (0)

#define CALL_NATIVE_WITH_RET(retvar, sig, nfunc, ...) \
    JVALUE retvar; \
    do { \
        JVALUE call_args[] = { __VA_ARGS__ }; \
        retvar = jit_insn_call_native(jd->func, NULL, nfunc, sig, call_args, sizeof(call_args)/sizeof(JVALUE), JIT_CALL_NOTHROW); \
    } while (0)

#define HANDLE_RV(inval) do { \
    SET_VALUE(jd->rv, (inval)); \
    jit_insn_branch_if(jd->func, jd->rv, &jd->j_special[0]); \
} while (0)

#define HANDLE_RV_INTERNAL(inval) do { \
    SET_VALUE(jd->rv, (inval)); \
    jit_insn_branch_if(jd->func, jd->rv, &jd->j_special_internal[0]); \
} while (0)

#define LABEL(ptr)          jit_insn_label(jd->func, (ptr))
#define BRANCH(ptr)         jit_insn_branch(jd->func, (ptr))

/*
#define MOVE_TO_END(from_label, to_label) do { \
    move_entry *m = PyMem_RawMalloc(sizeof(move_entry)); \
    assert(m); \
    assert(from_label != JLABEL_INIT); \
    assert(to_label != JLABEL_INIT); \
    m->from_label = from_label; \
    m->to_label = to_label; \
    m->next = jd->move_entry_list; \
    jd->move_entry_list = m; \
} while (0)
*/


#define ADD(v1, v2)         jit_insn_add(jd->func, (v1), (v2))
#define SUBTRACT(v1, v2)    jit_insn_sub(jd->func, (v1), (v2))
#define SHIFT_RIGHT(v1, v2) jit_insn_shr(jd->func, (v1), (v2))
#define CMP_LT(v1, v2)      jit_insn_lt(jd->func, (v1), (v2))

/* Constant int value */
#define CONSTANT_INT(n)  jit_value_create_nint_constant(jd->func, jit_type_int, (n))

/* Constant uintptr_t value */
#define CONSTANT_NINT(n) jit_value_create_nint_constant(jd->func, jit_type_nint, (n))

/* Constant pointer value */
#define CONSTANT_PTR(p) \
    jit_value_create_nint_constant(jd->func, jit_type_void_ptr, (jit_nint)((void*)(p)))

#define LOAD_FIELD(ptrval, structname, fieldname, fieldtype) \
    jit_insn_load_relative(jd->func, (ptrval), offsetof(structname, fieldname), (fieldtype))

#define STORE_FIELD(ptrval, structname, fieldname, fieldtype, val) \
    jit_insn_store_relative(jd->func, (ptrval), offsetof(structname, fieldname), jit_insn_convert(jd->func, (val), (fieldtype), 0))

#define LOAD_AT_INDEX(ptrval, elemtype, indexval) \
    jit_insn_load_elem(jd->func, (ptrval), (indexval), elemtype)

#define STORE_AT_INDEX(ptrval, elemtype, indexval, val) \
    jit_insn_store_elem(jd->func, (ptrval), (indexval), jit_insn_convert(jd->func, (val), (elemtype), 0))

#define LOAD(ptrval, fieldtype) \
    jit_insn_load_relative(jd->func, (ptrval), 0, (fieldtype))

#define STORE(ptrval, fieldtype, val) \
    jit_insn_store_relative(jd->func, (ptrval), 0, jit_insn_convert(jd->func, (val), (fieldtype), 0))

#define CRASH() \
    STORE(CONSTANT_PTR(0), jit_type_int, CONSTANT_INT(0));

/* High-level Python macros */

#define INCREF(objval) \
    SET_REFCNT((objval), ADD(GET_REFCNT((objval)), CONSTANT_INT(1)))

#define GET_REFCNT(objval) \
    LOAD_FIELD((objval), PyObject, ob_refcnt, jit_type_nint)

#define SET_REFCNT(objval, val) \
    STORE_FIELD((objval), PyObject, ob_refcnt, jit_type_nint, (val))

#if defined(Py_DEBUG) || defined(Py_TRACE_REFS)
#  define _DEALLOC(objval) do { \
        CREATE_SIGNATURE(sig, jit_type_void, jit_type_void_ptr); \
        CALL_NATIVE(sig, _Py_Dealloc, (objval)); \
    } while (0)
#else
#  define _DEALLOC(objval) do { \
        JVALUE _dealloc_type = LOAD_FIELD((objval), PyObject, ob_type, jit_type_void_ptr); \
        JVALUE _dealloc_func = LOAD_FIELD(_dealloc_type, PyTypeObject, tp_dealloc, jit_type_void_ptr); \
        CREATE_SIGNATURE(sig, jit_type_void, jit_type_void_ptr); \
        CALL_INDIRECT(sig, _dealloc_func, (objval)); \
    } while (0)
#endif

#define DECREF(_objval) do { \
    JVALUE _obj = (_objval); \
    JLABEL skip_dealloc = JLABEL_INIT; \
    JVALUE new_refcount = SUBTRACT(GET_REFCNT(_obj), CONSTANT_INT(1)); \
    SET_REFCNT(_obj, new_refcount); \
    BRANCH_IF(new_refcount, &skip_dealloc); \
    _DEALLOC(_obj); \
    LABEL(&skip_dealloc); \
} while (0);

#define XDECREF(_objval) do { \
    JVALUE __obj = (_objval); \
    JLABEL skip_if_null = JLABEL_INIT; \
    BRANCH_IF_ZERO(__obj, &skip_if_null); \
    DECREF(__obj); \
    LABEL(&skip_if_null); \
} while (0)

/* Stack operations */

#define STACKPTR()    (jd->stack_pointer)

#define PUSH(objval) do { \
    STORE(STACKPTR(), jit_type_void_ptr, (objval)); \
    STACKADJ(1); \
} while (0)

#define STACKADJ(n)   SET_VALUE(STACKPTR(), ADD(STACKPTR(), CONSTANT_INT((n) * (int)sizeof(PyObject*))))

#define STACKADDR(n)  SUBTRACT(STACKPTR(), CONSTANT_INT((n) * (int)sizeof(PyObject*)))
#define PEEK(n)       LOAD(STACKADDR(n), jit_type_void_ptr)
#define PUT(n, v)     STORE(STACKADDR(n), jit_type_void_ptr, (v))

#define TOP()     PEEK(1)
#define SECOND()  PEEK(2)
#define THIRD()   PEEK(3)
#define FOURTH()  PEEK(4)

#define SET_TOP(v)     PUT(1, (v))
#define SET_SECOND(v)  PUT(2, (v))
#define SET_THIRD(v)   PUT(3, (v))
#define SET_FOURTH(v)  PUT(4, (v))
#define POP()          (STACKADJ(-1), PEEK(0))

// TODO: This needs to be adjusted depending on the configuration of _Py_atomic_int
#define LOAD_EVAL_BREAKER() \
    LOAD(CONSTANT_PTR(&_PyRuntime.ceval.eval_breaker._value), jit_type_int)

/* Check eval breaker, and jump to handler if set. This can only be used
   in instructions with no jumping, since it assumes the next instruction
   is computed using f_lasti.
 */
#define CHECK_EVAL_BREAKER() \
    BRANCH_IF(LOAD_EVAL_BREAKER(), &jd->j_special[JIT_RC_NEXT_OPCODE]);

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

/* Defeat nested macro evaluation problem.
   TODO: Can this be done in a less hacky way?
 */
#define EMITTER_FOR(op) \
    EMITTER_FOR_BASE(_ ## op)

#define EMITTER_FOR_BASE(op) \
    void _PyJIT_EMIT_TARGET##op (JITData *jd, int next_instr_index, int opcode, int oparg)

#define EMIT_AS_SUBROUTINE(op) \
    void _PyJIT_EMIT_TARGET_##op (JITData *jd, int next_instr_index, int opcode, int oparg) { \
        STORE_FIELD(jd->ctx, EvalContext, stack_pointer, jit_type_void_ptr, STACKPTR()); \
        CREATE_SIGNATURE(instr_sig, jit_type_int, jit_type_void_ptr, jit_type_void_ptr, jit_type_int, jit_type_int, jit_type_int, jit_type_int); \
        CALL_NATIVE_WITH_RET(tmprv, instr_sig, opcode_function_table[opcode], jd->ctx, jd->f, CONSTANT_INT(next_instr_index), CONSTANT_INT(opcode), CONSTANT_INT(oparg), /*jumpev=*/ CONSTANT_INT(0)); \
        SET_VALUE(STACKPTR(), LOAD_FIELD(jd->ctx, EvalContext, stack_pointer, jit_type_void_ptr)); \
        HANDLE_RV_INTERNAL(tmprv); \
    }

/* Set next_instr_index based on the current f_lasti. This must be done
   when a regular instruction jumps to a special handler. */
#define SET_NEXT_INSTR_INDEX() do { \
        /* ctx->next_instr_index = (f->f_lasti / 2) + 1 */ \
        JVALUE f_lasti_val = LOAD_FIELD(jd->f, PyFrameObject, f_lasti, jit_type_int); \
        JVALUE computed_next_instr_index = ADD(SHIFT_RIGHT(f_lasti_val, CONSTANT_INT(1)), CONSTANT_INT(1)); \
        STORE_FIELD(jd->ctx, EvalContext, next_instr_index, jit_type_int, computed_next_instr_index); \
} while (0)

#define EMITTER_FOR_SPECIAL(op) \
    void _PyJIT_EMIT_SPECIAL_##op (JITData *jd)

#define GET_CTX_NEXT_INSTR_INDEX() \
    LOAD_FIELD(jd->ctx, EvalContext, next_instr_index, jit_type_int)

#define SET_CTX_NEXT_INSTR_INDEX(val) \
    STORE_FIELD(jd->ctx, EvalContext, next_instr_index, jit_type_int, (val))

#define EMIT_SPECIAL_AS_SUBROUTINE(op) \
    void _PyJIT_EMIT_SPECIAL_##op (JITData *jd) { \
        STORE_FIELD(jd->ctx, EvalContext, stack_pointer, jit_type_void_ptr, STACKPTR()); \
        CREATE_SIGNATURE(sig, jit_type_int, jit_type_void_ptr, jit_type_void_ptr, jit_type_int, jit_type_int); \
        CALL_NATIVE_WITH_RET(tmprv, sig, _PyEval_FUNC_JIT_TARGET_II_##op, jd->ctx, jd->f, GET_CTX_NEXT_INSTR_INDEX(), /*jumpev=*/ CONSTANT_INT(0)); \
        SET_VALUE(STACKPTR(), LOAD_FIELD(jd->ctx, EvalContext, stack_pointer, jit_type_void_ptr)); \
        HANDLE_RV_INTERNAL(tmprv); \
    }

#define GETLOCAL(i) \
    LOAD_AT_INDEX(jd->fastlocals, jit_type_void_ptr, CONSTANT_INT(i))

#define SETLOCAL(i, val) do { \
    JVALUE tmp = GETLOCAL(i); \
    STORE_AT_INDEX(jd->fastlocals, jit_type_void_ptr, CONSTANT_INT(i), (val)); \
    XDECREF(tmp); \
} while (0)

#define BRANCH_IF_ZERO(val, labelptr) \
    jit_insn_branch_if_not(jd->func, (val), (labelptr))

#define BRANCH_IF(val, labelptr) \
    jit_insn_branch_if(jd->func, (val), (labelptr))

#define EMIT_JUMP(check_eval_breaker) do { \
    if (check_eval_breaker) { \
        BRANCH_IF_ZERO(LOAD_EVAL_BREAKER(), &jd->jmptab[next_instr_index]); \
        SET_CTX_NEXT_INSTR_INDEX(CONSTANT_INT(next_instr_index)); \
        BRANCH(&jd->j_special_internal[JIT_RC_NEXT_OPCODE]); \
    } else { \
        BRANCH(&jd->jmptab[next_instr_index]); \
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

#define HANDLERS_FOR(op) \
    void emit_exceptional_handlers_for_##op (JITData *jd, int opcode)

#define INSTALL_HANDLERS(op)   jd->handlers[opcode] = emit_exceptional_handlers_for_##op

/* Unlike PyErr_Occurred, this assumes tstate != NULL */
#define PYERR_OCCURRED()  LOAD_FIELD(TSTATE(), PyThreadState, curexc_type, jit_type_void_ptr)
#define TSTATE()    LOAD(CONSTANT_PTR(&_PyRuntime.gilstate.tstate_current._value), jit_type_void_ptr)

EMITTER_FOR_SPECIAL(FLOW) {
    // Index 0 is special, it dispatches to the other handlers based on 'rv'
    jit_insn_jump_table(jd->func, jd->rv, jd->j_special_internal, JIT_RC_EXIT + 1);
}

EMITTER_FOR_SPECIAL(JUMP) {
    Py_ssize_t inst_count = PyBytes_GET_SIZE(jd->co->co_code)/sizeof(_Py_CODEUNIT);
    JVALUE index_val = GET_CTX_NEXT_INSTR_INDEX();
    jit_insn_jump_table(jd->func, index_val, jd->jmptab, inst_count);
}

EMIT_SPECIAL_AS_SUBROUTINE(NEXT_OPCODE)
EMIT_SPECIAL_AS_SUBROUTINE(ERROR)
EMIT_SPECIAL_AS_SUBROUTINE(FAST_BLOCK_END)
EMIT_SPECIAL_AS_SUBROUTINE(FAST_YIELD)
EMIT_SPECIAL_AS_SUBROUTINE(UNWIND_CLEANUP)

EMITTER_FOR_SPECIAL(EXIT) {
    jit_insn_return(jd->func, NULL);
}

EMITTER_FOR(INVALID_OPCODE) {
    Py_UNREACHABLE();
}

EMITTER_FOR(NOP) {
}

typedef struct {
    JLABEL err;
    JVALUE oparg;
} LOAD_FAST_INFO;

void format_exc_check_arg(PyObject *, const char *, PyObject *);
#define UNBOUNDLOCAL_ERROR_MSG \
    "local variable '%.200s' referenced before assignment"

int handle_load_fast_unbound_local(EvalContext *ctx, int opcode, int oparg) {
    format_exc_check_arg(PyExc_UnboundLocalError,
                         UNBOUNDLOCAL_ERROR_MSG,
                         PyTuple_GetItem(ctx->co->co_varnames, oparg));
    return JIT_RC_ERROR;
}

HANDLERS_FOR(LOAD_FAST) {
    FETCH_INFO(LOAD_FAST);
    CREATE_SIGNATURE(sig, jit_type_int, jit_type_void_ptr, jit_type_int, jit_type_int);
    LABEL(&info->err);
    CALL_NATIVE_WITH_RET(rvtmp, sig, handle_load_fast_unbound_local, jd->ctx, CONSTANT_INT(opcode), info->oparg);
    HANDLE_RV(rvtmp);
}

EMITTER_FOR(LOAD_FAST) {
    INIT_INFO(LOAD_FAST) {
        info->err = JLABEL_INIT;
        info->oparg = jit_value_create(jd->func, jit_type_int);
        INSTALL_HANDLERS(LOAD_FAST);
    }
    SET_VALUE(info->oparg, CONSTANT_INT(oparg));
    JVALUE v = GETLOCAL(oparg);
    BRANCH_IF_ZERO(v, &info->err);
    INCREF(v);
    PUSH(v);
}

EMITTER_FOR(LOAD_CONST) {
    PyObject *obj = PyTuple_GET_ITEM(jd->co->co_consts, oparg);
    assert(obj != NULL);
    JVALUE v = CONSTANT_PTR(obj);
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
        CREATE_SIGNATURE(sig, jit_type_void_ptr, jit_type_void_ptr); \
        CALL_NATIVE_WITH_RET(res, sig, func, objval); \
        DECREF(objval); \
        SET_TOP(res); \
        BRANCH_IF_ZERO(res, &jd->j_special[JIT_RC_ERROR]); \
        CHECK_EVAL_BREAKER(); \
    }

EMIT_AS_UNARY_OP(UNARY_POSITIVE, PyNumber_Positive)
EMIT_AS_UNARY_OP(UNARY_NEGATIVE, PyNumber_Negative)
EMIT_AS_UNARY_OP(UNARY_INVERT, PyNumber_Invert)

EMITTER_FOR(UNARY_NOT) {
    JLABEL real_error = JLABEL_INIT;
    JLABEL zero_case = JLABEL_INIT;
    JLABEL fin = JLABEL_INIT;
    JVALUE value = TOP();
    CREATE_SIGNATURE(sig, jit_type_int, jit_type_void_ptr);
    CALL_NATIVE_WITH_RET(err, sig, PyObject_IsTrue, value);
    DECREF(value);

    BRANCH_IF(CMP_LT(err, CONSTANT_INT(0)), &real_error); // UNLIKELY
    BRANCH_IF_ZERO(err, &zero_case);

    /* Handle err > 0 case */
    JVALUE pyfalse = CONSTANT_PTR(Py_False);
    INCREF(pyfalse);
    SET_TOP(pyfalse);
    CHECK_EVAL_BREAKER();
    BRANCH(&fin);

    /* Handle err == 0 case */
    LABEL(&zero_case);
    JVALUE pytrue = CONSTANT_PTR(Py_True);
    INCREF(pytrue);
    SET_TOP(pytrue);
    CHECK_EVAL_BREAKER();
    BRANCH(&fin);

    /* Handle err < 0 case */
    LABEL(&real_error);
    STACKADJ(-1);
    BRANCH(&jd->j_special[JIT_RC_ERROR]);

    LABEL(&fin);
    //MOVE_TO_END(real_error, fin);
}


EMIT_AS_SUBROUTINE(BINARY_POWER)
EMIT_AS_SUBROUTINE(BINARY_MULTIPLY)
EMIT_AS_SUBROUTINE(BINARY_MATRIX_MULTIPLY)
EMIT_AS_SUBROUTINE(BINARY_TRUE_DIVIDE)
EMIT_AS_SUBROUTINE(BINARY_FLOOR_DIVIDE)
EMIT_AS_SUBROUTINE(BINARY_MODULO)
EMIT_AS_SUBROUTINE(BINARY_ADD)
EMIT_AS_SUBROUTINE(BINARY_SUBTRACT)
EMIT_AS_SUBROUTINE(BINARY_SUBSCR)
EMIT_AS_SUBROUTINE(BINARY_LSHIFT)
EMIT_AS_SUBROUTINE(BINARY_RSHIFT)
EMIT_AS_SUBROUTINE(BINARY_AND)
EMIT_AS_SUBROUTINE(BINARY_XOR)
EMIT_AS_SUBROUTINE(BINARY_OR)
EMIT_AS_SUBROUTINE(LIST_APPEND)
EMIT_AS_SUBROUTINE(SET_ADD)
EMIT_AS_SUBROUTINE(INPLACE_POWER)
EMIT_AS_SUBROUTINE(INPLACE_MULTIPLY)
EMIT_AS_SUBROUTINE(INPLACE_MATRIX_MULTIPLY)
EMIT_AS_SUBROUTINE(INPLACE_TRUE_DIVIDE)
EMIT_AS_SUBROUTINE(INPLACE_FLOOR_DIVIDE)
EMIT_AS_SUBROUTINE(INPLACE_MODULO)
EMIT_AS_SUBROUTINE(INPLACE_ADD)
EMIT_AS_SUBROUTINE(INPLACE_SUBTRACT)
EMIT_AS_SUBROUTINE(INPLACE_LSHIFT)
EMIT_AS_SUBROUTINE(INPLACE_RSHIFT)
EMIT_AS_SUBROUTINE(INPLACE_AND)
EMIT_AS_SUBROUTINE(INPLACE_XOR)
EMIT_AS_SUBROUTINE(INPLACE_OR)
EMIT_AS_SUBROUTINE(STORE_SUBSCR)
EMIT_AS_SUBROUTINE(STORE_ANNOTATION)
EMIT_AS_SUBROUTINE(DELETE_SUBSCR)
EMIT_AS_SUBROUTINE(PRINT_EXPR)
EMIT_AS_SUBROUTINE(RAISE_VARARGS)
EMIT_AS_SUBROUTINE(RETURN_VALUE)
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
EMIT_AS_SUBROUTINE(STORE_ATTR)
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
EMIT_AS_SUBROUTINE(LOAD_ATTR)
EMIT_AS_SUBROUTINE(COMPARE_OP)
EMIT_AS_SUBROUTINE(IMPORT_NAME)
EMIT_AS_SUBROUTINE(IMPORT_STAR)
EMIT_AS_SUBROUTINE(IMPORT_FROM)
EMIT_AS_SUBROUTINE(JUMP_FORWARD)
EMIT_AS_SUBROUTINE(POP_JUMP_IF_FALSE)
EMIT_AS_SUBROUTINE(POP_JUMP_IF_TRUE)
EMIT_AS_SUBROUTINE(JUMP_IF_FALSE_OR_POP)
EMIT_AS_SUBROUTINE(JUMP_IF_TRUE_OR_POP)


EMITTER_FOR(JUMP_ABSOLUTE) {
    JUMPTO(oparg, 1);
}

EMIT_AS_SUBROUTINE(GET_ITER)
EMIT_AS_SUBROUTINE(GET_YIELD_FROM_ITER)


EMITTER_FOR(FOR_ITER) {
    JLABEL handle_null = JLABEL_INIT;
    JLABEL cleanup = JLABEL_INIT;
    JLABEL next_instruction = JLABEL_INIT;
    JVALUE iter_obj = TOP();
    JVALUE type_obj = LOAD_FIELD(iter_obj, PyObject, ob_type, jit_type_void_ptr);
    JVALUE tp_iternext = LOAD_FIELD(type_obj, PyTypeObject, tp_iternext, jit_type_void_ptr);
    CREATE_SIGNATURE(sig, jit_type_void_ptr, jit_type_void_ptr);
    CALL_INDIRECT_WITH_RET(next, sig, tp_iternext, iter_obj);
    BRANCH_IF_ZERO(next, &handle_null);
    PUSH(next);
    BRANCH(&next_instruction);

    /* Handle NULL case */
    LABEL(&handle_null);
    BRANCH_IF_ZERO(PYERR_OCCURRED(), &cleanup);
    CREATE_SIGNATURE(sig2, jit_type_int, jit_type_void_ptr);
    CALL_NATIVE_WITH_RET(ret, sig2, PyErr_ExceptionMatches, CONSTANT_PTR(PyExc_StopIteration));
    BRANCH_IF_ZERO(ret, &jd->j_special[JIT_RC_ERROR]);
    CREATE_SIGNATURE(sig3, jit_type_void);
    CALL_NATIVE(sig3, PyErr_Clear);

    LABEL(&cleanup);
    STACKADJ(-1);
    DECREF(iter_obj);
    JUMPBY(oparg, 1);
    LABEL(&next_instruction);
    CHECK_EVAL_BREAKER();
}

EMIT_AS_SUBROUTINE(BREAK_LOOP)
EMIT_AS_SUBROUTINE(CONTINUE_LOOP)
EMIT_AS_SUBROUTINE(SETUP_LOOP)
EMIT_AS_SUBROUTINE(SETUP_EXCEPT)
EMIT_AS_SUBROUTINE(SETUP_FINALLY)
EMIT_AS_SUBROUTINE(BEFORE_ASYNC_WITH)
EMIT_AS_SUBROUTINE(SETUP_ASYNC_WITH)
EMIT_AS_SUBROUTINE(SETUP_WITH)
EMIT_AS_SUBROUTINE(WITH_CLEANUP_START)
EMIT_AS_SUBROUTINE(WITH_CLEANUP_FINISH)
EMIT_AS_SUBROUTINE(LOAD_METHOD)
EMIT_AS_SUBROUTINE(CALL_METHOD)
EMIT_AS_SUBROUTINE(CALL_FUNCTION)
EMIT_AS_SUBROUTINE(CALL_FUNCTION_KW)
EMIT_AS_SUBROUTINE(CALL_FUNCTION_EX)
EMIT_AS_SUBROUTINE(MAKE_FUNCTION)
EMIT_AS_SUBROUTINE(BUILD_SLICE)
EMIT_AS_SUBROUTINE(FORMAT_VALUE)
EMIT_AS_SUBROUTINE(EXTENDED_ARG)
