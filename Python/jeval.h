#include "Include/internal/ceval.h"
#include "Include/internal/pystate.h"

/* Special pseudo-instructions */
#define DECLARE_SPECIAL(name)  int _PyEval_FUNC_JIT_TARGET_II_##name (EvalContext *ctx, PyFrameObject *f, int jumpev)

DECLARE_SPECIAL(ERROR);
DECLARE_SPECIAL(FAST_YIELD);
DECLARE_SPECIAL(FAST_BLOCK_END);
DECLARE_SPECIAL(UNWIND_CLEANUP);
DECLARE_SPECIAL(NEXT_OPCODE);

#define CREATE_SIGNATURE(sigvar, ret_type, ...) \
    jit_type_t sigvar##_args[] = { __VA_ARGS__ }; \
    jit_type_t sigvar = jit_type_create_signature(jit_abi_cdecl, ret_type, sigvar##_args, sizeof(sigvar##_args)/sizeof(jit_type_t), 1);

#define CALL_INDIRECT(sig, funcval, ...) \
    do { \
        jit_value_t call_args[] = { __VA_ARGS__ }; \
        jit_insn_call_indirect(jd->func, (funcval), sig, call_args, sizeof(call_args)/sizeof(jit_value_t), JIT_CALL_NOTHROW); \
    } while (0)

#define CALL_INDIRECT_WITH_RET(retvar, sig, funcval, ...) \
    jit_value_t retvar; \
    do { \
        jit_value_t call_args[] = { __VA_ARGS__ }; \
        retvar = jit_insn_call_indirect(jd->func, (funcval), sig, call_args, sizeof(call_args)/sizeof(jit_value_t), JIT_CALL_NOTHROW); \
    } while (0)

#define CALL_NATIVE(sig, nfunc, ...) do { \
    jit_value_t call_args[] = { __VA_ARGS__ }; \
    jit_insn_call_native(jd->func, NULL, nfunc, sig, call_args, sizeof(call_args)/sizeof(jit_value_t), JIT_CALL_NOTHROW); \
} while (0)

#define CALL_NATIVE_WITH_RET(retvar, sig, nfunc, ...) \
    jit_value_t retvar; \
    do { \
        jit_value_t call_args[] = { __VA_ARGS__ }; \
        retvar = jit_insn_call_native(jd->func, NULL, nfunc, sig, call_args, sizeof(call_args)/sizeof(jit_value_t), JIT_CALL_NOTHROW); \
    } while (0)

#define HANDLE_RV(inval) do { \
    jit_insn_store(jd->func, jd->rv, (inval)); \
    jit_insn_branch_if(jd->func, jd->rv, &jd->j_special[0]); \
} while (0)

#define HANDLE_RV_INTERNAL(inval) do { \
    jit_insn_store(jd->func, jd->rv, (inval)); \
    jit_insn_branch_if(jd->func, jd->rv, &jd->j_special_internal[0]); \
} while (0)

#define LABEL(ptr)    jit_insn_label(jd->func, (ptr))

#define BRANCH(ptr)   jit_insn_branch(jd->func, (ptr))

#define JIT_NOP()     jit_insn_nop(jd->func)

#define ADD(v1, v2) \
    jit_insn_add(jd->func, (v1), (v2))

#define SUBTRACT(v1, v2) \
    jit_insn_sub(jd->func, (v1), (v2))

#define CONSTANT_INT(n) \
    jit_value_create_nint_constant(jd->func, jit_type_int, (n))

#define CONSTANT_NINT(n) \
    jit_value_create_nint_constant(jd->func, jit_type_nint, (n))

#define CONSTANT_PTR(p) \
    jit_value_create_nint_constant(jd->func, jit_type_void_ptr, (jit_nint)((void*)(p)))

#define SHIFT_RIGHT(v1, v2) \
    jit_insn_shr(jd->func, (v1), (v2))

#define GET_FIELD(ptrval, structname, fieldname, fieldtype) \
    jit_insn_load_relative(jd->func, (ptrval), offsetof(structname, fieldname), (fieldtype))

#define SET_FIELD(ptrval, structname, fieldname, fieldtype, val) \
    jit_insn_store_relative(jd->func, (ptrval), offsetof(structname, fieldname), jit_insn_convert(jd->func, (val), (fieldtype), 0))

#define INCREF(objval) \
    SET_REFCNT((objval), ADD(GET_REFCNT((objval)), CONSTANT_INT(1)))

#define GET_REFCNT(objval) \
    GET_FIELD((objval), PyObject, ob_refcnt, jit_type_nint)

#define SET_REFCNT(objval, val) \
    SET_FIELD((objval), PyObject, ob_refcnt, jit_type_nint, (val))

#if defined(Py_DEBUG) || defined(Py_TRACE_REFS)
#  define _DEALLOC(objval) do { \
        CREATE_SIGNATURE(sig, jit_type_void, jit_type_void_ptr); \
        CALL_NATIVE(sig, _Py_Dealloc, (objval)); \
    } while (0)
#else
#  define _DEALLOC(objval) do { \
        jit_value_t _dealloc_type = GET_FIELD((objval), PyObject, ob_type, jit_type_void_ptr); \
        jit_value_t _dealloc_func = GET_FIELD(_dealloc_type, PyTypeObject, tp_dealloc, jit_type_void_ptr); \
        CREATE_SIGNATURE(sig, jit_type_void, jit_type_void_ptr); \
        CALL_INDIRECT(sig, _dealloc_func, (objval)); \
    } while (0)
#endif

#define DECREF(_objval) do { \
    jit_value_t _obj = (_objval); \
    jit_label_t skip_dealloc = jit_label_undefined; \
    jit_value_t new_refcount = SUBTRACT(GET_REFCNT(_obj), CONSTANT_INT(1)); \
    SET_REFCNT(_obj, new_refcount); \
    BRANCH_IF_NOT_ZERO(new_refcount, &skip_dealloc); \
    _DEALLOC(_obj); \
    LABEL(&skip_dealloc); \
} while (0);

#define XDECREF(_objval) do { \
    jit_value_t __obj = (_objval); \
    jit_label_t skip_if_null = jit_label_undefined; \
    BRANCH_IF_ZERO(__obj, &skip_if_null); \
    DECREF(__obj); \
    LABEL(&skip_if_null); \
} while (0)

#define PUSH(objval) do { \
    jit_insn_store_relative(jd->func, jd->stack_pointer, 0, (objval)); \
    jit_insn_store(jd->func, jd->stack_pointer, ADD(jd->stack_pointer, CONSTANT_INT(sizeof(PyObject*)))); \
} while (0)

#define STACKADJ(n) \
    jit_insn_store(jd->func, jd->stack_pointer, ADD(jd->stack_pointer, CONSTANT_INT(n * sizeof(PyObject*))))

#define TOP() \
     LOAD(SUBTRACT(jd->stack_pointer, CONSTANT_INT(sizeof(PyObject*))), jit_type_void_ptr)

#define POP() \
    (jit_insn_store(jd->func, jd->stack_pointer, SUBTRACT(jd->stack_pointer, CONSTANT_INT(sizeof(PyObject*)))), \
     LOAD(jd->stack_pointer, jit_type_void_ptr))


#define GET_INDEX(ptrval, elemtype, indexval) \
    jit_insn_load_elem(jd->func, (ptrval), (indexval), elemtype)

#define SET_INDEX(ptrval, elemtype, indexval, val) \
    jit_insn_store_elem(jd->func, (ptrval), (indexval), jit_insn_convert(jd->func, (val), (elemtype), 0))

#define LOAD(ptrval, fieldtype) \
    jit_insn_load_relative(jd->func, (ptrval), 0, (fieldtype))

#define STORE(ptrval, fieldtype, val) \
    jit_insn_store_relative(jd->func, (ptrval), 0, jit_insn_convert(jd->func, (val), (fieldtype), 0))

#define CRASH() \
    STORE(CONSTANT_PTR(0), jit_type_int, CONSTANT_INT(0));

// TODO: This needs to be adjusted depending on the configuration of _Py_atomic_int
#define LOAD_EVAL_BREAKER() \
    LOAD(CONSTANT_PTR(&_PyRuntime.ceval.eval_breaker._value), jit_type_int)

/* Check eval breaker, and jump to handler if set. This can only be used
   in instructions with no jumping, since it assumes the next instruction
   is computed using f_lasti.
 */
#define CHECK_EVAL_BREAKER() \
    BRANCH_IF_NOT_ZERO(LOAD_EVAL_BREAKER(), &jd->j_special[JIT_RC_NEXT_OPCODE]);

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

#define JIT_FOR(op) \
    void _PyJIT_EMIT_TARGET_##op (JITData *jd, int next_instr_index, int opcode, int oparg)

#define JIT_AS_SUBROUTINE(op) \
    void _PyJIT_EMIT_TARGET_##op (JITData *jd, int next_instr_index, int opcode, int oparg) { \
        SET_FIELD(jd->ctx, EvalContext, stack_pointer, jit_type_void_ptr, jd->stack_pointer); \
        CREATE_SIGNATURE(instr_sig, jit_type_int, jit_type_void_ptr, jit_type_void_ptr, jit_type_int, jit_type_int, jit_type_int, jit_type_int); \
        CALL_NATIVE_WITH_RET(tmprv, instr_sig, opcode_function_table[opcode], jd->ctx, jd->f, CONSTANT_INT(next_instr_index), CONSTANT_INT(opcode), CONSTANT_INT(oparg), /*jumpev=*/ CONSTANT_INT(0)); \
        jit_insn_store(jd->func, jd->stack_pointer, GET_FIELD(jd->ctx, EvalContext, stack_pointer, jit_type_void_ptr)); \
        HANDLE_RV_INTERNAL(tmprv); \
    }

/* Set next_instr_index based on the current f_lasti. This must be done
   when a regular instruction jumps to a special handler. */
#define SET_NEXT_INSTR_INDEX() do { \
        /* ctx->next_instr_index = (f->f_lasti / 2) + 1 */ \
        jit_value_t f_lasti_val = GET_FIELD(jd->f, PyFrameObject, f_lasti, jit_type_int); \
        jit_value_t computed_next_instr_index = ADD(SHIFT_RIGHT(f_lasti_val, CONSTANT_INT(1)), CONSTANT_INT(1)); \
        SET_FIELD(jd->ctx, EvalContext, next_instr_index, jit_type_int, computed_next_instr_index); \
} while (0)

#define JIT_SPECIAL(op) \
    void _PyJIT_EMIT_SPECIAL_##op (JITData *jd)

#define GET_CTX_NEXT_INSTR_INDEX() \
    GET_FIELD(jd->ctx, EvalContext, next_instr_index, jit_type_int)

#define SET_CTX_NEXT_INSTR_INDEX(val) \
    SET_FIELD(jd->ctx, EvalContext, next_instr_index, jit_type_int, (val))

#define JIT_SPECIAL_AS_SUBROUTINE(op) \
    void _PyJIT_EMIT_SPECIAL_##op (JITData *jd) { \
        SET_FIELD(jd->ctx, EvalContext, stack_pointer, jit_type_void_ptr, jd->stack_pointer); \
        CREATE_SIGNATURE(sig, jit_type_int, jit_type_void_ptr, jit_type_void_ptr, jit_type_int, jit_type_int); \
        CALL_NATIVE_WITH_RET(tmprv, sig, _PyEval_FUNC_JIT_TARGET_II_##op, jd->ctx, jd->f, GET_CTX_NEXT_INSTR_INDEX(), /*jumpev=*/ CONSTANT_INT(0)); \
        jit_insn_store(jd->func, jd->stack_pointer, GET_FIELD(jd->ctx, EvalContext, stack_pointer, jit_type_void_ptr)); \
        HANDLE_RV_INTERNAL(tmprv); \
    }

#define GETLOCAL(i) \
    GET_INDEX(jd->fastlocals, jit_type_void_ptr, CONSTANT_INT(i))

#define SETLOCAL(i, val) do { \
    jit_value_t tmp = GETLOCAL(i); \
    SET_INDEX(jd->fastlocals, jit_type_void_ptr, CONSTANT_INT(i), (val)); \
    XDECREF(tmp); \
} while (0)

#define BRANCH_IF_ZERO(val, labelptr) \
    jit_insn_branch_if_not(jd->func, (val), (labelptr))

#define BRANCH_IF_NOT_ZERO(val, labelptr) \
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
#define PYERR_OCCURRED()  GET_FIELD(TSTATE(), PyThreadState, curexc_type, jit_type_void_ptr)
#define TSTATE()    LOAD(CONSTANT_PTR(&_PyRuntime.gilstate.tstate_current._value), jit_type_void_ptr)

JIT_SPECIAL(FLOW) {
    // Index 0 is special, it dispatches to the other handlers based on 'rv'
    jit_insn_jump_table(jd->func, jd->rv, jd->j_special_internal, JIT_RC_EXIT + 1);
}

JIT_SPECIAL(JUMP) {
    Py_ssize_t inst_count = PyBytes_GET_SIZE(jd->co->co_code)/sizeof(_Py_CODEUNIT);
    jit_value_t index_val = GET_CTX_NEXT_INSTR_INDEX();
    jit_insn_jump_table(jd->func, index_val, jd->jmptab, inst_count);
}

JIT_SPECIAL_AS_SUBROUTINE(NEXT_OPCODE)
JIT_SPECIAL_AS_SUBROUTINE(ERROR)
JIT_SPECIAL_AS_SUBROUTINE(FAST_BLOCK_END)
JIT_SPECIAL_AS_SUBROUTINE(FAST_YIELD)
JIT_SPECIAL_AS_SUBROUTINE(UNWIND_CLEANUP)

JIT_SPECIAL(EXIT) {
    jit_insn_return(jd->func, NULL);
}

JIT_FOR(INVALID_OPCODE) {
    Py_UNREACHABLE();
}

JIT_FOR(NOP) {
}

typedef struct {
    jit_label_t err;
    jit_value_t oparg;
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

JIT_FOR(LOAD_FAST) {
    INIT_INFO(LOAD_FAST) {
        info->err = jit_label_undefined;
        info->oparg = jit_value_create(jd->func, jit_type_int);
        INSTALL_HANDLERS(LOAD_FAST);
    }
    jit_insn_store(jd->func, info->oparg, CONSTANT_INT(oparg));
    jit_value_t v = GETLOCAL(oparg);
    BRANCH_IF_ZERO(v, &info->err);
    INCREF(v);
    PUSH(v);
    //FAST_DISPATCH();
}

JIT_FOR(LOAD_CONST) {
    PyObject *obj = PyTuple_GET_ITEM(jd->co->co_consts, oparg);
    assert(obj != NULL);
    jit_value_t v = CONSTANT_PTR(obj);
    INCREF(v);
    PUSH(v);
    //FAST_DISPATCH();
}

JIT_FOR(STORE_FAST) {
    jit_value_t v = POP();
    SETLOCAL(oparg, v);
    //FAST_DISPATCH();
}


JIT_AS_SUBROUTINE(POP_TOP)
JIT_AS_SUBROUTINE(ROT_TWO)
JIT_AS_SUBROUTINE(ROT_THREE)
JIT_AS_SUBROUTINE(DUP_TOP)
JIT_AS_SUBROUTINE(DUP_TOP_TWO)
JIT_AS_SUBROUTINE(UNARY_POSITIVE)
JIT_AS_SUBROUTINE(UNARY_NEGATIVE)
JIT_AS_SUBROUTINE(UNARY_NOT)
JIT_AS_SUBROUTINE(UNARY_INVERT)
JIT_AS_SUBROUTINE(BINARY_POWER)
JIT_AS_SUBROUTINE(BINARY_MULTIPLY)
JIT_AS_SUBROUTINE(BINARY_MATRIX_MULTIPLY)
JIT_AS_SUBROUTINE(BINARY_TRUE_DIVIDE)
JIT_AS_SUBROUTINE(BINARY_FLOOR_DIVIDE)
JIT_AS_SUBROUTINE(BINARY_MODULO)
JIT_AS_SUBROUTINE(BINARY_ADD)
JIT_AS_SUBROUTINE(BINARY_SUBTRACT)
JIT_AS_SUBROUTINE(BINARY_SUBSCR)
JIT_AS_SUBROUTINE(BINARY_LSHIFT)
JIT_AS_SUBROUTINE(BINARY_RSHIFT)
JIT_AS_SUBROUTINE(BINARY_AND)
JIT_AS_SUBROUTINE(BINARY_XOR)
JIT_AS_SUBROUTINE(BINARY_OR)
JIT_AS_SUBROUTINE(LIST_APPEND)
JIT_AS_SUBROUTINE(SET_ADD)
JIT_AS_SUBROUTINE(INPLACE_POWER)
JIT_AS_SUBROUTINE(INPLACE_MULTIPLY)
JIT_AS_SUBROUTINE(INPLACE_MATRIX_MULTIPLY)
JIT_AS_SUBROUTINE(INPLACE_TRUE_DIVIDE)
JIT_AS_SUBROUTINE(INPLACE_FLOOR_DIVIDE)
JIT_AS_SUBROUTINE(INPLACE_MODULO)
JIT_AS_SUBROUTINE(INPLACE_ADD)
JIT_AS_SUBROUTINE(INPLACE_SUBTRACT)
JIT_AS_SUBROUTINE(INPLACE_LSHIFT)
JIT_AS_SUBROUTINE(INPLACE_RSHIFT)
JIT_AS_SUBROUTINE(INPLACE_AND)
JIT_AS_SUBROUTINE(INPLACE_XOR)
JIT_AS_SUBROUTINE(INPLACE_OR)
JIT_AS_SUBROUTINE(STORE_SUBSCR)
JIT_AS_SUBROUTINE(STORE_ANNOTATION)
JIT_AS_SUBROUTINE(DELETE_SUBSCR)
JIT_AS_SUBROUTINE(PRINT_EXPR)
JIT_AS_SUBROUTINE(RAISE_VARARGS)
JIT_AS_SUBROUTINE(RETURN_VALUE)
JIT_AS_SUBROUTINE(GET_AITER)
JIT_AS_SUBROUTINE(GET_ANEXT)
JIT_AS_SUBROUTINE(GET_AWAITABLE)
JIT_AS_SUBROUTINE(YIELD_FROM)
JIT_AS_SUBROUTINE(YIELD_VALUE)
JIT_AS_SUBROUTINE(POP_EXCEPT)
JIT_AS_SUBROUTINE(POP_BLOCK)
JIT_AS_SUBROUTINE(END_FINALLY)
JIT_AS_SUBROUTINE(LOAD_BUILD_CLASS)
JIT_AS_SUBROUTINE(STORE_NAME)
JIT_AS_SUBROUTINE(DELETE_NAME)
JIT_AS_SUBROUTINE(UNPACK_SEQUENCE)
JIT_AS_SUBROUTINE(UNPACK_EX)
JIT_AS_SUBROUTINE(STORE_ATTR)
JIT_AS_SUBROUTINE(DELETE_ATTR)
JIT_AS_SUBROUTINE(STORE_GLOBAL)
JIT_AS_SUBROUTINE(DELETE_GLOBAL)
JIT_AS_SUBROUTINE(LOAD_NAME)
JIT_AS_SUBROUTINE(LOAD_GLOBAL)
JIT_AS_SUBROUTINE(DELETE_FAST)
JIT_AS_SUBROUTINE(DELETE_DEREF)
JIT_AS_SUBROUTINE(LOAD_CLOSURE)
JIT_AS_SUBROUTINE(LOAD_CLASSDEREF)
JIT_AS_SUBROUTINE(LOAD_DEREF)
JIT_AS_SUBROUTINE(STORE_DEREF)
JIT_AS_SUBROUTINE(BUILD_STRING)
JIT_AS_SUBROUTINE(BUILD_TUPLE)
JIT_AS_SUBROUTINE(BUILD_LIST)
JIT_AS_SUBROUTINE(BUILD_TUPLE_UNPACK_WITH_CALL)
JIT_AS_SUBROUTINE(BUILD_TUPLE_UNPACK)
JIT_AS_SUBROUTINE(BUILD_LIST_UNPACK)
JIT_AS_SUBROUTINE(BUILD_SET)
JIT_AS_SUBROUTINE(BUILD_SET_UNPACK)
JIT_AS_SUBROUTINE(BUILD_MAP)
JIT_AS_SUBROUTINE(SETUP_ANNOTATIONS)
JIT_AS_SUBROUTINE(BUILD_CONST_KEY_MAP)
JIT_AS_SUBROUTINE(BUILD_MAP_UNPACK)
JIT_AS_SUBROUTINE(BUILD_MAP_UNPACK_WITH_CALL)
JIT_AS_SUBROUTINE(MAP_ADD)
JIT_AS_SUBROUTINE(LOAD_ATTR)
JIT_AS_SUBROUTINE(COMPARE_OP)
JIT_AS_SUBROUTINE(IMPORT_NAME)
JIT_AS_SUBROUTINE(IMPORT_STAR)
JIT_AS_SUBROUTINE(IMPORT_FROM)
JIT_AS_SUBROUTINE(JUMP_FORWARD)
JIT_AS_SUBROUTINE(POP_JUMP_IF_FALSE)
JIT_AS_SUBROUTINE(POP_JUMP_IF_TRUE)
JIT_AS_SUBROUTINE(JUMP_IF_FALSE_OR_POP)
JIT_AS_SUBROUTINE(JUMP_IF_TRUE_OR_POP)


JIT_FOR(JUMP_ABSOLUTE) {
    JUMPTO(oparg, 1);
}

JIT_AS_SUBROUTINE(GET_ITER)
JIT_AS_SUBROUTINE(GET_YIELD_FROM_ITER)


JIT_FOR(FOR_ITER) {
    jit_label_t handle_null = jit_label_undefined;
    jit_label_t cleanup = jit_label_undefined;
    jit_label_t next_instruction = jit_label_undefined;
    jit_value_t iter_obj = TOP();
    jit_value_t type_obj = GET_FIELD(iter_obj, PyObject, ob_type, jit_type_void_ptr);
    jit_value_t tp_iternext = GET_FIELD(type_obj, PyTypeObject, tp_iternext, jit_type_void_ptr);
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

JIT_AS_SUBROUTINE(BREAK_LOOP)
JIT_AS_SUBROUTINE(CONTINUE_LOOP)
JIT_AS_SUBROUTINE(SETUP_LOOP)
JIT_AS_SUBROUTINE(SETUP_EXCEPT)
JIT_AS_SUBROUTINE(SETUP_FINALLY)
JIT_AS_SUBROUTINE(BEFORE_ASYNC_WITH)
JIT_AS_SUBROUTINE(SETUP_ASYNC_WITH)
JIT_AS_SUBROUTINE(SETUP_WITH)
JIT_AS_SUBROUTINE(WITH_CLEANUP_START)
JIT_AS_SUBROUTINE(WITH_CLEANUP_FINISH)
JIT_AS_SUBROUTINE(LOAD_METHOD)
JIT_AS_SUBROUTINE(CALL_METHOD)
JIT_AS_SUBROUTINE(CALL_FUNCTION)
JIT_AS_SUBROUTINE(CALL_FUNCTION_KW)
JIT_AS_SUBROUTINE(CALL_FUNCTION_EX)
JIT_AS_SUBROUTINE(MAKE_FUNCTION)
JIT_AS_SUBROUTINE(BUILD_SLICE)
JIT_AS_SUBROUTINE(FORMAT_VALUE)
JIT_AS_SUBROUTINE(EXTENDED_ARG)
