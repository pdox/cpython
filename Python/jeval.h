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

#define CALL_NATIVE_WITH_RET(sig, retvar, nfunc, ...) \
    jit_value_t call_args[] = { __VA_ARGS__ }; \
    jit_value_t retvar = jit_insn_call_native(jd->func, NULL, nfunc, sig, call_args, sizeof(call_args)/sizeof(jit_value_t), JIT_CALL_NOTHROW)

#define HANDLE_RV(inval) do { \
    jit_insn_store(jd->func, jd->rv, (inval)); \
    jit_insn_branch_if(jd->func, jd->rv, &jd->j_special[0]); \
} while (0)

#define LABEL(ptr)    jit_insn_label(jd->func, (ptr))

#define BRANCH(ptr)   jit_insn_branch(jd->func, (ptr))

#define ADD(v1, v2) \
    jit_insn_add(jd->func, (v1), (v2))

#define SUBTRACT(v1, v2) \
    jit_insn_sub(jd->func, (v1), (v2))

#define INSTR_OFFSET() \
    SUBTRACT(READ_NEXT_INSTR(), READ_FIRST_INSTR())

#define CONSTANT_INT(n) \
    jit_value_create_nint_constant(jd->func, jit_type_int, (n))

#define SHIFT_RIGHT(v1, v2) \
    jit_insn_shr(jd->func, (v1), (v2))

#define GET_FIELD(ptrval, structname, fieldname, fieldtype) \
    jit_insn_load_relative(jd->func, (ptrval), offsetof(structname, fieldname), (fieldtype))

#define SET_FIELD(ptrval, structname, fieldname, fieldtype, val) \
    jit_insn_store_relative(jd->func, (ptrval), offsetof(structname, fieldname), jit_insn_convert(jd->func, (val), (fieldtype), 0))

#define READ_NEXT_INSTR() \
    GET_FIELD(jd->ctx, EvalContext, next_instr, jit_type_void_ptr)

#define READ_FIRST_INSTR() \
    GET_FIELD(jd->ctx, EvalContext, first_instr, jit_type_void_ptr)

#define SET_NEXT_INSTR(v) \
    SET_FIELD(jd->ctx, EvalContext, next_instr, jit_type_void_ptr, (v))

/* IR for popping a stack element and returning the value */


#define JIT_FOR(op) \
    void _PyJIT_EMIT_TARGET_##op (JITData *jd, int opcode, int oparg)

#define JIT_AS_SUBROUTINE(op) \
    void _PyJIT_EMIT_TARGET_##op (JITData *jd, int opcode, int oparg) { \
        SET_FIELD(jd->ctx, EvalContext, stack_pointer, jit_type_void_ptr, jd->stack_pointer); \
        CREATE_SIGNATURE(instr_sig, jit_type_int, jit_type_void_ptr, jit_type_void_ptr, jit_type_int, jit_type_int, jit_type_int); \
        CALL_NATIVE_WITH_RET(instr_sig, tmprv, opcode_function_table[opcode], jd->ctx, jd->f, CONSTANT_INT(opcode), CONSTANT_INT(oparg), /*jumpev=*/ CONSTANT_INT(0)); \
        jit_insn_store(jd->func, jd->stack_pointer, GET_FIELD(jd->ctx, EvalContext, stack_pointer, jit_type_void_ptr)); \
        HANDLE_RV(tmprv); \
    }

#define JIT_AS_SPECIAL(op) \
    void _PyJIT_EMIT_SPECIAL_##op (JITData *jd) { \
        SET_FIELD(jd->ctx, EvalContext, stack_pointer, jit_type_void_ptr, jd->stack_pointer); \
        CREATE_SIGNATURE(sig, jit_type_int, jit_type_void_ptr, jit_type_void_ptr, jit_type_int); \
        CALL_NATIVE_WITH_RET(sig, tmprv, _PyEval_FUNC_JIT_TARGET_II_##op, jd->ctx, jd->f, /*jumpev=*/ CONSTANT_INT(0)); \
        jit_insn_store(jd->func, jd->stack_pointer, GET_FIELD(jd->ctx, EvalContext, stack_pointer, jit_type_void_ptr)); \
        HANDLE_RV(tmprv); \
    }

JIT_AS_SPECIAL(NEXT_OPCODE)
JIT_AS_SPECIAL(ERROR)
JIT_AS_SPECIAL(FAST_BLOCK_END)
JIT_AS_SPECIAL(FAST_YIELD)
JIT_AS_SPECIAL(UNWIND_CLEANUP)

JIT_FOR(INVALID_OPCODE) {
    Py_UNREACHABLE();
}

JIT_FOR(NOP) {
}

JIT_AS_SUBROUTINE(LOAD_FAST)
JIT_AS_SUBROUTINE(LOAD_CONST)
JIT_AS_SUBROUTINE(STORE_FAST)
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
JIT_AS_SUBROUTINE(JUMP_ABSOLUTE)
JIT_AS_SUBROUTINE(GET_ITER)
JIT_AS_SUBROUTINE(GET_YIELD_FROM_ITER)
JIT_AS_SUBROUTINE(FOR_ITER)
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
