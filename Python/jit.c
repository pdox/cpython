/* This is required to get MAP_ANONYMOUS with std=c99 */
#define _BSD_SOURCE
#include <sys/mman.h>
#include <stddef.h>

#include "Python.h"
#include "opcode.h"
#include "frameobject.h"
#include "internal/jit.h"
#include <jit/jit.h>

typedef int (*PyJITTargetFunction)(EvalContext *ctx, PyFrameObject *f, int opcode, int oparg, int jumpev);

int _PyEval_FUNC_JIT__unknown_opcode(EvalContext *ctx, PyFrameObject *f, int opcode, int oparg, int jumpev) {
    assert(0);
}

#include "opcode_function_table.h"

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

#define CALL_NATIVE_RV(sig, nfunc, ...) \
do { \
    jit_value_t call_args[] = { __VA_ARGS__ }; \
    HANDLE_RV(jit_insn_call_native(jd->func, NULL, nfunc, sig, call_args, sizeof(call_args)/sizeof(jit_value_t), JIT_CALL_NOTHROW)); \
} while (0)

#define HANDLE_RV(inval) do { \
    jit_insn_store(jd->func, rv, (inval)); \
    jit_insn_branch_if(jd->func, rv, &jd->j_special[0]); \
} while (0)

#define LABEL(ptr)    jit_insn_label(jd->func, (ptr))

#define CALL_SPECIAL(name) \
    CALL_NATIVE_RV(special_sig, _PyEval_FUNC_JIT_TARGET_II_##name, v_ctx, v_f, CONSTANT_INT(0))

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
    GET_FIELD(v_ctx, EvalContext, next_instr, jit_type_void_ptr)

#define READ_FIRST_INSTR() \
    GET_FIELD(v_ctx, EvalContext, first_instr, jit_type_void_ptr)

#define SET_NEXT_INSTR(v) \
    SET_FIELD(v_ctx, EvalContext, next_instr, jit_type_void_ptr, (v))

static void
translate_bytecode(JITData *jd, PyCodeObject *co)
{
    int is_gen = (co->co_flags & (CO_GENERATOR | CO_COROUTINE | CO_ASYNC_GENERATOR));
    _Py_CODEUNIT *code = (_Py_CODEUNIT*)PyBytes_AS_STRING(co->co_code);
    Py_ssize_t i;
    Py_ssize_t inst_count = PyBytes_GET_SIZE(co->co_code)/sizeof(_Py_CODEUNIT);

    /* instruction signature */
    CREATE_SIGNATURE(instr_sig, jit_type_int, jit_type_void_ptr, jit_type_void_ptr, jit_type_int, jit_type_int, jit_type_int);
    CREATE_SIGNATURE(special_sig, jit_type_int, jit_type_void_ptr, jit_type_void_ptr, jit_type_int);

    /* Arguments: ctx, f */

    jit_value_t rv = jit_value_create(jd->func, jit_type_int);
    jit_value_t v_ctx = jit_value_get_param(jd->func, 0);
    jit_value_t v_f = jit_value_get_param(jd->func, 1);

    // We have to do signal check immediately upon function entry.

    // ctx, f, jumpev
    
    CALL_SPECIAL(NEXT_OPCODE);

    if (is_gen) {
        /* jump to next_instr */
        jit_value_t v_index = SHIFT_RIGHT(INSTR_OFFSET(), CONSTANT_INT(1));
        jit_insn_jump_table(jd->func, v_index, jd->jmptab, inst_count);
    }

    for (i = 0; i < inst_count; i++) {
        int istart = i;
        int opcode = _Py_OPCODE(code[i]);
        int oparg = _Py_OPARG(code[i]);
        LABEL(&jd->jmptab[i]);

        while (opcode == EXTENDED_ARG) {
            ++i;
            opcode = _Py_OPCODE(code[i]);
            oparg = (oparg << 8) | _Py_OPARG(code[i]);
            LABEL(&jd->jmptab[i]);
        }

        // Set f->f_lasti
        SET_FIELD(v_f, PyFrameObject, f_lasti, jit_type_int, CONSTANT_INT(istart * sizeof(_Py_CODEUNIT)));

        // Increment next_instr
        SET_NEXT_INSTR( ADD(READ_NEXT_INSTR(), CONSTANT_INT( sizeof(_Py_CODEUNIT) * (i+1 - istart) )));

        // Call function: ctx, f, opcode, oparg, jumpev
        CALL_NATIVE_RV(instr_sig, opcode_function_table[opcode], v_ctx, v_f, CONSTANT_INT(opcode), CONSTANT_INT(oparg), /*jumpev=*/ CONSTANT_INT(0));
    }

    /* Special sections */

    // Index 0 is special, it dispatches to the others.
    LABEL(&jd->j_special[0]);
    jit_insn_jump_table(jd->func, rv, jd->j_special, JIT_RC_EXIT + 1);

    LABEL(&jd->j_special[JIT_RC_JUMP]);
    jit_value_t v_index = SHIFT_RIGHT(INSTR_OFFSET(), CONSTANT_INT(1));
    jit_insn_jump_table(jd->func, v_index, jd->jmptab, inst_count);

    LABEL(&jd->j_special[JIT_RC_FAST_YIELD]);
    CALL_SPECIAL(FAST_YIELD);
    BRANCH(&jd->j_special[JIT_RC_JUMP]);

    LABEL(&jd->j_special[JIT_RC_FAST_BLOCK_END]);
    CALL_SPECIAL(FAST_BLOCK_END);
    BRANCH(&jd->j_special[JIT_RC_JUMP]);

    LABEL(&jd->j_special[JIT_RC_ERROR]);
    CALL_SPECIAL(ERROR);
    BRANCH(&jd->j_special[JIT_RC_JUMP]);

    LABEL(&jd->j_special[JIT_RC_UNWIND_CLEANUP]);
    CALL_SPECIAL(UNWIND_CLEANUP);
    BRANCH(&jd->j_special[JIT_RC_JUMP]);

    LABEL(&jd->j_special[JIT_RC_NEXT_OPCODE]);
    CALL_SPECIAL(NEXT_OPCODE);
    BRANCH(&jd->j_special[JIT_RC_JUMP]);

    LABEL(&jd->j_special[JIT_RC_EXIT]);
    jit_insn_return(jd->func, NULL);
}

jit_context_t gcontext = NULL;
jit_type_t sig_params[2];
jit_type_t signature;

static int
_PyJIT_CodeGen(PyCodeObject *co) {
    if (gcontext == NULL) {
        gcontext = jit_context_create();
        sig_params[0] = jit_type_void_ptr;
        sig_params[1] = jit_type_void_ptr;
        signature = jit_type_create_signature(jit_abi_cdecl, jit_type_void, sig_params, 2, 1);
    }
    Py_ssize_t inst_count = PyBytes_GET_SIZE(co->co_code)/sizeof(_Py_CODEUNIT);
    JITData *jd = PyMem_RawMalloc(sizeof(JITData) + inst_count * sizeof(jit_label_t));
    if (jd == NULL) {
        PyErr_NoMemory();
        return -1;
    }
    Py_ssize_t i;
    for (i = 0; i <= JIT_RC_EXIT; i++) {
        jd->j_special[i] = jit_label_undefined;
    }
    for (i = 0; i < inst_count; i++) {
        jd->jmptab[i] = jit_label_undefined;
    }

    jit_context_build_start(gcontext);
    jd->func = jit_function_create(gcontext, signature);

    translate_bytecode(jd, co);
    jit_function_set_optimization_level(jd->func, jit_function_get_max_optimization_level());
    int ok = jit_function_compile(jd->func);
    assert(ok);
    jd->entry = jit_function_to_closure(jd->func);
    jit_context_build_end(gcontext);

    co->co_jit_data = jd;
    return 0;
}

int _PyJIT_Execute(EvalContext *ctx, PyFrameObject *f, PyObject **sp) {
    JITData *jd;
    if (ctx->co->co_jit_data == NULL) {
        if (_PyJIT_CodeGen(ctx->co) != 0) {
            return -1;
        }
        assert(ctx->co->co_jit_data != NULL);
    }
    jd = (JITData*)ctx->co->co_jit_data;
    //fprintf(stderr, "Executing %s...\n", PyUnicode_AsUTF8(ctx->co->co_name));
    ctx->stack_pointer = sp;
    ((PyJITEntryFunction)jd->entry)(ctx, f);
    return 0;
}
