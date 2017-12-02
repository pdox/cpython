/* This is required to get MAP_ANONYMOUS with std=c99 */
#define _BSD_SOURCE
#include <sys/mman.h>
#include <stddef.h>

#include "Python.h"
#include "opcode.h"
#include "frameobject.h"
#include "internal/jit.h"
#include <jit/jit.h>

typedef int (*PyJITTargetFunction)(EvalContext *ctx, PyFrameObject *f, int next_instr_index, int opcode, int oparg, int jumpev);

int _PyEval_FUNC_JIT__unknown_opcode(EvalContext *ctx, PyFrameObject *f, int next_instr_index, int opcode, int oparg, int jumpev) {
    Py_UNREACHABLE();
}

#include "opcode_function_table.h"
#include "jeval.h"
#include "opcode_emitter_table.h"


PyJITSpecialEmitterFunction special_emitter_table[] = {
        _PyJIT_EMIT_SPECIAL_FLOW,
        _PyJIT_EMIT_SPECIAL_JUMP,
        _PyJIT_EMIT_SPECIAL_FAST_YIELD,
        _PyJIT_EMIT_SPECIAL_FAST_BLOCK_END,
        _PyJIT_EMIT_SPECIAL_ERROR,
        _PyJIT_EMIT_SPECIAL_UNWIND_CLEANUP,
        _PyJIT_EMIT_SPECIAL_NEXT_OPCODE,
        _PyJIT_EMIT_SPECIAL_EXIT
};

static void
translate_bytecode(JITData *jd, PyCodeObject *co)
{
    int is_gen = (co->co_flags & (CO_GENERATOR | CO_COROUTINE | CO_ASYNC_GENERATOR));
    _Py_CODEUNIT *code = (_Py_CODEUNIT*)PyBytes_AS_STRING(co->co_code);
    Py_ssize_t i;
    Py_ssize_t inst_count = PyBytes_GET_SIZE(co->co_code)/sizeof(_Py_CODEUNIT);

    jd->rv = jit_value_create(jd->func, jit_type_int);

    /* Arguments: ctx, f, sp */
    jd->ctx = jit_value_get_param(jd->func, 0);
    jd->f = jit_value_get_param(jd->func, 1);
    jd->stack_pointer = jit_value_get_param(jd->func, 2);
    jd->fastlocals = ADD(jd->f, CONSTANT_NINT(offsetof(PyFrameObject, f_localsplus)));
    jd->move_entry_list = NULL;

    for (i = 0; i < 256; i++) {
        jd->priv[i] = NULL;
        jd->handlers[i] = NULL;
    }

    // We have to do signal check immediately upon function entry.
    special_emitter_table[JIT_RC_NEXT_OPCODE](jd);

    if (is_gen) {
        /* jump to next_instr */
        jit_value_t v_index = LOAD_FIELD(jd->ctx, EvalContext, next_instr_index, jit_type_int);
        jit_insn_jump_table(jd->func, v_index, jd->jmptab, inst_count);
    }

    for (i = 0; i < inst_count; i++) {
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
        STORE_FIELD(jd->f, PyFrameObject, f_lasti, jit_type_int, CONSTANT_INT(i * sizeof(_Py_CODEUNIT)));

        // Emit instruction
        opcode_emitter_table[opcode](jd, i + 1, opcode, oparg);
    }
    CRASH();

    /* Add extra handling sections */
    for (i = 0; i < 256; i++) {
        if (jd->handlers[i]) {
            jd->handlers[i](jd, i);
            // Force crash if handler continues
            CRASH();
        }
        // Free private info
        FREE_INFO(i);
    }

    /* Special sections. While in a special section, ctx->next_instr_index must
       be valid and point to the next instruction to execute (for next dispatch).
       There are two entrypoints to each special section. One for instructions,
       and one for coming from another special section.
     */
    for (i = 0; i <= JIT_RC_EXIT; i++) {
        LABEL(&jd->j_special[i]);
        SET_NEXT_INSTR_INDEX();
        LABEL(&jd->j_special_internal[i]);
        special_emitter_table[i](jd);
        BRANCH(&jd->j_special_internal[JIT_RC_JUMP]);
    }

    /* Move extra sections to end */
    {
        move_entry *m = jd->move_entry_list;
        move_entry *next;
        while (m != NULL) {
            next = m->next;
            jit_insn_move_blocks_to_end(jd->func, m->from_label, m->to_label);
            PyMem_RawFree(m);
            m = next;
        }
        jd->move_entry_list = NULL;
    }
}

jit_context_t gcontext = NULL;

static int
_PyJIT_CodeGen(PyCodeObject *co) {
    if (gcontext == NULL) {
        gcontext = jit_context_create();
    }
    Py_ssize_t inst_count = PyBytes_GET_SIZE(co->co_code)/sizeof(_Py_CODEUNIT);
    JITData *jd = PyMem_RawMalloc(sizeof(JITData) + inst_count * sizeof(jit_label_t));
    if (jd == NULL) {
        PyErr_NoMemory();
        return -1;
    }
    jit_context_build_start(gcontext);
    jd->co = co;

    Py_ssize_t i;
    for (i = 0; i <= JIT_RC_EXIT; i++) {
        jd->j_special[i] = jit_label_undefined;
        jd->j_special_internal[i] = jit_label_undefined;
    }
    for (i = 0; i < inst_count; i++) {
        jd->jmptab[i] = jit_label_undefined;
    }

    CREATE_SIGNATURE(entry_sig, jit_type_void, jit_type_void_ptr, jit_type_void_ptr, jit_type_void_ptr);
    jd->func = jit_function_create(gcontext, entry_sig);
    translate_bytecode(jd, co);
    jit_function_set_optimization_level(jd->func, jit_function_get_max_optimization_level());
    int ok = jit_function_compile(jd->func);
    if (!ok) {
        Py_FatalError("JIT compile failed");
    }
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
    ((PyJITEntryFunction)jd->entry)(ctx, f, sp);
    return 0;
}
