/* This is required to get MAP_ANONYMOUS with std=c99 */
#define _BSD_SOURCE
#include <sys/mman.h>
#include <stddef.h>

#include "Python.h"
#include "opcode.h"
#include "frameobject.h"
#include "ir.h"
#include "internal/jit.h"

extern int Py_JITFlag;
extern int Py_JITDebugFlag;

typedef int (*PyJITTargetFunction)(EvalContext *ctx, PyFrameObject *f, int next_instr_index, int opcode, int oparg, int jumpev);

int _PyEval_FUNC_JIT__unknown_opcode(EvalContext *ctx, PyFrameObject *f, int next_instr_index, int opcode, int oparg, int jumpev) {
    Py_UNREACHABLE();
}

#include "opcode_function_table.h"
#include "jeval.h"
#include "opcode_emitter_table.h"
#include "opcode_names.h"


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

    /* Setup temporary stack */
    jd->tmpstack_size = 0;
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
    jd->sig_io = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr);
    jd->sig_ioo = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    jd->sig_iooo = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);

    jd->rv = ir_value_new(jd->func, ir_type_int);

    /* libjit miscompiles if you don't set a value before using it.
       Since we can't guarantee block ordering, be on the safe side and assign it on entry.
       TODO: Fix this during compilation, in ir_libjit.c.
     */
    ir_set_value(jd->func, jd->rv, ir_constant_int(jd->func, 0, NULL));

    /* Arguments: ctx, f, sp */
    jd->ctx = ir_func_get_argument(jd->func, 0);
    jd->f = ir_func_get_argument(jd->func, 1);
    jd->stack_pointer = ir_func_get_argument(jd->func, 2);
    jd->fastlocals = ir_get_element_ptr(jd->func, jd->f, offsetof(PyFrameObject, f_localsplus), ir_type_pyobject_ptr, "f_localsplus");
    jd->retval = ir_value_new(jd->func, ir_type_pyobject_ptr);
    ir_set_value(jd->func, jd->retval, LOAD_FIELD(jd->ctx, EvalContext, retval, ir_type_pyobject_ptr));
    jd->why = ir_value_new(jd->func, ir_type_int);
    ir_set_value(jd->func, jd->why, LOAD_FIELD(jd->ctx, EvalContext, why, ir_type_int));

    jd->move_entry_list = NULL;

    for (i = 0; i < 256; i++) {
        jd->priv[i] = NULL;
        jd->handlers[i] = NULL;
    }

    // We need to do an eval breaker check immediately upon function entry.
    // Jump to the internal label, because ctx->next_instr_index is set upon entry.
    BRANCH_IF(LOAD_EVAL_BREAKER(), jd->j_special_internal[JIT_RC_NEXT_OPCODE], IR_UNLIKELY);

    if (is_gen) {
        /* jump to next_instr */
        ir_value v_index = LOAD_FIELD(jd->ctx, EvalContext, next_instr_index, ir_type_int);
        ir_jumptable(jd->func, v_index, jd->jmptab, inst_count);
    }

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
        LABEL(jd->j_special[i]);
        SET_NEXT_INSTR_INDEX();
        LABEL(jd->j_special_internal[i]);
        special_emitter_table[i](jd);
        BRANCH(jd->j_special_internal[JIT_RC_JUMP]);
    }

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

static int
_PyJIT_CodeGen(PyCodeObject *co) {
    char namebuf[32];
    ir_type sig;
    Py_ssize_t i;
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

    /* func(ctx, f, sp); */
    ir_type argtypes[] = { ir_type_evalcontext_ptr, ir_type_pyframeobject_ptr, ir_type_pyobject_ptr_ptr };
    sig = ir_create_function_type(jd->context, ir_type_void, sizeof(argtypes)/sizeof(argtypes[0]), argtypes);
    jd->func = ir_func_new(jd->context, sig);

    for (i = 0; i <= JIT_RC_EXIT; i++) {
        sprintf(namebuf, "j_special_%ld", (long)i);
        jd->j_special[i] = ir_label_new(jd->func, namebuf);
        sprintf(namebuf, "j_special_internal_%ld", (long)i);
        jd->j_special_internal[i] = ir_label_new(jd->func, namebuf);
    }
    for (i = 0; i < inst_count; i++) {
        sprintf(namebuf, "inst_%ld", (long)i);
        jd->jmptab[i] = ir_label_new(jd->func, namebuf);
    }
    translate_bytecode(jd, co);
#ifdef IR_DEBUG
    ir_func_verify(jd->func);
#endif
    if (Py_JITDebugFlag > 0) {
        ir_func_dump_file(jd->func, "/tmp/before.ir", "Before lowering");
    }
    ir_lower(jd->func, jd->fastlocals, jd->stack_pointer, jd->j_special[JIT_RC_NEXT_OPCODE]);
    if (Py_JITDebugFlag > 0) {
        ir_func_dump_file(jd->func, "/tmp/after.ir", "After lowering");
    }
#ifdef IR_DEBUG
    ir_func_verify(jd->func);
#endif
    if (Py_JITFlag == 1) {
        jd->entry = (PyJITEntryFunction)ir_libjit_compile(jd->func);
    } else if (Py_JITFlag == 2) {
        jd->entry = (PyJITEntryFunction)ir_llvm_compile(jd->func);
    } else {
        Py_FatalError("invalid PYJIT value");
    }
    ir_context_destroy(jd->context);
    jd->context = NULL;
    jd->func = NULL;
    co->co_jit_data = jd;
    return 0;
}

int _PyJIT_Execute(EvalContext *ctx, PyFrameObject *f, PyObject **sp) {
    JITData *jd;
    if (ctx->co->co_jit_data == NULL) {
        if (Py_JITDebugFlag > 0) {
            fprintf(stderr, "Entering CodeGen for %s from %s (f->f_code == %p)\n",
                PyUnicode_AsUTF8(f->f_code->co_name),
                PyUnicode_AsUTF8(f->f_code->co_filename),
                f->f_code);
        }
        if (_PyJIT_CodeGen(ctx->co) != 0) {
            return -1;
        }
        assert(ctx->co->co_jit_data != NULL);
    }
    jd = (JITData*)ctx->co->co_jit_data;
    jd->entry(ctx, f, sp);
    return 0;
}
