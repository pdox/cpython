/* This is required to get MAP_ANONYMOUS with std=c99 */
#define _BSD_SOURCE
#include <sys/mman.h>
#include <stddef.h>

#include "Python.h"
#include "frameobject.h"
#include "internal/jit.h"

typedef struct _CodePage {
    struct _CodePage *prev;
    struct _CodePage *next;
    char *base;
    char *pos;
    char *end;
} CodePage;

#define CODEPAGE_ALIGN 2097152
static CodePage *cp_head;
static CodePage *cp_tail;

typedef void (*PyJITTargetFunction)(EvalContext *);

void _PyEval_FUNC_JIT__unknown_opcode(EvalContext *ctx) {
    assert(0);
}

#include "opcode_function_table.h"

/* Special pseudo-instructions */
#define DECLARE_SPECIAL(name)  void _PyEval_FUNC_JIT_TARGET_II_##name (EvalContext *)

DECLARE_SPECIAL(NEXT_OPCODE);
DECLARE_SPECIAL(ERROR);
DECLARE_SPECIAL(FAST_YIELD);
DECLARE_SPECIAL(FAST_BLOCK_END);
DECLARE_SPECIAL(UNWIND_CLEANUP);
DECLARE_SPECIAL(NEXT_OPCODE);

static int
_PyJIT_GrowCodePage(Py_ssize_t needed) {
    CodePage *cp;
    uintptr_t alloc_size;
    if (cp_tail != NULL && needed <= (cp_tail->end - cp_tail->pos))
        return 0;

    alloc_size = (uintptr_t) _Py_ALIGN_UP(needed, CODEPAGE_ALIGN);
    cp = PyMem_RawMalloc(sizeof(CodePage));
    if (cp == NULL) {
        PyErr_NoMemory();
        return -1;
    }
    cp->prev = cp_tail;
    cp->next = NULL;
    cp->base = mmap(NULL, alloc_size, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
    if (cp->base == MAP_FAILED) {
        PyMem_Free(cp);
        PyErr_SetString(PyExc_RuntimeError, "JIT mmap error");
        return -1;
    }
    cp->pos = cp->base;
    cp->end = cp->base + alloc_size;
    if (cp_tail)
        cp_tail->next = cp;
    else
        cp_head = cp;
    cp_tail = cp;
    return 0;
}

Py_LOCAL_INLINE(char*)
current_code_pos(void) {
    return cp_tail->pos;
}

Py_LOCAL_INLINE(void)
insert_code(Py_ssize_t length, const char *data) {
    assert(length <= (cp_tail->end - cp_tail->pos));
    memcpy(&cp_tail->pos[0], data, length);
    cp_tail->pos += length;
}

Py_LOCAL_INLINE(void)
reloc_64(int offset, long value) {
    memcpy(&cp_tail->pos[offset], &value, 8);
}

Py_LOCAL_INLINE(void)
reloc_32(int offset, int value) {
    memcpy(&cp_tail->pos[offset], &value, 4);
}

Py_LOCAL_INLINE(void)
reloc_16(int offset, short value) {
    memcpy(&cp_tail->pos[offset], &value, 2);
}

Py_LOCAL_INLINE(void)
reloc_8(int offset, int value) {
    assert(-128 <= value && value < 128);
    memcpy(&cp_tail->pos[offset], &value, 1);
}

Py_LOCAL_INLINE(void)
insert_call(void *function_addr) {
    // movabs $function_addr, %rax
    insert_code(2, "\x48\xb8");
    memcpy(&cp_tail->pos[0], &function_addr, 8);
    cp_tail->pos += 8;
    // call *%rax
    insert_code(2, "\xff\xd0");
}

Py_LOCAL_INLINE(void)
finish_code(void) {
    cp_tail->pos = _Py_ALIGN_UP(cp_tail->pos, 32);
}

Py_LOCAL_INLINE(void)
insert_special_section(void *function_addr) {
    // mov %r12, %rdi
    insert_code(3, "\x4c\x89\xe7");

    // call function
    insert_call(function_addr);
}

static void _resume(EvalContext *ctx) {
    JITData *jd = (JITData*)ctx->co->co_jit_data;
    int idx = ctx->next_instr - ctx->first_instr;
    set_return_address(jd->jmptab[idx]);
}

static void
translate_bytecode(JITData *jd, PyCodeObject *co)
{
    // This specialized to System V AMD64 ABI calling convention.
    // The function we're generating is going to look like the following:
    //
    //   function:  # ctx is passed in as %rdi
    //      # Preserve %r12
    //      pushq %r12
    //      # Save ctx in %r12 for quick access
    //      mov %rdi, %r12
    //      # Save return address location in ctx
    //      mov %rsp, jit_ret_addr(%r12)
    //      # Present in generators: jump to instruction
    //      jmp *%rsi
    //
    //      # Repeated for each instruction
    //      movq next_instr(%r12), %rcx
    //      movq %rcx, f_lasti(%r13)
    //      addq $0x2, next_instr(%r12)
    //      movl OPCODE1, opcode(%r12)
    //      movl OPARG1, oparg(%r12)
    //      movq %r12, %rdi
    //      call OPCODE1_FUNC
    //
    //      # After all the bytecodes, special sections:
    //      error:
    //        mov %r12, %rdi
    //        call II_ERROR_HANDLER
    //      fast_block_end:
    //        mov %r12, %rdi
    //        call II_FAST_BLOCK_END
    //      [other labels with a similar section]
    //        ...
    //      exit_eval_frame:
    //        pop %r12
    //        ret
    // }
    //
    // Control flows sequentially through the opcodes, unless one of the
    // opcodes re-writes it's return address using one of the helper functions
    // (see _PyJIT_JUMP). This is done to jump to one of the sepcial sections.
    //
    int is_gen = (co->co_flags & (CO_GENERATOR | CO_COROUTINE | CO_ASYNC_GENERATOR));
    _Py_CODEUNIT *code = (_Py_CODEUNIT*)PyBytes_AS_STRING(co->co_code);
    _Py_CODEUNIT *p;
    Py_ssize_t i;
    Py_ssize_t inst_count = PyBytes_GET_SIZE(co->co_code)/sizeof(_Py_CODEUNIT);

    jd->entry = current_code_pos();

    // pushq %r12
    insert_code(2, "\x41\x54");

    // pushq %r13
    insert_code(2, "\x41\x55");

    // pushq %r14
    insert_code(2, "\x41\x56");

    // movq %rdi,%r12
    insert_code(3, "\x49\x89\xfc");

    // mov %rsp, jit_ret_addr(%rdi)
    insert_code(4, "\x48\x89\x67\x00");
    reloc_8(-1, offsetof(EvalContext, jit_ret_addr));

    // movq f_offset(%r12), %r13
    insert_code(5, "\x4d\x8b\x6c\x24\x00");
    reloc_8(-1, offsetof(EvalContext, f));

    // We have to do signal check immediately upon function entry.
    insert_special_section(_PyEval_FUNC_JIT_TARGET_II_NEXT_OPCODE);

    if (is_gen) {
        // mov %r12, %rdi
        insert_code(3, "\x4c\x89\xe7");
        insert_call(_resume);
    }

    for (i = 0; i < inst_count; i++) {
        p = &code[i];
        jd->jmptab[i] = current_code_pos();

        // movl $f_lasti, f_lasti_offset(%r13)
        insert_code(11, "\x41\xc7\x85\x00\x00\x00\x00\x00\x00\x00\x00");
        reloc_32(-8, offsetof(PyFrameObject, f_lasti));
        reloc_32(-4, i*2);

        // addq $0x2, next_instr(%r12)
        insert_code(6, "\x49\x83\x44\x24\x00\x02");
        reloc_8(-2, offsetof(EvalContext, next_instr));

        // movl $opcode, opcode_offset(%r12)
        insert_code(9, "\x41\xc7\x44\x24\x00\x00\x00\x00\x00");
        reloc_32(-4, _Py_OPCODE(*p));
        reloc_8(-5, offsetof(EvalContext, opcode));

        // movl $oparg, oparg_offset(%r12)
        insert_code(9, "\x41\xc7\x44\x24\x00\x00\x00\x00\x00");
        reloc_32(-4, _Py_OPARG(*p));
        reloc_8(-5, offsetof(EvalContext, oparg));

        // mov %r12, %rdi
        insert_code(3, "\x4c\x89\xe7");

        // call 32-bit relative function
        insert_call(opcode_function_table[_Py_OPCODE(*p)]);

        ++p;
    }

    /* Dispatch opcode is special. It wants us to run the instruction
       described by the values of ctx->opcode and ctx->oparg,
       and then continue at next_instr.
     */
    jd->j_dispatch_opcode = current_code_pos();

    // movabs $opcode_function_table, %rax
    insert_code(10, "\x48\xb8\x00\x00\x00\x00\x00\x00\x00\x00");
    reloc_64(-8, (unsigned long)&opcode_function_table[0]);

    // movl opcode_offset(%r12), %ecx
    insert_code(5, "\x41\x8b\x4c\x24\x00");
    reloc_8(-1, offsetof(EvalContext, opcode));

    // movq (%rax,%rcx,8), %rax
    insert_code(4, "\x48\x8b\x04\xc8");

    // mov %r12, %rdi
    insert_code(3, "\x4c\x89\xe7");

    // call *%rax
    insert_code(2, "\xff\xd0");

    // if we returned, jump to the next instruction
    // mov %r12, %rdi
    insert_code(3, "\x4c\x89\xe7");
    insert_call(_resume);

    /* Special sections */
    jd->j_error = current_code_pos();
    insert_special_section(_PyEval_FUNC_JIT_TARGET_II_ERROR);
    // mov %r12, %rdi
    insert_code(3, "\x4c\x89\xe7");
    insert_call(_resume);

    jd->j_fast_yield = current_code_pos();
    insert_special_section(_PyEval_FUNC_JIT_TARGET_II_FAST_YIELD);
    // mov %r12, %rdi
    insert_code(3, "\x4c\x89\xe7");
    insert_call(_resume);

    jd->j_fast_block_end = current_code_pos();
    insert_special_section(_PyEval_FUNC_JIT_TARGET_II_FAST_BLOCK_END);
    // mov %r12, %rdi
    insert_code(3, "\x4c\x89\xe7");
    insert_call(_resume);

    jd->j_unwind_cleanup = current_code_pos();
    insert_special_section(_PyEval_FUNC_JIT_TARGET_II_UNWIND_CLEANUP);
    // mov %r12, %rdi
    insert_code(3, "\x4c\x89\xe7");
    insert_call(_resume);

    jd->j_next_opcode = current_code_pos();
    insert_special_section(_PyEval_FUNC_JIT_TARGET_II_NEXT_OPCODE);
    // mov %r12, %rdi
    insert_code(3, "\x4c\x89\xe7");
    insert_call(_resume);

    jd->j_ret = current_code_pos();
    // pop %r14
    insert_code(2, "\x41\x5e");
    // pop %r13
    insert_code(2, "\x41\x5d");
    // pop %r12
    insert_code(2, "\x41\x5c");
    // ret
    insert_code(1, "\xc3");
    finish_code();
}

static int
_PyJIT_CodeGen(PyCodeObject *co) {
    Py_ssize_t inst_count = PyBytes_GET_SIZE(co->co_code)/sizeof(_Py_CODEUNIT);
    JITData *data = PyMem_RawMalloc(sizeof(JITData) + sizeof(JumpTarget) * inst_count);
    if (data == NULL) {
        PyErr_NoMemory();
        return -1;
    }

    if (_PyJIT_GrowCodePage(1024 + inst_count * 64) != 0) {
        PyMem_Free(data);
        return -1;
    }
    translate_bytecode(data, co);
    co->co_jit_data = data;
    return 0;
}

int _PyJIT_Execute(EvalContext *ctx) {
    JITData *jd;
    if (ctx->co->co_jit_data == NULL) {
        if (_PyJIT_CodeGen(ctx->co) != 0) {
            return -1;
        }
        assert(ctx->co->co_jit_data != NULL);
    }
    jd = (JITData*)ctx->co->co_jit_data;

    ((PyJITTargetFunction)jd->entry)(ctx);
    return 0;
}
