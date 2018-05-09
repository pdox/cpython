#define SAVE_AREA_SIZE   0x100

.text
.globl jeval_error_handler_trampoline
.align 8
.type jeval_error_handler_trampoline,@function
jeval_error_handler_trampoline:
    leaq -SAVE_AREA_SIZE(%rsp), %rsp

    /* save registers in order by DWARF number */
    movq %rax, 0x0(%rsp)
    movq %rdx, 0x8(%rsp)
    movq %rcx, 0x10(%rsp)
    movq %rbx, 0x18(%rsp)
    movq %rsi, 0x20(%rsp)
    movq %rdi, 0x28(%rsp)
    movq %rbp, 0x30(%rsp)

    /* Save original stack pointer */
    leaq SAVE_AREA_SIZE(%rsp), %rax
    movq %rax, 0x38(%rsp)

    movq %r8, 0x40(%rsp)
    movq %r9, 0x48(%rsp)
    movq %r10, 0x50(%rsp)
    movq %r11, 0x58(%rsp)
    movq %r12, 0x60(%rsp)
    movq %r13, 0x68(%rsp)
    movq %r14, 0x70(%rsp)
    movq %r15, 0x78(%rsp)

    /* save return address */
    movq SAVE_AREA_SIZE(%rsp), %rax
    movq %rax, 0x80(%rsp)

    /* SSE/MMX registers not implemented */

    /* %rdi already contains the _error_info */
    /* %rsi should have the start of the save area */
    movq %rsp, %rsi
    xorl %eax, %eax
    call jeval_error_handler

    /* Restore registers *
    movq 0x0(%rsp), %rax
    movq 0x8(%rsp), %rdx
    movq 0x10(%rsp), %rcx
    movq 0x18(%rsp), %rbx
    movq 0x20(%rsp), %rsi
    movq 0x28(%rsp), %rdi
    movq 0x30(%rsp), %rbp
    /* skip stack pointer */
    movq 0x40(%rsp), %r8
    movq 0x48(%rsp), %r9
    movq 0x50(%rsp), %r10
    movq 0x58(%rsp), %r11
    movq 0x60(%rsp), %r12
    movq 0x68(%rsp), %r13
    movq 0x70(%rsp), %r14
    movq 0x78(%rsp), %r15
    /* skip return address */

    /* restore stack pointer, then return */
    leaq SAVE_AREA_SIZE(%rsp), %rsp
    ret
