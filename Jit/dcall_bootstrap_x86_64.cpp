#include "Jit/asmhelper_x86_64.h"
#include "Jit/dcall_bootstrap.h"

void*
PyJIT_DCall_MakeBootstrap_X86_64(void *target) {
    PrintErrorHandler eh;
    CodeHolder code;
    code.init(jrt.getCodeInfo());
    code.setErrorHandler(&eh);
    X86Assembler a(&code);

    /* Save argument registers */
    X86Gp saveregs[6] = {x86::rdi, x86::rsi, x86::rdx, x86::rcx, x86::r8, x86::r9};
    for (int i = 0; i < 6; i++) {
        a.push(saveregs[i]);
    }

    /* Prepare the function for JIT (returns the new trampoline in %rax) */
    a.sub(x86::rsp, 8); /* Align stack for call */
    a.mov(x86::r10, (uintptr_t)target);
    a.call(x86::r10);
    a.add(x86::rsp, 8);

    /* Restore original argument registers */
    for (int i = 0; i < 6; i++) {
        a.pop(saveregs[6-1-i]);
    }

    /* Jump directly to the new trampoline */
    a.jmp(x86::rax);

    void *fn;
    jrt.add(&fn, &code);
    return (void*)fn;
}
