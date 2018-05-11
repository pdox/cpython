#ifndef Py_ASMHELPER_X86_64_H
#define Py_ASMHELPER_X86_64_H

#include <asmjit/asmjit.h>

using namespace asmjit;

extern JitRuntime jrt;

#define CCARG0 x86::rdi
#define CCARG1 x86::rsi
#define CCARG2 x86::rdx
#define CCARG3 x86::rcx
#define CCARG4 x86::r8
#define CCARG5 x86::r9

#define CCARG(i) _ccarg(i)
static inline X86Gp _ccarg(int i) {
    switch (i) {
    case 0: return CCARG0;
    case 1: return CCARG1;
    case 2: return CCARG2;
    case 3: return CCARG3;
    case 4: return CCARG4;
    case 5: return CCARG5;
    }
    abort();
}

class PrintErrorHandler : public asmjit::ErrorHandler {
public:
  // Return `true` to set last error to `err`, return `false` to do nothing.
  bool handleError(asmjit::Error err, const char* message, asmjit::CodeEmitter* origin) override {
    fprintf(stderr, "ASMJIT ERROR: %s\n", message);
    abort();
    return true;
  }
};

/* Copy the Python arguments into it. 'stack_depth' is the number of words from the
   current %rsp to call stack arguments. %rsp + stack_depth*8 should be the address
   of the fourth PyObject* argument (first non-register argument in AMD64 calling conv)
   Effects:
     %rax - Clobbered (used as scratch register)
 */
static void _copy_pyargs_to_stack(X86Assembler &a, int argcount, int stack_depth) {
    const size_t slotsize = sizeof(void*);
    for (size_t i = 0; i < argcount; i++) {
        if (i < 4) {
            /* First 4 arguments copy from registers */
            a.mov(x86::ptr(x86::rsp, i * slotsize), CCARG(2 + i));
        } else {
            /* Copy from stack (TODO: proper memcpy?) */
            a.mov(x86::rax, x86::ptr(x86::rsp, (stack_depth + i - 4) * slotsize));
            a.mov(x86::ptr(x86::rsp, i * slotsize), x86::rax);
        }
    }
}

#endif /* !Py_ASMHELPER_X86_64_H */

