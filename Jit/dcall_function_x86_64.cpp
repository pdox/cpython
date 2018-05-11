#include "Python.h"
#include "Jit/dcall_signature.h"
#include "Jit/dcall_function.h"
#include "Jit/asmhelper_x86_64.h"

void*
PyJIT_DCall_MakeTrampolineForFunction_X86_64(PyJITFunctionObject *jf) {
    PyCodeObject *co = (PyCodeObject*)jf->code;
    int argcount = co->co_argcount;
    PrintErrorHandler eh;
    CodeHolder code;
    code.init(jrt.getCodeInfo());
    code.setErrorHandler(&eh);

    X86Assembler a(&code);
    Label generic_case = a.newLabel();

    /* If call signature matches, jump to actual entrypoint */
    PyJIT_DCall_Signature *fastsig = PyJIT_DCall_GetSignature(argcount, NULL);
    a.mov(x86::r10, (uintptr_t)fastsig);
    a.cmp(x86::rsi, x86::r10);
    a.jne(generic_case);

    a.mov(x86::r10, (uintptr_t)jf->direct_entrypoint);
    a.jmp(x86::r10);

    /* Generic case */
    a.bind(generic_case);
    a.mov(x86::r10, (uintptr_t)PyJIT_DCall_GetGenericEntrypoint());
    a.jmp(x86::r10);

    void *fn;
    jrt.add(&fn, &code);
    return fn;
}
