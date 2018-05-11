#include "Python.h"
#include "internal/pystate.h"
#include "Jit/dcall_signature.h"
#include "Jit/dcall_cfunction.h"
#include "Jit/asmhelper_x86_64.h"

/* On entry, expects:
     Top of stack contains return address (for tail call)
     %rdi contains function that was invoked
     %rax contains value that the function returned

   This emits code equivalent to:

     return _Py_CheckFunctionResult(func, result, NULL);

   But avoids making a function call if necessary, by handling
   the common case (value != NULL, exc == NULL) itself.
 */
static void
_emit_check_function_result_and_return(X86Assembler &a) {
    Label slowpath = a.newLabel();
    /* If value == NULL, jump to slowpath */
    a.test(x86::rax, x86::rax);
    a.je(slowpath);

    /* Store return value into temporary register, since we need to use RAX below */
    X86Gp tmp = x86::r10;
    a.mov(tmp, x86::rax);

    /* Get the tstate and then tstate->cur_exctype */
    a.mov(x86::rax, x86::ptr((uintptr_t)&_PyRuntime.gilstate.tstate_current._value));
    a.mov(x86::rax, x86::ptr(x86::rax, offsetof(PyThreadState, curexc_type)));

    /* If cur_exctype != NULL, jump to slowpath */
    a.test(x86::rax, x86::rax);
    a.mov(x86::rax, tmp);
    a.jne(slowpath);

    /* All done */
    a.mov(x86::rax, tmp);
    a.ret();

    a.bind(slowpath);
    /* Py_CheckFunctionResult(callable, result, where) */
    /* %rdi already has the callable. */
    a.mov(CCARG1, x86::rax);
    a.xor_(CCARG2, CCARG2);
    a.xor_(x86::rax, x86::rax);
    /* Tail call */
    a.mov(x86::r10, (uintptr_t)_Py_CheckFunctionResult);
    a.jmp(x86::r10);
}

/*
   NOTE: Python calls Py_EnterRecursiveCall / Py_LeaveRecursiveCall
   for these, but we don't.
 */
void*
PyJIT_DCall_MakeTrampolineForCFunction_X86_64(PyMethodDef *method, PyJIT_DCall_Signature *cursite) {
    int flags = method->ml_flags & ~(METH_CLASS | METH_STATIC | METH_COEXIST);
    PrintErrorHandler eh;
    CodeHolder code;
    code.init(jrt.getCodeInfo());
    code.setErrorHandler(&eh);
    X86Assembler a(&code);
    Label generic_case = a.newLabel();

    /* Args are: PyCFunction*, PyJIT_DCall_Signature*
       Push the function, since we might need it later.
       This also 16-byte aligns the stack. */
    a.push(CCARG0);

    /* This macro sets %rdi to func->m_self */
#define SET_SELF()  a.mov(CCARG0, x86::ptr(CCARG0, offsetof(PyCFunctionObject, m_self)))
    int finishup = 1;
    switch (flags) {
    case METH_NOARGS: {
        PyJIT_DCall_Signature *sig = PyJIT_DCall_GetSignature(0, NULL);
        a.mov(x86::r10, (uintptr_t)sig);
        a.cmp(CCARG1, x86::r10);
        a.jne(generic_case);
        SET_SELF();
        a.xor_(CCARG1, CCARG1);
        a.call((uintptr_t)method->ml_meth);
        break;
    }
    case METH_O: {
        PyJIT_DCall_Signature *sig = PyJIT_DCall_GetSignature(1, NULL);
        a.mov(x86::r10, (uintptr_t)sig);
        a.cmp(CCARG1, x86::r10);
        a.jne(generic_case);
        SET_SELF();
        a.mov(CCARG1, CCARG2);
        a.call((uintptr_t)method->ml_meth);
        break;
    }
    case METH_FASTCALL:
        if (cursite->kwnames != NULL) {
            /* kwnames is provided, but not supported by the method */
            break;
        }
        /* Fallthrough */
    case METH_FASTCALL | METH_KEYWORDS: {
        /* Specialize to 'cursite' */
        PyJIT_DCall_Signature *sig = cursite;
        Py_ssize_t nkwargs = (sig->kwnames == NULL) ? 0 : PyTuple_GET_SIZE(sig->kwnames);
        a.mov(x86::r10, (uintptr_t)sig);
        a.cmp(CCARG1, x86::r10);
        a.jne(generic_case);

        /* Allocate space on the stack. Careful to stay 16-byte aligned. */
        int alignment_extra = (sig->argcount % 2) ? 1 : 0;
        a.sub(x86::rsp, (sig->argcount + alignment_extra) * sizeof(void*));
        _copy_pyargs_to_stack(a, sig->argcount, 2 + sig->argcount + alignment_extra);

        /* result = ((_PyCFunctionFast)meth) (self, args, nargs, kwnames); */
        SET_SELF();
        a.mov(CCARG1, x86::rsp);
        a.mov(CCARG2, (uintptr_t)(sig->argcount - nkwargs));
        a.mov(CCARG3, (uintptr_t)sig->kwnames);
        a.mov(x86::r10, (uintptr_t)method->ml_meth);
        a.call(x86::r10);
        a.add(x86::rsp, (sig->argcount + alignment_extra) * sizeof(void*));
        break;
    }
    default:
        a.jmp(generic_case);
        finishup = 0;
        break;
    }

    /* Return value is now in %rax, stack has been restored */
    if (finishup) {
        a.pop(x86::rdi); /* Pop 'callable' */
        _emit_check_function_result_and_return(a);
    }

    /* Generic case */
    a.bind(generic_case);
    a.pop(x86::rdi); /* Pop callable */
    a.mov(x86::r10, (uintptr_t)PyJIT_DCall_GetGenericEntrypoint());
    a.jmp(x86::r10);

    void *fn;
    jrt.add(&fn, &code);
    return fn;
}
