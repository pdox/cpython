#include "Python.h"
#include "internal/pystate.h"
#include "jitcall.h"
#include <cstdarg>
#include <cstddef>
#include <asmjit/asmjit.h>

using namespace asmjit;

static JitRuntime jrt;

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

extern "C" PyObject *call_function_extern(PyObject **, Py_ssize_t, PyObject *);

class PrintErrorHandler : public asmjit::ErrorHandler {
public:
  // Return `true` to set last error to `err`, return `false` to do nothing.
  bool handleError(asmjit::Error err, const char* message, asmjit::CodeEmitter* origin) override {
    fprintf(stderr, "ASMJIT ERROR: %s\n", message);
    abort();
    return true;
  }
};

extern "C"
PyObject* _PyJIT_GenericTrampoline(PyObject *callable, PyJITCallSiteSig *css, ...) {
    if (Py_JITDebugFlag > 0) {
        if (PyFunction_Check(callable)) {
            PyFunctionObject *func = (PyFunctionObject*)callable;
            PyCodeObject *co = (PyCodeObject*)func->func_code;
            fprintf(stderr, "Entering generic trampoline for func=%p, code=%p (%s:%s)\n",
                    func,
                    co,
                    PyUnicode_AsUTF8(co->co_filename),
                    PyUnicode_AsUTF8(co->co_name));
        } else if (PyCFunction_Check(callable)) {
            PyCFunctionObject *func = (PyCFunctionObject*)callable;
            fprintf(stderr, "Entering generic trampoline for method %s\n", func->m_ml->ml_name);
        } else {
            fprintf(stderr, "Entering generic trampoline for callable=%p, type '%s'\n",
                callable,
                Py_TYPE(callable)->tp_name);
        }
    }
    assert(css->argcount < 1024); /* Sanity check */
    PyObject *stack[css->argcount + 1];
    va_list ap;
    stack[0] = callable;
    va_start(ap, css);
    for (int i = 0; i < css->argcount; i++) {
        stack[1 + i] = va_arg(ap, PyObject*);
    }
    return call_function_extern(&stack[css->argcount + 1], css->argcount, css->kwnames);
}

/* Copy the Python arguments into it. 'stack_depth' is the number of words from the
   current %rsp to call stack arguments. %rsp + stack_depth*8 should be the address
   of the fourth PyObject* argument (first non-register argument in AMD64 calling conv)
   Effects:
     %rax - Clobbered (used as scratch register)
 */
static void _copy_pyargs_to_stack(X86Assembler &a, PyJITCallSiteSig *sig, int stack_depth) {
    const size_t slotsize = sizeof(PyObject*);
    for (size_t i = 0; i < sig->argcount; i++) {
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

static
PyJIT_CallTrampoline*
_PyJIT_CallTrampoline_Create(PyJITFunctionObject *jf) {
    PyCodeObject *co = (PyCodeObject*)jf->code;
    int argcount = co->co_argcount;
    PrintErrorHandler eh;
    CodeHolder code;
    code.init(jrt.getCodeInfo());
    code.setErrorHandler(&eh);

    X86Assembler a(&code);
    Label generic_case = a.newLabel();

    /* If call signature matches, jump to actual entrypoint */
    PyJITCallSiteSig *fastsig = PyJIT_CallSiteSig_GetOrCreate(argcount, NULL);
    a.mov(x86::r10, (uintptr_t)fastsig);
    a.cmp(x86::rsi, x86::r10);
    a.jne(generic_case);

    a.mov(x86::r10, (uintptr_t)jf->direct_entrypoint);
    a.jmp(x86::r10);

    /* Generic case */
    a.bind(generic_case);
    a.mov(x86::r10, (uintptr_t)_PyJIT_GenericTrampoline);
    a.jmp(x86::r10);

    void *fn;
    jrt.add(&fn, &code);
    PyJIT_CallTrampoline *ret = (PyJIT_CallTrampoline*)calloc(sizeof(PyJIT_CallTrampoline), 1);
    ret->handled[0] = fastsig;
    ret->entrypoint = (void*)fn;
    return ret;
}

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

/* TODO:
   Python calls Py_EnterRecursiveCall / Py_LeaveRecursiveCall / Py_CheckFunctionResult
   for these. Do we actually want to do that?
 */
static
PyJIT_CallTrampoline*
_PyJIT_CallTrampoline_CreateForMethod(PyMethodDef *method, PyJITCallSiteSig *cursite) {
    int flags = method->ml_flags & ~(METH_CLASS | METH_STATIC | METH_COEXIST);
    PrintErrorHandler eh;
    CodeHolder code;
    code.init(jrt.getCodeInfo());
    code.setErrorHandler(&eh);
    X86Assembler a(&code);
    Label generic_case = a.newLabel();

    /* Args are: PyCFunction*, PyJITCallSiteSig*
       Push the function, since we might need it later.
       This also 16-byte aligns the stack. */
    a.push(CCARG0);

    /* This macro sets %rdi to func->m_self */
#define SET_SELF()  a.mov(CCARG0, x86::ptr(CCARG0, offsetof(PyCFunctionObject, m_self)))
    int finishup = 1;
    switch (flags) {
    case METH_NOARGS: {
        PyJITCallSiteSig *sig = PyJIT_CallSiteSig_GetOrCreate(0, NULL);
        a.mov(x86::r10, (uintptr_t)sig);
        a.cmp(CCARG1, x86::r10);
        a.jne(generic_case);
        SET_SELF();
        a.xor_(CCARG1, CCARG1);
        a.call((uintptr_t)method->ml_meth);
        break;
    }
    case METH_O: {
        PyJITCallSiteSig *sig = PyJIT_CallSiteSig_GetOrCreate(1, NULL);
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
        PyJITCallSiteSig *sig = cursite;
        Py_ssize_t nkwargs = (sig->kwnames == NULL) ? 0 : PyTuple_GET_SIZE(sig->kwnames);
        a.mov(x86::r10, (uintptr_t)sig);
        a.cmp(CCARG1, x86::r10);
        a.jne(generic_case);

        /* Allocate space on the stack. Careful to stay 16-byte aligned. */
        int alignment_extra = (sig->argcount % 2) ? 1 : 0;
        a.sub(x86::rsp, (sig->argcount + alignment_extra) * sizeof(void*));
        _copy_pyargs_to_stack(a, sig, 2 + sig->argcount + alignment_extra);

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
    a.mov(x86::r10, (uintptr_t)_PyJIT_GenericTrampoline);
    a.jmp(x86::r10);

    void *fn;
    jrt.add(&fn, &code);
    PyJIT_CallTrampoline *ret = (PyJIT_CallTrampoline*)calloc(sizeof(PyJIT_CallTrampoline), 1);
    ret->entrypoint = (void*)fn;
    return ret;
}

extern "C" void*
_PyJIT_set_function_trampoline(PyFunctionObject *func, PyJITCallSiteSig *css) {
    /* Can we JIT this function? */
    if (func->func_jit_function == NULL) {
        PyObject *hint = PyJIT_Prepare((PyObject*)func);
        Py_XDECREF(hint);
    }
    PyJITFunctionObject *jf = (PyJITFunctionObject*)func->func_jit_function;
    PyCodeObject *co = (PyCodeObject*)func->func_code;
    int use_generic = (
        jf == NULL ||
        jf->uses_frame_object ||
        func->func_defaults != NULL ||
        func->func_kwdefaults != NULL ||
        func->func_closure != NULL ||
        co->co_kwonlyargcount != 0 ||
        (co->co_flags & CO_VARKEYWORDS) ||
        (co->co_flags & CO_VARARGS) ||
        (co->co_flags & ~PyCF_MASK) != (CO_OPTIMIZED | CO_NEWLOCALS | CO_NOFREE));
    if (use_generic) {
        func->func_jit_call = (void*)_PyJIT_GenericTrampoline;
    } else if (jf->trampoline != NULL) {
        func->func_jit_call = jf->trampoline->entrypoint;
    } else {
        jf->trampoline = _PyJIT_CallTrampoline_Create(jf);
        func->func_jit_call = jf->trampoline->entrypoint;
    }
    return func->func_jit_call;
}

extern "C" void*
_PyJIT_set_cfunction_trampoline(PyCFunctionObject *func, PyJITCallSiteSig *css) {
    PyJIT_CallTrampoline *trampoline;
    if (func->m_ml->ml_trampoline != NULL) {
        trampoline = (PyJIT_CallTrampoline*)func->m_ml->ml_trampoline;
    } else {
        trampoline = _PyJIT_CallTrampoline_CreateForMethod(func->m_ml, css);
        func->m_ml->ml_trampoline = (void*)trampoline;
    }
    func->m_jit_call = trampoline->entrypoint;
    return func->m_jit_call;
}

extern "C" void*
_PyJIT_set_trampoline(PyObject *callable, PyJITCallSiteSig *css) {
    if (PyFunction_Check(callable)) {
        return _PyJIT_set_function_trampoline((PyFunctionObject*)callable, css);
    } else if (PyCFunction_Check(callable)) {
        return _PyJIT_set_cfunction_trampoline((PyCFunctionObject*)callable, css);
    }
    abort(); /* Unrecognized object */
}

static void*
_PyJIT_CallTrampoline_MakeBootstrap(void) {
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
    a.mov(x86::r10, (uintptr_t)_PyJIT_set_trampoline);
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

static void *_bootstrap_trampoline;

void* PyJIT_CallTrampoline_GetBootstrap(void) {
    if (_bootstrap_trampoline == NULL) {
        _bootstrap_trampoline = _PyJIT_CallTrampoline_MakeBootstrap();
    }
    return _bootstrap_trampoline;
}

void* PyJIT_CallTrampoline_GetEntryPoint(PyJIT_CallTrampoline *t) {
    if (t == NULL)
        return PyJIT_CallTrampoline_GetBootstrap();
    return t->entrypoint;
}

void PyJIT_CallTrampoline_Free(PyJIT_CallTrampoline *t) {
    jrt.release(t->entrypoint);
    free(t);
}
