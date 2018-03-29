#include "Python.h"
#include "jitcall.h"
#include <cstdarg>
#include <asmjit/asmjit.h>

using namespace asmjit;

static JitRuntime jrt;

extern "C" {
    PyObject *call_function_extern(PyObject **, Py_ssize_t, PyObject *);
    static PyObject* _generic_trampoline(PyFunctionObject *func, PyJITCallSiteSig *css, ...);
    static void * _jit_and_set_trampoline(PyFunctionObject *func, PyJITCallSiteSig *css);
    void _PyJIT_CallTrampoline_SetupPyFunction(PyFunctionObject *func);
}

static PyObject* _generic_trampoline(PyFunctionObject *func, PyJITCallSiteSig *css, ...) {
    if (Py_JITDebugFlag > 0) {
        PyCodeObject *co = (PyCodeObject*)func->func_code;
        fprintf(stderr, "Entering generic trampoline for func=%p, code=%p (%s:%s)\n",
                func,
                co,
                PyUnicode_AsUTF8(co->co_filename),
                PyUnicode_AsUTF8(co->co_name));
    }
    assert(css->argcount < 1024); /* Sanity check */
    PyObject *stack[css->argcount + 1];
    va_list ap;
    stack[0] = (PyObject*)func;
    va_start(ap, css);
    for (int i = 0; i < css->argcount; i++) {
        stack[1 + i] = va_arg(ap, PyObject*);
    }
    return call_function_extern(&stack[css->argcount + 1], css->argcount, css->kwnames);
}

static void * _jit_and_set_trampoline(PyFunctionObject *func, PyJITCallSiteSig *css) {
    assert(PyFunction_Check(func));

    /* See if we can JIT this */
    if (func->func_jit_function == NULL) {
        PyObject *hint = PyJIT_Prepare((PyObject*)func);
        Py_XDECREF(hint);
    }
    _PyJIT_CallTrampoline_SetupPyFunction(func);
    return func->func_jit_call;
}

static void*
_PyJIT_CallTrampoline_MakeBootstrap(void) {
    CodeHolder code;
    code.init(jrt.getCodeInfo());
    X86Assembler a(&code);

    /* Save argument registers */
    X86Gp saveregs[6] = {x86::rdi, x86::rsi, x86::rdx, x86::rcx, x86::r8, x86::r9};
    for (int i = 0; i < 6; i++) {
        a.push(saveregs[i]);
    }

    /* Prepare the function for JIT (this returns the new trampoline in %rax) */
    a.call((uintptr_t)_jit_and_set_trampoline);

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

static
PyJIT_CallTrampoline*
_PyJIT_CallTrampoline_Create(PyJITFunctionObject *jf) {
    PyCodeObject *co = (PyCodeObject*)jf->code;
    int argcount = co->co_argcount;
    CodeHolder code;
    code.init(jrt.getCodeInfo());
    X86Assembler a(&code);

    /* If call signature matches, jump to actual entrypoint */
    PyJITCallSiteSig *fastsig = PyJIT_CallSiteSig_GetOrCreate(argcount, NULL);
    a.cmp(x86::rsi, (uintptr_t)fastsig);
    a.je((uintptr_t)jf->direct_entrypoint);

    /* Generic case */
    a.jmp((uintptr_t)_generic_trampoline);

    void *fn;
    jrt.add(&fn, &code);
    PyJIT_CallTrampoline *ret = (PyJIT_CallTrampoline*)PyMem_RawCalloc(sizeof(PyJIT_CallTrampoline), 1);
    ret->handled[0] = fastsig;
    ret->entrypoint = (void*)fn;
    return ret;
}

void
_PyJIT_CallTrampoline_SetupPyFunction(PyFunctionObject *func) {
    PyJITFunctionObject *jf = (PyJITFunctionObject*)func->func_jit_function;
    PyCodeObject *co = jf ? (PyCodeObject*)jf->code : NULL;
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
        func->func_jit_call = (void*)_generic_trampoline;
    } else if (jf->trampoline != NULL) {
        func->func_jit_call = jf->trampoline->entrypoint;
    } else {
        jf->trampoline = _PyJIT_CallTrampoline_Create(jf);
        func->func_jit_call = jf->trampoline->entrypoint;
    }
}
