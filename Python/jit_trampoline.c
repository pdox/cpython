#if 0

#include "Python.h"
#include "internal/jit.h"
#include "jit.h"

#include "jit_macros.h"

extern int Py_JITFlag;
extern int Py_JITDebugFlag;

typedef struct {
    void *entrypoint;   /* Function entrypoint */
    size_t refcount;    /* Number of threads inside "entrypoint" */
    ir_context context; /* IR context for "entrypoint" */

    /* We keep a linked list of previous "zombie" trampolines. These are
       previous trampolines that are still alive, because
       they are on the call stack for some thread. Once the last caller
       exits, the zombie's refcount will drop to 0, and it will be
       destroyed.
     */
    _pyjit_trampoline *prev;
} _pyjit_trampoline;

typedef struct {
    _pyjit_trampoline *current;
    size_t nargs;
    size_t nkwargs;
    PyObject *kwnames;
    size_t alive;      /* Code which uses this callsite is still around */
} _pyjit_callsite;

typedef struct {
    ir_context context;
    _pyjit_callsite *callsite;

    ir_func func;
    ir_type func_sig;

    ir_value retval;
    ir_label exit;
    ir_label error_exit;
    ir_value callable;

    size_t nargs;
    ir_value *args;

    size_t nkwargs;
    PyObject *kwnames_tuple;
    ir_value *kwargs;

    ir_value kwdict;
} _jitdata;

static void
_shift_args_left(_jitdata *jd, size_t count) {
    assert(count <= jd->nargs);
    for (size_t i = 0; i < jd->nargs - count; i++) {
        jd->args[i] = jd->args[i + count];
    }
    jd->nargs -= count;
}

/* args[0] through args[count-1] are left in an uninitialized state */
static void
_shift_args_right(_jitdata *jd, size_t count) {
    ir_value *newargs = (ir_value*)PyMem_RawMalloc(sizeof(ir_value) * (jd->nargs + count));
    for (size_t i = 0; i < jd->nargs; i++) {
        newargs[count + i] = jd->args[i];
    }
    PyMem_RawFree(jd->args);
    jd->args = newargs;
    jd->nargs += count;
}


static void no_keyword_error(const char *name) {
    PyErr_Format(PyExc_TypeError,
                 "%.200s() takes no keyword arguments",
                 name);
}

#define CALL_no_keyword_error(nameval) do { \
    JTYPE sig = CREATE_SIGNATURE(ir_type_void, ir_type_char_ptr); \
    CALL_NATIVE(sig, no_keyword_error, (nameval)); \
} while (0)

#define IR_LOAD_Py_CheckRecursionLimit() \
    LOAD(CONSTANT_INT_PTR(&_Py_CheckRecursionLimit))

#ifdef Py_DEBUG
#  define IF_PY_DEBUG(x)    x
#else
#  define IF_PY_DEBUG(x)
#endif

#define IR_Py_CheckFunctionResult(_callable, _resultval, _where) ({ \
    PyObject *callable = (_callable); \
    ir_value resultval = (_resultval); \
    const char *where = (_where); \
    ir_value ret = ir_value_new(jd->func, ir_type_pyobject_ptr); \
    ir_value err_occurred = BOOL(IR_PyErr_Occurred()); \
    SET_VALUE(ret, resultval); \
    IF_ELSE(NOTBOOL(resultval), IR_SOMETIMES, { \
        IF_NOT(err_occurred, IR_UNLIKELY, { \
            if (callable) { \
                CALL_PyErr_Format(PyExc_SystemError, \
                             "%R returned NULL without setting an error", \
                             CONSTANT_PYOBJ(callable)); \
            } else { \
                CALL_PyErr_Format(PyExc_SystemError, \
                             "%s returned NULL without setting an error", \
                             CONSTANT_CHAR_PTR((char*)where)); \
            } \
            /* Ensure that the bug is caught in debug mode */ \
            IF_PY_DEBUG(CALL_Py_FatalError("a function returned NULL without setting an error");) \
        }); \
    }, /* else */ { \
        IF(err_occurred, IR_UNLIKELY, { \
            DECREF(resultval); \
            SET_VALUE(ret, CONSTANT_PYOBJ(NULL)); \
            if (callable) { \
                CALL_PyErr_FormatFromCause(PyExc_SystemError, \
                        "%R returned a result with an error set", \
                        CONSTANT_PYOBJ(callable)); \
            } \
            else { \
                CALL_PyErr_FormatFromCause(PyExc_SystemError, \
                        "%s returned a result with an error set", \
                        CONSTANT_CHAR_PTR((char*)where)); \
            } \
            /* Ensure that the bug is caught in debug mode */ \
            IF_PY_DEBUG(CALL_Py_FatalError("a function returned a result with an error set");) \
        }); \
    }); \
    ret; \
})

#define IR_Py_EnterRecursiveCall(msg) ({ \
    IR_LABEL_INIT(recursion_ok); \
    JVALUE tstate = IR_PyThreadState_GET(); \
    JVALUE curdepth = LOAD_FIELD(tstate, PyThreadState, recursion_depth, ir_type_int); \
    JVALUE newdepth = ADD(curdepth, CONSTANT_INT(1)); \
    STORE_FIELD(tstate, PyThreadState, recursion_depth, ir_type_int, newdepth); \
    JVALUE recursion_limit = IR_LOAD_Py_CheckRecursionLimit(); \
    JVALUE ret = JVALUE_CREATE(ir_type_int); \
    SET_VALUE(ret, CONSTANT_INT(0)); \
    BRANCH_IF_NOT(CMP_GT(newdepth, recursion_limit), recursion_ok, IR_LIKELY); \
    JTYPE _sig = CREATE_SIGNATURE(ir_type_int, ir_type_char_ptr); \
    SET_VALUE(ret, CALL_NATIVE(_sig, _Py_CheckRecursiveCall, CONSTANT_CHAR_PTR(msg))); \
    LABEL(recursion_ok); \
    ret; \
})

/* Who designed this?!?! */
#define IR_Py_RecursionLimitLowerWaterMark(limitval) ({ \
    JVALUE _limit = (limitval); \
    JVALUE ret = \
        TERNARY(CMP_GT(_limit, CONSTANT_INT(200)), \
                SUBTRACT(_limit, CONSTANT_INT(50)), \
                MULTIPLY(CONSTANT_INT(3), SHIFT_RIGHT(_limit, CONSTANT_INT(2)))); \
    ret; \
})

#define IR_Py_LeaveRecursiveCall() do { \
    IR_LABEL_INIT(skip_overflowed_clear); \
    JVALUE tstate = IR_PyThreadState_GET(); \
    JVALUE curdepth = LOAD_FIELD(tstate, PyThreadState, recursion_depth, ir_type_int); \
    JVALUE newdepth = SUBTRACT(curdepth, CONSTANT_INT(1)); \
    STORE_FIELD(tstate, PyThreadState, recursion_depth, ir_type_int, newdepth); \
    JVALUE watermark = IR_Py_RecursionLimitLowerWaterMark(IR_LOAD_Py_CheckRecursionLimit()); \
    BRANCH_IF_NOT(CMP_LT(newdepth, watermark), skip_overflowed_clear, IR_UNLIKELY); \
    STORE_FIELD(tstate, PyThreadState, overflowed, ir_type_char, CONSTANT_CHAR(0)); \
    LABEL(skip_overflowed_clear); \
} while (0)


/* Store args into a tuple. Does not include kwargs. */
static ir_value
_args_to_tuple(_jitdata *jd) {
    size_t nargs = jd->nargs;
    ir_type sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyssizet);
    ir_value argtuple = CALL_NATIVE(sig, PyTuple_New, CONSTANT_PYSSIZET(nargs));
    BRANCH_IF_NOT(argtuple, jd->error_exit, IR_UNLIKELY);
    ir_value ob_item = IR_PyTuple_OB_ITEM(argtuple);
    for (size_t i = 0; i < nargs; i++) {
        ir_value item = jd->args[i];
        INCREF(item);
        STORE_AT_INDEX(ob_item, CONSTANT_SIZET(i), item);
    }
    return argtuple;
}

/* Store kwnames/kwargs to a dict */
static ir_value
_kwnames_to_dict(_jitdata *jd) {
    IR_LABEL_INIT(handle_error);
    IR_LABEL_INIT(no_error);
    size_t nkwargs = jd->nkwargs;
    JTYPE sig = CREATE_SIGNATURE(ir_type_pyobject_ptr);
    ir_value kwdict = CALL_NATIVE(sig, PyDict_New);
    BRANCH_IF_NOT(kwdict, jd->error_exit, IR_UNLIKELY);
    JTYPE setitem_sig = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
    for (size_t i = 0; i < nkwargs; i++) {
        PyObject *key = PyTuple_GET_ITEM(jd->kwnames_tuple, i);
        ir_value value = jd->kwargs[i];
        ir_value res = CALL_NATIVE(setitem_sig, PyDict_SetItem, kwdict, CONSTANT_PYOBJ(key), value);
        BRANCH_IF(res, handle_error, IR_UNLIKELY);
    }
    BRANCH(no_error);
    LABEL(handle_error);
    DECREF(kwdict);
    BRANCH(jd->error_exit);
    LABEL(no_error);
    return kwdict;
}

/* Place arguments into a linear stack, using borrowed references */
static ir_value
_args_to_stack(_jitdata *jd, int include_kwnames) {
    size_t nargs = jd->nargs;
    size_t nkwargs = include_kwnames ? jd->nkwargs : 0;
    ir_value stack = ir_alloca(jd->func, ir_type_pyobject_ptr, CONSTANT_SIZET(nargs + nkwargs));
    for (size_t i = 0; i < nargs; i++) {
        ir_value item = jd->args[i];
        STORE_AT_INDEX(stack, CONSTANT_SIZET(i), item);
    }
    for (size_t i = 0; i < nkwargs; i++) {
        ir_value item = jd->kwargs[i];
        STORE_AT_INDEX(stack, CONSTANT_SIZET(nargs + i), item);
    }
    return stack;
}

static void
_verify_kwdict_empty(_jitdata *jd, const char *ml_name) {
    /* generate 'no_keyword_error' if kwdict is not empty */
    IR_LABEL_INIT(empty_dict);
    BRANCH_IF_NOT(IR_PyDict_GET_SIZE(jd->kwdict), empty_dict, IR_LIKELY);
    CALL_no_keyword_error(CONSTANT_CHAR_PTR((char*)ml_name));
    BRANCH(jd->error_exit);
    LABEL(empty_dict);
}

static int
_emit_pymethoddef_call(
        _jitdata *jd,
        PyMethodDef *method)
{
    size_t nargs = jd->nargs;
    size_t nkwargs = jd->nkwargs;
    int flags = method->ml_flags & ~(METH_CLASS | METH_STATIC | METH_COEXIST);
    const char *ml_name = method->ml_name;
    void *ml_meth = method->ml_meth;

    BRANCH_IF(
        IR_Py_EnterRecursiveCall(" while calling a Python object"),
        jd->error_exit,
        IR_UNLIKELY);

    switch (flags) {
    case METH_NOARGS: {
        if (nkwargs > 0) {
            no_keyword_error(ml_name);
            return -1;
        }
        if (nargs != 0) {
            PyErr_Format(PyExc_TypeError,
                         "%.200s() takes no arguments (%zd given)",
                         ml_name, nargs);
            return -1;
        }
        if (jd->kwdict) {
            _verify_kwdict_empty(jd, ml_name);
        }
        ir_type sig_cfunc = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
        ir_value ret = CALL_NATIVE(sig_cfunc, ml_meth, self, CONSTANT_PYOBJ(NULL));
        SET_VALUE(jd->retval, ret);
        break;
    }
    case METH_O: {
        if (nkwargs > 0) {
            no_keyword_error(ml_name);
            return -1;
        }
        if (nargs != 1) {
            PyErr_Format(PyExc_TypeError,
                         "%.200s() takes no arguments (%zd given)",
                         ml_name, nargs);
            return -1;
        }
        if (jd->kwdict) {
            _verify_kwdict_empty(jd, ml_name);
        }
        ir_type sig_cfunc = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
        ir_value ret = CALL_NATIVE(sig_cfunc, ml_meth, self, jd->args[0]);
        SET_VALUE(jd->retval, ret);
        break;
    }
    case METH_VARARGS:
        if (nkwargs > 0) {
            no_keyword_error(ml_name);
            return -1;
        }
        if (jd->kwdict) {
            _verify_kwdict_empty(jd, ml_name);
        }
        /* Fall through */
    case METH_VARARGS | METH_KEYWORDS: {
        /* Slow-path: create a temporary tuple for positional arguments */
        ir_value argtuple = _args_to_tuple(jd);
        ir_value ret;
        if (flags & METH_KEYWORDS) {
            /* Mixing kwnames and kwdict is not yet implemented */
            assert(nkwargs == 0 || jd->kwdict == NULL);
            ir_value kwdict;
            int kwdict_decref = 0;
            if (jd->kwdict) {
                kwdict = jd->kwdict;
            } else if (nkwargs > 0) {
                kwdict = _kwnames_to_dict(jd);
                kwdict_decref = 1;
            } else {
                kwdict = CONSTANT_PYOBJ(NULL);
            }
            ir_type cfunc_with_kw = CREATE_SIGNATURE(
                ir_type_pyobject_ptr,
                ir_type_pyobject_ptr,
                ir_type_pyobject_ptr,
                ir_type_pyobject_ptr);
            ret = CALL_NATIVE(cfunc_with_kw, ml_meth, self, argtuple, kwdict);
            if (kwdict_decref) {
                DECREF(kwdict);
            }
        } else {
            ir_type cfunc = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr);
            ret = CALL_NATIVE(cfunc, ml_meth, self, argtuple);
        }
        SET_VALUE(jd->retval, ret);
        DECREF(argtuple);
        break;
     }
    case METH_FASTCALL: {
        if (nkwargs > 0) {
            no_keyword_error(ml_name);
            return -1;
        }
        if (jd->kwdict) {
            _verify_kwdict_empty(jd, ml_name);
        }
        ir_type cfuncfast = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr, ir_type_pyobject_ptr_ptr, ir_type_pyssizet);
        ir_value argstack = _args_to_stack(jd, 0);
        ir_value ret = CALL_NATIVE(cfuncfast, ml_meth, self, argstack, CONSTANT_PYSSIZET(nargs));
        SET_VALUE(jd->retval, ret);
        break;
    }
    case METH_FASTCALL | METH_KEYWORDS: {
        assert(jd->kwdict == NULL); /* Not yet implemented */
        ir_value argstack = _args_to_stack(jd, 1);
        ir_type cfuncfastkw = CREATE_SIGNATURE(
            ir_type_pyobject_ptr,
            ir_type_pyobject_ptr,
            ir_type_pyobject_ptr_ptr,
            ir_type_pyssizet,
            ir_type_pyobject_ptr);
        ir_value ret = CALL_NATIVE(
                cfuncfastkw,
                ml_meth,
                self,
                argstack,
                CONSTANT_PYSSIZET(nargs),
                CONSTANT_PYOBJ(jd->kwnames_tuple));
        SET_VALUE(jd->retval, ret);
        break;
    }
    default: {
        PyErr_SetString(PyExc_SystemError,
                        "Bad call flags in _emit_pymethoddef_call.");
        return -1;
    }
    } // switch

    IR_Py_LeaveRecursiveCall();
    return 0;
}

static int
_emit_cfunction_call(_jitdata *jd, PyObject *callable) {
    PyCFunctionObject *func = (PyCFunctionObject*)callable;
    int err = _emit_pymethoddef_call(jd, func->m_ml);
    if (err != 0) {
        return err;
    }
    ir_value ret = IR_Py_CheckFunctionResult(callable, jd->retval, NULL);
    SET_VALUE(jd->retval, ret);
    return 0;
}

/* Copied from descrobject.c */
static PyObject *
descr_name(PyDescrObject *descr)
{
    if (descr->d_name != NULL && PyUnicode_Check(descr->d_name))
        return descr->d_name;
    return NULL;
}


static int
_emit_methoddescr_call(_jitdata *jd, PyObject *callable) {
    PyMethodDescrObject *descr = (PyMethodDescrObject*)callable;

    /* Make sure that the first argument is acceptable as 'self' */
    if (jd->nargs < 1) {
        PyErr_Format(PyExc_TypeError,
                     "descriptor '%V' of '%.100s' "
                     "object needs an argument",
                     descr_name((PyDescrObject *)descr), "?",
                     PyDescr_TYPE(descr)->tp_name);
        return -1;
    }

    /* Extract 'self' and shift arguments down by 1 */
    ir_value self = jd->args[0];
    _shift_args_left(jd, 1);

    IR_LABEL_INIT(type_ok);
    ir_value is_subclass = CALL_PyObject_RealIsSubclass(IR_Py_TYPE(self), CONSTANT_PYOBJ((PyObject*)PyDescr_TYPE(descr)));
    BRANCH_IF(is_subclass, type_ok, IR_LIKELY);
    CALL_PyErr_Format(PyExc_TypeError,
                      "descriptor '%V' "
                      "requires a '%.100s' object "
                      "but received a '%.100s'",
                      CONSTANT_CHAR_PTR((char*)descr_name((PyDescrObject *)descr)),
                      CONSTANT_CHAR_PTR("?"),
                      CONSTANT_CHAR_PTR((char*)PyDescr_TYPE(descr)->tp_name),
                      LOAD_FIELD(IR_Py_TYPE(self), PyTypeObject, tp_name, ir_type_char_ptr));
    BRANCH(jd->error_exit);
    LABEL(type_ok);
    return _emit_pymethoddef_call(jd, descr->d_method, self);
}

/* Forward declaration */
static int
_emit_call(_jitdata *jd, PyObject *callable);

static int
_emit_method_call(_jitdata *jd, PyObject *callable) {
    assert(PyMethod_Check(callable));
    PyObject *im_func = PyMethod_GET_FUNCTION(callable);
    PyObject *im_self = PyMethod_GET_SELF(callable);

    /* im_func can be any callable. Prepend 'self' and recursively call */
    _shift_args_right(jd, 1);
    jd->args[0] = CONSTANT_PYOBJ(im_self);

    return _emit_call(jd, im_func);
}

static int
_emit_function_call(_jitdata *jd, PyObject *callable)
{
    assert(PyFunction_Check(callable));
    abort(); /* Not ready */
}

static int
_emit_call(_jitdata *jd, PyObject *callable)
{
    PyTypeObject *type = Py_TYPE(callable);
    int err;
    if (type == &PyCFunction_Type) {
        err = _emit_cfunction_call(jd, callable);
    }
    else if (type == &PyMethodDescr_Type) {
        err = _emit_methoddescr_call(jd, callable);
    }
    else if (type == &PyMethod_Type) {
        err = _emit_method_call(jd, callable);
    }
    else if (type == &PyFunction_Type) {
        err = _emit_function_call(jd, callable);
    }
    else {
        char buf[256];
        sprintf(buf, "Unrecognized callable type: %s", type->tp_name);
        Py_FatalError(buf);
    }
    return err;
}

static int _emit_stub(_jitdata *jd);

static
ir_type
_create_trampoline_signature(ir_context context, _py_callsite *cs) {
    size_t total_nargs = 1 + cs->nargs + cs->nkwargs + (cs->has_kwdict ? 1 : 0);
    ir_type *argtypes = PyMem_RawMalloc(sizeof(ir_type) * total_nargs);
    for (size_t i = 0; i < total_nargs; i++) {
        argtypes[i] = ir_type_pyobject_ptr;
    }
    ir_type sig = ir_create_function_type(context, ir_type_pyobject_ptr, total_nargs, argtypes);
    PyMem_RawFree(argtypes);
    return sig;
}

/* Make a trampoline for calling a python callable as if it were a native function.
   Exactly one of 'callable' or 'method' should be supplied, but not both.

   The result is a native function with signature:

      entry(arg1, ..., argN, [kwarg1,...,kwargM], [kwdict])

   where N = nargs, M = PyTuple_GET_SIZE(kwnames) (or 0 if kwnames == NULL).
   kwdict is present when has_kwdict is true.
   All arguments have type PyObject*, but kwdict, if not NULL, must point
   to a PyDictObject. The return value is PyObject*.

   If 'callable' is a python function, 'entry' will do the following:
     1) Create a new Python frame
     2) Populate the frame with the arguments
     3) Invoke (with a direct call) the native (JIT) version of the function.
     4) Return the result.

   If 'callable' is instead a PyCFunction or PyMetrodDescr, 'entry' will
   transform the arguments as required, and call the native C function.

   If an error occurs during trampoline creation, a Python exception will be
   set, and NULL returned. This error may be an invocation error (like the wrong
   number of arguments) which prevents a sane trampoline from being created.
 */
void*
_pyjit_generate_trampoline(
        ir_context context,
        _pyjit_callsite *cs,
        PyObject *first_callable,
        int stub_only) {
    void *entrypoint = NULL;
    _jitdata _jd;
    _jitdata *jd = &_jd;

    /* Initialize trampoline context and function */
    jd->context = context;
    jd->callsite = cs;

    jd->func_sig = _create_trampoline_signature(jd->context, cs);
    jd->func = ir_func_new(jd->context, jd->func_sig);
    jd->retval = ir_value_new(jd->func, ir_type_pyobject_ptr);
    jd->exit = ir_label_new(jd->func, "exit");
    jd->error_exit = ir_label_new(jd->func, "error_exit");

    jd->nargs = cs->nargs;
    jd->args = PyMem_RawMalloc(sizeof(ir_value) * nargs);
    size_t j = 0;
    jd->callable = ir_func_get_argment(jd->func, j++);
    for (size_t i = 0; i < nargs; i++) {
        jd->args[i] = ir_func_get_argument(jd->func, j++);
    }

    jd->nkwargs = cs->nkwargs;
    jd->kwargs = PyMem_RawMalloc(sizeof(ir_value) * nkwargs);
    jd->kwnames_tuple = kwnames;
    for (size_t i = 0; i < nkwargs; i++) {
        jd->kwargs[i] = ir_func_get_argument(jd->func, j++);
    }
    if (has_kwdict) {
        jd->kwdict = ir_func_get_argument(jd->func, j++);
    } else {
        jd->kwdict = NULL;
    }

    int err;
    if (stub_only) {
        assert(!first_callable);
        err = _emit_stub(jd);
    } else {
        assert(first_callable);
        err = _emit_call(jd, callable);
    }

    if (err != 0) {
        /* Something went wrong, clean up and exit */
        goto genexit;
    }

    LABEL(jd->exit);
    ir_ret(jd->func, jd->retval);

    LABEL(jd->error_exit);
    ir_ret(jd->func, CONSTANT_PYOBJ(NULL));

#ifdef IR_DEBUG
    ir_func_verify(jd->func);
    if (Py_JITDebugFlag > 0) {
        ir_func_dump_file(jd->func, "/tmp/tbefore.ir", "Trampoline before lowering");
    }
#endif

    /* Not providing fastlocals, stack_pointer or eval_breaker_label.
       Locals and stack operations do not make sense in trampolines.
    */
    ir_lower(jd->func, NULL, NULL, NULL);

#ifdef IR_DEBUG
    if (Py_JITDebugFlag > 0) {
        ir_func_dump_file(jd->func, "/tmp/tafter.ir", "Trampoline after lowering");
    }
    ir_func_verify(jd->func);
#endif

    if (Py_JITFlag == 1) {
        entrypoint = ir_libjit_compile(jd->func);
    } else if (Py_JITFlag == 2) {
        entrypoint = ir_llvm_compile(jd->func);
    } else {
        Py_FatalError("Invalid PYJIT value");
    }
    assert(entrypoint);

genexit:
    PyMem_RawFree(jd->args);
    PyMem_RawFree(jd->kwargs);
    return entrypoint;
}

void
_PyJIT_destroy_trampoline(PyJIT_Trampoline *jt) {
    // TODO: Actually deallocate code
    PyMem_RawFree(jt);
}

static void *
_pyjit_materialize(_pyjit_callsite *cs, PyObject *callable) {
    
}

/* The stub is the initial code sitting at the entrypoint for the
   trampoline. The only thing it does is invoke _pyjit_materialize,
   and then invoke the newly generated trampoline.
  */
static int _emit_stub(_jitdata *jd) {
    JTYPE sig = CREATE_SIGNATURE(ir_type_void_ptr, ir_type_void_ptr, ir_type_pyobject_ptr);
    JVALUE new_entry = CALL_NATIVE(sig, _pyjit_materialize, CONSTANT_VOID_PTR(jd->callsite), jd->callable);
    BRANCH_IF_NOT(new_entry, jd->error_exit, IR_UNLIKELY);

    /* Call the new callsite */
    ir_call

    CALL_INDIRECT(new_entry, CONSTANT_PTR(ir_void_ptr, jt);

_pyjit_callsite*
_pyjit_new_fastcall(size_t nargs, PyObject *kwnames) {
    _pyjit_callsite *cs = PyMem_RawMalloc(sizeof(_pyjit_callsite));
    cs->nargs = nargs;
    cs->nkwargs = kwnames ? PyTuple_GET_SIZE(kwnames) : 0;
    cs->kwnames = kwnames;
    cs->alive = 1;

    ir_context context = ir_context_new();
    cs->current.entrypoint = _pyjit_generate_trampoline(context, cs, NULL, 1);
    cs->current.refcount = 0;
    cs->current.context = context;
    cs->current.prev = NULL;

    return cs;
}
#endif
