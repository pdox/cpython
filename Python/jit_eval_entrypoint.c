#include "Python.h"
#include "frameobject.h"
#include "ir.h"
#include "Include/jit_macros.h"
#include "Include/jit.h"


typedef struct {
    ir_context context;
    ir_func func;
    ir_value f; /* PyFrameObject* */
    ir_value throwflag;
} _jitdata;


/* Make an EvalFrameDefault entrypoint for a JIT generated function.
   This has signature:

       PyObject* entrypoint(PyFrameObject *f, int throwflag);

   This is here for compatibility with the existing EvalFrameDefault
   mechanism for calling a function. In particular, the first generated
   function to be called must go through this mechanism.

   This function is not performance critical, because calls between
   generated functions will use the direct entrypoint instead.

   Note that for generators/coroutines, the direct function already
   has an eval-like signature, so this entrypoint is never required.
 */
ir_object
PyJIT_MakeEvalEntrypoint(PyCodeObject *co, void *direct_entrypoint) {
    /* Should not be called for generators */
    assert(!(co->co_flags & (CO_GENERATOR | CO_COROUTINE | CO_ASYNC_GENERATOR)));

    _jitdata _jd;
    _jitdata *jd = &_jd;
    jd->context = ir_context_new();
    ir_type argtypes[] = { ir_type_pyframeobject_ptr, ir_type_int };
    ir_type sig = ir_create_function_type(jd->context, ir_type_pyobject_ptr, sizeof(argtypes)/sizeof(argtypes[0]), argtypes);
    char namebuf[128];
    sprintf(namebuf, "%.100s_eval_entrypoint", PyUnicode_AsUTF8(co->co_name));
    jd->func = ir_func_new(jd->context, namebuf, sig);
    jd->f = ir_func_get_argument(jd->func, 0);
    jd->throwflag = ir_func_get_argument(jd->func, 1);
    ir_value nullobj = CONSTANT_PYOBJ(NULL);

    /* throwflag should never be set for non-generators */
    IR_ASSERT(NOTBOOL(jd->throwflag));

    Py_ssize_t nlocals = co->co_nlocals;
    Py_ssize_t ncellvars = PyTuple_GET_SIZE(co->co_cellvars);
    Py_ssize_t nfreevars = PyTuple_GET_SIZE(co->co_freevars);
    int has_argtuple = (co->co_flags & CO_VARARGS) ? 1 : 0;
    int has_kwdict = (co->co_flags & CO_VARKEYWORDS) ? 1 : 0;
    int has_closure = (nfreevars > 0) ? 1 : 0;
    int has_locals = !(co->co_flags & CO_NEWLOCALS);
    Py_ssize_t total_pyargs = co->co_argcount + co->co_kwonlyargcount + has_argtuple + has_kwdict;
    assert(total_pyargs <= nlocals);
    /* Direct entrypoint signature:

         PyObject *entrypoint(...pyargs..., closure?, locals?)

     */
    Py_ssize_t total_direct_args = total_pyargs + has_closure + has_locals;

    /* Copy all of fastlocals into temporary buffers, and NULL out the frame's
       fastlocals. This is to prevent any code which might look at the frame
       from seeing the wrong version of the variables. This will be unnecessary
       once local inspection is implemented properly for JIT functions. */
    IR_ASSERT(CMP_EQ(CONSTANT_CHAR(0), LOAD_FIELD(jd->f, PyFrameObject, f_partial, ir_type_char)));
    ir_value fastlocals = ir_get_element_ptr(jd->func, jd->f, offsetof(PyFrameObject, f_localsplus_), ir_type_pyobject_ptr, "f_localsplus_");
    int nbuffers = nlocals + ncellvars + nfreevars;
    ir_value* buffers = (ir_value*)malloc(nbuffers * sizeof(ir_value));
    for (int j = 0; j < nbuffers; j++) {
        ir_value j_value = CONSTANT_INT(j);
        buffers[j] = ALLOCA(ir_type_pyobject_ptr);
        STORE(buffers[j], LOAD_AT_INDEX(fastlocals, j_value));
        STORE_AT_INDEX(fastlocals, j_value, nullobj);
    }

    /* Because the caller created the frame, they already created cells for local
       variables and function arguments which are used in closures. We need to undo
       that process here, discarding the cells, because the direct entrypoint
       expects to create those cells for itself. We also need to create a new closures
       tuple to mimic the original. This is extremely wasteful, but this code shouldn't
       be called frequently. (famous last words...) */
    ir_value* direct_args = (ir_value*)malloc(total_direct_args * sizeof(ir_value));
    memset(direct_args, 0, total_direct_args * sizeof(ir_value));

    /* Extract arguments from cells */
    for (int j = 0; j < ncellvars; j++) {
        Py_ssize_t arg;
        if (co->co_cell2arg != NULL &&
            (arg = co->co_cell2arg[j]) != CO_CELL_NOT_AN_ARG) {
            assert(arg >= 0 && arg < total_pyargs);
            ir_value cell = LOAD(buffers[nlocals + j]);
            assert(direct_args[arg] == NULL);
            direct_args[arg] = IR_PyCell_GET(cell);
        }
    }
    
    /* Copy remaining arguments */
    int i;
    for (i = 0; i < total_pyargs; i++) {
        if (direct_args[i] == NULL) {
            direct_args[i] = LOAD(buffers[i]);
        }
    }

    /* Deal with closure. The tuple has to be reconstituted. */
    ir_value closure = NULL;
    if (has_closure) {
        ir_type pytuple_new_sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyssizet);
        closure = CALL_NATIVE(pytuple_new_sig, PyTuple_New, CONSTANT_PYSSIZET(nfreevars));
        /* These are borrowed references! Clear before destroying the tuple, or
           else the tuple will DECREF them! */
        ir_value ob_item = IR_PyTuple_OB_ITEM(closure);
        int offset = nlocals + ncellvars;
        for (Py_ssize_t j = 0; j < nfreevars; j++) {
            STORE_AT_INDEX(ob_item, CONSTANT_PYSSIZET(j), LOAD(buffers[offset + j]));
        }
        direct_args[i++] = closure;
    }
    ir_value locals = NULL;
    if (has_locals) {
        locals = LOAD_FIELD(jd->f, PyFrameObject, f_locals, ir_type_pyobject_ptr);
        direct_args[i++] = locals;
    }
    assert(i == total_direct_args);

    ir_type *direct_arg_types = (ir_type*)malloc(total_direct_args * sizeof(ir_type));
    for (i = 0; i < total_direct_args; i++) {
        direct_arg_types[i] = ir_type_pyobject_ptr;
    }
    ir_type direct_sig = ir_create_function_type(jd->context, ir_type_pyobject_ptr, total_direct_args, direct_arg_types);
    ir_value direct_func = ir_constant_from_ptr(jd->func, direct_sig, direct_entrypoint, "direct_entrypoint");
    ir_value ret = ir_call(jd->func, direct_func, total_direct_args, direct_args);

    if (has_closure) {
        /* Before returning, we need to clear out and decref the temporary closure tuple */
        ir_value ob_item = IR_PyTuple_OB_ITEM(closure);
        for (Py_ssize_t j = 0; j < PyTuple_GET_SIZE(co->co_freevars); j++) {
            STORE_AT_INDEX(ob_item, CONSTANT_PYSSIZET(j), nullobj);
        }
        CALL_Py_DECREF(closure);
    }

    /* Destroy the references we hold */
    for (i = 0; i < nbuffers; i++) {
        CALL_Py_XDECREF(LOAD(buffers[i]));
    }

    ir_retval(jd->func, ret);
    ir_cursor_close(jd->func);

    ir_object object;
    if (Py_JITFlag == 1) {
        object = ir_libjit_compile(jd->func);
    } else if (Py_JITFlag == 2) {
        object = ir_llvm_compile(jd->func);
    } else {
        Py_FatalError("invalid PYJIT value");
        Py_UNREACHABLE();
    }
    free(buffers);
    free(direct_args);
    free(direct_arg_types);
    ir_context_destroy(jd->context);
    return object;
}
