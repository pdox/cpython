/*
 * Common macros for building JIT functions (using ir.h)
 *
 * These macros assume the existence of a local variable "jd" which
 * contains at least "context" (an ir_context) and "func" (an ir_func).
 *
 * These macros heavily pollute the macro namespace, so they should only be
 * included in files which build IR functions.
 *
 */

#include "internal/pystate.h"

#define JTYPE             ir_type
#define JVALUE            ir_value
#define JLABEL            ir_label
#define JLABEL_INIT(name) ir_label_new(jd->func, name)

#define IR_LABEL_INIT(name) \
    ir_label name = ir_label_new(jd->func, #name);

#define ALLOCA(irtype)    ir_alloca(jd->func, (irtype), 1)

#define CREATE_SIGNATURE(ret_type, ...) ({ \
    JTYPE _args[] = { __VA_ARGS__ }; \
    ir_create_function_type(jd->context, ret_type, sizeof(_args)/sizeof(JTYPE), _args); \
    })

#define CALL_INDIRECT(funcval, ...) ({ \
    JVALUE call_args[] = { __VA_ARGS__ }; \
    ir_call(jd->func, (funcval), sizeof(call_args)/sizeof(JVALUE), call_args); \
    })

#define CALL_NATIVE(sig, native_func, ...) ({ \
    JVALUE call_args[] = { __VA_ARGS__ }; \
    JVALUE funcval = ir_constant_from_ptr(jd->func, (sig), native_func, #native_func); \
    ir_call(jd->func, funcval, sizeof(call_args)/sizeof(JVALUE), call_args); \
})

#define LABEL(label)                ir_label_here(jd->func, (label))
#define BRANCH(label)               ir_branch(jd->func, (label))
#define BRANCH_IF_NOT(val, label, likelyhood)    ir_branch_if_not(jd->func, (val), (label), likelyhood)
#define BRANCH_IF(val, label, likelyhood)        ir_branch_if(jd->func, (val), (label), likelyhood)

#define IF_ELSE(_cond, _likelyhood, _iftrue, _ifnot) do { \
    ir_label iftrue_label = ir_label_new(jd->func, "iftrue." #_cond); \
    ir_label after_label = ir_label_new(jd->func, "after." #_cond); \
    BRANCH_IF((_cond), iftrue_label, (_likelyhood)); \
    { _ifnot }; \
    BRANCH(after_label); \
    LABEL(iftrue_label); \
    { _iftrue }; \
    LABEL(after_label); \
} while (0)

#define IF(_cond, _likelyhood, _todo) do { \
    ir_label after_label = ir_label_new(jd->func, "after_if." #_cond); \
    BRANCH_IF_NOT((_cond), after_label, ir_invert_likelyhood(_likelyhood)); \
    { _todo }; \
    LABEL(after_label); \
} while (0)

#define IF_NOT(_cond, _likelyhood, _todo) do { \
    ir_label after_label = ir_label_new(jd->func, "after_if_not." #_cond); \
    BRANCH_IF((_cond), after_label, ir_invert_likelyhood(_likelyhood)); \
    { _todo }; \
    LABEL(after_label); \
} while (0)

#define ADD(v1, v2)         ir_add(jd->func, (v1), (v2))
#define SUBTRACT(v1, v2)    ir_sub(jd->func, (v1), (v2))
#define MULTIPLY(v1, v2)    ir_mul(jd->func, (v1), (v2))

#define BITWISE_AND(v1, v2) ir_and(jd->func, (v1), (v2))
#define BITWISE_OR(v1, v2)  ir_or(jd->func, (v1), (v2))
#define BITWISE_XOR(v1, v2) ir_xor(jd->func, (v1), (v2))

/* Logical AND with no short-circuiting */
#define LOGICAL_AND_NSC(v1, v2) ir_and(jd->func, ir_bool(jd->func, (v1)), ir_bool(jd->func, (v2)))

/* Logical OR with no short-circuiting */
#define LOGICAL_OR(v1, v2)  ir_or(jd->func, ir_bool(jd->func, (v1)), ir_bool(jd->func, (v2)))

/* Logical AND with short-circuiting */
#define LOGICAL_AND_SC(v1, v2) ({ \
    JLABEL logical_and_end = JLABEL_INIT("logical_and_end"); \
    JVALUE ret = ALLOCA(ir_type_int); \
    STORE(ret, BOOL(v1)); \
    BRANCH_IF_NOT(LOAD(ret), logical_and_end, IR_SEMILIKELY); \
    STORE(ret, BOOL(v2)); \
    LABEL(logical_and_end); \
    LOAD(ret); \
})

#define SHIFT_RIGHT(v1, v2) ir_shr(jd->func, (v1), (v2))
#define CMP_LT(v1, v2)      ir_lt(jd->func, (v1), (v2))
#define CMP_EQ(v1, v2)      ir_eq(jd->func, (v1), (v2))
#define CMP_NE(v1, v2)      ir_ne(jd->func, (v1), (v2))
#define CMP_GT(v1, v2)      ir_gt(jd->func, (v1), (v2))
#define CMP_GE(v1, v2)      ir_ge(jd->func, (v1), (v2))
#define CMP_LE(v1, v2)      ir_le(jd->func, (v1), (v2))
#define TERNARY(v, if_true, if_false)   ir_ternary(jd->func, (v), (if_true), (if_false))

#define BOOL(v)             ir_bool(jd->func, (v))
#define NOTBOOL(v)          ir_notbool(jd->func, (v))

/* Constant int value */
#define CONSTANT_INT(n)   ir_constant_int(jd->func, (n), NULL)
#define CONSTANT_UINT(n)  ir_constant_uint(jd->func, (n), NULL)

#define CONSTANT_LONG(n)  ir_constant_long(jd->func, (n), NULL)
#define CONSTANT_ULONG(n) ir_constant_ulong(jd->func, (n), NULL)

#define CONSTANT_CHAR(n)   ir_constant_char(jd->func, (n), NULL)
#define CONSTANT_UCHAR(n)  ir_constant_uchar(jd->func, (n), NULL)

#define CONSTANT_PYSSIZET(n) ir_constant_pyssizet(jd->func, (n), NULL)
#define CONSTANT_SIZET(n)    ir_constant_sizet(jd->func, (n), NULL)
#define CONSTANT_UINT32(n)   ir_constant_uint(jd->func, (n), NULL)

/* Constant uintptr_t value */
#define CONSTANT_UINTPTR(n)  ir_constant_uintptr(jd->func, (n), NULL)

/* Constant pointer value */
#define CONSTANT_PTR(type, p)    ir_constant_from_ptr(jd->func, (type), (p), #p)
#define CONSTANT_PYOBJ(p)        ir_constant_pyobject_ptr(jd->func, (p), #p)
#define CONSTANT_VOID_PTR(p)     ir_constant_void_ptr(jd->func, (p), #p)
#define CONSTANT_CHAR_PTR(p)     ir_constant_char_ptr(jd->func, (p), #p)
#define CONSTANT_INT_PTR(p)      ir_constant_int_ptr(jd->func, (p), #p)

#define CAST(fieldtype, val) ir_cast(jd->func, (fieldtype), (val))
#define CAST_PYOBJ(val)      CAST(ir_type_pyobject_ptr, (val))

#define LOAD_FIELD(ptrval, structname, fieldname, fieldtype) \
    IR_LOAD_FIELD(jd->func, (ptrval), structname, fieldname, (fieldtype))

#define STORE_FIELD(ptrval, structname, fieldname, fieldtype, val) \
    IR_STORE_FIELD(jd->func, (ptrval), structname, fieldname, (fieldtype), (val))

#define LOAD_AT_INDEX(ptrval, indexval) \
    ir_load(jd->func, ir_get_index_ptr(jd->func, (ptrval), (indexval)))

#define STORE_AT_INDEX(ptrval, indexval, val) \
    ir_store(jd->func, ir_get_index_ptr(jd->func, (ptrval), (indexval)), (val))

#define LOAD(ptrval) \
    ir_load(jd->func, (ptrval))

#define STORE(ptrval, val) \
    ir_store(jd->func, (ptrval), (val))

#define POINTER_ADD(ptr, n) \
    ir_get_index_ptr(jd->func, ptr, CONSTANT_INT(n))

#define CRASH() do { \
    IR_LABEL_INIT(crashpoint); \
    LABEL(crashpoint); \
    STORE(CONSTANT_PTR(ir_type_int_ptr, 0), CONSTANT_INT(0)); \
    BRANCH(crashpoint); \
} while (0)

void _jit_macros_assert(int expr, const char *expr_str);

#  define IR_ASSERT(expr) do { \
    if (Py_JITAsserts) { \
        JTYPE _sig = CREATE_SIGNATURE(ir_type_void, ir_type_int, ir_type_char_ptr); \
        CALL_NATIVE(_sig, _jit_macros_assert, BOOL(expr), CONSTANT_CHAR_PTR(#expr)); \
    } \
} while (0)

/* High-level Python macros */

#define INCREF(_objval)    ir_incref(jd->func, (_objval), 0)
#define XINCREF(_objval)   ir_incref(jd->func, (_objval), 1)

#define DECREF(_objval)    ir_decref(jd->func, (_objval), 0)
#define XDECREF(_objval)   ir_decref(jd->func, (_objval), 1)

/* Python-equivalents */

#define IR_Py_TYPE(obj) \
    LOAD_FIELD((obj), PyObject, ob_type, ir_type_pytypeobject_ptr)

#define LOAD_TP_FLAGS(typeobj) \
    LOAD_FIELD((typeobj), PyTypeObject, tp_flags, ir_type_ulong)

#define IR_PyType_FastSubclass(typeobj, feature) \
    BOOL(BITWISE_AND(LOAD_TP_FLAGS(typeobj), CONSTANT_ULONG(feature)))

#define IR_PyTuple_Check(obj) \
    IR_PyType_FastSubclass(IR_Py_TYPE(obj), Py_TPFLAGS_TUPLE_SUBCLASS)

#define IR_PyType_Check(obj) \
    IR_PyType_FastSubclass(IR_Py_TYPE(obj), Py_TPFLAGS_TYPE_SUBCLASS)

#define IR_PyLong_Check(obj) \
    IR_PyType_FastSubclass(IR_Py_TYPE(obj), Py_TPFLAGS_LONG_SUBCLASS)

#define IR_PyTuple_CheckExact(obj) \
    CMP_EQ(IR_Py_TYPE(obj), CONSTANT_PTR(ir_type_pytypeobject_ptr, &PyTuple_Type))

#define IR_PyCoro_CheckExact(obj) \
    CMP_EQ(IR_Py_TYPE(obj), CONSTANT_PTR(ir_type_pytypeobject_ptr, &PyCoro_Type))

#define IR_PyGen_CheckExact(obj) \
    CMP_EQ(IR_Py_TYPE(obj), CONSTANT_PTR(ir_type_pytypeobject_ptr, &PyGen_Type))

#define IR_PyDict_CheckExact(obj) \
    CMP_EQ(IR_Py_TYPE(obj), CONSTANT_PTR(ir_type_pytypeobject_ptr, &PyDict_Type))

#define IR_PyUnicode_CheckExact(obj) \
    CMP_EQ(IR_Py_TYPE(obj), CONSTANT_PTR(ir_type_pytypeobject_ptr, &PyUnicode_Type))

#define IR_PyCFunction_Check(obj) \
    CMP_EQ(IR_Py_TYPE(obj), CONSTANT_PTR(ir_type_pytypeobject_ptr, &PyCFunction_Type))

#define IR_PyFunction_Check(obj) \
    CMP_EQ(IR_Py_TYPE(obj), CONSTANT_PTR(ir_type_pytypeobject_ptr, &PyFunction_Type))

#define IR_PyExceptionClass_Check(x) \
    LOGICAL_AND_SC( \
        IR_PyType_Check((x)), \
        IR_PyType_FastSubclass(CAST(ir_type_pytypeobject_ptr, (x)), Py_TPFLAGS_BASE_EXC_SUBCLASS))

#define IR_Py_SIZE(objval) \
    LOAD_FIELD(CAST(ir_type_pyvarobject_ptr, (objval)), PyVarObject, ob_size, ir_type_pyssizet)

#define IR_SET_Py_SIZE(objval, newval) \
    STORE_FIELD(CAST(ir_type_pyvarobject_ptr, (objval)), PyVarObject, ob_size, ir_type_pyssizet, (newval))

/* TODO: Add type-check assertions in debug mode */
#define IR_PyTuple_GET_SIZE(objval) IR_Py_SIZE(objval)
#define IR_PyList_GET_SIZE(objval)  IR_Py_SIZE(objval)
#define IR_PyDict_GET_SIZE(objval) \
    LOAD_FIELD(CAST(ir_type_pydictobject_ptr, (objval)), PyDictObject, ma_used, ir_type_pyssizet)

#define IR_PyTuple_OB_ITEM(obj) \
    ir_get_element_ptr(jd->func, (obj), offsetof(PyTupleObject, ob_item), ir_type_pyobject_ptr, "ob_item");

#define IR_PyList_OB_ITEM(obj) \
    LOAD_FIELD(CAST(ir_type_pylistobject_ptr, (obj)), PyListObject, ob_item, ir_type_pyobject_ptr_ptr)

#define IR_PyCell_GET(cell) \
    LOAD_FIELD(CAST(ir_type_pycellobject_ptr, (cell)), PyCellObject, ob_ref, ir_type_pyobject_ptr)

#define IR_PyCell_SET(cell, newval) \
    STORE_FIELD(CAST(ir_type_pycellobject_ptr, (cell)), PyCellObject, ob_ref, ir_type_pyobject_ptr, (newval))

#define CALL_Py_FatalError(msg) do { \
    JTYPE sig = CREATE_SIGNATURE(ir_type_void, ir_type_char_ptr); \
    CALL_NATIVE(sig, Py_FatalError, CONSTANT_CHAR_PTR((char*)(msg))); \
} while (0)

#define CALL_PyObject_RealIsSubclass(derivedval, clsval) ({ \
    JTYPE _sig = CREATE_SIGNATURE(ir_type_int, ir_type_pyobject_ptr, ir_type_pyobject_ptr); \
    CALL_NATIVE(_sig, _PyObject_RealIsSubclass, (derivedval), (clsval)); \
})

void _jit_macros_decref_helper(PyObject *obj);
void _jit_macros_xdecref_helper(PyObject *obj);

#define CALL_Py_DECREF(objval) do { \
    JTYPE _sig = CREATE_SIGNATURE(ir_type_void, ir_type_pyobject_ptr); \
    CALL_NATIVE(_sig, _jit_macros_decref_helper, (objval)); \
} while (0)

#define CALL_Py_XDECREF(objval) do { \
    JTYPE _sig = CREATE_SIGNATURE(ir_type_void, ir_type_pyobject_ptr); \
    CALL_NATIVE(_sig, _jit_macros_xdecref_helper, (objval)); \
} while (0)

#define CALL_PyErr_SetString(exc, msg) do { \
    JTYPE sig = CREATE_SIGNATURE(ir_type_void, ir_type_void_ptr, ir_type_void_ptr); \
    CALL_NATIVE(sig, PyErr_SetString, CONSTANT_VOID_PTR(exc), CONSTANT_VOID_PTR(msg)); \
} while (0)

#define CALL_PyErr_NoMemory() ({ \
    JTYPE _sig = CREATE_SIGNATURE(ir_type_pyobject_ptr); \
    CALL_NATIVE(_sig, PyErr_NoMemory); \
})

#define CALL_PyErr_Clear() do { \
    JTYPE _sig = CREATE_SIGNATURE(ir_type_void); \
    CALL_NATIVE(_sig, PyErr_Clear); \
} while (0)

#define CALL_PyErr_Format(exc, format, ...) do { \
    ir_value _values[] = { CONSTANT_PYOBJ(exc), CONSTANT_CHAR_PTR(format), __VA_ARGS__ }; \
    ir_type _types[sizeof(_values)/sizeof(ir_value)]; \
    for (size_t _i = 0; _i < sizeof(_values)/sizeof(ir_value); _i++) { \
        _types[_i] = ir_typeof(_values[_i]); \
    } \
    JTYPE _sig = ir_create_function_type(jd->context, ir_type_pyobject_ptr, sizeof(_types)/sizeof(ir_type), _types); \
    JVALUE _funcval = ir_constant_from_ptr(jd->func, (_sig), PyErr_Format, "PyErr_Format"); \
    ir_call(jd->func, _funcval, sizeof(_values)/sizeof(ir_value), _values); \
} while (0)

#define CALL_PyErr_FormatFromCause(exc, format, ...) do { \
    ir_value _values[] = { CONSTANT_PYOBJ(exc), CONSTANT_CHAR_PTR(format), __VA_ARGS__ }; \
    ir_type _types[sizeof(_values)/sizeof(ir_value)]; \
    for (size_t _i = 0; _i < sizeof(_values)/sizeof(ir_value); _i++) { \
        _types[_i] = ir_typeof(_values[_i]); \
    } \
    JTYPE _sig = ir_create_function_type(jd->context, ir_type_pyobject_ptr, sizeof(_types)/sizeof(ir_type), _types); \
    JVALUE _funcval = ir_constant_from_ptr(jd->func, (_sig), _PyErr_FormatFromCause, "_PyErr_FormatFromCause"); \
    ir_call(jd->func, _funcval, sizeof(_values)/sizeof(ir_value), _values); \
} while (0)

#define IR_PyThreadState_GET() \
    CAST(ir_type_pythreadstate_ptr, \
         LOAD(CONSTANT_PTR(ir_type_uintptr_ptr, &_PyRuntime.gilstate.tstate_current._value)))

/* Unlike PyErr_Occurred, this assumes tstate != NULL */
#define IR_PyErr_Occurred() \
    LOAD_FIELD(IR_PyThreadState_GET(), PyThreadState, curexc_type, ir_type_void_ptr)

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
    ir_value ret = ALLOCA(ir_type_pyobject_ptr); \
    ir_value err_occurred = BOOL(IR_PyErr_Occurred()); \
    STORE(ret, resultval); \
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
            STORE(ret, CONSTANT_PYOBJ(NULL)); \
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
    LOAD(ret); \
})

#define IR_Py_EnterRecursiveCall(msg) ({ \
    IR_LABEL_INIT(recursion_ok); \
    JVALUE tstate = IR_PyThreadState_GET(); \
    JVALUE curdepth = LOAD_FIELD(tstate, PyThreadState, recursion_depth, ir_type_int); \
    JVALUE newdepth = ADD(curdepth, CONSTANT_INT(1)); \
    STORE_FIELD(tstate, PyThreadState, recursion_depth, ir_type_int, newdepth); \
    JVALUE recursion_limit = IR_LOAD_Py_CheckRecursionLimit(); \
    JVALUE ret = ALLOCA(ir_type_int); \
    STORE(ret, CONSTANT_INT(0)); \
    BRANCH_IF_NOT(CMP_GT(newdepth, recursion_limit), recursion_ok, IR_LIKELY); \
    JTYPE _sig = CREATE_SIGNATURE(ir_type_int, ir_type_char_ptr); \
    STORE(ret, CALL_NATIVE(_sig, _Py_CheckRecursiveCall, CONSTANT_CHAR_PTR(msg))); \
    LABEL(recursion_ok); \
    LOAD(ret); \
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

#define CALL_PyCell_New(objval) ({ \
    JTYPE _sig = CREATE_SIGNATURE(ir_type_pyobject_ptr, ir_type_pyobject_ptr); \
    CALL_NATIVE(_sig, PyCell_New, (objval)); \
})

/* PyThreadState getters/setters */

#define IR_PyThreadState_GET_RUNFRAME(tstate) \
    LOAD_FIELD((tstate), PyThreadState, runframe, ir_type_pyrunframe_ptr)

#define IR_PyThreadState_SET_RUNFRAME(tstate, rf) \
    STORE_FIELD((tstate), PyThreadState, runframe, ir_type_pyrunframe_ptr, (rf))

/* PyFrameObject getters/setters */

#define IR_PyFrameObject_GET_RUNFRAME(f) \
    LOAD_FIELD((f), PyFrameObject, f_runframe, ir_type_pyrunframe_ptr)

#define IR_PyFrameObject_SET_RUNFRAME(f, rf) \
    STORE_FIELD((f), PyFrameObject, f_runframe, ir_type_pyrunframe_ptr, (rf))

#define IR_PyFrameObject_GET_JIT_FUNCTION(f) \
    LOAD_FIELD((f), PyFrameObject, f_jit_function, ir_type_pyjitfunctionobject_ptr)

#define IR_PyFrameObject_SET_JIT_FUNCTION(f, jf) \
    STORE_FIELD((f), PyFrameObject, f_jit_function, ir_type_pyjitfunctionobject_ptr, (jf))

#define IR_PyFrameObject_SET_EXECUTING(f, newval) \
    STORE_FIELD((f), PyFrameObject, f_executing, ir_type_char, (newval))

#define IR_PyFrameObject_SET_LASTI_CEVAL(f, newval) \
    STORE_FIELD((f), PyFrameObject, f_lasti_ceval, ir_type_int, (newval))

/* PyRunFrame getters/setters */

#define IR_PyRunFrame_GET_PREV(rf) \
    LOAD_FIELD((rf), PyRunFrame, prev, ir_type_pyrunframe_ptr)

#define IR_PyRunFrame_SET_PREV(rf, new_prev) \
    STORE_FIELD((rf), PyRunFrame, prev, ir_type_pyrunframe_ptr, (new_prev))

#define IR_PyRunFrame_GET_REF(rf) \
    LOAD_FIELD((rf), PyRunFrame, ref, ir_type_uintptr)

#define IR_PyRunFrame_SET_REF_FRAME(rf, f) \
    STORE_FIELD((rf), PyRunFrame, ref, ir_type_uintptr, CAST(ir_type_uintptr, (f)))

#define IR_PyRunFrame_SET_REF_JIT(rf, jit_function) \
    STORE_FIELD((rf), PyRunFrame, ref, ir_type_uintptr, \
        BITWISE_OR(CAST(ir_type_uintptr, (jit_function)), \
                   CONSTANT_UINTPTR(PY_RUNFRAME_TAG_JIT_FUNCTION)))

#define IR_PyRunFrame_REF_TAG(ref) \
    BITWISE_AND(ref, CONSTANT_UINTPTR(PY_RUNFRAME_TAG_MASK))

#define IR_PyRunFrame_REF_IS_FRAME(ref) \
    CMP_EQ(IR_PyRunFrame_REF_TAG(ref), CONSTANT_UINTPTR(PY_RUNFRAME_TAG_FRAME))

#define IR_PyRunFrame_GET_LOCALS(rf) \
    LOAD_FIELD((rf), PyRunFrame, f_locals, ir_type_pyobject_ptr)

#define IR_PyRunFrame_SET_LOCALS(rf, locals) \
    STORE_FIELD((rf), PyRunFrame, f_locals, ir_type_pyobject_ptr, (locals))

#define IR_PyRunFrame_GET_LASTI(rf) \
    LOAD_FIELD((rf), PyRunFrame, f_lasti, ir_type_int)

#define IR_PyRunFrame_SET_LASTI(rf, lasti) \
    STORE_FIELD((rf), PyRunFrame, f_lasti, ir_type_int, (lasti))

#define IR_PyRunFrame_REF_AS_FRAME(ref) \
    CAST(ir_type_pyframeobject_ptr, ref)

#define IR_PyRunFrame_REF_AS_JIT_FUNCTION(ref) \
    CAST(ir_type_pyjitfunctionobject_ptr, \
         BITWISE_AND(ref, CONSTANT_UINTPTR(~(uintptr_t)PY_RUNFRAME_TAG_MASK)))

/* Push PyRunFrame with pre-materialized frame f.
   This is the jeval equivalent of PyRunFrame_Push.
 */
#define IR_PyRunFrame_Push(rf, tstate, f) do { \
    IR_ASSERT(NOTBOOL(IR_PyFrameObject_GET_RUNFRAME(f))); \
    IR_ASSERT(IR_PyFrameObject_GET_JIT_FUNCTION(f)); \
    IR_PyFrameObject_SET_RUNFRAME(f, rf); \
    IR_PyFrameObject_SET_EXECUTING(f, CONSTANT_CHAR(1)); \
    IR_PyRunFrame_SET_PREV(rf, IR_PyThreadState_GET_RUNFRAME(tstate)); \
    INCREF(CAST_PYOBJ(f)); \
    IR_PyRunFrame_SET_REF_FRAME(rf, f); \
    IR_PyRunFrame_SET_LOCALS(rf, CONSTANT_PYOBJ(NULL)); \
    IR_PyRunFrame_SET_LASTI(rf, CONSTANT_INT(-1)); \
    IR_PyThreadState_SET_RUNFRAME(tstate, rf); \
} while (0)

/* Push PyRunFrame without frame (steals reference to 'locals') */
#define IR_PyRunFrame_PushNoFrame(rf, tstate, jit_function, locals) do { \
    IR_PyRunFrame_SET_PREV(rf, IR_PyThreadState_GET_RUNFRAME(tstate)); \
    INCREF(CAST_PYOBJ(jit_function)); \
    IR_PyRunFrame_SET_REF_JIT(rf, jit_function); \
    IR_PyRunFrame_SET_LOCALS(rf, locals); \
    IR_PyRunFrame_SET_LASTI(rf, CONSTANT_INT(-1)); \
    IR_PyThreadState_SET_RUNFRAME(tstate, rf); \
} while (0)

/* Unlink the PyRunFrame from the thread state.
   This clears all references held by the PyRunFrame.

   If the PyRunFrame was materialized, this also modifies the PyFrameObject by:
     * Clearing f_runframe
     * Setting f_executing to 0
     * Copying f_lasti from the PyRunFrame (if the PyRunFrame was the source of truth)
 */
#define IR_PyRunFrame_Pop(rf, tstate, jit_uses_frame_object) do { \
    /* Unlink PyRunFrame */ \
    IR_ASSERT(CMP_EQ(IR_PyThreadState_GET_RUNFRAME(tstate), (rf))); \
    IR_PyThreadState_SET_RUNFRAME(tstate, IR_PyRunFrame_GET_PREV(rf)); \
    /* If materialized, clear f_runframe and f_executing, and decref f */ \
    JVALUE ref = IR_PyRunFrame_GET_REF(rf); \
    IF_ELSE(IR_PyRunFrame_REF_IS_FRAME(ref), \
            (jit_uses_frame_object) ? IR_LIKELY : IR_UNLIKELY, \
    { \
        JVALUE f = IR_PyRunFrame_REF_AS_FRAME(ref); \
        IR_PyFrameObject_SET_RUNFRAME(f, CONSTANT_PTR(ir_type_pyrunframe_ptr, NULL)); \
        IR_PyFrameObject_SET_EXECUTING(f, CONSTANT_CHAR(0)); \
        if (!(jit_uses_frame_object)) { \
            IR_PyFrameObject_SET_LASTI_CEVAL(f, IR_PyRunFrame_GET_LASTI(rf)); \
        } \
        DECREF(CAST_PYOBJ(f)); \
    }, { \
        JVALUE jit_function = IR_PyRunFrame_REF_AS_JIT_FUNCTION(ref); \
        DECREF(CAST_PYOBJ(jit_function)); \
        XDECREF(IR_PyRunFrame_GET_LOCALS(rf)); \
    }); \
} while (0)

/* Get &ic->entries[0] */
#define IR_ATTRCACHE_ENTRIES(icptr) \
    ir_get_element_ptr( \
        jd->func, \
        (icptr), \
        offsetof(PyJITAttrCache, entries), \
        ir_type_pyjitattrcacheentry, "entries")

/* Get &ic->entries[i] */
#define IR_ATTRCACHE_ENTRIES_INDEX(entries, i) \
    ir_get_index_ptr(jd->func, (entries), CONSTANT_INT(i))

/* Get entry->tp */
#define IR_ATTRCACHE_ENTRY_TP(entryptr) \
    LOAD_FIELD((entryptr), PyJITAttrCacheEntry, tp, ir_type_pytypeobject_ptr)

/* Get entry->stub */
#define IR_ATTRCACHE_ENTRY_STUB(entryptr) \
    LOAD_FIELD((entryptr), PyJITAttrCacheEntry, stub, ir_type_pyjitattrcachestub_ptr)

/* Get stub->handler */
#define IR_ATTRCACHE_STUB_HANDLER(stubptr) \
    LOAD_FIELD((stubptr), PyJITAttrCacheStub, handler, ir_type_void_ptr)
