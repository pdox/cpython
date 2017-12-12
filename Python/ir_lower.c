#include "Python.h"
#include "Include/internal/pystate.h"
#include "ir.h"

static inline
void _ir_lower_one_instr(
        ir_func func,
        ir_instr _instr,
        ir_value fastlocals,
        ir_value stack_pointer,
        ir_label eval_breaker_label,
        ir_type dealloc_sig) {
    switch (_instr->opcode) {
    case ir_opcode_getlocal: {
        IR_INSTR_AS(getlocal)
        ir_value addr = ir_get_index_ptr(func, fastlocals, ir_constant_int(func, instr->index, NULL));
        ir_value tmp = ir_load(func, addr);
        ir_set_value(func, _instr->dest, tmp);
        break;
    }
    case ir_opcode_setlocal: {
        IR_INSTR_AS(setlocal)
        ir_value addr = ir_get_index_ptr(func, fastlocals, ir_constant_int(func, instr->index, NULL));
        ir_store(func, addr, instr->value);
        break;
    }
    case ir_opcode_incref: {
        IR_INSTR_AS(incref)
        ir_value obj = instr->obj;
        ir_label skip_incref;
        if (instr->is_xincref) {
            skip_incref = ir_label_new(func, "x_skip_incref");
            ir_branch_if_not(func, obj, skip_incref);
        }
        ir_value addr = ir_get_element_ptr(func, obj, offsetof(PyObject, ob_refcnt), ir_type_pyssizet, "ob_refcnt");
        ir_value old_value = ir_load(func, addr);
        ir_value new_value = ir_add(func, old_value, ir_constant_pyssizet(func, 1, NULL));
        ir_store(func, addr, new_value);
        if (instr->is_xincref) {
            ir_label_here(func, skip_incref);
        }
        break;
    }
    case ir_opcode_decref: {
        IR_INSTR_AS(decref)
        ir_label skip_dealloc = ir_label_new(func, "decref_skip_dealloc");
        ir_value obj = instr->obj;
        if (instr->is_xdecref) {
            ir_branch_if_not(func, obj, skip_dealloc);
        }
        ir_value old_value = IR_LOAD_FIELD(func, obj, PyObject, ob_refcnt, ir_type_pyssizet);
        ir_value new_value = ir_sub(func, old_value, ir_constant_pyssizet(func, 1, NULL));
        IR_STORE_FIELD(func, obj, PyObject, ob_refcnt, ir_type_pyssizet, new_value);
        ir_branch_if(func, new_value, skip_dealloc);
        ir_value dealloc_func;
#if defined(Py_DEBUG) || defined(Py_TRACE_REFS)
        dealloc_func = ir_constant_from_ptr(func, dealloc_sig, _Py_Dealloc, "_Py_Dealloc");
#else
        ir_value typeobj = IR_LOAD_FIELD(func, obj, PyObject, ob_type, ir_type_pytypeobject_ptr);
        dealloc_func = IR_LOAD_FIELD(func, typeobj, PyTypeObject, tp_dealloc, dealloc_sig);
#endif
        ir_value args[] = {obj};
        ir_call(func, dealloc_func, 1, args);
        ir_label_here(func, skip_dealloc);
        break;
    }
    case ir_opcode_stackadj: {
        IR_INSTR_AS(stackadj)
        ir_value new_value =
            ir_get_index_ptr(func, stack_pointer, ir_constant_int(func, instr->amount, NULL));
        ir_set_value(func, stack_pointer, new_value);
        break;
    }
    case ir_opcode_stack_peek: {
        IR_INSTR_AS(stack_peek)
        ir_value addr =
            ir_get_index_ptr(func, stack_pointer, ir_constant_int(func, -instr->offset, NULL));
        ir_set_value(func, _instr->dest, ir_load(func, addr));
        break;
    }
    case ir_opcode_stack_put: {
        IR_INSTR_AS(stack_put)
        ir_value addr =
            ir_get_index_ptr(func, stack_pointer, ir_constant_int(func, -instr->offset, NULL));
        ir_store(func, addr, instr->value);
        break;
    }
    case ir_opcode_check_eval_breaker: {
        //IR_INSTR_AS(check_eval_breaker)
        ir_value eval_breaker_addr = ir_constant_from_ptr(func, ir_type_int_ptr, &_PyRuntime.ceval.eval_breaker._value, "&_PyRuntime.ceval.eval_breaker._value");
        ir_value eval_breaker_value = ir_load(func, eval_breaker_addr);
        ir_branch_if(func, eval_breaker_value, eval_breaker_label);
        break;
    }
    default:
        Py_FatalError("Unhandled python-specific opcode");
        break;
    } // switch
}

void ir_lower(ir_func func, ir_value fastlocals, ir_value stack_pointer, ir_label eval_breaker_label) {
    ir_block b;
    ir_instr _instr;
    ir_instr _instr_next;

    ir_type dealloc_arg_types[] = {ir_type_pyobject_ptr};
    ir_type dealloc_sig = ir_create_function_type(func->context, ir_type_void, 1, dealloc_arg_types);

    for (b = func->first_block; b != NULL; b = b->next) {
        for (_instr = b->first_instr; _instr != NULL; _instr = _instr_next) {
            _instr_next = _instr->next;
            if (!ir_opcode_is_python_specific(_instr->opcode))
                continue;

            /* Set our insertion position to right after the instruction we're replacing */
            func->current_block = b;
            b->current_instr = _instr;

            _ir_lower_one_instr(func, _instr, fastlocals, stack_pointer, eval_breaker_label, dealloc_sig);

            /* Reposition cursor to after _instr */
            _instr_next = _instr->next;

            /* Delete the original instruction */
            _ir_instr_remove(func, b, _instr);
        }
    }
}
