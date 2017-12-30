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
        assert(fastlocals != NULL);
        ir_value addr = ir_get_index_ptr(func, fastlocals, ir_constant_int(func, instr->index, NULL));
        ir_value tmp = ir_load(func, addr);
        _ir_instr_replace_dest(func, tmp, _instr->dest);
        break;
    }
    case ir_opcode_setlocal: {
        IR_INSTR_AS(setlocal)
        assert(fastlocals != NULL);
        ir_value addr = ir_get_index_ptr(func, fastlocals, ir_constant_int(func, instr->index, NULL));
        ir_store(func, addr, instr->value);
        break;
    }
    case ir_opcode_incref: {
        IR_INSTR_AS(incref)
        ir_value obj = instr->obj;
        ir_label skip_incref = NULL;
        if (instr->is_xincref) {
            skip_incref = ir_label_new(func, "x_skip_incref");
            ir_branch_if_not(func, obj, skip_incref, IR_UNLIKELY);
        }
#ifdef Py_REF_DEBUG
        ir_value total_addr = ir_constant_pyssizet_ptr(func, &_Py_RefTotal, "&_Py_RefTotal");
        ir_value old_total = ir_load(func, total_addr);
        ir_value new_total = ir_add(func, old_total, ir_constant_pyssizet(func, 1, NULL));
        ir_store(func, total_addr, new_total);
#endif
        ir_value addr = ir_get_element_ptr(func, obj, offsetof(PyObject, ob_refcnt), ir_type_pyssizet, "ob_refcnt");
        ir_value old_value = ir_load(func, addr);
        ir_value new_value = ir_add(func, old_value, ir_constant_pyssizet(func, 1, NULL));
        ir_store(func, addr, new_value);
        if (skip_incref) {
            ir_label_here(func, skip_incref);
        }
        break;
    }
    case ir_opcode_decref: {
        IR_INSTR_AS(decref)
        ir_label skip_dealloc = ir_label_new(func, "decref_skip_dealloc");
        ir_value obj = instr->obj;
        if (instr->is_xdecref) {
            ir_branch_if_not(func, obj, skip_dealloc, IR_UNLIKELY);
        }
#ifdef Py_REF_DEBUG
        ir_value total_addr = ir_constant_pyssizet_ptr(func, &_Py_RefTotal, "&_Py_RefTotal");
        ir_value old_total = ir_load(func, total_addr);
        ir_value new_total = ir_sub(func, old_total, ir_constant_pyssizet(func, 1, NULL));
        ir_store(func, total_addr, new_total);
#endif
        ir_value old_value = IR_LOAD_FIELD(func, obj, PyObject, ob_refcnt, ir_type_pyssizet);
        ir_value new_value = ir_sub(func, old_value, ir_constant_pyssizet(func, 1, NULL));
        IR_STORE_FIELD(func, obj, PyObject, ob_refcnt, ir_type_pyssizet, new_value);
        ir_branch_if(func, new_value, skip_dealloc, IR_SEMILIKELY);
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
        assert(stack_pointer != NULL);
        ir_value new_value =
            ir_get_index_ptr(func, stack_pointer, ir_constant_int(func, instr->amount, NULL));
        ir_set_value(func, stack_pointer, new_value);
        break;
    }
    case ir_opcode_stack_peek: {
        IR_INSTR_AS(stack_peek)
        assert(stack_pointer != NULL);
        ir_value addr =
            ir_get_index_ptr(func, stack_pointer, ir_constant_int(func, -instr->offset, NULL));
        ir_value tmp = ir_load(func, addr);
        _ir_instr_replace_dest(func, tmp, _instr->dest);
        break;
    }
    case ir_opcode_stack_put: {
        IR_INSTR_AS(stack_put)
        assert(stack_pointer != NULL);
        ir_value addr =
            ir_get_index_ptr(func, stack_pointer, ir_constant_int(func, -instr->offset, NULL));
        ir_store(func, addr, instr->value);
        break;
    }
    case ir_opcode_check_eval_breaker: {
        //IR_INSTR_AS(check_eval_breaker)
        assert(eval_breaker_label != NULL);
        ir_value eval_breaker_addr = ir_constant_from_ptr(func, ir_type_int_ptr, &_PyRuntime.ceval.eval_breaker._value, "&_PyRuntime.ceval.eval_breaker._value");
        ir_value eval_breaker_value = ir_load(func, eval_breaker_addr);
        ir_branch_if(func, eval_breaker_value, eval_breaker_label, IR_UNLIKELY);
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

void ir_verify_stack_effect(ir_func func) {
    ir_block b;
    ir_instr _instr;
    ssize_t num_blocks = ir_func_largest_block_index(func);
    int *stack_at_entry = (int*)malloc(num_blocks * sizeof(int));
    for (int i = 0; i < num_blocks; i++) {
        stack_at_entry[i] = -1;
    }
    stack_at_entry[func->first_block->index] = 0;

    int errors = 0;
    int rerun = 1;
    while (rerun) {
        rerun = 0;
        for (b = func->first_block; b != NULL; b = b->next) {
            int entry_stack_level = stack_at_entry[b->index];
            if (entry_stack_level == -1) {
                rerun = 1;
                continue;
            }
            int block_stack_effect = 0;
            for (_instr = b->first_instr; _instr != NULL; _instr = _instr->next) {
                if (_instr->opcode == ir_opcode_stackadj) {
                    IR_INSTR_AS(stackadj)
                    block_stack_effect += instr->amount;
                } else if (ir_instr_opcode_is_flow_control(_instr->opcode)) {
                    break;
                }
            }
            assert(_instr != NULL && _instr->next == NULL);
            int exit_stack_level = entry_stack_level + block_stack_effect;
#define SET_AND_CHECK(_label) do { \
    ir_label label = (_label); \
    size_t index = label->block->index; \
    if (stack_at_entry[index] == -1) { \
        stack_at_entry[index] = exit_stack_level; \
    } \
    if (stack_at_entry[index] != exit_stack_level) { \
        fprintf(stderr, "Stack level mismatch at branch from %p to %s (%p)\n", b, label->name ? label->name : "<NULL>", label->block); \
        errors = 1; \
    } \
} while (0)
            switch (_instr->opcode) {
            case ir_opcode_branch: {
                IR_INSTR_AS(branch)
                SET_AND_CHECK(instr->target);
                break;
            }
            case ir_opcode_branch_cond: {
                IR_INSTR_AS(branch_cond)
                SET_AND_CHECK(instr->if_true);
                SET_AND_CHECK(instr->if_false);
                break;
            }
            case ir_opcode_jumptable: {
                IR_INSTR_AS(jumptable)
                for (size_t i = 0; i < instr->table_size; i++) {
                    SET_AND_CHECK(instr->table[i]);
                }
                break;
            }
            case ir_opcode_ret: {
                assert(exit_stack_level == 0);
            }
            default: abort(); /* Unhandled flow control */
            } // switch
#undef SET_AND_CHECK
        }
    }
    assert(!errors);
}
