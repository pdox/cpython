#include "Python.h"
#include <stdio.h>
#include "ir.h"

ir_label*
ir_list_outgoing_labels(ir_block b, size_t *count) {
    ir_instr _instr = b->last_instr;
    switch (IR_INSTR_OPCODE(_instr)) {
    case ir_opcode_branch: {
        IR_INSTR_AS(branch)
        *count = 1;
        return &(instr->target);
    }
    case ir_opcode_branch_cond: {
        IR_INSTR_AS(branch_cond)
        *count = 2;
        return &(instr->if_true);
    }
    case ir_opcode_jumptable: {
        IR_INSTR_AS(jumptable)
        *count = instr->table_size;
        return &instr->table[0];
    }
    case ir_opcode_ret: {
        *count = 0;
        return NULL;
    }
    case ir_opcode_retval: {
        *count = 0;
        return NULL;
    }
    case ir_opcode_goto_error: {
        IR_INSTR_AS(goto_error)
        *count = 1;
        return &instr->error_exit;
    }
    case ir_opcode_goto_fbe: {
        IR_INSTR_AS(goto_fbe)
        *count = 2;
        return &instr->continue_target;
    }
    case ir_opcode_yield: {
        IR_INSTR_AS(yield)
        *count = 3;
        return &instr->resume_inst_label;
    }
    case ir_opcode_yield_dispatch: {
        IR_INSTR_AS(yield_dispatch)
        *count = 1;
        return &instr->body_start;
    }
    case ir_opcode_end_finally: {
        IR_INSTR_AS(end_finally)
        *count = 1;
        return &instr->fallthrough;
    }
    default:
        abort(); /* Block without ending branch */
    }
    return NULL;
}


static inline
const char *
_ir_opcode_repr(ir_opcode opcode) {
    switch (opcode) {

#define OPCODE_CASE(base) \
    case ir_opcode_ ## base: return #base;

    OPCODE_CASE(neg)
    OPCODE_CASE(not)

    OPCODE_CASE(add)
    OPCODE_CASE(sub)
    OPCODE_CASE(mul)
    OPCODE_CASE(div)
    OPCODE_CASE(rem)
    OPCODE_CASE(and)
    OPCODE_CASE(or)
    OPCODE_CASE(xor)
    OPCODE_CASE(shl)
    OPCODE_CASE(shr)

    OPCODE_CASE(notbool)
    OPCODE_CASE(bool)

    OPCODE_CASE(lt)
    OPCODE_CASE(gt)
    OPCODE_CASE(eq)
    OPCODE_CASE(ne)
    OPCODE_CASE(le)
    OPCODE_CASE(ge)

    OPCODE_CASE(ternary)
    OPCODE_CASE(call)
    OPCODE_CASE(patchpoint)
    OPCODE_CASE(get_element_ptr)
    OPCODE_CASE(get_index_ptr)
    OPCODE_CASE(load)
    OPCODE_CASE(store)
    OPCODE_CASE(alloca)

    OPCODE_CASE(constant)
    OPCODE_CASE(cast)
    OPCODE_CASE(func_arg)
    OPCODE_CASE(label_here)
    OPCODE_CASE(info_here)
    OPCODE_CASE(branch)
    OPCODE_CASE(branch_cond)
    OPCODE_CASE(jumptable)
    OPCODE_CASE(ret)
    OPCODE_CASE(retval)
    OPCODE_CASE(getlocal)
    OPCODE_CASE(setlocal)
    OPCODE_CASE(incref)
    OPCODE_CASE(decref)
    OPCODE_CASE(stackadj)
    OPCODE_CASE(stack_peek)
    OPCODE_CASE(stack_put)
    OPCODE_CASE(check_eval_breaker)
    OPCODE_CASE(setup_block)
    OPCODE_CASE(pop_block)
    OPCODE_CASE(goto_error)
    OPCODE_CASE(goto_fbe)
    OPCODE_CASE(yield)
    OPCODE_CASE(yield_dispatch)
    OPCODE_CASE(end_finally)

#undef OPCODE_CASE

    default: return "<invalid_opcode>";
    }
}


static char *
_list_operands(char *p, ir_instr _instr, size_t start, size_t stop) {
    for (size_t i = start; i < stop; i++) {
        if (i != start) p += sprintf(p, ", ");
        p = ir_use_repr(p, IR_INSTR_OPERAND(_instr, i));
    }
    return p;
}

char *
ir_instr_repr(char *p, ir_instr _instr) {
    p += sprintf(p, "      ");
    ir_value dest = IR_INSTR_DEST(_instr);
    ir_type dest_type = ir_typeof(dest);
    if (dest_type != ir_type_void) {
        p = ir_value_repr(p, dest);
        p += sprintf(p, " = ");
    }
    ir_opcode opcode = IR_INSTR_OPCODE(_instr);
    p += sprintf(p, "%s ", _ir_opcode_repr(opcode));

    switch (opcode) {
        default: {
            /* Default action is to just list the operands */
            size_t count = IR_INSTR_NUM_OPERANDS(_instr);
            p = _list_operands(p, _instr, 0, count);
            break;
        }
        case ir_opcode_call: {
            size_t num_operands = IR_INSTR_NUM_OPERANDS(_instr);
            /* First operand is function to call */
            p = ir_use_repr(p, IR_INSTR_OPERAND(_instr, 0));
            p += sprintf(p, " (");
            p = _list_operands(p, _instr, 1, num_operands);
            p += sprintf(p, ")");
            break;
        }
        case ir_opcode_patchpoint: {
            IR_INSTR_AS(patchpoint)
            size_t num_operands = IR_INSTR_NUM_OPERANDS(_instr);
            /* First operand is function to call */
            p = ir_use_repr(p, IR_INSTR_OPERAND(_instr, 0));
            p += sprintf(p, " (");
            /* The first 'real_arg_count' values are actual arguments to the call,
               the rest are present in the stackmap only. Mark the separation between
               the two groups. */
            p = _list_operands(p, _instr, 1, 1 + instr->real_arg_count);
            p += sprintf(p, " | ");
            p = _list_operands(p, _instr, 1 + instr->real_arg_count, num_operands);
            p += sprintf(p, ")");
            p += sprintf(p, " [user_data=%p] ", instr->user_data);
            break;
        }
        case ir_opcode_get_element_ptr: {
            IR_INSTR_AS(get_element_ptr)
            /* struct name */
            ir_value ptr = IR_USE_VALUE(IR_INSTR_OPERAND(_instr, 0));
            p = ir_type_repr(p, ir_pointer_base(ir_typeof(ptr)));
            /* member name */
            p += sprintf(p, " %s", instr->member_name ? instr->member_name : "<NULL>");
            p += sprintf(p, " ");
            p = ir_value_repr(p, ptr);
            p += sprintf(p, " (offset %ld)", (long)instr->offset);
            break;
        }
        case ir_opcode_get_index_ptr: {
            ir_value ptr = IR_USE_VALUE(IR_INSTR_OPERAND(_instr, 0));
            ir_value index = IR_USE_VALUE(IR_INSTR_OPERAND(_instr, 1));
            p = ir_type_repr(p, ir_pointer_base(ir_typeof(ptr)));
            p += sprintf(p, " ");
            p = ir_value_repr(p, ptr);
            p += sprintf(p, " index ");
            p = ir_value_repr(p, index);
            break;
        }
        case ir_opcode_load: {
            ir_value ptr = IR_USE_VALUE(IR_INSTR_OPERAND(_instr, 0));
            p = ir_type_repr(p, ir_pointer_base(ir_typeof(ptr)));
            p += sprintf(p, " ");
            p = ir_value_repr(p, ptr);
            break;
        }
        case ir_opcode_store: {
            ir_value ptr = IR_USE_VALUE(IR_INSTR_OPERAND(_instr, 0));
            ir_value value = IR_USE_VALUE(IR_INSTR_OPERAND(_instr, 1));
            p = ir_value_repr(p, ptr);
            p += sprintf(p, " <- ");
            p = ir_value_repr(p, value);
            break;
        }
        case ir_opcode_alloca: {
            IR_INSTR_AS(alloca)
            p = ir_type_repr(p, ir_pointer_base(dest_type));
            p += sprintf(p, " %zu", instr->num_elements);
            break;
        }
        case ir_opcode_constant: {
            IR_INSTR_AS(constant)
            p = ir_type_repr(p, dest_type);
            p += sprintf(p, " ");
            p = ir_imm_repr(p, dest_type, instr->imm);
            if (instr->debug_name) {
                p += sprintf(p, " (expr: %s)", instr->debug_name);
            }
            break;
        }
        case ir_opcode_cast: {
            p = ir_type_repr(p, dest_type);
            p += sprintf(p, " ");
            p = ir_use_repr(p, IR_INSTR_OPERAND(_instr, 0));
            break;
        }
        case ir_opcode_func_arg: {
            IR_INSTR_AS(func_arg)
            p += sprintf(p, "%zu", instr->index);
            break;
        }
        case ir_opcode_label_here: {
            IR_INSTR_AS(label_here)
            p = ir_label_repr(p, instr->label);
            break;
        }
        case ir_opcode_info_here: {
            IR_INSTR_AS(info_here)
            p += sprintf(p, "%s:", instr->info);
            break;
        }
        case ir_opcode_branch: {
            IR_INSTR_AS(branch)
            p = ir_label_repr(p, instr->target);
            break;
        }
        case ir_opcode_branch_cond: {
            IR_INSTR_AS(branch_cond)
            ir_value cond = IR_USE_VALUE(IR_INSTR_OPERAND(_instr, 0));
            p = ir_value_repr(p, cond);
            p += sprintf(p, " ? ");
            p = ir_label_repr(p, instr->if_true);
            p += sprintf(p, " : ");
            p = ir_label_repr(p, instr->if_false);
            p += sprintf(p, " [%d:%d]", instr->if_true_weight, instr->if_false_weight);
            break;
        }
        case ir_opcode_jumptable: {
            IR_INSTR_AS(jumptable)
            p = ir_use_repr(p, IR_INSTR_OPERAND(_instr, 0));
            p += sprintf(p, " [ ");
            size_t i;
            for (i = 0; i < instr->table_size && i < 10; i++) {
                if (i != 0) p += sprintf(p, ", ");
                p = ir_label_repr(p, instr->table[i]);
            }
            if (i > 0 && i < instr->table_size) {
                p += sprintf(p, ", ...");
            }
            p += sprintf(p, " ]");
            break;
        }
        case ir_opcode_getlocal: {
            IR_INSTR_AS(getlocal)
            p += sprintf(p, "%ld", (long)instr->index);
            break;
        }
        case ir_opcode_setlocal: {
            IR_INSTR_AS(setlocal)
            p += sprintf(p, "%ld ", (long)instr->index);
            p = ir_use_repr(p, IR_INSTR_OPERAND(_instr, 0));
            break;
        }
        case ir_opcode_incref: {
            IR_INSTR_AS(incref)
            p = ir_use_repr(p, IR_INSTR_OPERAND(_instr, 0));
            if (instr->is_xincref) {
                p += sprintf(p, " maybe_null");
            }
            break;
        }
        case ir_opcode_decref: {
            IR_INSTR_AS(decref)
            p = ir_use_repr(p, IR_INSTR_OPERAND(_instr, 0));
            if (instr->is_xdecref) {
                p += sprintf(p, " maybe_null");
            }
            break;
        }
        case ir_opcode_stackadj: {
            IR_INSTR_AS(stackadj)
            p += sprintf(p, "%d", instr->amount);
            break;
        }
        case ir_opcode_stack_peek: {
            IR_INSTR_AS(stack_peek)
            p += sprintf(p, "%d", instr->offset);
            break;
        }
        case ir_opcode_stack_put: {
            IR_INSTR_AS(stack_put)
            p += sprintf(p, "%d ", instr->offset);
            p = ir_use_repr(p, IR_INSTR_OPERAND(_instr, 0));
            break;
        }
        case ir_opcode_check_eval_breaker: {
            break;
        }
        case ir_opcode_setup_block: {
            IR_INSTR_AS(setup_block)
            p += sprintf(p, "%s ", ir_pyblock_type_repr(instr->b_type));
            if (instr->b_handler) {
                p = ir_label_repr(p, instr->b_handler);
            }
            break;
        }
        case ir_opcode_pop_block: {
            IR_INSTR_AS(pop_block)
            p += sprintf(p, "%s", ir_pyblock_type_repr(instr->b_type));
            break;
        }
        case ir_opcode_goto_error: {
            IR_INSTR_AS(goto_error)
            p = ir_label_repr(p, instr->error_exit);
            break;
        }
        case ir_opcode_goto_fbe: {
            IR_INSTR_AS(goto_fbe)
            p += sprintf(p, "%s ", ir_why_repr(instr->why));
            if (instr->continue_target != NULL) {
                p = ir_label_repr(p, instr->continue_target);
            }
            break;
        }
        case ir_opcode_yield: {
            IR_INSTR_AS(yield)
            p = ir_use_repr(p, IR_INSTR_OPERAND(_instr, 0));
            p += sprintf(p, " %d ", instr->resume_instr_index);
            p = ir_label_repr(p, instr->resume_inst_label);
            if (instr->throw_inst_label) {
                p += sprintf(p, " ");
                p = ir_label_repr(p, instr->throw_inst_label);
            }
            break;
        }
        case ir_opcode_yield_dispatch: {
            IR_INSTR_AS(yield_dispatch)
            p = ir_label_repr(p, instr->body_start);
            break;
        }
        case ir_opcode_end_finally: {
            IR_INSTR_AS(end_finally)
            p = ir_label_repr(p, instr->fallthrough);
            break;
        }
    }
    p += sprintf(p, "\n");
    return p;
}
