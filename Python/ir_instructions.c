#include "Python.h"
#include <stdio.h>
#include "ir.h"

ir_value* ir_get_uses(ir_instr _instr, size_t *count) {
    static ir_value uses[5];
    ir_value *u = uses;

    switch (_instr->opcode) {
    case ir_opcode_neg:
    case ir_opcode_not: {
        IR_INSTR_AS(unop)
        *u++ = instr->value;
        break;
    }
    case ir_opcode_add:
    case ir_opcode_sub:
    case ir_opcode_mul:
    case ir_opcode_div:
    case ir_opcode_rem:
    case ir_opcode_and:
    case ir_opcode_or:
    case ir_opcode_xor:
    case ir_opcode_shl:
    case ir_opcode_shr: {
        IR_INSTR_AS(binop)
        *u++ = instr->left;
        *u++ = instr->right;
        break;
    }
    case ir_opcode_notbool:
    case ir_opcode_bool: {
        IR_INSTR_AS(boolean)
        *u++ = instr->value;
        break;
    }
    case ir_opcode_lt:
    case ir_opcode_gt:
    case ir_opcode_eq:
    case ir_opcode_ne:
    case ir_opcode_le:
    case ir_opcode_ge: {
        IR_INSTR_AS(comparison)
        *u++ = instr->left;
        *u++ = instr->right;
        break;
    }
    case ir_opcode_ternary: {
        IR_INSTR_AS(ternary)
        *u++ = instr->cond;
        *u++ = instr->if_true;
        *u++ = instr->if_false;
        break;
    }
    case ir_opcode_call: {
        IR_INSTR_AS(call)
        /* Super hacky */
        *count = 1 + instr->arg_count;
        return &(instr->target);
    }
    case ir_opcode_get_element_ptr: {
        IR_INSTR_AS(get_element_ptr)
        *u++ = instr->ptr;
        break;
    }
    case ir_opcode_get_index_ptr: {
        IR_INSTR_AS(get_index_ptr)
        *u++ = instr->ptr;
        *u++ = instr->index;
        break;
    }
    case ir_opcode_load: {
        IR_INSTR_AS(load)
        *u++ = instr->ptr;
        break;
    }
    case ir_opcode_store: {
        IR_INSTR_AS(store)
        *u++ = instr->ptr;
        *u++ = instr->value;
        break;
    }
    case ir_opcode_address_of: {
        /* doesn't actually use 'value' */
        break;
    }
    case ir_opcode_alloca: {
        IR_INSTR_AS(alloca)
        *u++ = instr->num_elements;
        break;
    }
    case ir_opcode_constant:
        break;
    case ir_opcode_cast: {
        IR_INSTR_AS(cast)
        *u++ = instr->value;
        break;
    }
    case ir_opcode_set_value: {
        IR_INSTR_AS(set_value)
        *u++ = instr->src;
        break;
    }
    case ir_opcode_info_here:
        break;
    case ir_opcode_branch:
        break;
    case ir_opcode_branch_cond: {
        IR_INSTR_AS(branch_cond)
        *u++ = instr->cond;
        break;
    }
    case ir_opcode_jumptable: {
        IR_INSTR_AS(jumptable)
        *u++ = instr->index;
        break;
    }
    case ir_opcode_ret: {
        IR_INSTR_AS(ret)
        if (instr->value) {
            *u++ = instr->value;
        }
        break;
    }
    case ir_opcode_getlocal:
        break;
    case ir_opcode_setlocal: {
        IR_INSTR_AS(setlocal)
        *u++ = instr->value;
        break;
    }
    case ir_opcode_incref: {
        IR_INSTR_AS(incref)
        *u++ = instr->obj;
        break;
    }
    case ir_opcode_decref: {
        IR_INSTR_AS(decref)
        *u++ = instr->obj;
        break;
    }
    case ir_opcode_stackadj:
        break;
    case ir_opcode_stack_peek:
        break;
    case ir_opcode_stack_put: {
        IR_INSTR_AS(stack_put)
        *u++ = instr->value;
        break;
    }
    case ir_opcode_check_eval_breaker:
        break;
    case ir_opcode_setup_block:
        break;
    case ir_opcode_pop_block:
        break;
    case ir_opcode_goto_error: {
        IR_INSTR_AS(goto_error)
        if (instr->cond) {
            *u++ = instr->cond;
        }
        break;
    }
    case ir_opcode_goto_fbe:
        break;
    case ir_opcode_yield: {
        IR_INSTR_AS(yield)
        *u++ = instr->value;
        break;
    }
    } // switch
    *count = u - uses;
    assert(*count <= 5);
    return uses;
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
    OPCODE_CASE(get_element_ptr)
    OPCODE_CASE(get_index_ptr)
    OPCODE_CASE(load)
    OPCODE_CASE(store)
    OPCODE_CASE(address_of)
    OPCODE_CASE(alloca)

    OPCODE_CASE(constant)
    OPCODE_CASE(cast)
    OPCODE_CASE(set_value)
    OPCODE_CASE(info_here)
    OPCODE_CASE(branch)
    OPCODE_CASE(branch_cond)
    OPCODE_CASE(jumptable)
    OPCODE_CASE(ret)
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

#undef OPCODE_CASE

    default: return "<invalid_opcode>";
    }
}

const char *
_pyblock_id(ir_pyblock b_type) {
    switch (b_type) {
    case IR_PYBLOCK_ANY: return "any";
    case IR_PYBLOCK_LOOP: return "loop";
    case IR_PYBLOCK_EXCEPT: return "except";
    case IR_PYBLOCK_FINALLY_TRY: return "finally_try";
    case IR_PYBLOCK_FINALLY_END: return "finally_end";
    case IR_PYBLOCK_EXCEPT_HANDLER: return "except_handler";
    }
    return "???";
}

const char *
_why_id(ir_fbe_why why) {
    switch (why) {
    case IR_FBE_EXCEPTION: return "exception";
    case IR_FBE_RETURN: return "return";
    case IR_FBE_BREAK: return "break";
    case IR_FBE_CONTINUE: return "continue";
    case IR_FBE_SILENCED: return "silenced";
    }
    return "???";
}

char *
ir_instr_repr(char *p, ir_instr _instr) {
    p += sprintf(p, "      ");
    if (_instr->dest) {
        p = ir_value_repr(p, _instr->dest);
        p += sprintf(p, " = ");
    }
    p += sprintf(p, "%s ", _ir_opcode_repr(_instr->opcode));

    switch (_instr->opcode) {
        /* Unary ops */
        case ir_opcode_neg:
        case ir_opcode_not: {
            IR_INSTR_AS(unop)
            p = ir_value_repr(p, instr->value);
            break;
        }

        /* Binary ops */
        case ir_opcode_add:
        case ir_opcode_sub:
        case ir_opcode_mul:
        case ir_opcode_div:
        case ir_opcode_rem:
        case ir_opcode_and:
        case ir_opcode_or:
        case ir_opcode_xor:
        case ir_opcode_shl:
        case ir_opcode_shr: {
            IR_INSTR_AS(binop)
            p = ir_value_repr(p, instr->left);
            p += sprintf(p, ", ");
            p = ir_value_repr(p, instr->right);
            break;
        }
        case ir_opcode_lt:
        case ir_opcode_gt:
        case ir_opcode_eq:
        case ir_opcode_ne:
        case ir_opcode_le:
        case ir_opcode_ge: {
            IR_INSTR_AS(comparison)
            p = ir_value_repr(p, instr->left);
            p += sprintf(p, ", ");
            p = ir_value_repr(p, instr->right);
            break;
        }
        case ir_opcode_notbool:
        case ir_opcode_bool: {
            IR_INSTR_AS(boolean)
            p = ir_value_repr(p, instr->value);
            break;
        }
        case ir_opcode_ternary: {
            IR_INSTR_AS(ternary)
            p = ir_value_repr(p, instr->cond);
            p += sprintf(p, " ? ");
            p = ir_value_repr(p, instr->if_true);
            p += sprintf(p, " : ");
            p = ir_value_repr(p, instr->if_false);
            break;
        }
        case ir_opcode_call: {
            IR_INSTR_AS(call)
            int i;
            p = ir_type_repr(p, ir_typeof(instr->target));
            p += sprintf(p, " ");
            p = ir_value_repr(p, instr->target);
            p += sprintf(p, " (");
            for (i = 0; i < instr->arg_count; i++) {
                if (i != 0)
                    p += sprintf(p, ", ");
                p = ir_value_repr(p, instr->arg[i]);
            }
            p += sprintf(p, ")");
            break;
        }
        case ir_opcode_get_element_ptr: {
            IR_INSTR_AS(get_element_ptr)
            /* struct name */
            p = ir_type_repr(p, ir_pointer_base(ir_typeof(instr->ptr)));
            /* member name */
            p += sprintf(p, " %s", instr->member_name ? instr->member_name : "<NULL>");
            p += sprintf(p, " ");
            p = ir_value_repr(p, instr->ptr);
            p += sprintf(p, " (offset %ld)", (long)instr->offset);
            break;
        }
        case ir_opcode_get_index_ptr: {
            IR_INSTR_AS(get_index_ptr)
            p = ir_type_repr(p, ir_pointer_base(ir_typeof(instr->ptr)));
            p += sprintf(p, " ");
            p = ir_value_repr(p, instr->ptr);
            p += sprintf(p, " index ");
            p = ir_value_repr(p, instr->index);
            break;
        }
        case ir_opcode_load: {
            IR_INSTR_AS(load)
            p = ir_type_repr(p, ir_pointer_base(ir_typeof(instr->ptr)));
            p += sprintf(p, " ");
            p = ir_value_repr(p, instr->ptr);
            break;
        }
        case ir_opcode_store: {
            IR_INSTR_AS(store)
            p = ir_value_repr(p, instr->ptr);
            p += sprintf(p, " <- ");
            p = ir_value_repr(p, instr->value);
            break;
        }
        case ir_opcode_address_of: {
            IR_INSTR_AS(address_of)
            p = ir_value_repr(p, instr->value);
            break;
        }
        case ir_opcode_alloca: {
            IR_INSTR_AS(alloca)
            p = ir_type_repr(p, ir_pointer_base(ir_typeof(_instr->dest)));
            p += sprintf(p, " ");
            p = ir_value_repr(p, instr->num_elements);
            break;
        }
        case ir_opcode_constant: {
            IR_INSTR_AS(constant)
            ir_type type = ir_typeof(_instr->dest);
            p = ir_type_repr(p, type);
            p += sprintf(p, " ");
            p = ir_imm_repr(p, type, instr->imm);
            if (instr->debug_name) {
                p += sprintf(p, " (expr: %s)", instr->debug_name);
            }
            break;
        }
        case ir_opcode_cast: {
            IR_INSTR_AS(cast)
            p = ir_value_repr(p, instr->value);
            break;
        }
        case ir_opcode_set_value: {
            IR_INSTR_AS(cast)
            p = ir_value_repr(p, instr->value);
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
            p = ir_value_repr(p, instr->cond);
            p += sprintf(p, " ? ");
            p = ir_label_repr(p, instr->if_true);
            p += sprintf(p, " : ");
            p = ir_label_repr(p, instr->if_false);
            break;
        }
        case ir_opcode_jumptable: {
            IR_INSTR_AS(jumptable)
            unsigned int i;
            p = ir_value_repr(p, instr->index);
            p += sprintf(p, " [ ");
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
        case ir_opcode_ret: {
            IR_INSTR_AS(ret)
            if (instr->value) {
                p = ir_value_repr(p, instr->value);
            }
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
            p = ir_value_repr(p, instr->value);
            break;
        }
        case ir_opcode_incref: {
            IR_INSTR_AS(incref)
            p = ir_value_repr(p, instr->obj);
            if (instr->is_xincref) {
                p += sprintf(p, " allows_null");
            }
            break;
        }
        case ir_opcode_decref: {
            IR_INSTR_AS(decref)
            p = ir_value_repr(p, instr->obj);
            if (instr->is_xdecref) {
                p += sprintf(p, " allows_null");
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
            p = ir_value_repr(p, instr->value);
            break;
        }
        case ir_opcode_check_eval_breaker: {
            //IR_INSTR_AS(check_eval_breaker)
            break;
        }
        case ir_opcode_setup_block: {
            IR_INSTR_AS(setup_block)
            p += sprintf(p, "%s ", _pyblock_id(instr->b_type));
            p = ir_label_repr(p, instr->b_handler);
            break;
        }
        case ir_opcode_pop_block: {
            IR_INSTR_AS(pop_block)
            p += sprintf(p, "%s", _pyblock_id(instr->b_type));
            break;
        }
        case ir_opcode_goto_error: {
            IR_INSTR_AS(goto_error)
            if (instr->cond != NULL) {
                p = ir_value_repr(p, instr->cond);
            }
            break;
        }
        case ir_opcode_goto_fbe: {
            IR_INSTR_AS(goto_fbe)
            p += sprintf(p, "%s ", _why_id(instr->why));
            if (instr->continue_target != NULL) {
                p = ir_label_repr(p, instr->continue_target);
            }
            break;
        }
        case ir_opcode_yield: {
            IR_INSTR_AS(yield)
            p = ir_value_repr(p, instr->value);
            break;
        }
    }
    p += sprintf(p, "\n");
    return p;
}
