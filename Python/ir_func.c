#include "Python.h"
#include <sys/types.h>
#include <assert.h>
#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include "ir.h"

ir_func ir_func_new(ir_context context, ir_type sig) {
    size_t i;
    IR_ALLOC(func, ir_func, sizeof(ir_value) * sig->param_count)
    assert(sig->kind == ir_type_kind_function);
    memset(func, 0, sizeof(ir_func_t));
    func->context = context;
    func->sig = sig;
    func->param_count = sig->param_count;
    for (i = 0; i < sig->param_count; i++) {
        func->param[i] = ir_value_new(func, sig->param[i]);
    }
    return func;
}

const char* _ir_label_qualname(ir_func func, const char *name) {
    int i;
    int qualname_size = 0;
    for (i = 0; i < LABEL_PREFIX_STACK_SIZE; i++) {
        const char *prefix = func->label_prefix_stack[i];
        if (prefix == NULL)
            break;
        qualname_size += strlen(prefix) + 1;
    }
    qualname_size += strlen(name);
    char *qualname = _ir_alloc(func->context, qualname_size + 1, 1);
    char *p = qualname;
    for (i = 0; i < LABEL_PREFIX_STACK_SIZE; i++) {
        const char *prefix = func->label_prefix_stack[i];
        if (prefix == NULL)
            break;
        p += sprintf(p, "%s.", prefix);
    }
    p += sprintf(p, "%s", name);
    assert(p == qualname + qualname_size);
    return qualname;
}

char* ir_block_repr(char *p, ir_block b) {
    ir_instr instr;
    p += sprintf(p, "    block %ld (%p)\n", (long)b->index, b);
    for (instr = b->first_instr; instr != NULL; instr = instr->next) {
        p = ir_instr_repr(p, instr);
    }
    return p;
}

void ir_func_verify(ir_func func) {
    ir_block b_prev = NULL;
    ir_block b;
    for (b = func->first_block; b != NULL; b = b->next) {
        assert(b_prev == b->prev);
        b_prev = b;

        ir_instr _instr;
        ir_instr _instr_prev = NULL;
        int in_label_section = 1;
        int found_branch = 0;
        for (_instr = b->first_instr; _instr != NULL; _instr = _instr->next) {
            assert(_instr_prev == _instr->prev);
            _instr_prev = _instr;

            if (_instr->opcode == ir_opcode_label_here) {
                assert(in_label_section == 1 && "Label inside block");
            } else {
                in_label_section = 0;
            }

            if (ir_instr_opcode_is_flow_control(_instr->opcode)) {
                assert(_instr == b->last_instr &&
                       "Flow control inside block");
                assert(_instr->next == NULL);
                found_branch = 1;
                switch (_instr->opcode) {
                case ir_opcode_branch: {
                    IR_INSTR_AS(branch)
                    assert(instr->target->block != NULL);
                    break;
                }
                case ir_opcode_branch_cond: {
                    IR_INSTR_AS(branch_cond)
                    assert(instr->if_true->block != NULL);
                    assert(instr->if_false->block != NULL);
                    break;
                }
                case ir_opcode_jumptable: {
                    IR_INSTR_AS(jumptable)
                    size_t i;
                    for (i = 0; i < instr->table_size; i++) {
                        assert(instr->table[i]->block != NULL);
                    }
                    break;
                }
                case ir_opcode_ret:
                    break;
                default:
                    Py_FatalError("Unhandled branch opcode");
                    break;
                }
            } else {
                assert(_instr != b->last_instr);
                assert(_instr->next != NULL);
            }
        }
        assert(found_branch);
    }
}

char* ir_func_dump(ir_func func) {
    char *buf = (char*)malloc(10*1024*1024);
    char *p = buf;
    ssize_t num_values = ir_func_largest_value_index(func);
    ssize_t num_blocks = ir_func_largest_block_index(func);
    p += sprintf(p, "DUMPING FUNCTION:\n");
    p += sprintf(p, "  Signature: ");
    p = ir_type_repr(p, func->sig);
    p += sprintf(p, "\n");
    p += sprintf(p, "  Arguments: ");
    size_t i;
    for (i = 1; i < func->sig->param_count; i++) {
        p = ir_value_repr(p, func->param[i]);
        p += sprintf(p, " ");
    }
    p += sprintf(p, "\n");
    p += sprintf(p, "  num_blocks: %ld\n", (long)num_blocks);
    p += sprintf(p, "  num_values: %ld\n", (long)num_values);
    p += sprintf(p, "  blocks:\n");

    ir_block b;
    for (b = func->first_block; b != NULL; b = b->next) {
        p = ir_block_repr(p, b);
        p += sprintf(p, "\n");
    }
    return buf;
}

char *ir_value_repr(char *p, ir_value value) {
    p += sprintf(p, "%%%ld", (long)value->index);
    return p;
}

void ir_func_move_blocks_to_end(ir_func func, ir_label from, ir_label to) {
    ir_block from_block = from->block;
    ir_block to_block = to->block;
    ir_block pre_to_block = to_block->prev;

    assert(from_block && to_block && pre_to_block);

#ifdef IR_DEBUG
    {
      /* Ensure they are in order */
      ir_block b;
      for (b = from_block; b != NULL && b != pre_to_block; b = b->next);
      assert(b == pre_to_block);
    }
#endif

    IR_LL_DETACH_CHAIN(func->first_block, func->last_block, from_block, pre_to_block);
    /* This needs to be a copy (rather than a reference) to last_block */
    ir_block after = func->last_block;
    IR_LL_ATTACH_CHAIN(func->first_block, func->last_block, from_block, pre_to_block, after);
}
