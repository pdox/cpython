#include "Python.h"
#include <sys/types.h>
#include <assert.h>
#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include "ir.h"

ir_func ir_func_new(ir_context context, const char *name, ir_type sig) {
    size_t i;
    IR_ALLOC(func, ir_func, sizeof(ir_value) * sig->param_count)
    assert(sig->kind == ir_type_kind_function);
    memset(func, 0, sizeof(ir_func_t));
    func->context = context;
    func->name = _ir_strdup(context, name);
    func->sig = sig;
    func->param_count = sig->param_count;
    for (i = 0; i < sig->param_count; i++) {
        func->param[i] = ir_value_new(func, sig->param[i]);
    }
    func->entry_label = ir_label_new(func, "entry");
    func->current_pyblock = INVALID_PYBLOCK;
    func->current_stack_level = -1;

    /* Open a cursor at the entry block */
    _ir_func_new_block(func);
    ir_label_here(func, func->entry_label);
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

/* This assert should still work even when NDEBUG is defined. */
#define ASSERT(cond, msg) do { \
    if (!(cond)) { \
        fprintf(stderr, "ir_func_verify assertion failed: " #cond "\n"); \
        fflush(stderr); \
        Py_FatalError(msg); \
    } \
} while (0)

void ir_func_verify(ir_func func) {
    ir_block b_prev = NULL;
    ir_block b;
    for (b = func->first_block; b != NULL; b = b->next) {
        /* Make sure the block linked list is correct */
        ASSERT(b_prev == b->prev, "Corrupted block list");
        b_prev = b;

        ASSERT(b->first_instr != NULL,
               "Empty block, without label");
        ASSERT(b->first_instr->opcode == ir_opcode_label_here,
               "First instruction must be label");

        /* Scan instructions */
        ir_instr _instr;
        ir_instr _instr_prev = NULL;
        int found_branch = 0;
        for (_instr = b->first_instr; _instr != NULL; _instr = _instr->next) {
            ASSERT(_instr_prev == _instr->prev, "Corrupted instruction list");
            _instr_prev = _instr;

            if (ir_instr_opcode_needs_to_be_at_front(_instr->opcode)) {
                ASSERT(_instr == b->first_instr->next,
                       "Instruction not at front of block");
            }
            if (ir_instr_opcode_is_flow_control(_instr->opcode)) {
                ASSERT(_instr == b->last_instr,
                       "Control flow in block interior");
                ASSERT(_instr->next == NULL,
                       "Unexpected instruction after control flow");
                found_branch = 1;
                switch (_instr->opcode) {
                case ir_opcode_branch: {
                    IR_INSTR_AS(branch)
                    ASSERT(instr->target->block != NULL,
                           "Branch to unresolved label");
                    break;
                }
                case ir_opcode_branch_cond: {
                    IR_INSTR_AS(branch_cond)
                    ASSERT(instr->if_true->block != NULL,
                           "First label of conditional branch is unresolved");
                    ASSERT(instr->if_false->block != NULL,
                           "Second label of conditional branch is unresolved");
                    break;
                }
                case ir_opcode_jumptable: {
                    IR_INSTR_AS(jumptable)
                    for (size_t i = 0; i < instr->table_size; i++) {
                        ASSERT(instr->table[i]->block != NULL,
                               "Unresolved label in jumptable");
                    }
                    break;
                }
                case ir_opcode_ret: {
                    IR_INSTR_AS(ret)
                    if (instr->value == NULL) {
                        ASSERT(ir_type_equal(func->sig->param[0], ir_type_void),
                               "void return in non-void function");
                    } else {
                        ASSERT(ir_type_equal(func->sig->param[0], ir_typeof(instr->value)),
                               "Return value type does not match function return type");
                    }
                    break;
                }
                case ir_opcode_goto_error: {
                    IR_INSTR_AS(goto_error)
                    if (instr->cond) {
                        ASSERT(instr->fallthrough, "goto_error missing fallthrough");
                        ASSERT(instr->fallthrough->block != NULL,
                               "Unresolved label in goto_error");
                    } else {
                        ASSERT(instr->fallthrough == NULL,
                               "Unconditional error shouldn't have fallthrough");
                    }
                    break;
                }
                case ir_opcode_goto_fbe: {
                    IR_INSTR_AS(goto_fbe)
                    if (instr->continue_target) {
                        ASSERT(instr->continue_target->block != NULL,
                               "Unresolved continue_target label");
                    }
                    break;
                }
                case ir_opcode_yield: {
                    IR_INSTR_AS(yield)
                    ASSERT(instr->resume_inst_label->block != NULL,
                           "Unresolved label (resume_inst_label)");
                    if (instr->throw_inst_label) {
                        ASSERT(instr->throw_inst_label->block != NULL,
                               "Unresolved label (throw_inst_label)");
                    }
                    break;
                }
                case ir_opcode_yield_dispatch: {
                    IR_INSTR_AS(yield_dispatch)
                    ASSERT(instr->body_start->block != NULL,
                           "Unresolved label (body_start)");
                    break;
                }
                case ir_opcode_end_finally: {
                    IR_INSTR_AS(end_finally)
                    ASSERT(instr->fallthrough->block != NULL,
                           "Unresolved label (fallthrough)");
                    break;
                }
                default:
                    Py_FatalError("Unhandled branch opcode");
                    break;
                }
            } else {
                ASSERT(_instr != b->last_instr && _instr->next != NULL,
                       "Block ends in instruction that isn't control flow");
            }
        }
        /* Every block must end with a control flow op */
        ASSERT(found_branch, "No control flow instruction");
    }
}

#undef ASSERT

char* ir_func_dump(ir_func func) {
    ssize_t num_values = ir_func_largest_value_index(func);
    ssize_t num_blocks = ir_func_largest_block_index(func);
    /* TODO: Rather than guess at a maximum buffer size, resize the buffer dynamically */
    char *buf = (char*)malloc(1024*1024 + 200 * num_values + 200 * num_blocks);
    char *p = buf;
    p += sprintf(p, "DUMPING FUNCTION:\n");
    p += sprintf(p, "  Name: %s\n", func->name);
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

void ir_func_dump_file(ir_func func, const char *filename, const char *description) {
    FILE *fp = fopen(filename, "w");
    char* dump = ir_func_dump(func);
    assert(fp && dump);
    fprintf(fp, "%s", dump);
    fclose(fp);
    free(dump);
    fprintf(stderr, "%s IR dumped to %s\n", description, filename);
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
