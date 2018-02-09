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
    func->entry_label = ir_label_new(func, "entry");
    func->current_pyblock = INVALID_PYBLOCK;
    func->current_stack_level = -1;

    /* Open a cursor at the entry block */
    _ir_func_new_block(func);
    ir_label_here(func, func->entry_label);

    /* Emit the instructions that fetch the arguments */
    func->arg[0] = NULL;
    for (i = 1; i < sig->param_count; i++) {
        func->arg[i] = _ir_func_arg(func, i, sig->param[i]);
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

/* This assert should still work even when NDEBUG is defined. */
#define ASSERT(cond, msg) do { \
    if (!(cond)) { \
        fprintf(stderr, "ir_func_verify assertion failed: " #cond "\n"); \
        fflush(stderr); \
        Py_FatalError(msg); \
    } \
} while (0)

typedef struct {
    ir_value v;
    size_t count;
    uintptr_t checksum;
} _value_use_info;

static void ir_func_verify_use_lists(ir_func func) {
    size_t num_values = ir_func_next_value_index(func);
    _value_use_info *infos = (_value_use_info*)malloc(num_values * sizeof(_value_use_info));
    memset(infos, 0, num_values * sizeof(_value_use_info));

    /* Scan through all uses in all instructions. Compute the number of uses
       for every value, as well as a checksum (XOR of use pointers) */
    ir_block b;
    ir_instr _instr;
    for (b = func->first_block; b != NULL; b = b->next) {
        for (_instr = b->first_instr; _instr != NULL; _instr = _instr->next) {
            size_t uses_count;
            ir_use uses = ir_get_uses(_instr, &uses_count);
            for (size_t i = 0; i < uses_count; i++) {
                ir_value v = IR_USE_VALUE(uses[i]);
                ASSERT(v->index < num_values, "Invalid value index");
                _value_use_info *info = &infos[v->index];
                info->count++;
                if (info->v == NULL) {
                    info->v = v;
                } else {
                    ASSERT(info->v == v, "Value index not unique");
                }
                info->checksum ^= (uintptr_t)&uses[i];
            }
        }
    }

    /* For each value, verify that the information gathered above
       matches the use_list for that value. */
    for (size_t index = 0; index < num_values; index++) {
        _value_use_info *info = &infos[index];
        if (info->v == NULL) continue;
        size_t count = 0;
        uintptr_t checksum = 0;
        ir_use cur_prev = NULL;
        ir_use cur = info->v->use_list;
        while (cur != NULL) {
            ASSERT(cur->value == info->v, "Use is on wrong use list");
            ASSERT(cur->prev == cur_prev, "Use prev pointer incorrect");
            count++;
            checksum ^= (uintptr_t)cur;
            cur_prev = cur;
            cur = cur->next;
        }
        ASSERT(count == info->count, "Number of uses on use list does not match use count");
        ASSERT(checksum == info->checksum, "Value use list differs from actual uses");
    }

    free(infos);
}

static void ir_func_verify_structure(ir_func func) {
    ir_block b_prev = NULL;
    ir_block b;
    for (b = func->first_block; b != NULL; b = b->next) {
        /* Make sure the block linked list is correct */
        ASSERT(b_prev == b->prev, "Corrupted block list");
        b_prev = b;

        ASSERT(b->first_instr != NULL,
               "Empty block, without label");
        ASSERT(IR_INSTR_OPCODE(b->first_instr) == ir_opcode_label_here,
               "First instruction must be label");

        /* Scan instructions */
        ir_instr _instr;
        ir_instr _instr_prev = NULL;
        int found_branch = 0;
        for (_instr = b->first_instr; _instr != NULL; _instr = _instr->next) {
            ASSERT(_instr_prev == _instr->prev, "Corrupted instruction list");
            ASSERT(_instr->parent == b, "Instruction 'parent' points to wrong block");
            ASSERT(_instr->magic == IR_INSTR_MAGIC, "Corrupted magic pointer");
            _instr_prev = _instr;
            ir_opcode opcode = IR_INSTR_OPCODE(_instr);

            if (ir_opcode_needs_to_be_at_front(opcode)) {
                ASSERT(_instr == b->first_instr->next,
                       "Instruction not at front of block");
            }
            if (ir_opcode_is_control_flow(opcode)) {
                ASSERT(_instr == b->last_instr,
                       "Control flow in block interior");
                ASSERT(_instr->next == NULL,
                       "Unexpected instruction after control flow");
                found_branch = 1;
            } else {
                ASSERT(_instr != b->last_instr && _instr->next != NULL,
                       "Block ends in instruction that isn't control flow");
            }
        }

        /* Every block must end with a control flow op */
        ASSERT(found_branch, "No control flow instruction");

        /* Make sure that outgoing labels are resolved */
        size_t label_count;
        ir_label *labels = ir_list_outgoing_labels(b, &label_count);
        for (size_t i = 0; i < label_count; i++) {
            if (labels[i] != NULL) {
                ASSERT(labels[i]->block != NULL, "Branch to unresolved label");
            }
        }
    }
}

void ir_func_verify(ir_func func) {
    ir_func_verify_structure(func);
    ir_func_verify_use_lists(func);
}

#undef ASSERT

char* ir_func_dump(ir_func func) {
    size_t num_values = ir_func_next_value_index(func);
    size_t num_blocks = ir_func_next_block_index(func);
    /* TODO: Rather than guess at a maximum buffer size, resize the buffer dynamically */
    char *buf = (char*)malloc(1024*1024 + 200 * num_values + 200 * num_blocks);
    char *p = buf;
    p += sprintf(p, "DUMPING FUNCTION:\n");
    p += sprintf(p, "  Name: %s\n", func->name);
    p += sprintf(p, "  Signature: ");
    p = ir_type_repr(p, func->sig);
    p += sprintf(p, "\n");
    p += sprintf(p, "  num_blocks: %lu\n", (unsigned long)num_blocks);
    p += sprintf(p, "  num_values: %lu\n", (unsigned long)num_values);
    p += sprintf(p, "  blocks:\n");

    ir_block b;
    for (b = func->first_block; b != NULL; b = b->next) {
        p = ir_block_repr(p, b);
        p += sprintf(p, "\n");
    }
    return buf;
}

void ir_func_dump_file(ir_func func, const char *filename) {
    FILE *fp = fopen(filename, "w");
    char* dump = ir_func_dump(func);
    assert(fp && dump);
    fprintf(fp, "%s", dump);
    fclose(fp);
    free(dump);
    fprintf(stderr, "IR dumped to %s\n", filename);
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

#ifndef NDEBUG
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
