#include "Python.h"
#include "ir.h"

/***********************************************************/
/*                  Dead Blocks Analysis                   */
/***********************************************************/

/* Remove dead blocks */
#define MARK_REACHABLE(label) do { \
    int _index = (label)->block->index; \
    if (!reachable[_index]) { \
        reachable[_index] = 1; \
        rerun = 1; \
    } \
} while (0)

void ir_remove_dead_blocks(ir_func func) {
    size_t num_blocks = ir_func_largest_block_index(func);
    char *reachable = (char*)malloc(sizeof(char) * num_blocks);
    memset(reachable, 0, sizeof(char) * num_blocks);
    reachable[func->first_block->index] = 1;

    /* Determine which blocks are reachable */
    int rerun;
    ir_block b;
    do {
        rerun = 0;
        for (b = func->first_block; b != NULL; b = b->next) {
            if (!reachable[b->index]) continue;
            size_t labels_count;
            ir_label *labels = ir_list_outgoing_labels(b, &labels_count);
            for (size_t i = 0; i < labels_count; i++) {
                if (!labels[i]) continue;
                MARK_REACHABLE(labels[i]);
            }
            /* Look for labels in setup_block */
            assert(b->first_instr->next);
            if (b->first_instr->next->opcode == ir_opcode_setup_block) {
                ir_instr _instr = b->first_instr->next;
                IR_INSTR_AS(setup_block);
                if (instr->b_handler) {
                    MARK_REACHABLE(instr->b_handler);
                }
            }
        }
    } while (rerun);

    /* Remove unreachable blocks */
    ir_block b_next;
    for (b = func->first_block; b != NULL; b = b_next) {
        b_next = b->next;
        if (!reachable[b->index]) {
            IR_LL_REMOVE(func->first_block, func->last_block, b);
        }
    }
    func->current_block = NULL;
}

/***********************************************************/
/*                   PyBlocks Analysis                     */
/***********************************************************/

//#define ANALYZE_PYBLOCKS_DEBUG

/* Marks an uninitialized pyblock value */
#define INVALID_PYBLOCK   ((ir_pyblock)(~0))

/* Marks a block that is being ignored in pyblock analysis */
#define IGNORE_PYBLOCK    ((ir_pyblock)(~1))

char* ir_pyblock_repr(char *p, ir_pyblock pb) {
    if (pb == NULL) {
        p += sprintf(p, "NULL");
        return p;
    }
    p += sprintf(p, "%p:(%s, b_handler=", pb, ir_pyblock_type_repr(pb->b_type));
    if (pb->b_handler) {
        p = ir_label_repr(p, pb->b_handler);
    } else {
        p += sprintf(p, "NULL");
    }
    p += sprintf(p, ", b_level=%d, prev=%p)", pb->b_level, pb->prev);
    return p;
}

typedef struct {
    size_t num_blocks;
    ir_pyblock mem;
    size_t memi;
    ir_pyblock *incoming;
    ir_pyblock *at;
    int rerun;
} compute_pyblock_state;

static inline
ir_pyblock _new_pyblock(
        compute_pyblock_state *state,
        ir_pyblock_type b_type,
        ir_label b_handler,
        int b_level,
        ir_pyblock prev) {
    ir_pyblock ret = &state->mem[state->memi++];
    assert(state->memi <= state->num_blocks);
    ret->b_type = b_type;
    ret->b_handler = b_handler;
    ret->b_level = b_level;
    ret->prev = prev;
    return ret;
}

/* Update incoming data, taking into account a branch to "label" with pyblock "pb".
   If add_except_handler is true, this branch implicitly adds an EXCEPT_HANDLER pyblock.
 */
static inline
void _transfer_pyblock(compute_pyblock_state *state, ir_label label, ir_pyblock pb, int add_except_handler) {
    size_t index = label->block->index;
    ir_pyblock existing = state->incoming[index];
#ifdef ANALYZE_PYBLOCKS_DEBUG
    {
        char pb_repr[1024];
        ir_pyblock_repr(pb_repr, pb);
        fprintf(stderr, "  branch to block %zu from pyblock %s%s\n",
                index, pb_repr, add_except_handler ? " (+ except handler)" : "");
    }
#endif
    if (existing == IGNORE_PYBLOCK) {
        /* Ignored */
    } else if (existing == INVALID_PYBLOCK) {
        state->incoming[index] = add_except_handler ?
                                 _new_pyblock(state, IR_PYBLOCK_EXCEPT_HANDLER, NULL, pb ? pb->b_level : 0, pb) :
                                 pb;
        state->rerun = 1;
    } else {
        /* Ensure they are the same */
        if (add_except_handler) {
            if (existing == NULL || existing->b_type != IR_PYBLOCK_EXCEPT_HANDLER) {
                Py_FatalError("Non-EXCEPT_HANDLER target in pyblock assignments!");
            } else if (existing->prev != pb || existing->b_level != pb->b_level) {
                Py_FatalError("Mismatched EXCEPT_HANDLERs in pyblock assignments!");
            }
        } else {
            if (existing && existing->b_type == IR_PYBLOCK_EXCEPT_HANDLER &&
                pb && pb->b_type == IR_PYBLOCK_EXCEPT_HANDLER) {
                if (existing->prev != pb->prev || existing->b_level != pb->b_level) {
                    Py_FatalError("Mismatched EXCEPT_HANDLERs in pyblock assignments!");
                }
            } else if (existing != pb) {
                Py_FatalError("Inconsistent pyblock assignments!");
            }
        }
    }
}

/* Statically determine the PyTryBlock stack at eack block of 'func' */
ir_pyblock_map
ir_compute_pyblock_map(ir_func func, ir_label *ignored) {
    compute_pyblock_state state;
    size_t num_blocks = state.num_blocks = ir_func_largest_block_index(func);
    state.memi = 0;
    state.mem = (ir_pyblock)malloc(sizeof(ir_pyblock_t) * num_blocks);
    state.incoming = (ir_pyblock*)malloc(sizeof(ir_pyblock) * num_blocks);
    state.at = (ir_pyblock*)malloc(sizeof(ir_pyblock) * num_blocks);

    /* Initialize incoming entries to INVALID_PYBLOCK */
    for (size_t i = 0; i < num_blocks; i++) {
        state.incoming[i] = INVALID_PYBLOCK;
    }
    memset(state.at, 0, sizeof(ir_pyblock) * num_blocks);

    /* Entry block starts with empty blockstack */
    state.incoming[func->entry_label->block->index] = NULL;

    /* Jumps to special sections should be be ignored */
    for (size_t i = 0; ignored[i]; i++) {
        state.incoming[ignored[i]->block->index] = IGNORE_PYBLOCK;
    }

    /* For each block, determine the incoming block stack from all
       branches into the block. Make sure they are identical.
       And then compute the outgoing block stack, and propagate
       to other blocks. */
    do {
        state.rerun = 0;
        ir_block b;
        for (b = func->first_block; b != NULL; b = b->next) {
            ir_pyblock in = state.incoming[b->index];
            ir_pyblock at = state.at[b->index];

            if (in == INVALID_PYBLOCK || in == IGNORE_PYBLOCK || at != NULL)
                continue;

#ifdef ANALYZE_PYBLOCKS_DEBUG
            {
                char pb_repr[1024];
                ir_pyblock_repr(pb_repr, in);;
                fprintf(stderr,
                    "Starting block %zu with incoming pyblock %s\n", b->index, pb_repr);
            }
#endif
            ir_instr _instr = b->first_instr->next;
            assert(_instr);
            if (_instr->opcode == ir_opcode_setup_block) {
                IR_INSTR_AS(setup_block)
                at = _new_pyblock(&state, instr->b_type, instr->b_handler, 0, in);

                /* There is an implicit branch to 'b_handler' after this block
                   has been popped (i.e. at the 'in' level). For EXCEPT or FINALLY_TRY,
                   the branch also adds an implicit EXCEPT_HANDLER block. */
                if (instr->b_type == IR_PYBLOCK_EXCEPT ||
                    instr->b_type == IR_PYBLOCK_FINALLY_TRY) {
                    _transfer_pyblock(&state, instr->b_handler, in, 1);
                }
            } else if (_instr->opcode == ir_opcode_pop_block) {
                IR_INSTR_AS(pop_block)
                assert(in != NULL);
                if (instr->b_type == IR_PYBLOCK_EXCEPT_HANDLER) {
                    assert(in->b_type == IR_PYBLOCK_EXCEPT_HANDLER);
                } else {
                    assert(instr->b_type == IR_PYBLOCK_ANY);
                }
                at = in->prev;
            } else {
                at = in;
            }
            state.at[b->index] = at;

            /* For each block this block may branch to, set incoming pyblock to 'at' */
            size_t labels_count;
            ir_label *labels = ir_list_outgoing_labels(b, &labels_count);
            ir_pyblock out = at;
            if (b->last_instr->opcode == ir_opcode_goto_fbe) {
                /* Fast block end pops all non-loop blocks before it branches to the continue target 
                   Rather than try to make this work, just ignore the labels on goto_fbe. */
                labels_count = 0;
                labels = NULL;
            } else if (b->last_instr->opcode == ir_opcode_end_finally) {
                /* end_finally pops an EXCEPT_HANDLER block before branching */
                assert(at->b_type == IR_PYBLOCK_EXCEPT_HANDLER);
                out = at->prev;
            }
            for (size_t i = 0; i < labels_count; i++) {
                if (!labels[i]) continue;
                _transfer_pyblock(&state, labels[i], out, 0);
            }
        }
    } while (state.rerun);

    free(state.incoming);

    ir_pyblock_map map = (ir_pyblock_map)malloc(sizeof(ir_pyblock_map_t));
    map->num_blocks = num_blocks;
    map->_mem = state.mem;
    map->at = state.at;
    return map;
}

void ir_dump_pyblock_map(ir_pyblock_map map, const char *filename) {
    FILE *fp = fopen(filename, "w");
    assert(fp);
    for (int i = 0; i < map->num_blocks; i++) {
        fprintf(fp, "block %d:", i);

        ir_pyblock last = NULL;
        while (last != map->at[i]) {
            /* List in reverse order. This is O(n^2), but n is at most 10 */
            ir_pyblock cur = map->at[i];
            while (cur->prev != last) cur = cur->prev;
            last = cur;

            char handler[64];
            if (cur->b_handler) {
                sprintf(handler, "block_%d", (int)cur->b_handler->block->index);
            } else {
                sprintf(handler, "NULL");
            }
            fprintf(fp, " %s(%s)", ir_pyblock_type_repr(cur->b_type), handler);
        }
        fprintf(fp, "\n");
    }
    fclose(fp);
    fprintf(stderr, "Dumped pyblock map to %s\n", filename);
}

/* Free pyblocks map */
void ir_free_pyblock_map(ir_pyblock_map map) {
    free(map->_mem);
    free(map);
}
