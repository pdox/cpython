#include "Python.h"
#include "ir.h"

/***********************************************************/
/*                  Helper functions                       */
/***********************************************************/


/* Compute exit stack level of a block. pyblock_stack_level should be
   the b_level of the top pyblock (or 0 if none). A pop_block inside the
   block restores the stack level to pyblock_stack_level.

   There are three other special cases:
     1) If a block ends with yield_dispatch or goto_fbe, UNMODELED_STACK_EFFECT is returned.
     2) If a block ends with yield, the value returned assumes we are branching
        to resume_inst_label. This assumes an additional element has been pushed onto
        the stack from outside the evaluator.
     3) If a block ends with end_finally, the value returned assumes we are taking the
        fallthrough label.
 */
#define UNMODELED_STACK_EFFECT  (~((int)0))
static int
_exit_stack_level(ir_block b, int entry_stack_level, int pyblock_stack_level) {
    ir_instr _instr;
    int level = entry_stack_level;
    for (_instr = b->first_instr; _instr != NULL; _instr = _instr->next) {
        if (_instr->opcode == ir_opcode_stackadj) {
            IR_INSTR_AS(stackadj)
            level += instr->amount;
            assert(level >= 0);
        } else if (_instr->opcode == ir_opcode_pop_block) {
            assert(level >= pyblock_stack_level);
            level = pyblock_stack_level;
        } else if (_instr->opcode == ir_opcode_yield) {
            level += 1;
        } else if (_instr->opcode == ir_opcode_end_finally) {
            level -= 6;
            assert(level == pyblock_stack_level);
        } else if (_instr->opcode == ir_opcode_yield_dispatch ||
                   _instr->opcode == ir_opcode_goto_fbe) {
            return UNMODELED_STACK_EFFECT;
        }
    }
    return level;
}

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
    int *incoming_stack_level;
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

static inline
void _transfer_stack_level(compute_pyblock_state *state, ir_label label, int level) {
    size_t index = label->block->index;
    int existing = state->incoming_stack_level[index];
    assert(level != UNMODELED_STACK_EFFECT);
    if (existing == -1) {
        state->incoming_stack_level[index] = level;
        state->rerun = 1;
    } else if (existing != level) {
        Py_FatalError("Stack level mismatch during pyblock analysis");
    }
}

/* Update incoming data, taking into account a branch to "label" with pyblock "pb".
   If except_handler is true, implicitly switches pb with an EXCEPT_HANDLER pyblock.
 */
static inline
void _transfer_pyblock(compute_pyblock_state *state, ir_label label, ir_pyblock pb, int except_handler) {
    size_t index = label->block->index;
    ir_pyblock existing = state->incoming[index];
#ifdef ANALYZE_PYBLOCKS_DEBUG
    {
        char pb_repr[1024];
        ir_pyblock_repr(pb_repr, pb);
        fprintf(stderr, "  branch to block %zu from pyblock %s%s\n",
                index, pb_repr, except_handler ? " (+ except handler)" : "");
    }
#endif
    if (existing == IGNORE_PYBLOCK) {
        /* Ignored */
    } else if (existing == INVALID_PYBLOCK) {
        assert(!except_handler || pb != NULL);
        state->incoming[index] = except_handler ?
                                 _new_pyblock(state, IR_PYBLOCK_EXCEPT_HANDLER, NULL, pb->b_level, pb->prev) :
                                 pb;
        state->rerun = 1;
    } else {
        /* Ensure they are the same */
        if (except_handler) {
            if (existing == NULL || existing->b_type != IR_PYBLOCK_EXCEPT_HANDLER) {
                Py_FatalError("Non-EXCEPT_HANDLER target in pyblock assignments!");
            } else if (existing->prev != pb->prev || existing->b_level != pb->b_level) {
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
    state.incoming_stack_level = (int*)malloc(sizeof(int) * num_blocks);
    state.at = (ir_pyblock*)malloc(sizeof(ir_pyblock) * num_blocks);

    /* Initialize incoming entries to INVALID_PYBLOCK */
    for (size_t i = 0; i < num_blocks; i++) {
        state.incoming[i] = INVALID_PYBLOCK;
        state.incoming_stack_level[i] = -1;
    }
    memset(state.at, 0, sizeof(ir_pyblock) * num_blocks);

    /* Entry block starts with empty blockstack */
    state.incoming[func->entry_label->block->index] = NULL;
    state.incoming_stack_level[func->entry_label->block->index] = 0;

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
            int in_stack = state.incoming_stack_level[b->index];
            ir_instr _instr = b->first_instr->next;
            assert(_instr);
            if (_instr->opcode == ir_opcode_setup_block) {
                IR_INSTR_AS(setup_block)
                at = _new_pyblock(&state, instr->b_type, instr->b_handler, in_stack, in);

                /* There is an implicit branch to 'b_handler' after this block
                   has been popped (i.e. at the 'in' level). For EXCEPT or FINALLY_TRY,
                   the branch also adds an implicit EXCEPT_HANDLER block. */
                if (instr->b_type == IR_PYBLOCK_EXCEPT ||
                    instr->b_type == IR_PYBLOCK_FINALLY_TRY) {
                    _transfer_pyblock(&state, instr->b_handler, at, 1);
                    _transfer_stack_level(&state, instr->b_handler, in_stack + 6);
                } else if (instr->b_type == IR_PYBLOCK_LOOP) {
                    _transfer_pyblock(&state, instr->b_handler, at->prev, 0);
                    _transfer_stack_level(&state, instr->b_handler, in_stack);
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

            /* Compute exit stack level */
            assert(in_stack >= 0);
            int out_stack_level = _exit_stack_level(b, in_stack, in ? in->b_level : 0);
            ir_pyblock out = at;

            /* Decide what to do based on the control flow opcode */
            _instr = b->last_instr; /* Need to set this for IR_INSTR_AS */
            switch (_instr->opcode) {
            case ir_opcode_goto_error: {
                IR_INSTR_AS(goto_error)
                /* Only map fall through */
                if (instr->fallthrough) {
                    _transfer_pyblock(&state, instr->fallthrough, out, 0);
                    _transfer_stack_level(&state, instr->fallthrough, out_stack_level);
                }
                break;
            }
            case ir_opcode_goto_fbe: {
                /* Fast block end pops all non-loop blocks before it branches to the continue target 
                   Rather than try to make this work, just ignore the labels on goto_fbe. */
                break;
            }
            case ir_opcode_yield: {
                IR_INSTR_AS(yield)
                _transfer_pyblock(&state, instr->resume_inst_label, out, 0);
                _transfer_stack_level(&state, instr->resume_inst_label, out_stack_level);
                if (instr->throw_inst_label) {
                    _transfer_pyblock(&state, instr->throw_inst_label, out, 0);
                    _transfer_stack_level(&state, instr->throw_inst_label, out_stack_level - 1);
                }
                break;
            }
            case ir_opcode_yield_dispatch: {
                IR_INSTR_AS(yield_dispatch)
                _transfer_pyblock(&state, instr->body_start, NULL, 0);
                _transfer_stack_level(&state, instr->body_start, 0);
                break;
            }
            case ir_opcode_end_finally: {
                IR_INSTR_AS(end_finally)
                /* end_finally pops an EXCEPT_HANDLER block before branching */
                assert(out->b_type == IR_PYBLOCK_EXCEPT_HANDLER);
                _transfer_pyblock(&state, instr->fallthrough, out->prev, 0);
                _transfer_stack_level(&state, instr->fallthrough, out_stack_level);
                break;
            }
            default: {
                size_t labels_count;
                ir_label *labels = ir_list_outgoing_labels(b, &labels_count);
                for (size_t i = 0; i < labels_count; i++) {
                    if (!labels[i]) continue;
                    _transfer_pyblock(&state, labels[i], out, 0);
                    _transfer_stack_level(&state, labels[i], out_stack_level);
                }
                break;
            }
            } // switch
        }
    } while (state.rerun);

    ir_pyblock_map map = (ir_pyblock_map)malloc(sizeof(ir_pyblock_map_t));
    map->num_blocks = num_blocks;
    map->_mem = state.mem;
    map->at = state.at;
    map->stack_level = state.incoming_stack_level;
    free(state.incoming);
    return map;
}

void ir_dump_pyblock_map(ir_pyblock_map map, const char *filename) {
    FILE *fp = fopen(filename, "w");
    assert(fp);
    for (int i = 0; i < map->num_blocks; i++) {
        fprintf(fp, "block %d: stack_level=%d   block_stack=", i, map->stack_level[i]);

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
    free(map->at);
    free(map->stack_level);
    free(map);
}
