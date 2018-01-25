#include "Python.h"
#include "ir.h"

/***********************************************************/
/*                  Helper functions                       */
/***********************************************************/


/* Compute stack level of a block just before the last instruction.
   pyblock_stack_level should be the b_level of the top pyblock
   (or 0 if none). A pop_block at the start of a block restores
   the stack level to pyblock_stack_level.

   NOTE: The last instruction of the block may affect the stack level,
         but since thisaffect may depend on the branch target,
         it is not computed by this function.
 */
static int
_end_stack_level(ir_block b, int entry_stack_level, int pyblock_stack_level) {
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
    ret->b_handler_precursor = NULL;
    ret->b_continue = NULL; /* NULL unless we see a goto_fbe with a continue_target */
    ret->b_level = b_level;
    ret->prev = prev;
    return ret;
}

static inline
void _transfer_stack_level(compute_pyblock_state *state, ir_label label, int level) {
    size_t index = label->block->index;
    int existing = state->incoming_stack_level[index];
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
                instr->pb = at;
                instr->entry_stack_level = in_stack;

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
                instr->pb = in;
                instr->entry_stack_level = in_stack;
                at = in->prev;
            } else {
                at = in;
            }
            state.at[b->index] = at;

            /* Compute exit stack level */
            assert(in_stack >= 0);
            int out_stack_level = _end_stack_level(b, in_stack, in ? in->b_level : 0);

            /* Decide what to do based on the control flow opcode */
            _instr = b->last_instr; /* Need to set this for IR_INSTR_AS */
            switch (_instr->opcode) {
            case ir_opcode_goto_error: {
                IR_INSTR_AS(goto_error)
                instr->pb = at;
                instr->entry_stack_level = out_stack_level;
                /* Only map fall through */
                if (instr->fallthrough) {
                    _transfer_pyblock(&state, instr->fallthrough, at, 0);
                    _transfer_stack_level(&state, instr->fallthrough, out_stack_level);
                }
                break;
            }
            case ir_opcode_goto_fbe: {
                IR_INSTR_AS(goto_fbe)
                instr->pb = at;
                instr->entry_stack_level = out_stack_level;
                if (instr->why == IR_WHY_CONTINUE) {
                    assert(instr->continue_target != NULL);
                    /* This continue binds to the nearest loop in the blockstack */
                    ir_pyblock cur = at;
                    while (cur && cur->b_type != IR_PYBLOCK_LOOP) {
                        cur = cur->prev;
                    }
                    assert(cur->b_type == IR_PYBLOCK_LOOP);
                    if (cur->b_continue == NULL) {
                        cur->b_continue = instr->continue_target;
                    } else if (cur->b_continue->block != instr->continue_target->block) {
                        Py_FatalError("'continue' target mismatch inside loop");
                    }
                }
                break;
            }
            case ir_opcode_yield: {
                IR_INSTR_AS(yield)
                instr->pb = at;
                instr->entry_stack_level = out_stack_level;

                _transfer_pyblock(&state, instr->resume_inst_label, at, 0);
                _transfer_stack_level(&state, instr->resume_inst_label, out_stack_level + 1);
                if (instr->throw_inst_label) {
                    _transfer_pyblock(&state, instr->throw_inst_label, at, 0);
                    _transfer_stack_level(&state, instr->throw_inst_label, out_stack_level);
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
                instr->pb = at;
                instr->entry_stack_level = out_stack_level;
                /* end_finally pops an EXCEPT_HANDLER block before branching */
                assert(at->b_type == IR_PYBLOCK_EXCEPT_HANDLER);
                assert(out_stack_level >= 6);
                _transfer_pyblock(&state, instr->fallthrough, at->prev, 0);
                _transfer_stack_level(&state, instr->fallthrough, out_stack_level - 6);
                break;
            }
            default: {
                size_t labels_count;
                ir_label *labels = ir_list_outgoing_labels(b, &labels_count);
                for (size_t i = 0; i < labels_count; i++) {
                    if (!labels[i]) continue;
                    _transfer_pyblock(&state, labels[i], at, 0);
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

/***********************************************************/
/*                    Stack Positions                      */
/***********************************************************/

int
ir_compute_stack_positions(ir_func func) {
    int num_blocks = ir_func_largest_block_index(func);
    int *incoming_stack_level = (int*)malloc(sizeof(int) * num_blocks);
    char *block_done = (char*)malloc(sizeof(char) * num_blocks);
    for (int i = 0; i < num_blocks; i++) {
        incoming_stack_level[i] = -1;
        block_done[i] = 0;
    }
    incoming_stack_level[func->entry_label->block->index] = 0;

    ir_block b;
    ir_instr _instr;
    int max_index = -1; /* maximum index accessed */
    int rerun;
    do {
        rerun = 0;
        for (b = func->first_block; b != NULL; b = b->next) {
            int stack_level = incoming_stack_level[b->index];
            if (stack_level == -1 || block_done[b->index]) continue;
            assert(stack_level >= 0);
            block_done[b->index] = 1;

            for (_instr = b->first_instr; _instr != NULL; _instr = _instr->next) {
                if (_instr->opcode == ir_opcode_stackadj) {
                    IR_INSTR_AS(stackadj)
                    stack_level += instr->amount;
                    assert(stack_level >= 0);
                } else if (_instr->opcode == ir_opcode_stack_peek) {
                    IR_INSTR_AS(stack_peek)
                    instr->abs_offset = stack_level - instr->offset;
                    assert(instr->abs_offset >= 0);
                    max_index = Py_MAX(max_index, instr->abs_offset);
                } else if (_instr->opcode == ir_opcode_stack_put) {
                    IR_INSTR_AS(stack_put)
                    instr->abs_offset = stack_level - instr->offset;
                    assert(instr->abs_offset >= 0);
                    max_index = Py_MAX(max_index, instr->abs_offset);
                } else {
                    /* Python-specific flow control should have been lowered */
                    assert(!(ir_opcode_is_python_specific(_instr->opcode) &&
                             ir_instr_opcode_is_flow_control(_instr->opcode)));
                }
            }
            /* Transfer the final stack level to target blocks */
            size_t count;
            ir_label *labels = ir_list_outgoing_labels(b, &count);
            for (size_t i = 0; i < count; i++) {
                if (!labels[i]) continue;
                int existing = incoming_stack_level[labels[i]->block->index];
                if (existing == -1) {
                    incoming_stack_level[labels[i]->block->index] = stack_level;
                    rerun = 1;
                } else if (existing != stack_level) {
                    Py_FatalError("Stack level mismatch when computing stack positions!");
                }
            }
        }
    } while (rerun);

#ifndef NDEBUG
    /* We should have assigned a stack slot to every peek/put */
    for (b = func->first_block; b != NULL; b = b->next) {
        for (_instr = b->first_instr; _instr != NULL; _instr = _instr->next) {
            if (_instr->opcode == ir_opcode_stack_put) {
                IR_INSTR_AS(stack_put)
                assert(instr->abs_offset >= 0);
            } else if (_instr->opcode == ir_opcode_stack_peek) {
                IR_INSTR_AS(stack_peek)
                assert(instr->abs_offset >= 0);
            }
        }
    }
#endif

    free(block_done);
    free(incoming_stack_level);
    return max_index + 1;
}
