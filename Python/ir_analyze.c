#include "Python.h"
#include "ir.h"
#include "adt_bitset.h"

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
        if (IR_INSTR_OPCODE(_instr) == ir_opcode_stackadj) {
            IR_INSTR_AS(stackadj)
            level += instr->amount;
            assert(level >= 0);
        } else if (IR_INSTR_OPCODE(_instr) == ir_opcode_pop_block) {
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
    int _index = IR_LABEL_BLOCK(label)->index; \
    if (!reachable[_index]) { \
        reachable[_index] = 1; \
        rerun = 1; \
    } \
} while (0)

void ir_remove_dead_blocks(ir_func func) {
    size_t num_blocks = ir_func_next_block_index(func);
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
            ir_use labels = ir_list_outgoing_labels(b, &labels_count);
            for (size_t i = 0; i < labels_count; i++) {
                MARK_REACHABLE(IR_USE_VALUE(labels[i]));
            }
            /* Look for labels in setup_block */
            assert(b->first_instr->next);
            if (IR_INSTR_OPCODE(b->first_instr->next) == ir_opcode_setup_block) {
                ir_instr _instr = b->first_instr->next;
                if (IR_INSTR_NUM_OPERANDS(_instr) > 0) {
                    MARK_REACHABLE(IR_INSTR_OPERAND(_instr, 0));
                }
            }
        }
    } while (rerun);

    /* Remove unreachable blocks */
    ir_block b_next;
    for (b = func->first_block; b != NULL; b = b_next) {
        b_next = b->next;
        if (!reachable[b->index]) {
            /* Remove all the instructions. This fixes the use lists. */
            while (b->last_instr != NULL) {
                _ir_remove_instr(b, b->last_instr);
            }
            IR_LL_REMOVE(func->first_block, func->last_block, b);
        }
    }
    func->current_block = NULL;
}

/***********************************************************/
/*                   PyBlocks Analysis                     */
/***********************************************************/

//#define ANALYZE_PYBLOCKS_DEBUG

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
    ir_func func;
    ir_label unknown_handler;
    ir_label exit_label;
    size_t num_blocks;
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
    ir_pyblock ret = (ir_pyblock)_ir_alloc(state->func->context, sizeof(ir_pyblock_t), _alignof(ir_pyblock_t));
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
    size_t index = IR_LABEL_BLOCK(label)->index;
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
    size_t index = IR_LABEL_BLOCK(label)->index;
    ir_pyblock existing = state->incoming[index];
#ifdef ANALYZE_PYBLOCKS_DEBUG
    {
        char pb_repr[1024];
        ir_pyblock_repr(pb_repr, pb);
        fprintf(stderr, "  branch to block %zu from pyblock %s%s\n",
                index, pb_repr, except_handler ? " (+ except handler)" : "");
    }
#endif
    if (existing == INVALID_PYBLOCK) {
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

static
ir_label _compute_handler(compute_pyblock_state *state, ir_pyblock pb, ir_why why) {
    ir_pyblock cur = pb;
    while (cur) {
        if (cur->b_type == IR_PYBLOCK_LOOP && why == IR_WHY_CONTINUE) {
            assert(cur->b_continue != NULL);
            return cur->b_continue;
        }
        if (cur->b_type == IR_PYBLOCK_LOOP && why == IR_WHY_BREAK) {
            assert(cur->b_handler != NULL);
            return cur->b_handler;
        }
        if (why == IR_WHY_EXCEPTION && (cur->b_type == IR_PYBLOCK_EXCEPT ||
                                        cur->b_type == IR_PYBLOCK_FINALLY_TRY)) {
            assert(cur->b_handler != NULL);
            return cur->b_handler;
        }
        if (cur->b_type == IR_PYBLOCK_FINALLY_TRY) {
            assert(cur->b_handler != NULL);
            return cur->b_handler;
        }
        cur = cur->prev;
    }
    assert(why == IR_WHY_RETURN || why == IR_WHY_EXCEPTION);
    assert(state->exit_label != NULL);
    return state->exit_label;
}

/* Return the pyblock for the closest loop relative to 'pb'.
   This is the one that CONTINUE and BREAK operate relative to. */
static
ir_pyblock _bound_loop(ir_pyblock pb) {
    ir_pyblock cur = pb;
    while (cur) {
        if (cur->b_type == IR_PYBLOCK_LOOP) return cur;
        cur = cur->prev;
    }
    return NULL;
}

/* Statically determine the PyTryBlock stack at eack block of 'func' */
void
ir_compute_pyblock_info(ir_func func, ir_label unknown_handler, ir_label exit_label) {
    size_t num_blocks = ir_func_next_block_index(func);
    compute_pyblock_state state;
    state.func = func;
    state.unknown_handler = unknown_handler;
    state.exit_label = exit_label;
    state.num_blocks = num_blocks;
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
    state.incoming[func->entry_block->index] = NULL;
    state.incoming_stack_level[func->entry_block->index] = 0;

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

            if (in == INVALID_PYBLOCK || at != NULL)
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
            if (IR_INSTR_OPCODE(_instr) == ir_opcode_setup_block) {
                IR_INSTR_AS(setup_block)
                ir_label b_handler = NULL;
                if (IR_INSTR_NUM_OPERANDS(_instr) > 0) {
                    b_handler = IR_INSTR_OPERAND(_instr, 0);
                }
                at = _new_pyblock(&state, instr->b_type, b_handler, in_stack, in);
                instr->pb = at;
                instr->entry_stack_level = in_stack;

                /* There is an implicit branch to 'b_handler' after this block
                   has been popped (i.e. at the 'in' level). For EXCEPT or FINALLY_TRY,
                   the branch also adds an implicit EXCEPT_HANDLER block. */
                if (instr->b_type == IR_PYBLOCK_EXCEPT ||
                    instr->b_type == IR_PYBLOCK_FINALLY_TRY) {
                    _transfer_pyblock(&state, b_handler, at, 1);
                    _transfer_stack_level(&state, b_handler, in_stack + 6);
                } else if (instr->b_type == IR_PYBLOCK_LOOP) {
                    _transfer_pyblock(&state, b_handler, at->prev, 0);
                    _transfer_stack_level(&state, b_handler, in_stack);
                }
            } else if (IR_INSTR_OPCODE(_instr) == ir_opcode_pop_block) {
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
            switch (IR_INSTR_OPCODE(_instr)) {
            case ir_opcode_goto_error: {
                IR_INSTR_AS(goto_error)
                instr->pb = at;
                instr->entry_stack_level = out_stack_level;
                break;
            }
            case ir_opcode_getlocal: {
                IR_INSTR_AS(getlocal)
                instr->pb = at;
                instr->entry_stack_level = out_stack_level;
                ir_label fallthrough = IR_INSTR_OPERAND(_instr, 0);
                _transfer_pyblock(&state, fallthrough, at, 0);
                _transfer_stack_level(&state, fallthrough, out_stack_level);
                break;
            }
            case ir_opcode_goto_fbe: {
                IR_INSTR_AS(goto_fbe)
                instr->pb = at;
                instr->entry_stack_level = out_stack_level;
                if (instr->why == IR_WHY_CONTINUE) {
                    ir_label continue_target = IR_INSTR_OPERAND(_instr, 1);

                    /* This continue belongs to the nearest loop in the blockstack */
                    ir_pyblock loop = _bound_loop(at);
                    assert(loop);
                    if (loop->b_continue == NULL) {
                        loop->b_continue = continue_target;
                    } else if (IR_LABEL_BLOCK(loop->b_continue) != IR_LABEL_BLOCK(continue_target)) {
                        Py_FatalError("'continue' target mismatch inside loop");
                    }
                }
                break;
            }
            case ir_opcode_yield: {
                IR_INSTR_AS(yield)
                instr->pb = at;
                instr->entry_stack_level = out_stack_level;
                ir_label resume_inst_label = IR_INSTR_OPERAND(_instr, 2);
                _transfer_pyblock(&state, resume_inst_label, at, 0);
                _transfer_stack_level(&state, resume_inst_label, out_stack_level + 1);
                if (IR_INSTR_NUM_OPERANDS(_instr) > 3) {
                    ir_label throw_inst_label = IR_INSTR_OPERAND(_instr, 3);
                    _transfer_pyblock(&state, throw_inst_label, at, 0);
                    _transfer_stack_level(&state, throw_inst_label, out_stack_level);
                }
                break;
            }
            case ir_opcode_yield_dispatch: {
                ir_label body_start = IR_INSTR_OPERAND(_instr, 0);
                _transfer_pyblock(&state, body_start, NULL, 0);
                _transfer_stack_level(&state, body_start, 0);
                break;
            }
            case ir_opcode_end_finally: {
                IR_INSTR_AS(end_finally)
                instr->pb = at;
                instr->entry_stack_level = out_stack_level;
                ir_label fallthrough = IR_INSTR_OPERAND(_instr, 0);

                /* end_finally pops an EXCEPT_HANDLER block before branching */
                assert(at->b_type == IR_PYBLOCK_EXCEPT_HANDLER);
                assert(out_stack_level >= 6);
                _transfer_pyblock(&state, fallthrough, at->prev, 0);
                _transfer_stack_level(&state, fallthrough, out_stack_level - 6);
                break;
            }
            default: {
                size_t labels_count;
                ir_use labels = ir_list_outgoing_labels(b, &labels_count);
                for (size_t i = 0; i < labels_count; i++) {
                    ir_value label = IR_USE_VALUE(labels[i]);
                    _transfer_pyblock(&state, label, at, 0);
                    _transfer_stack_level(&state, label, out_stack_level);
                }
                break;
            }
            } // switch
        }
    } while (state.rerun);

    /* Go through and assign the handlers to the instruction operations.
       This is deferred because of continue targets, which may not be assigned
       at the time these instructions are seen above. */
    for (ir_block b = func->first_block; b != NULL; b = b->next) {
        ir_instr _instr = b->last_instr;
        ir_opcode opcode = IR_INSTR_OPCODE(_instr);
        switch (opcode) {
        case ir_opcode_goto_error: {
            IR_INSTR_AS(goto_error)
            if (instr->pb == INVALID_PYBLOCK) continue;
            IR_REPLACE_OPERAND(_instr, 0, _compute_handler(&state, instr->pb, IR_WHY_EXCEPTION));
            break;
        }
        case ir_opcode_getlocal: {
            IR_INSTR_AS(getlocal)
            if (instr->pb == INVALID_PYBLOCK) continue;
            IR_REPLACE_OPERAND(_instr, 1, _compute_handler(&state, instr->pb, IR_WHY_EXCEPTION));
            break;
        }
        case ir_opcode_goto_fbe: {
            IR_INSTR_AS(goto_fbe)
            if (instr->pb == INVALID_PYBLOCK) continue;
            IR_REPLACE_OPERAND(_instr, 0, _compute_handler(&state, instr->pb, instr->why));
            break;
        }
        case ir_opcode_end_finally: {
            IR_INSTR_AS(end_finally)
            if (instr->pb == INVALID_PYBLOCK) continue;
            IR_REPLACE_OPERAND(_instr, 1, _compute_handler(&state, instr->pb, IR_WHY_EXCEPTION));
            IR_REPLACE_OPERAND(_instr, 2, _compute_handler(&state, instr->pb, IR_WHY_RETURN));
            ir_pyblock loop = _bound_loop(instr->pb);
            if (loop) {
                IR_REPLACE_OPERAND(_instr, 3, _compute_handler(&state, instr->pb, IR_WHY_BREAK));
                if (loop->b_continue) {
                    IR_REPLACE_OPERAND(_instr, 4, _compute_handler(&state, instr->pb, IR_WHY_CONTINUE));
                }
            }
            break;
        }
        case ir_opcode_yield: {
            IR_INSTR_AS(yield)
            if (instr->pb == INVALID_PYBLOCK) continue;
            IR_REPLACE_OPERAND(_instr, 1, _compute_handler(&state, instr->pb, IR_WHY_EXCEPTION));
            break;
        }
        default:
            break; {
        }
        } //switch
    }

    free(state.incoming);
    free(state.incoming_stack_level);
    free(state.at);
}

/***********************************************************/
/*                    Stack Positions                      */
/***********************************************************/

int
ir_compute_stack_positions(ir_func func) {
    size_t num_blocks = ir_func_next_block_index(func);
    int *incoming_stack_level = (int*)malloc(sizeof(int) * num_blocks);
    char *block_done = (char*)malloc(sizeof(char) * num_blocks);
    for (size_t i = 0; i < num_blocks; i++) {
        incoming_stack_level[i] = -1;
        block_done[i] = 0;
    }
    incoming_stack_level[func->entry_block->index] = 0;

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
                ir_opcode opcode = IR_INSTR_OPCODE(_instr);
                if (opcode == ir_opcode_stackadj) {
                    IR_INSTR_AS(stackadj)
                    stack_level += instr->amount;
                    assert(stack_level >= 0);
                } else if (opcode == ir_opcode_stack_peek) {
                    IR_INSTR_AS(stack_peek)
                    instr->abs_offset = stack_level - instr->offset;
                    assert(instr->abs_offset >= 0);
                    max_index = Py_MAX(max_index, instr->abs_offset);
                } else if (opcode == ir_opcode_stack_put) {
                    IR_INSTR_AS(stack_put)
                    instr->abs_offset = stack_level - instr->offset;
                    assert(instr->abs_offset >= 0);
                    max_index = Py_MAX(max_index, instr->abs_offset);
                } else {
                    /* Python-specific control flow should have been lowered */
                    assert(!(ir_opcode_is_python_specific(opcode) &&
                             ir_opcode_is_control_flow(opcode)));
                }
            }
            /* Transfer the final stack level to target blocks */
            size_t count;
            ir_use labels = ir_list_outgoing_labels(b, &count);
            for (size_t i = 0; i < count; i++) {
                ir_block target = IR_LABEL_BLOCK(IR_USE_VALUE(labels[i]));
                int existing = incoming_stack_level[target->index];
                if (existing == -1) {
                    incoming_stack_level[target->index] = stack_level;
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
            if (IR_INSTR_OPCODE(_instr) == ir_opcode_stack_put) {
                IR_INSTR_AS(stack_put)
                assert(instr->abs_offset >= 0);
            } else if (IR_INSTR_OPCODE(_instr) == ir_opcode_stack_peek) {
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

/***********************************************************/
/*                    Mark defined locals                  */
/***********************************************************/

typedef struct {
    adt_bitset in;
} _defined_locals_block_info;

void ir_mark_defined_locals(ir_func func, size_t nlocals) {
    // This is a forward data-flow analysis. The goal is to compute,
    // for each basic block, which locals are guaranteed to be defined at
    // entry (and exit) of the block. This data is tracked using a bitset
    // where the i'th bit corresponds to the i'th local.
    if (nlocals == 0)
        return;

    size_t num_blocks = ir_func_next_block_index(func);
    _defined_locals_block_info *info_for_block = (_defined_locals_block_info*)malloc(sizeof(_defined_locals_block_info) * num_blocks);
    for (size_t i = 0; i < num_blocks; i++) {
       info_for_block[i].in = adt_bitset_alloc(nlocals);
       adt_bitset_setall(info_for_block[i].in, 1);
    }

    /* All locals are unset on entry */
    adt_bitset_setall(info_for_block[func->entry_block->index].in, 0);

    adt_bitset out = adt_bitset_alloc(nlocals);
    adt_bitset tmp = adt_bitset_alloc(nlocals);

    /* Track which blocks have had their 'in' bitset change */
    adt_bitset dirty = adt_bitset_alloc(num_blocks);
    adt_bitset_setall(dirty, 1);
    int rerun;
    do {
        rerun = 0;
        for (ir_block b = func->first_block; b != NULL; b = b->next) {
            if (!adt_bitset_getbit(dirty, b->index)) continue;
            adt_bitset_setbit(dirty, b->index, 0);

            /* Compute outgoing bitset for this block */
            adt_bitset_copy(out, info_for_block[b->index].in);
            for (ir_instr _instr = b->first_instr; _instr != NULL; _instr = _instr->next) {
                ir_opcode opcode = IR_INSTR_OPCODE(_instr);
                if (opcode == ir_opcode_getlocal) {
                    IR_INSTR_AS(getlocal)
                    instr->known_defined = adt_bitset_getbit(out, instr->index);
                } else if (opcode == ir_opcode_setlocal) {
                    IR_INSTR_AS(setlocal)
                    adt_bitset_setbit(out, instr->index, 1);
                } else if (opcode == ir_opcode_dellocal) {
                    IR_INSTR_AS(dellocal)
                    adt_bitset_setbit(out, instr->index, 0);
                }
            }

            /* AND into the in bitsets of branch targets */
            /* TODO: Optimistically assume that getlocal exception branches don't occur
                     when the is_defined bit is still true in this pass. This can
                     increase the opportunities for optimization in some cases. */
            size_t labels_count;
            ir_use labels = ir_list_outgoing_labels(b, &labels_count);
            for (size_t i = 0; i < labels_count; i++) {
                ir_label label = IR_USE_VALUE(labels[i]);
                size_t label_index = IR_LABEL_BLOCK(label)->index;
                adt_bitset existing_in = info_for_block[label_index].in;
                adt_bitset_and(tmp, out, existing_in);
                /* If tmp != existing_in, mark the block as dirty */
                if (!adt_bitset_eq(tmp, existing_in)) {
                    adt_bitset_copy(existing_in, tmp);
                    adt_bitset_setbit(dirty, label_index, 1);
                    rerun = 1;
                }
            }
        }
    } while (rerun);
    adt_bitset_free(dirty);
    adt_bitset_free(tmp);
    adt_bitset_free(out);
    for (size_t i = 0; i < num_blocks; i++) {
       adt_bitset_free(info_for_block[i].in);
    }
    free(info_for_block);
}
