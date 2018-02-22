#include "Python.h"
#include "ir.h"
#include "adt.h"

/* Compute dominator tree using the iterative algorithm described in:

       "A Simple, Fast Dominance Algorithm"
       Cooper, Keith D.; Harvey, Timothy J; Kennedy, Ken (2001).
       https://www.cs.rice.edu/~keith/Embed/dom.pdf

 */

IR_PROTOTYPE(runstate)
struct runstate_t {
    ir_func func;
    size_t num_blocks;
    size_t max_preorder_index;

    /* Maps block index to preorder index */
    size_t *index_map;

    /* Maps preorder index to block */
    ir_block *reverse_map;

    /* doms table. Indexed by preorder. Values are also preorder index.
       0 represents 'undefined'
     */
    size_t doms[0];
};

static void _setup_preordering(runstate state) {
    size_t next_preorder_index = 1;
    adt_vector worklist = adt_vector_new(64, sizeof(ir_block));
    adt_vector_push_back(worklist, &state->func->entry_block);
    ir_block b;
    while (adt_vector_pop_back(worklist, &b)) {
        /* Skip if already visited */
        if (state->index_map[b->index] != 0)
            continue;
        size_t preorder_index = next_preorder_index++;
        state->index_map[b->index] = preorder_index;
        state->reverse_map[preorder_index] = b;

        /* Add successors to worklist */
        size_t label_count;
        ir_use labels = ir_list_outgoing_labels(b, &label_count);
        for (size_t i = 0; i < label_count; i++) {
            ir_block successor_block = IR_LABEL_BLOCK(IR_USE_VALUE(labels[i]));
            if (state->index_map[successor_block->index] == 0) {
                adt_vector_push_back(worklist, &successor_block);
            }
        }
    }
    state->max_preorder_index = next_preorder_index - 1;
    assert(state->max_preorder_index <= state->num_blocks);
    adt_vector_delete(worklist);
}

static inline size_t _intersect(runstate state, size_t b1, size_t b2) {
    size_t f1 = b1;
    size_t f2 = b2;
    while (f1 != f2) {
        while (f1 > f2) f1 = state->doms[f1];
        while (f2 > f1) f2 = state->doms[f2];
    }
    return f1;
}

static void _solve(runstate state) {
    /* Initialize doms for the entry_block */
    assert(state->index_map[state->func->entry_block->index] == 1);
    state->doms[1] = 1;
    int changed = 1;
    while (changed) {
        changed = 0;
        for (size_t i = 2; i <= state->max_preorder_index; i++) {
            size_t new_idom = 0;

            /* Iterate over predecessors */
            ir_block b = state->reverse_map[i];
            ir_use cur = ir_get_incoming_blocks(b);
            while (cur != NULL) {
                ir_block pred = IR_USE_PARENT(*cur);
                size_t pred_index = state->index_map[pred->index];
                if (state->doms[pred_index] != 0) {
                    if (new_idom == 0) {
                        new_idom = pred_index;
                    } else {
                        new_idom = _intersect(state, pred_index, new_idom);
                    }
                }
                cur = cur->next;
            }
            assert(new_idom != 0);
            if (state->doms[i] != new_idom) {
                state->doms[i] = new_idom;
                changed = 1;
            }
        }
    }
}

ir_domtree ir_compute_domtree_dataflow(ir_func func) {
    size_t num_blocks = ir_func_next_block_index(func);
    runstate state = (runstate)calloc(1, sizeof(runstate_t) + (1 + num_blocks) * sizeof(size_t));
    state->func = func;
    state->num_blocks = num_blocks;
    state->index_map = (size_t*)calloc(1, num_blocks * sizeof(size_t));
    state->reverse_map = (ir_block*)calloc(1, (1 + num_blocks) * sizeof(ir_block));

    /* Assign blocks by post-order index and setup reverse map */
    _setup_preordering(state);

    /* Solve data-flow equations */
    _solve(state);

    /* Build the dominator tree */
    ir_domtree dt = ir_domtree_new(state->num_blocks);
    for (size_t i = 1; i <= state->max_preorder_index; i++) {
        ir_block b = state->reverse_map[i];
        ir_domtree_node dtnode = ir_domtree_get_node_by_index(dt, b->index);
        ir_block parent = state->reverse_map[state->doms[i]];
        dtnode->block = b;
        if (i == 1) {
            assert(b == parent);
            assert(b == state->func->entry_block);
            dtnode->parent = NULL;
            dtnode->next = NULL;
            assert(dt->root == NULL);
            dt->root = dtnode;
        } else {
            ir_domtree_node dtparent = ir_domtree_get_node_by_index(dt, parent->index);
            dtnode->parent = dtparent;
            dtnode->next = dtparent->children;
            dtparent->children = dtnode;
        }
    }
    assert(dt->root != NULL);
    free(state->reverse_map);
    free(state->index_map);
    free(state);
    return dt;
}
