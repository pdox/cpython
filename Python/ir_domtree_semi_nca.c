#include "Python.h"
#include "ir.h"
#include "adt.h"

/* Compute dominator tree using the SEMI-NCA algorithm described in:

     Linear-Time Algorithms for Dominators and Related Problems
     Loukas Georgiadis
     ftp://ftp.cs.princeton.edu/reports/2005/737.pdf

 */

IR_PROTOTYPE(node)
struct node_t {
    ir_block b;
    node parent; /* Parent in DFS spanning tree.
                    Note: This is modified during compression by eval */
    node label;  /* Label attached to this node (modified by eval). */
    node idom;   /* Immediate dominator, when the algorithm is complete */
    size_t semi;   /* Semi-dominator index (dfs index) */
    size_t dfs_index;
};

IR_PROTOTYPE(runstate)
struct runstate_t {
    ir_func func;
    size_t num_blocks;
    size_t max_dfs_index;

    adt_vector worklist;
    adt_vector visited;

    /* Quick lookup by DFS index */
    node* node_by_dfs;

    /* Actual storage for node info, sorted by block index */
    node_t node_by_block[0];
};

static runstate init_state(ir_func func) {
    assert(_alignof(runstate_t) == _alignof(node_t));
    assert(_alignof(node_t) == _alignof(void*));
    /* Layout of allocated memory:
         runstate_t | node_by_block storage | node_by_dfs storage
     */
    size_t num_blocks = ir_func_next_block_index(func);
    size_t node_by_block_size = num_blocks * sizeof(node_t);
    size_t node_by_dfs_size = (num_blocks + 1) * sizeof(node);
    size_t nbytes = sizeof(runstate_t) + node_by_block_size + node_by_dfs_size;
    runstate state = (runstate)calloc(1, nbytes);
    state->node_by_dfs = (node*)&state->node_by_block[num_blocks];
    assert((char*)&state->node_by_dfs[num_blocks + 1] == (char*)state + nbytes);

    state->func = func;
    state->num_blocks = num_blocks;
    /* These are temporary vectors for use in eval. They do not store state between calls. */
    state->worklist = adt_vector_new(64, sizeof(node));
    state->visited = adt_vector_new(64, sizeof(node));
    return state;
}

static void destroy_state(runstate state) {
    adt_vector_delete(state->worklist);
    adt_vector_delete(state->visited);
    free(state);
}

static node get_node_by_block(runstate state, ir_block b) {
    assert(b->index < state->num_blocks);
    return &state->node_by_block[b->index];
}

static node get_node_by_dfs_index(runstate state, size_t dfs_index) {
    assert(dfs_index >= 1 && dfs_index <= state->max_dfs_index);
    return state->node_by_dfs[dfs_index];
}

static void insert_node_into_dfs_index(runstate state, node n) {
    assert(n->dfs_index != 0);
    assert(state->node_by_dfs[n->dfs_index] == NULL);
    assert(n->dfs_index >= 1 && n->dfs_index <= state->num_blocks);
    state->node_by_dfs[n->dfs_index] = n;
}

static void assign_node_indices_by_dfs(runstate state) {
    size_t next_dfs_index = 1;
    adt_vector worklist = state->worklist;
    adt_vector_clear(worklist);
    adt_vector_push_back(worklist, &state->func->entry_block);
    ir_block b;
    while (adt_vector_pop_back(worklist, &b)) {
        node n = get_node_by_block(state, b);

        /* Skip already labeled nodes */
        if (n->dfs_index != 0)
            continue;

        n->b = b;
        n->dfs_index = next_dfs_index++;
        n->label = n;
        n->semi = n->dfs_index;
        insert_node_into_dfs_index(state, n);

        /* Add successors to worklist */
        size_t label_count;
        ir_use labels = ir_list_outgoing_labels(b, &label_count);
        for (size_t i = 0; i < label_count; i++) {
            ir_block target_block = IR_LABEL_BLOCK(IR_USE_VALUE(labels[i]));
            node target_node = get_node_by_block(state, target_block);
            if (target_node->dfs_index == 0) {
                adt_vector_push_back(worklist, &target_block);
                target_node->parent = n;
                target_node->idom = n; /* Initialize idom to DFS tree parent */
            }
        }
    }
    state->max_dfs_index = next_dfs_index - 1;
}

static int has_visited(adt_vector visited, node n) {
    size_t size = adt_vector_size(visited);
    node *data = (node*)adt_vector_data(visited);
    for (size_t i = 0; i < size; i++) {
        if (data[i] == n) return 1;
    }
    return 0;
}

static size_t compute_eval(runstate state, node in, size_t dfs_cutoff) {
    if (in->dfs_index < dfs_cutoff)
        return in->semi;

    adt_vector worklist = state->worklist;
    adt_vector visited = state->visited;
    adt_vector_clear(visited);
    adt_vector_clear(worklist);
    if (in->parent->dfs_index >= dfs_cutoff)
        adt_vector_push_back(worklist, &in);

    node n;
    while (adt_vector_pop_back(worklist, &n)) {
        node ancestor = n->parent;
        if (!has_visited(visited, ancestor) && ancestor->dfs_index >= dfs_cutoff) {
            /* Visit ancestor first */
            adt_vector_push_back(visited, &ancestor);
            adt_vector_push_back(worklist, &n);
            adt_vector_push_back(worklist, &ancestor);
            continue;
        }
        if (ancestor->dfs_index < dfs_cutoff)
            continue;
        if (ancestor->label->semi < n->label->semi)
            n->label = ancestor->label;
        n->parent = ancestor->parent;
    }
    return in->label->semi;
}

static void compute_semidominators(runstate state) {
    for (size_t i = state->max_dfs_index; i >= 2; i--) {
        node n = get_node_by_dfs_index(state, i);

        /* Initialize semi as parent */
        n->semi = n->parent->dfs_index;

        /* Iterate through incoming blocks (blocks that branch into 'b') */
        ir_use cur = ir_get_incoming_blocks(n->b);
        while (cur != NULL) {
            ir_block in_block = IR_USE_PARENT(*cur);
            node in_node = get_node_by_block(state, in_block);
            size_t semi = compute_eval(state, in_node, i + 1);
            if (semi < n->semi) n->semi = semi;
            cur = cur->next;
        }
    }
}

static void compute_idominators(runstate state) {
    for (size_t i = 2; i <= state->max_dfs_index; i++) {
        node n = get_node_by_dfs_index(state, i);
        /* Walk up the original DFS tree to find the NCA of the sdom and spanning tree parent of n.
           Note that 'idom' contains the original tree parent, and that the 'parent' field was
           modified by eval (compression), and so is no longer valid. */
        size_t sdom = n->semi;
        node cur = n->idom;
        while (cur->dfs_index > sdom)
            cur = cur->idom;
        n->idom = cur;
    }
}

ir_domtree ir_compute_domtree_semi_nca(ir_func func) {
    runstate state = init_state(func);

    /* Build a DFS spanning tree over the nodes */
    assign_node_indices_by_dfs(state);

    /* Compute semidominators */
    compute_semidominators(state);

    /* Compute immediate dominators */
    compute_idominators(state);

    /* Build domtree */
    ir_domtree dt = ir_domtree_new(state->num_blocks);
    for (size_t i = 0; i < state->num_blocks; i++) {
        node n = &state->node_by_block[i];
        if (n->dfs_index == 0) continue;
        ir_domtree_node dtnode = ir_domtree_get_node_by_index(dt, i);
        dtnode->block = n->b;
        if (n->idom == NULL) {
            assert(dt->root == NULL);
            dt->root = dtnode;
            continue;
        }
        size_t idom_block_index = n->idom->b->index;
        ir_domtree_node dtparent = ir_domtree_get_node_by_index(dt, idom_block_index);
        dtnode->parent = dtparent;
        dtnode->next = dtparent->children;
        dtparent->children = dtnode;
    }
    destroy_state(state);
    return dt;
}
