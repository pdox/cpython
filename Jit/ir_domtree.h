/* Representation of dominator tree for IR */

IR_PROTOTYPE(ir_domtree_node)
IR_PROTOTYPE(ir_domtree)


struct ir_domtree_node_t {
    ir_block block;
    ir_domtree_node parent;
    ir_domtree_node children; /* Linked list */
    ir_domtree_node next; /* Next child of parent */
};

struct ir_domtree_t {
    size_t num_blocks;
    ir_domtree_node root;

    /* All nodes, by block index. */
    ir_domtree_node_t nodes[0];
};

static inline
ir_domtree ir_domtree_new(size_t num_blocks) {
    size_t nbytes = sizeof(ir_domtree_t) + num_blocks * sizeof(ir_domtree_node_t);
    ir_domtree dt = (ir_domtree)malloc(nbytes);
    memset(dt, 0, nbytes);
    dt->num_blocks = num_blocks;
    return dt;
}

static inline
void ir_domtree_delete(ir_domtree dt) {
    free(dt);
}

static inline
ir_domtree_node ir_domtree_get_node_by_index(ir_domtree dt, size_t block_index) {
    assert(block_index < dt->num_blocks);
    return &dt->nodes[block_index];
}

static inline
ir_domtree_node ir_domtree_get_node(ir_domtree dt, ir_block b) {
    return ir_domtree_get_node_by_index(dt, b->index);
}

static inline
size_t ir_domtree_node_index(ir_domtree dt, ir_domtree_node node) {
    size_t ret = node - &dt->nodes[0];
    assert(ret < dt->num_blocks);
    return ret;
}

/* Compute dominator tree using Semi-NCA */
ir_domtree ir_compute_domtree_semi_nca(ir_func func);

/* Compute dominator tree using data flow algorithm */
ir_domtree ir_compute_domtree_dataflow(ir_func func);

/* Verify that both dominator tree algorithms above produce identical results
   on 'func'. This burns CPU, and so should only be used for debugging/testing. */
void ir_domtree_verify(ir_func func);

/* Pretty-print the dominator tree */
void ir_domtree_pretty_print(ir_domtree dt);
