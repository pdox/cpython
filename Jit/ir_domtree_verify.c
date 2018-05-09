#include "Python.h"
#include "ir.h"
#include "adt.h"

#define ASSERT(_cond, _msg) do { \
    if (!(_cond)) { \
        fprintf(stderr, "ir_domtree_verify assertion failed: " #_cond "\n"); \
        fflush(stderr); \
        Py_FatalError(_msg); \
    } \
} while (0)

static void _verify_parent_child(ir_domtree dt, adt_bitset block_exists) {
    for (size_t i = 0; i < dt->num_blocks; i++) {
        ir_domtree_node n = &dt->nodes[i];
        int exists = adt_bitset_getbit(block_exists, i);
        if (exists) {
            ASSERT(n->block->index == i, "Incorrect block for index");
            if (n == dt->root) {
                ASSERT(n->parent == NULL, "Root node has a parent");
            } else {
                ASSERT(n->parent != NULL, "Non-root node without a parent");
            }
            /* Verify that the children of n have the correct parent */
            ir_domtree_node child = n->children;
            while (child != NULL) {
                ASSERT(child->parent == n, "Child with incorrect parent");
                child = child->next;
            }
        } else {
            ASSERT(n->block == NULL &&
                   n->parent == NULL &&
                   n->children == NULL &&
                   n->next == NULL,
                   "Non-existent block seems to be in tree");
        }
    }
}

static void _verify_is_tree(ir_domtree dt, adt_bitset block_exists) {
    adt_bitset seen = adt_bitset_new(dt->num_blocks);
    adt_bitset_setall(seen, 0);
    for (size_t i = 0; i < dt->num_blocks; i++) {
        if (!adt_bitset_getbit(block_exists, i)) continue;
        ir_domtree_node cur = &dt->nodes[i];
        while (cur != NULL) {
            size_t index = ir_domtree_node_index(dt, cur);
            if (adt_bitset_getbit(seen, index))
                break;
            adt_bitset_setbit(seen, index, 1);
            if (cur == dt->root)
                break;
            cur = cur->parent;
        }
        ASSERT(cur != NULL, "Found node with wrong root!");
    }
    ASSERT(adt_bitset_eq(block_exists, seen), "Block mismatch?");
    adt_bitset_delete(seen);
}

void ir_domtree_verify(ir_func func) {
    size_t num_blocks = ir_func_next_block_index(func);
    ir_domtree dt1 = ir_compute_domtree_semi_nca(func);
    ir_domtree dt2 = ir_compute_domtree_dataflow(func);

    ASSERT(dt1->num_blocks == num_blocks, "Incorrect num_blocks");
    ASSERT(dt2->num_blocks == num_blocks, "Incorrect num_blocks");

    /* Record any gaps in block indices */
    adt_bitset block_exists = adt_bitset_new(num_blocks);
    adt_bitset_setall(block_exists, 0);
    for (ir_block b = func->first_block; b != NULL; b = b->next) {
        adt_bitset_setbit(block_exists, b->index, 1);
    }

    /* Verify the parent/child relationships */
    _verify_parent_child(dt1, block_exists);
    _verify_parent_child(dt2, block_exists);

    /* Verify they are trees, and not something else, by tracing
       every node back up to the root. */
    _verify_is_tree(dt1, block_exists);
    _verify_is_tree(dt2, block_exists);

    /* Verify that for every block, the idom given by dt1 and dt2
       are identical. This is enough to guarantee the trees are
       identical. (the ordering of the child nodes may differ, but
       this is irrelevant to the dominator structure). */
    for (ir_block b = func->first_block; b != NULL; b = b->next) {
        ir_domtree_node n1 = ir_domtree_get_node(dt1, b);
        ir_domtree_node n2 = ir_domtree_get_node(dt2, b);
        ASSERT(n1 != n2, "Overlapping trees?");
        if (b == func->entry_block) {
            ASSERT(n1->parent == NULL, "Entry block has dominator in tree 1");
            ASSERT(n2->parent == NULL, "Entry block has dominator in tree 2");
            ASSERT(n1 == dt1->root, "Entry block is not root of dominator tree 1");
            ASSERT(n2 == dt2->root, "Entry block is not root of dominator tree 2");
        } else {
            ASSERT(n1->parent != NULL, "Block missing parent in tree 1");
            ASSERT(n2->parent != NULL, "Block missing parent in tree 2");
            ASSERT(n1->parent->block == n2->parent->block,
                   "Dominator trees not equal!");
        }
    }
    adt_bitset_delete(block_exists);
    ir_domtree_delete(dt1);
    ir_domtree_delete(dt2);
}

static void _pretty_print_helper(int depth, ir_domtree dt, ir_domtree_node dtnode) {
    ir_domtree_node child = dtnode->children;
    for (int i = 0; i < depth; i++)
        fprintf(stderr, "  ");
    fprintf(stderr, "block_%zu ->\n", ir_domtree_node_index(dt, dtnode));
    while (child != NULL) {
        _pretty_print_helper(depth + 1, dt, child);
        child = child->next;
    }
}

void ir_domtree_pretty_print(ir_domtree dt) {
    _pretty_print_helper(0, dt, dt->root);
}
