
/* Remove dead blocks */
void ir_remove_dead_blocks(ir_func func);

IR_PROTOTYPE(ir_pyblock_map)
struct ir_pyblock_map_t {
    int num_blocks;
    ir_pyblock_t *_mem; /* used only for freeing memory */
    ir_pyblock *at; /* at[i] is the pyblock during execution of the block with index i.
                       If the block starts with a setup_block or pop_block, this
                       includes the effect of that instruction.
                     */
    int *stack_level; /* stack_level[i] is the entry stack level for block with index i */
};

/* Statically determine the PyTryBlock stack at eack block of 'func' */
ir_pyblock_map ir_compute_pyblock_map(ir_func func, ir_label *ignored);

/* Dump pyblock map to file */
void ir_dump_pyblock_map(ir_pyblock_map map, const char *filename);

/* Free pyblocks map */
void ir_free_pyblock_map(ir_pyblock_map map);

int ir_compute_stack_positions(ir_func func);
