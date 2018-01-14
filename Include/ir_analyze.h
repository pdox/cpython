
/* Remove dead blocks */
void ir_remove_dead_blocks(ir_func func);

IR_PROTOTYPE(ir_pyblock)
struct ir_pyblock_t {
    ir_pyblock_type b_type;
    ir_label b_handler;
    int b_level;
    ir_pyblock prev;
};

IR_PROTOTYPE(ir_pyblock_map)
struct ir_pyblock_map_t {
    int num_blocks;
    ir_pyblock_t *_mem; /* used only for freeing memory */
    ir_pyblock *at; /* at[i] is the pyblock for block index i */
};

/* Statically determine the PyTryBlock stack at eack block of 'func' */
ir_pyblock_map ir_compute_pyblock_map(ir_func func, ir_label *ignored);

/* Dump pyblock map to file */
void ir_dump_pyblock_map(ir_pyblock_map map, const char *filename);

/* Free pyblocks map */
void ir_free_pyblock_map(ir_pyblock_map map);
