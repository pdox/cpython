
/* Remove dead blocks */
void ir_remove_dead_blocks(ir_func func);

/* Statically determine the PyTryBlock stack at eack block of 'func' */
void ir_compute_pyblock_info(ir_func func, ir_label unknown_handler, ir_label exit_label);

/* Compute stack positions for stack_peek and stack_put operations */
int ir_compute_stack_positions(ir_func func);

/* Determine which 'getlocal' instructions are guaranteed to be
   defined, and mark them as such. This allows the lowering pass to
   forgo emitting the check for an undefined local.
 */
void ir_mark_defined_locals(ir_func func, size_t nlocals);
