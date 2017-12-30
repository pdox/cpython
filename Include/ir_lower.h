
/* Lower Python-specific instructions, in preparation for code generation */
void ir_lower(ir_func func, ir_value fastlocals, ir_value stack_pointer, ir_label eval_breaker_label);

/* Verify that every block's entry stack level is consistent across all
   paths to that block */
void ir_verify_stack_effect(ir_func func);
