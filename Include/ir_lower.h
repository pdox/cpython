
/* Lower Python-specific instructions, in preparation for code generation */
void ir_lower(ir_func func, ir_value fastlocals, ir_value stack_pointer, ir_label eval_breaker_label);
