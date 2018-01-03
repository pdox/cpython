IR_PROTOTYPE(ir_instr)
IR_PROTOTYPE(ir_value)
IR_PROTOTYPE(ir_block)
IR_PROTOTYPE(ir_label)
IR_PROTOTYPE(ir_func)

struct ir_value_t {
    ir_type type;
    ir_instr def;    /* Set when a value is created by an instruction, and
                        never changed by set_value */
    ir_imm rtv;      /* Runtime value. Only used during interpretation */
    size_t index;   /* Used when register numbering */
};

struct ir_block_t {
    ir_block prev;
    ir_block next;
    ir_instr first_instr;
    ir_instr current_instr; /* NULL means 'insert before first_instr' */
    ir_instr last_instr;
    ir_label label;
    size_t index;
};

struct ir_label_t {
    /* This is NULL when the label hasn't been assigned yet. */
    ir_block block;
    const char *name;
};

#define LABEL_PREFIX_STACK_SIZE   8

struct ir_func_t {
    ir_context context;
    ir_type sig;
    ir_block first_block;
    ir_block current_block; /* NULL means 'insert after last_block' */
    ir_block last_block;
    size_t next_block_index;

    /* Used for user-visible label naming */
    const char *label_prefix_stack[LABEL_PREFIX_STACK_SIZE];
    int num_cond_if_true_labels;
    int num_cond_if_false_labels;

    /* Value numbering */
    size_t next_value_index;

    /* These match the signature. param[0] is not used. */
    size_t param_count;
    ir_value param[1];
};

/* Start a new function in this context. 'sig' must be a function pointer type. */
ir_func ir_func_new(ir_context context, ir_type sig);

/* Get the value corresponding to the i'th argument to this function */
static inline
ir_value ir_func_get_argument(ir_func func, size_t i) {
    assert(i + 1 < func->param_count);
    return func->param[i + 1];
}

#define IR_FUNC_ALLOC(_varname, _type, _extra) \
    _type _varname = (_type)_ir_alloc(func->context, sizeof(_type ## _t) + (_extra), _alignof(_type ## _t));

static inline
ir_type ir_typeof(ir_value v) {
    return v->type;
}

static inline
ir_value ir_value_new(ir_func func, ir_type type) {
    if (type->kind == ir_type_kind_void) {
        return NULL;
    }
    IR_FUNC_ALLOC(v, ir_value, 0)
    v->type = type;
    v->def = NULL;
    v->index = (func->next_value_index)++;
    return v;
}

static inline
ssize_t ir_value_index(ir_value value) {
    return value->index;
}

/* Generates fully-qualified label name, for use in debug mode */
const char* _ir_label_qualname(ir_func func, const char *name);

/* Create a not-yet-assigned label */
static inline
ir_label ir_label_new(ir_func func, const char *name) {
    IR_FUNC_ALLOC(ret, ir_label, 0)
    ret->block = NULL;
#ifdef IR_DEBUG
    ret->name = _ir_label_qualname(func, name);
#else
    ret->name = NULL;
#endif
    return ret;
}

static inline
void ir_branch(ir_func func, ir_label target);

/* Add a prefix to all labels from this point on.
   This must be matched with a corresponding ir_label_pop_prefix()
 */
static inline
void ir_label_push_prefix(ir_func func, const char *v) {
    int i;
    for (i = 0; i < LABEL_PREFIX_STACK_SIZE; i++) {
        if (func->label_prefix_stack[i] == NULL)
            break;
    }
    if (i == LABEL_PREFIX_STACK_SIZE - 1) {
        /* Overflowed label prefix stack */
        abort();
    }
    func->label_prefix_stack[i] = _ir_strdup(func->context, v);
}

static inline
void ir_label_pop_prefix(ir_func func) {
    int i;
    for (i = 0; i < LABEL_PREFIX_STACK_SIZE; i++) {
        if (func->label_prefix_stack[i] == NULL)
            break;
    }
    if (i == 0) {
        abort(); /* Nothing on the stack */
    }
    func->label_prefix_stack[i-1] = NULL;
}

static inline
char* ir_label_repr(char *p, ir_label label) {
    if (label->name) {
        p += sprintf(p, "%s(%p)", label->name, label->block);
    } else {
        p += sprintf(p, "%p", label->block);
    }
    return p;
}

/* Returns an upper bound on value indexes. (from ir_value_index).
   This may change if more values/instructions are added.
 */
static inline
ssize_t ir_func_largest_value_index(ir_func func) {
    return func->next_value_index;
}

static inline
ssize_t ir_func_largest_block_index(ir_func func) {
    return func->next_block_index;
}

/* Verify the correctness of the function. This:
   1) Ensures that every block ends up with a branch.
   2) Ensures that all labels have been resolved to a block.
   3) Ensures that no branch occurs in the middle of a block.
*/
void ir_func_verify(ir_func func);

/* Move all blocks from 'from' (inclusive) to 'to' (exclusive) to the end of the function. */
void ir_func_move_blocks_to_end(ir_func func, ir_label from, ir_label to);

/* Dump function IR to string */
char *ir_func_dump(ir_func func);

/* Print representation of value to buffer */
char *ir_value_repr(char *p, ir_value value);

void ir_func_dump_file(ir_func func, const char *filename, const char *description);
