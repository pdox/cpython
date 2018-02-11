IR_PROTOTYPE(ir_instr)
IR_PROTOTYPE(ir_value)
IR_PROTOTYPE(ir_use)
IR_PROTOTYPE(ir_block)
IR_PROTOTYPE(ir_func)
IR_PROTOTYPE(ir_pyblock)

/* ir_label is an alias to ir_value. It is used as a type hint,
   for when a value is required to contain a label. */
typedef ir_value ir_label;

/* Marks an uninitialized pyblock value */
#define INVALID_PYBLOCK   ((ir_pyblock)(~((uintptr_t)0)))

#define IR_INVALID_INDEX   (~((size_t)0))

struct ir_value_t {
    ir_type type;
    size_t index;    /* Register number assignment */
    ir_use use_list; /* Uses of this value (linked list) */
};

struct ir_use_t {
    ir_value value;
    ir_use prev;
    ir_use next;
};

#define IR_USE_VALUE(use)   ((use).value)

struct ir_block_t {
    ir_block prev;
    ir_block next;
    ir_instr first_instr;
    ir_instr current_instr;
    ir_instr last_instr;
    size_t index;
};

#define LABEL_PREFIX_STACK_SIZE   8

struct ir_func_t {
    ir_context context;
    char *name;
    ir_type sig;
    ir_block entry_block;
    ir_block first_block;
    ir_block current_block; /* NULL means 'insert after last_block' */
    ir_block last_block;
    size_t next_block_index;

    // Information used during lowering
    // TODO: Try to factor these out.
    ir_pyblock current_pyblock;
    int current_stack_level;

    /* Used for user-visible label naming */
    const char *label_prefix_stack[LABEL_PREFIX_STACK_SIZE];
    int num_cond_if_true_labels;
    int num_cond_if_false_labels;

    /* Value numbering */
    size_t next_value_index;

    /* These match the signature parameters. arg[0] is NULL. */
    ir_value arg[1];
};

/* Start a new function in this context. 'sig' must be a function pointer type. */
ir_func ir_func_new(ir_context context, const char *name, ir_type sig);

/* Get the value corresponding to the i'th argument to this function */
static inline
ir_value ir_func_get_argument(ir_func func, size_t i) {
    assert(i + 1 < func->sig->param_count);
    return func->arg[i + 1];
}

#define IR_FUNC_ALLOC(_varname, _type, _extra) \
    _type _varname = (_type)_ir_alloc(func->context, sizeof(_type ## _t) + (_extra), _alignof(_type ## _t));

static inline
ir_type ir_typeof(ir_value v) {
    return v->type;
}

static inline
ssize_t ir_value_index(ir_value value) {
    return value->index;
}

/* Generates fully-qualified label name, for use in debug mode */
const char* _ir_label_qualname(ir_func func, const char *name);

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

char* ir_label_repr(char *p, ir_label label);

/* Returns an upper bound on value indexes. (from ir_value_index).
   This may change if more values/instructions are added.
 */
static inline
size_t ir_func_next_value_index(ir_func func) {
    return func->next_value_index;
}

static inline
size_t ir_func_next_block_index(ir_func func) {
    return func->next_block_index;
}

/* Verify the correctness of the function. This:
   1) Ensures that every block ends up with a branch.
   2) Ensures that all labels have been resolved to a block.
   3) Ensures that no branch occurs in the middle of a block.
*/
void ir_func_verify(ir_func func);

/* Dump function IR to string */
char *ir_func_dump(ir_func func);

/* Print representation of value to buffer */
char *ir_value_repr(char *p, ir_value value);

void ir_func_dump_file(ir_func func, const char *filename);
