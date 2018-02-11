typedef enum {
    /* unary ops (see ir_opcode_is_unop) */
    ir_opcode_neg,   // -a
    ir_opcode_not,   // ~a

    /* binary ops (see ir_opcode_is_binop) */
    ir_opcode_add,  // a + b
    ir_opcode_sub,  // a - b
    ir_opcode_mul,  // a * b
    ir_opcode_div,  // a / b
    ir_opcode_rem,  // a % b
    ir_opcode_and,  // a & b
    ir_opcode_or,   // a | b
    ir_opcode_xor,  // a ^ b
    ir_opcode_shl,  // a << b
    ir_opcode_shr,  // a >> b

    /* boolean ops */
    ir_opcode_notbool, // !a
    ir_opcode_bool,    // !!a

    /* comparison ops (see ir_opcode_is_comparison) */
    ir_opcode_lt,   // a < b
    ir_opcode_gt,   // a > b
    ir_opcode_eq,   // a == b
    ir_opcode_ne,   // a != b
    ir_opcode_le,   // a <= b
    ir_opcode_ge,   // a >= b

    /* ternary */
    ir_opcode_ternary,  // a ? b : c

    /* call */
    ir_opcode_call,     // new_value = foo(arg1,arg2,...)
    ir_opcode_patchpoint,

    /* memory ops */
    ir_opcode_get_element_ptr, // &(ptr->member)
    ir_opcode_get_index_ptr,   // &ptr[i]
    ir_opcode_load,            // *ptr
    ir_opcode_store,           // *ptr = value;
    ir_opcode_alloca,          // allocate on stack

    /* assign constant value */
    ir_opcode_constant, // new_value = $imm;

    /* internal instruction to fetch function arguments */
    ir_opcode_func_arg,

    /* cast to register of another type */
    ir_opcode_cast,     // new_value = (cast_type)val;

    /* label */
    ir_opcode_label_here,

    /* user-specified debug info */
    ir_opcode_info_here,   // info (string) embedded at a specific point

    /* Control flow */
    ir_opcode_branch,      // goto label;
    ir_opcode_branch_cond, // cond ? goto if_true : goto if_false;
    ir_opcode_jumptable,   // goto *label[i];
    ir_opcode_ret,         // return;
    ir_opcode_retval,      // return value;

    /* Python-specific */
    ir_opcode_getlocal,
    ir_opcode_setlocal,
    ir_opcode_incref,
    ir_opcode_decref,
    ir_opcode_stackadj,
    ir_opcode_stack_peek,
    ir_opcode_stack_put,
    ir_opcode_check_eval_breaker,
    ir_opcode_setup_block,
    ir_opcode_pop_block,

    /* Python-specific control flow */
    ir_opcode_goto_error,   // error (i.e. raise exception)
    ir_opcode_goto_fbe,     // fast_block_end
    ir_opcode_yield,        // fast_yield
    ir_opcode_yield_dispatch, // yield dispatch table
    ir_opcode_end_finally,  // END_FINALLY opcode
} ir_opcode;

#define IR_INSTR_AS(kind) \
    ir_instr_##kind instr = (ir_instr_##kind) _instr;

static inline
int ir_opcode_is_control_flow(ir_opcode opcode) {
    return (opcode >= ir_opcode_branch &&
            opcode <= ir_opcode_retval) ||
           (opcode >= ir_opcode_goto_error &&
            opcode <= ir_opcode_end_finally);
}

static inline
int ir_opcode_needs_to_be_at_front(ir_opcode opcode) {
    return opcode == ir_opcode_setup_block ||
           opcode == ir_opcode_pop_block;
}

static inline
int ir_opcode_is_python_specific(ir_opcode opcode) {
    return opcode >= ir_opcode_getlocal &&
           opcode <= ir_opcode_end_finally;
}

#define IR_INSTR_MAGIC   ((void*)~((uintptr_t)0))

IR_PROTOTYPE(ir_instr)
struct ir_instr_t {
    void *magic; /* The ir_use_t's for an instruction are placed immediately before
                    the ir_instr_t. This magic value is placed here to make it
                    possible to find the start of the instruction that a use belongs
                    to by walking forward until seeing the magic code. LLVM does this
                    in a more clever way, by using the least significant bits
                    of the pointers in ir_use_t to store hints that allow rapid
                    traversal to the instruction. */
    ir_value_t dest; /* This has type ir_type_void to indicate no value. */
    unsigned int opcode;
    unsigned int num_operands;
    ir_block parent;
    ir_instr prev;
    ir_instr next;
};

/* Get the instruction which defines an ir_value */
#define IR_VALUE_DEF(val)       _ir_value_def(val)

static inline ir_instr _ir_value_def(ir_value value) {
    return (ir_instr)((char*)value - offsetof(ir_instr_t, dest));
}

/* Get the ir_value which is defined by an instruction.
   If the instruction's return type is ir_type_void, this will
   not be a real value, but a placeholder with type ir_type_void
   and index IR_INVALID_INDEX.
 */
#define IR_INSTR_DEST(_instr)       (&(_instr)->dest)

/* Get the opcode for instruction */
#define IR_INSTR_OPCODE(_instr)     ((ir_opcode)((_instr)->opcode))

/* Get the i'th operand (use) belonging to an instruction */
#define IR_INSTR_USE(_instr, i)        (*_ir_instr_operand((_instr), (i)))
#define IR_INSTR_OPERAND(_instr, i)    IR_USE_VALUE(IR_INSTR_USE((_instr), (i)))
#define IR_INSTR_NUM_OPERANDS(_instr)  ((size_t)((_instr)->num_operands))

static inline ir_use _ir_instr_operand(ir_instr _instr, size_t i) {
    assert(i < IR_INSTR_NUM_OPERANDS(_instr));
    return ((ir_use)_instr) - _instr->num_operands + i;
}

/* This initializes the operand, and should not be used to override an existing entry. */
#define IR_SET_OPERAND(use_index, val) _ir_set_operand((ir_instr)instr, (use_index), (val))

static inline void
_ir_set_operand(ir_instr _instr, size_t use_index, ir_value value) {
    ir_use use = &IR_INSTR_USE(_instr, use_index);
    use->value = value;
    use->prev = NULL;
    if (value) {
      use->next = value->use_list;
      if (use->next) {
           use->next->prev = use;
      }
      value->use_list = use;
    } else {
      use->next = NULL;
    }
}

static inline void
_ir_clear_use(ir_use use) {
    if (use->prev) {
        use->prev->next = use->next;
    } else {
        use->value->use_list = use->next;
    }
    if (use->next) {
        use->next->prev = use->prev;
    }
    use->prev = NULL;
    use->next = NULL;
    use->value = NULL;
}

/* Get the values that are used as operands by '_instr'.
   This does not include the instruction's return (dest) value.
   The return value will be a borrowed reference to an array of
   length *count. The reference is invalidated when either
   ir_get_uses() is called again, or the IR is modified.
 */
static inline ir_use ir_get_uses(ir_instr _instr, size_t *countp) {
    size_t count = IR_INSTR_NUM_OPERANDS(_instr);
    *countp = count;
    if (count == 0) {
        return NULL;
    } else {
        return &IR_INSTR_USE(_instr, 0);
    }
}

/* Get all outgoing labels for a block.
   This returns a borrowed reference which is only valid until
   the IR is modified, or another call to this function.
   This function assumes all label operands are contiguous.
*/
static inline ir_use
ir_list_outgoing_labels(ir_block b, size_t *count) {
    ir_instr _instr = b->last_instr;
    switch (IR_INSTR_OPCODE(_instr)) {
    case ir_opcode_branch: {
        *count = 1;
        return &IR_INSTR_USE(_instr, 0);
    }
    case ir_opcode_branch_cond: {
        *count = 2;
        return &IR_INSTR_USE(_instr, 1);
    }
    case ir_opcode_jumptable: {
        *count = IR_INSTR_NUM_OPERANDS(_instr) - 1;
        return &IR_INSTR_USE(_instr, 1);
    }
    case ir_opcode_ret: {
        *count = 0;
        return NULL;
    }
    case ir_opcode_retval: {
        *count = 0;
        return NULL;
    }
    case ir_opcode_goto_error: {
        *count = 1;
        return &IR_INSTR_USE(_instr, 0);
    }
    case ir_opcode_goto_fbe: {
        *count = IR_INSTR_NUM_OPERANDS(_instr);
        return &IR_INSTR_USE(_instr, 0);
    }
    case ir_opcode_yield: {
        *count = IR_INSTR_NUM_OPERANDS(_instr) - 1;
        return &IR_INSTR_USE(_instr, 1);
    }
    case ir_opcode_yield_dispatch: {
        *count = 1;
        return &IR_INSTR_USE(_instr, 0);
    }
    case ir_opcode_end_finally: {
        *count = 1;
        return &IR_INSTR_USE(_instr, 0);
    }
    default: {
        Py_FatalError("Unrecognized terminator instruction");
    }
    } // switch
    Py_UNREACHABLE();
}

/* Forward declaration. Actual code is at the bottom of this file. */
static inline
void ir_cursor_insert(ir_func func, ir_instr _instr);

#define IR_INSTR_HEADER    ir_instr_t base;
#define IR_INSTR_ALLOC(_type, opcode, dest_type, _num_uses, _extra_size) \
    _type instr = (_type)_ir_instr_alloc(func, (opcode), (dest_type), (_num_uses), sizeof(_type ## _t), _extra_size, _alignof(_type##_t));

static inline
ir_instr _ir_instr_alloc(ir_func func, ir_opcode opcode, ir_type dest_type,
                         size_t num_uses, size_t struct_size, size_t extra_size, size_t struct_align) {
    /* This is the physical layout of an instruction:
       useN | ... | use0 | ir_instr_t | [extra data...] */
    /* Since ir_use_t is pointer aligned, this layout cannot
       support alignment greater than that. */
    size_t ptr_align = sizeof(void*);
    assert(ptr_align % struct_align == 0);
    size_t uses_width = num_uses * sizeof(ir_use_t);
    char *mem = (char*)_ir_alloc(func->context, uses_width + struct_size + extra_size, ptr_align);
    ir_instr ret = (ir_instr)(mem + uses_width);
    ret->magic = IR_INSTR_MAGIC;
    ret->dest.type = dest_type;
    ret->dest.index = (dest_type == ir_type_void) ? IR_INVALID_INDEX : (func->next_value_index)++;
    ret->dest.use_list = NULL;
    ret->opcode = opcode;
    ret->num_operands = num_uses;
    ret->parent = NULL;
    ret->prev = NULL;
    ret->next = NULL;
    return ret;
}

#define IR_INSTR_INSERT()    _ir_instr_insert_helper(func, (ir_instr)instr)

/* Append an instruction to the current block of the current function (internal use only) */
static inline
ir_value _ir_instr_insert_helper(ir_func func, ir_instr instr) {
    ir_cursor_insert(func, instr);
    return IR_INSTR_DEST(instr);
}

/* Replace all uses of 'old_value' with 'new_value' */
static inline
void ir_replace_all_uses(ir_func func, ir_value old_value, ir_value new_value) {
    assert(ir_type_equal(ir_typeof(old_value), ir_typeof(new_value)));

    /* Find the first and last uses of 'old_value', and then attach them to 'new_value' */
    ir_use first = old_value->use_list;
    ir_use last = NULL;
    ir_use cur = first;
    while (cur != NULL) {
        assert(cur->value == old_value);
        cur->value = new_value;
        last = cur;
        cur = cur->next;
    }
    if (first) {
        assert(first->prev == NULL);
        assert(last->next == NULL);
        last->next = new_value->use_list;
        if (last->next) {
            last->next->prev = last;
        }
        new_value->use_list = first;
        old_value->use_list = NULL;
    }
}

/* Forward declaration */
static inline
ir_value ir_cast(ir_func func, ir_type type, ir_value value);


/* Helper to perform automatic promotion of small integer types */
static inline
ir_value ir_promote(ir_func func, ir_value value) {
    if (ir_type_promotes_to_int(ir_typeof(value))) {
        return ir_cast(func, ir_type_int, value);
    }
    return value;
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_unop)
struct ir_instr_unop_t {
    IR_INSTR_HEADER
};

static inline
int ir_opcode_is_unop(ir_opcode opcode) {
    return opcode >= ir_opcode_neg &&
           opcode <= ir_opcode_not;
}

static inline
ir_value _ir_insert_unop(ir_func func, ir_opcode opcode, ir_value value) {
    IR_INSTR_ALLOC(ir_instr_unop, opcode, ir_typeof(value), 1, 0)
    IR_SET_OPERAND(0, value);
    return IR_INSTR_INSERT();
}

#define UNOP_METHOD(name, opcode) \
    static inline \
    ir_value name (ir_func func, ir_value value) { \
        return _ir_insert_unop(func, opcode, value); \
    }

UNOP_METHOD(ir_neg, ir_opcode_neg)
UNOP_METHOD(ir_not, ir_opcode_not)

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_binop)
struct ir_instr_binop_t {
    IR_INSTR_HEADER
};

static inline
int ir_opcode_is_binop(ir_opcode opcode) {
    return opcode >= ir_opcode_add &&
           opcode <= ir_opcode_xor;
}

static inline
ir_value _ir_insert_binop(ir_func func, ir_opcode opcode, ir_value left, ir_value right) {
    /* Perform int promotion as necessary */
    left = ir_promote(func, left);
    right = ir_promote(func, right);
    assert(ir_type_equal(ir_typeof(left), ir_typeof(right)));
    IR_INSTR_ALLOC(ir_instr_binop, opcode, ir_typeof(left), 2, 0)
    IR_SET_OPERAND(0, left);
    IR_SET_OPERAND(1, right);
    return IR_INSTR_INSERT();
}

#define BINOP_METHOD(name, opcode) \
    static inline \
    ir_value name (ir_func func, ir_value left, ir_value right) { \
        return _ir_insert_binop(func, opcode, left, right); \
    }

BINOP_METHOD(ir_add, ir_opcode_add)
BINOP_METHOD(ir_sub, ir_opcode_sub)
BINOP_METHOD(ir_mul, ir_opcode_mul)
BINOP_METHOD(ir_div, ir_opcode_div)
BINOP_METHOD(ir_rem, ir_opcode_rem)
BINOP_METHOD(ir_and, ir_opcode_and)
BINOP_METHOD(ir_or, ir_opcode_or)
BINOP_METHOD(ir_xor, ir_opcode_xor)
BINOP_METHOD(ir_shl, ir_opcode_shl);
BINOP_METHOD(ir_shr, ir_opcode_shr);

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_boolean)
struct ir_instr_boolean_t {
    IR_INSTR_HEADER
};

static inline
ir_value ir_notbool(ir_func func, ir_value value) {
    IR_INSTR_ALLOC(ir_instr_boolean, ir_opcode_notbool, ir_type_int, 1, 0)
    assert(ir_type_is_integral(ir_typeof(value)) ||
           ir_type_is_pointer(ir_typeof(value)));
    IR_SET_OPERAND(0, value);
    return IR_INSTR_INSERT();
}

static inline
ir_value ir_bool(ir_func func, ir_value value) {
    IR_INSTR_ALLOC(ir_instr_boolean, ir_opcode_bool, ir_type_int, 1, 0)
    assert(ir_type_is_integral(ir_typeof(value)) ||
           ir_type_is_pointer(ir_typeof(value)));
    IR_SET_OPERAND(0, value);
    return IR_INSTR_INSERT();
}

/*****************************************************************************/


IR_PROTOTYPE(ir_instr_comparison)
struct ir_instr_comparison_t {
    IR_INSTR_HEADER
};

static inline
int ir_opcode_is_comparison(ir_opcode opcode) {
    return opcode >= ir_opcode_lt &&
           opcode <= ir_opcode_ge;
}

/* Comparison operators always return an int */
static inline
ir_value _ir_insert_comparison(ir_func func, ir_opcode opcode, ir_value left, ir_value right) {
    IR_INSTR_ALLOC(ir_instr_comparison, opcode, ir_type_int, 2, 0)
    assert(ir_type_equal(ir_typeof(left), ir_typeof(right)));
    IR_SET_OPERAND(0, left);
    IR_SET_OPERAND(1, right);
    return IR_INSTR_INSERT();
}

#define COMPARISON_METHOD(name, opcode) \
    static inline \
    ir_value name (ir_func func, ir_value left, ir_value right) { \
        return _ir_insert_comparison(func, opcode, left, right); \
    }

COMPARISON_METHOD(ir_lt, ir_opcode_lt)
COMPARISON_METHOD(ir_gt, ir_opcode_gt)
COMPARISON_METHOD(ir_eq, ir_opcode_eq)
COMPARISON_METHOD(ir_ne, ir_opcode_ne)
COMPARISON_METHOD(ir_le, ir_opcode_le)
COMPARISON_METHOD(ir_ge, ir_opcode_ge)

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_ternary)
struct ir_instr_ternary_t {
    IR_INSTR_HEADER
};

static inline
ir_value ir_ternary(ir_func func, ir_value cond, ir_value if_true, ir_value if_false) {
    IR_INSTR_ALLOC(ir_instr_ternary, ir_opcode_ternary, ir_typeof(if_true), 3, 0)
    assert(ir_type_equal(ir_typeof(if_true), ir_typeof(if_false)));
    IR_SET_OPERAND(0, cond);
    IR_SET_OPERAND(1, if_true);
    IR_SET_OPERAND(2, if_false);
    return IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_call)
struct ir_instr_call_t {
    IR_INSTR_HEADER
};

/* target must be a value of function type */
static inline
ir_value ir_call(ir_func func, ir_value target, size_t arg_count, ir_value *args) {
    size_t i;
    ir_type sig = ir_typeof(target);
    assert(sig->kind == ir_type_kind_function);
    assert(sig->param_count == 1 + arg_count);
    ir_type ret_type = sig->param[0];
    IR_INSTR_ALLOC(ir_instr_call, ir_opcode_call, ret_type, 1 + arg_count, 0)
    IR_SET_OPERAND(0, target);
    /* Check that the arguments match the signature */
    for (i = 0; i < arg_count; i++) {
        assert(ir_type_equal(sig->param[i+1], ir_typeof(args[i])));
        IR_SET_OPERAND(1 + i, args[i]);
    }
    return IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_patchpoint)
struct ir_instr_patchpoint_t {
    IR_INSTR_HEADER
    /* operands: target_function, arg1, arg2, ... */
    void *user_data; /* User-data associated with this patchpoint. The first
                        sizeof(void*) bytes of this structure must be reserved
                        for receiving the ir_stackmap_info associated to this
                        patchpoint. This will be written during compilation. */
    size_t real_arg_count; /* Number of actual arguments to pass to 'target'.
                              The remaining arg_count - real_arg_count values
                              will be in the stackmap information. */
};

/* Generate a patchpoint at this point in the code. */
static inline
ir_value ir_patchpoint(ir_func func, void *user_data, ir_value target,
                       size_t real_arg_count, size_t arg_count, ir_value *args) {
    ir_type sig = ir_typeof(target);
    assert(sig->kind == ir_type_kind_function);
    assert(sig->param_count == 1 + real_arg_count);
    ir_type ret_type = sig->param[0];
    IR_INSTR_ALLOC(ir_instr_patchpoint, ir_opcode_patchpoint, ret_type, 1 + arg_count, 0)
    instr->user_data = user_data;
    IR_SET_OPERAND(0, target);
    instr->real_arg_count = real_arg_count;
    assert(real_arg_count <= arg_count);
    for (size_t i = 0; i < arg_count; i++) {
        IR_SET_OPERAND(1 + i, args[i]);
    }
    return IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_get_element_ptr)
struct ir_instr_get_element_ptr_t {
    IR_INSTR_HEADER
    /* operands: ptr */
    size_t offset;
    const char *member_name;
};

static inline
ir_value ir_get_element_ptr(ir_func func, ir_value ptr, size_t offset,
                            ir_type member_type, const char *member_name) {
    assert(ir_typeof(ptr)->kind == ir_type_kind_pointer);
    ir_type ret_type = ir_create_pointer_type(func->context, member_type);
    IR_INSTR_ALLOC(ir_instr_get_element_ptr, ir_opcode_get_element_ptr, ret_type, 1, 0)
    IR_SET_OPERAND(0, ptr);
    instr->offset = offset;
    instr->member_name = _ir_strdup(func->context, member_name);
    return IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_get_index_ptr)
struct ir_instr_get_index_ptr_t {
    IR_INSTR_HEADER
    /* operands: ptr, index */
};

/* index is allowed to be any integral type */
static inline
ir_value ir_get_index_ptr(ir_func func, ir_value ptr, ir_value index) {
    ir_type ptr_type = ir_typeof(ptr);
    assert(ptr_type->kind == ir_type_kind_pointer);
    assert(ir_type_is_integral(ir_typeof(index)));
    IR_INSTR_ALLOC(ir_instr_get_index_ptr, ir_opcode_get_index_ptr, ptr_type, 2, 0)
    IR_SET_OPERAND(0, ptr);
    IR_SET_OPERAND(1, index);
    return IR_INSTR_INSERT();
};

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_load)
struct ir_instr_load_t {
    IR_INSTR_HEADER
    /* operands: ptr */
};

static inline
ir_value ir_load(ir_func func, ir_value ptr) {
    ir_type ptr_type = ir_typeof(ptr);
    ir_type base_type = ir_pointer_base(ptr_type);
    IR_INSTR_ALLOC(ir_instr_load, ir_opcode_load, base_type, 1, 0)
    IR_SET_OPERAND(0, ptr);
    return IR_INSTR_INSERT();
}

/* Helper to load a field from a structure type */
#define IR_LOAD_FIELD(func, ptr, c_struct_name, c_member_name, member_type) \
    _ir_load_field(func, (ptr), offsetof(c_struct_name, c_member_name), member_type, #c_struct_name, #c_member_name)

static inline
ir_value _ir_load_field(ir_func func, ir_value ptr, size_t offset, ir_type member_type,
                        const char *struct_name, const char *member_name) {
#ifndef NDEBUG
    ir_type ptr_type = ir_typeof(ptr);
    ir_type base_type = ir_pointer_base(ptr_type);
    assert(base_type->kind == ir_type_kind_struct);
    assert(strcmp(base_type->name, struct_name) == 0);
#endif
    ir_value addr = ir_get_element_ptr(func, ptr, offset, member_type, member_name);
    return ir_load(func, addr);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_store)
struct ir_instr_store_t {
    IR_INSTR_HEADER
    /* operands: ptr, value */
};

static inline
void ir_store(ir_func func, ir_value ptr, ir_value value) {
    IR_INSTR_ALLOC(ir_instr_store, ir_opcode_store, ir_type_void, 2, 0)
#ifndef NDEBUG
    ir_type base_type = ir_pointer_base(ir_typeof(ptr));
    assert(ir_type_equal(ir_typeof(value), base_type));
    assert(value != NULL);
#endif
    IR_SET_OPERAND(0, ptr);
    IR_SET_OPERAND(1, value);
    IR_INSTR_INSERT();
}

/* Helper to store a field into a structure type */
#define IR_STORE_FIELD(func, ptr, c_struct_name, c_member_name, member_type, new_value) \
    _ir_store_field(func, (ptr), offsetof(c_struct_name, c_member_name), member_type, #c_struct_name, #c_member_name, (new_value))

static inline
void _ir_store_field(ir_func func, ir_value ptr, size_t offset, ir_type member_type,
                         const char *struct_name, const char *member_name, ir_value new_value) {
#ifndef NDEBUG
    ir_type ptr_type = ir_typeof(ptr);
    ir_type base_type = ir_pointer_base(ptr_type);
    assert(base_type->kind == ir_type_kind_struct);
    assert(strcmp(base_type->name, struct_name) == 0);
#endif
    ir_value addr = ir_get_element_ptr(func, ptr, offset, member_type, member_name);
    ir_store(func, addr, new_value);
}

/*****************************************************************************/
IR_PROTOTYPE(ir_instr_alloca)
struct ir_instr_alloca_t {
    IR_INSTR_HEADER
    size_t num_elements;
};

static inline
ir_value ir_alloca(ir_func func, ir_type elem_type, size_t num_elements) {
    ir_type ret_type = ir_create_pointer_type(func->context, elem_type);
    IR_INSTR_ALLOC(ir_instr_alloca, ir_opcode_alloca, ret_type, 0, 0)
    instr->num_elements = num_elements;
    return IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_constant)
struct ir_instr_constant_t {
    IR_INSTR_HEADER
    ir_imm imm;
    const char *debug_name;
};

#define IR_CONSTANT_METHOD(_name, _type, _ctype, _fieldname) \
    static inline \
    ir_value _name (ir_func func, _ctype value, const char *debug_name) { \
        IR_INSTR_ALLOC(ir_instr_constant, ir_opcode_constant, _type, 0, 0) \
        (instr->imm) . _fieldname = value; \
        instr->debug_name = debug_name ? _ir_strdup(func->context, debug_name) : NULL; \
        return IR_INSTR_INSERT(); \
    }

IR_CONSTANT_METHOD(ir_constant_char, ir_type_char, char, c)
IR_CONSTANT_METHOD(ir_constant_uchar, ir_type_uchar, unsigned char, uc)
IR_CONSTANT_METHOD(ir_constant_short, ir_type_short, short, s)
IR_CONSTANT_METHOD(ir_constant_ushort, ir_type_ushort, unsigned short, us)
IR_CONSTANT_METHOD(ir_constant_int, ir_type_int, int, i)
IR_CONSTANT_METHOD(ir_constant_uint, ir_type_uint, unsigned int, ui)
IR_CONSTANT_METHOD(ir_constant_long, ir_type_long, long, l)
IR_CONSTANT_METHOD(ir_constant_ulong, ir_type_ulong, unsigned long, ul)
IR_CONSTANT_METHOD(ir_constant_longlong, ir_type_longlong, long long, ll)
IR_CONSTANT_METHOD(ir_constant_ulonglong, ir_type_ulonglong, unsigned long long, ull)
IR_CONSTANT_METHOD(ir_constant_void_ptr, ir_type_void_ptr, void *, ptr)
IR_CONSTANT_METHOD(ir_constant_char_ptr, ir_type_char_ptr, char *, ptr)
IR_CONSTANT_METHOD(ir_constant_int_ptr, ir_type_int_ptr, int *, ptr)
IR_CONSTANT_METHOD(ir_constant_intptr, ir_type_intptr, intptr_t, uip)
IR_CONSTANT_METHOD(ir_constant_uintptr, ir_type_uintptr, uintptr_t, uip)
IR_CONSTANT_METHOD(ir_constant_pyssizet, ir_type_pyssizet, Py_ssize_t, pyssizet)
IR_CONSTANT_METHOD(ir_constant_pyssizet_ptr, ir_type_pyssizet_ptr, Py_ssize_t*, ptr)
IR_CONSTANT_METHOD(ir_constant_sizet, ir_type_sizet, size_t, sizet)

struct _object;
typedef struct _object PyObject;
IR_CONSTANT_METHOD(ir_constant_pyobject_ptr, ir_type_pyobject_ptr, PyObject*, ptr)
IR_CONSTANT_METHOD(ir_constant_pyobject_ptr_ptr, ir_type_pyobject_ptr_ptr, PyObject**, ptr)

/* Arbitrary pointer type */
static inline
ir_value ir_constant_from_ptr(ir_func func, ir_type type, void *value, const char *debug_name) {
    IR_INSTR_ALLOC(ir_instr_constant, ir_opcode_constant, type, 0, 0)
    instr->imm.ptr = value;
    instr->debug_name = debug_name ? _ir_strdup(func->context, debug_name) : NULL;
    return IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_func_arg)
struct ir_instr_func_arg_t {
    IR_INSTR_HEADER
    size_t index;
};

static inline
ir_value _ir_func_arg(ir_func func, size_t index, ir_type type) {
    assert(index < func->sig->param_count);
    assert(ir_type_equal(func->sig->param[index], type));
    IR_INSTR_ALLOC(ir_instr_func_arg, ir_opcode_func_arg, type, 0, 0)
    instr->index = index;
    return IR_INSTR_INSERT();
}

/*****************************************************************************/

/* C-style cast from one type to another, without safety check. */
IR_PROTOTYPE(ir_instr_cast)
struct ir_instr_cast_t {
    IR_INSTR_HEADER
    /* operands: value */
};

static inline
ir_value ir_cast(ir_func func, ir_type type, ir_value value) {
    IR_INSTR_ALLOC(ir_instr_cast, ir_opcode_cast, type, 1, 0)
    IR_SET_OPERAND(0, value);
    return IR_INSTR_INSERT();
}

/*****************************************************************************/
IR_PROTOTYPE(ir_instr_info_here)
struct ir_instr_info_here_t {
    IR_INSTR_HEADER
    const char *info;
};

static inline
void ir_info_here(ir_func func, const char *info) {
    IR_INSTR_ALLOC(ir_instr_info_here, ir_opcode_info_here, ir_type_void, 0, 0)
    instr->info = _ir_strdup(func->context, info);
    IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_branch)
struct ir_instr_branch_t {
    IR_INSTR_HEADER
};

static inline
void ir_branch(ir_func func, ir_value target) {
    assert(ir_type_is_label(ir_typeof(target)));
    IR_INSTR_ALLOC(ir_instr_branch, ir_opcode_branch, ir_type_void, 1, 0)
    IR_SET_OPERAND(0, target);
    IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_label_here)
struct ir_instr_label_here_t {
    IR_INSTR_HEADER
    const char *name;
};

static inline
void ir_label_here(ir_func func, ir_label label) {
    /* The instruction was already created by ir_label_new,
       so we're just inserting it here. */
    assert(ir_type_is_label(ir_typeof(label)));
    ir_instr instr = IR_VALUE_DEF(label);
    assert(instr->parent == NULL);
    IR_INSTR_INSERT();
}

/* Create a not-yet-assigned label */
static inline
ir_label ir_label_new(ir_func func, const char *name) {
    IR_INSTR_ALLOC(ir_instr_label_here, ir_opcode_label_here, ir_type_label, 0, 0)
    instr->name = _ir_label_qualname(func, name);
    /* Instruction is not inserted until ir_label_here() is called */
    return IR_INSTR_DEST((ir_instr)instr);
}

/* Get block that label value points to */
#define IR_LABEL_BLOCK(label) _ir_label_block((label))

static inline ir_block _ir_label_block(ir_value label) {
    assert(ir_type_is_label(ir_typeof(label)));
    return IR_VALUE_DEF(label)->parent;
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_branch_cond)
struct ir_instr_branch_cond_t {
    IR_INSTR_HEADER
    /* operands: cond, if_true, if_false */
    int if_true_weight;
    int if_false_weight;
};

static inline
void ir_branch_cond(ir_func func, ir_value cond, ir_label if_true, ir_label if_false,
                    int if_true_weight, int if_false_weight) {
    assert(ir_type_is_label(ir_typeof(if_true)));
    assert(ir_type_is_label(ir_typeof(if_false)));
    IR_INSTR_ALLOC(ir_instr_branch_cond, ir_opcode_branch_cond, ir_type_void, 3, 0)
    IR_SET_OPERAND(0, cond);
    IR_SET_OPERAND(1, if_true);
    IR_SET_OPERAND(2, if_false);
    instr->if_true_weight = if_true_weight;
    instr->if_false_weight = if_false_weight;
    IR_INSTR_INSERT();
}

/* Measures of likelyhood, based off a total of 2001 */
#define IR_UNLIKELY            1
#define IR_SOMETIMES         200
#define IR_SEMILIKELY       1000
#define IR_LIKELY           2000

static inline
int ir_invert_likelyhood(int likelyhood) {
    return 2001 - likelyhood;
}

static inline
void ir_branch_if(ir_func func, ir_value cond, ir_label if_true, int likelyhood) {
    char namebuf[32];
    assert(likelyhood > 0 && likelyhood <= 2000);
    sprintf(namebuf, "cond.if_false.%d", ++(func->num_cond_if_false_labels));
    ir_label next_block = ir_label_new(func, namebuf);
    ir_branch_cond(func, cond, if_true, next_block, likelyhood, ir_invert_likelyhood(likelyhood));
    ir_label_here(func, next_block);
}

static inline
void ir_branch_if_not(ir_func func, ir_value cond, ir_label if_not, int likelyhood) {
    char namebuf[32];
    assert(likelyhood > 0 && likelyhood <= 2000);
    sprintf(namebuf, "cond.if_true.%d", ++(func->num_cond_if_true_labels));
    ir_label next_block = ir_label_new(func, namebuf);
    ir_branch_cond(func, cond, next_block, if_not, ir_invert_likelyhood(likelyhood), likelyhood);
    ir_label_here(func, next_block);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_jumptable)
struct ir_instr_jumptable_t {
    IR_INSTR_HEADER
    /* operand: index, target0, target1, ... */
};

static inline
void ir_jumptable(ir_func func, ir_value index, ir_label *table, size_t table_size) {
    IR_INSTR_ALLOC(ir_instr_jumptable, ir_opcode_jumptable, ir_type_void, 1 + table_size, 0)
    IR_SET_OPERAND(0, index);
    for (size_t i = 0; i < table_size; i++) {
        assert(ir_type_is_label(ir_typeof(table[i])));
        IR_SET_OPERAND(1 + i, table[i]);
    }
    IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_retval)
struct ir_instr_retval_t {
    IR_INSTR_HEADER
    /* operand: value */
};

static inline
void ir_retval(ir_func func, ir_value value) {
    IR_INSTR_ALLOC(ir_instr_retval, ir_opcode_retval, ir_type_void, 1, 0)
    assert(ir_type_equal(ir_typeof(value), func->sig->param[0]));
    IR_SET_OPERAND(0, value);
    IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_ret)
struct ir_instr_ret_t {
    IR_INSTR_HEADER
};

static inline
void ir_ret(ir_func func) {
    assert(func->sig->param[0] == ir_type_void);
    IR_INSTR_ALLOC(ir_instr_ret, ir_opcode_ret, ir_type_void, 0, 0)
    IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_getlocal)
struct ir_instr_getlocal_t {
    IR_INSTR_HEADER
    size_t index;
};

static inline
ir_value ir_getlocal(ir_func func, size_t index) {
    IR_INSTR_ALLOC(ir_instr_getlocal, ir_opcode_getlocal, ir_type_pyobject_ptr, 0, 0)
    instr->index = index;
    return IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_setlocal)
struct ir_instr_setlocal_t {
    IR_INSTR_HEADER
    /* operand: value */
    size_t index;
};

static inline
void ir_setlocal(ir_func func, size_t index, ir_value value) {
    IR_INSTR_ALLOC(ir_instr_setlocal, ir_opcode_setlocal, ir_type_void, 1, 0)
    instr->index = index;
    IR_SET_OPERAND(0, value);
    IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_incref)
struct ir_instr_incref_t {
    IR_INSTR_HEADER
    /* operand: obj */
    int is_xincref;
};

static inline
void ir_incref(ir_func func, ir_value obj, int is_xincref) {
    IR_INSTR_ALLOC(ir_instr_incref, ir_opcode_incref, ir_type_void, 1, 0)
    instr->is_xincref = is_xincref;
    IR_SET_OPERAND(0, obj);
    IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_decref)
struct ir_instr_decref_t {
    IR_INSTR_HEADER
    /* operand: obj */
    int is_xdecref;
};

static inline
void ir_decref(ir_func func, ir_value obj, int is_xdecref) {
    IR_INSTR_ALLOC(ir_instr_decref, ir_opcode_decref, ir_type_void, 1, 0)
    instr->is_xdecref = is_xdecref;
    IR_SET_OPERAND(0, obj);
    IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_stackadj)
struct ir_instr_stackadj_t {
    IR_INSTR_HEADER
    int amount;
};

static inline
void ir_stackadj(ir_func func, int amount) {
    IR_INSTR_ALLOC(ir_instr_stackadj, ir_opcode_stackadj, ir_type_void, 0, 0)
    instr->amount = amount;
    IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_stack_peek)
struct ir_instr_stack_peek_t {
    IR_INSTR_HEADER
    int offset;
    int abs_offset; /* computed */
};

static inline
ir_value ir_stack_peek(ir_func func, int offset) {
    IR_INSTR_ALLOC(ir_instr_stack_peek, ir_opcode_stack_peek, ir_type_pyobject_ptr, 0, 0)
    instr->offset = offset;
    instr->abs_offset = -1;
    return IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_stack_put)
struct ir_instr_stack_put_t {
    IR_INSTR_HEADER
    /* operand: value to put */
    int offset;
    int abs_offset; /* computed */
};

static inline
ir_value ir_stack_put(ir_func func, int offset, ir_value value) {
    assert(ir_pointer_base(ir_typeof(value)) == ir_type_pyobject);
    IR_INSTR_ALLOC(ir_instr_stack_put, ir_opcode_stack_put, ir_type_void, 1, 0)
    instr->offset = offset;
    IR_SET_OPERAND(0, value);
    instr->abs_offset = -1;
    return IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_check_eval_breaker)
struct ir_instr_check_eval_breaker_t {
    IR_INSTR_HEADER
    int next_instr_index;
};

static inline
void ir_check_eval_breaker(ir_func func, int next_instr_index) {
    IR_INSTR_ALLOC(ir_instr_check_eval_breaker, ir_opcode_check_eval_breaker, ir_type_void, 0, 0)
    instr->next_instr_index = next_instr_index;
    IR_INSTR_INSERT();
}

/*****************************************************************************/

typedef enum {
    IR_PYBLOCK_ANY,            /* used for generic POP_BLOCK */
    IR_PYBLOCK_LOOP,           /* inside loop (any kind) */
    IR_PYBLOCK_EXCEPT,         /* inside try: of try/except */
    IR_PYBLOCK_FINALLY_TRY,    /* inside try: of try/finally */
    IR_PYBLOCK_EXCEPT_HANDLER, /* inside except: or finally: */
} ir_pyblock_type;

static inline const char *
ir_pyblock_type_repr(ir_pyblock_type b_type) {
    switch (b_type) {
    case IR_PYBLOCK_ANY: return "any";
    case IR_PYBLOCK_LOOP: return "loop";
    case IR_PYBLOCK_EXCEPT: return "except_try";
    case IR_PYBLOCK_FINALLY_TRY: return "finally_try";
    case IR_PYBLOCK_EXCEPT_HANDLER: return "except_handler";
    }
    return "<invalid_pyblock_type>";
}

IR_PROTOTYPE(ir_pyblock)
struct ir_pyblock_t {
    ir_pyblock_type b_type;
    ir_label b_handler;
    ir_label b_handler_precursor; /* For IR_PYBLOCK_EXCEPT / IR_PYBLOCK_FINALLY_TRY */
    ir_label b_continue; /* For IR_PYBLOCK_LOOP only */
    int b_level;
    ir_pyblock prev;
};

IR_PROTOTYPE(ir_instr_setup_block)
struct ir_instr_setup_block_t {
    IR_INSTR_HEADER
    /* operands: [b_handler_label] */
    ir_pyblock_type b_type;
    ir_pyblock pb; /* pyblock that is setup by this instruction */
    int entry_stack_level; /* stack level when this instruction is entered */
};

static inline
void ir_setup_block(ir_func func, ir_pyblock_type b_type, ir_label b_handler) {
    size_t num_operands = b_handler ? 1 : 0;
    IR_INSTR_ALLOC(ir_instr_setup_block, ir_opcode_setup_block, ir_type_void, num_operands, 0)
    if (b_handler) {
        assert(ir_type_is_label(ir_typeof(b_handler)));
        IR_SET_OPERAND(0, b_handler);
    }
    instr->b_type = b_type;
    instr->pb = func->current_pyblock;
    instr->entry_stack_level = func->current_stack_level;
    IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_pop_block)
struct ir_instr_pop_block_t {
    IR_INSTR_HEADER
    ir_pyblock_type b_type;
    /* These are set by the pyblock analysis pass */
    ir_pyblock pb; /* pyblock that is pop'd by this instruction */
    int entry_stack_level; /* stack level when this instruction is entered */
};

static inline
void ir_pop_block(ir_func func, ir_pyblock_type b_type) {
    IR_INSTR_ALLOC(ir_instr_pop_block, ir_opcode_pop_block, ir_type_void, 0, 0)
    instr->b_type = b_type;
    instr->pb = func->current_pyblock;
    instr->entry_stack_level = func->current_stack_level;
    IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_goto_error)
struct ir_instr_goto_error_t {
    IR_INSTR_HEADER
    /* operands: error_exit */
    ir_pyblock pb; /* topmost pyblock at execution time */
    int entry_stack_level; /* stack level when this instruction is entered */
};

static inline
void ir_goto_error(ir_func func, ir_label error_exit) {
    IR_INSTR_ALLOC(ir_instr_goto_error, ir_opcode_goto_error, ir_type_void, 1, 0)
    assert(ir_type_is_label(ir_typeof(error_exit)));
    IR_SET_OPERAND(0, error_exit);
    instr->pb = func->current_pyblock;
    instr->entry_stack_level = func->current_stack_level;
    IR_INSTR_INSERT();
}

/*****************************************************************************/

typedef enum {
    IR_WHY_NOT,
    IR_WHY_EXCEPTION,
    IR_WHY_RETURN,
    IR_WHY_BREAK,
    IR_WHY_CONTINUE,
    IR_WHY_SILENCED,
} ir_why;

static inline const char *
ir_why_repr(ir_why why) {
    switch (why) {
    case IR_WHY_NOT: return "not";
    case IR_WHY_EXCEPTION: return "exception";
    case IR_WHY_RETURN: return "return";
    case IR_WHY_BREAK: return "break";
    case IR_WHY_CONTINUE: return "continue";
    case IR_WHY_SILENCED: return "silenced";
    }
    return "<invalid_ir_why>";
}

IR_PROTOTYPE(ir_instr_goto_fbe)
struct ir_instr_goto_fbe_t {
    IR_INSTR_HEADER
    /* operands: exit_label[, continue_target] */
    ir_why why;
    ir_pyblock pb; /* topmost pyblock at execution time */
    int entry_stack_level; /* stack level when this instruction is entered */
};

static inline
void ir_goto_fbe(ir_func func, ir_why why, ir_label exit, ir_label continue_target) {
    assert(ir_type_is_label(ir_typeof(exit)));
    assert(continue_target == NULL || ir_type_is_label(ir_typeof(continue_target)));
    size_t num_operands = continue_target ? 2 : 1;
    IR_INSTR_ALLOC(ir_instr_goto_fbe, ir_opcode_goto_fbe, ir_type_void, num_operands, 0)
    instr->why = why;
    IR_SET_OPERAND(0, exit);
    if (continue_target) {
        IR_SET_OPERAND(1, continue_target);
    }
    instr->pb = func->current_pyblock;
    instr->entry_stack_level = func->current_stack_level;
    IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_yield)
struct ir_instr_yield_t {
    IR_INSTR_HEADER
    /* operands: value to yield, exit_label, resume_inst_label[, throw_inst_label] */
    int resume_instr_index;
    ir_pyblock pb; /* topmost pyblock at execution time */
    int entry_stack_level; /* stack level when this instruction is entered */

};

static inline
void ir_yield(
        ir_func func,
        int resume_instr_index,
        ir_value value,
        ir_label exit,
        ir_label resume_inst_label,
        ir_label throw_inst_label) {
    size_t num_operands = 3 + (throw_inst_label ? 1 : 0);
    IR_INSTR_ALLOC(ir_instr_yield, ir_opcode_yield, ir_type_void, num_operands, 0)
    assert(ir_type_equal(ir_typeof(value), ir_type_pyobject_ptr));
    assert(ir_type_is_label(ir_typeof(exit)));
    assert(ir_type_is_label(ir_typeof(resume_inst_label)));
    assert(throw_inst_label == NULL || ir_type_is_label(ir_typeof(throw_inst_label)));
    instr->resume_instr_index = resume_instr_index;
    IR_SET_OPERAND(0, value);
    IR_SET_OPERAND(1, exit);
    IR_SET_OPERAND(2, resume_inst_label);
    if (throw_inst_label) {
        IR_SET_OPERAND(3, throw_inst_label);
    }
    instr->pb = func->current_pyblock;
    instr->entry_stack_level = func->current_stack_level;
    IR_INSTR_INSERT();
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_yield_dispatch)
struct ir_instr_yield_dispatch_t {
    IR_INSTR_HEADER
    /* operand: body_start(label) */
};

static inline
void ir_yield_dispatch(ir_func func, ir_label body_start) {
    assert(ir_type_is_label(ir_typeof(body_start)));
    IR_INSTR_ALLOC(ir_instr_yield_dispatch, ir_opcode_yield_dispatch, ir_type_void, 1, 0)
    IR_SET_OPERAND(0, body_start);
    IR_INSTR_INSERT();
}

/*****************************************************************************/
IR_PROTOTYPE(ir_instr_end_finally)
struct ir_instr_end_finally_t {
    IR_INSTR_HEADER
    /* operands: fallthrough */
    ir_pyblock pb; /* topmost pyblock at execution time */
    int entry_stack_level; /* stack level when this instruction is entered */
};

static inline
void ir_end_finally(ir_func func, ir_label fallthrough) {
    assert(ir_type_is_label(ir_typeof(fallthrough)));
    IR_INSTR_ALLOC(ir_instr_end_finally, ir_opcode_end_finally, ir_type_void, 1, 0)
    IR_SET_OPERAND(0, fallthrough);
    instr->pb = func->current_pyblock;
    instr->entry_stack_level = func->current_stack_level;
    IR_INSTR_INSERT();
}

/*****************************************************************************/

char* ir_instr_repr(char *p, ir_instr _instr);

/* _ir_func_new_block:
     Creates a new block after the current cursor position, and positions the cursor
     at the start of the new block. If the cursor is in the middle of an existing block,
     instructions after the current position will be moved into the new block.

     For example, suppose the current IR looks like:

         BLOCK1 -> BLOCK2 -> BLOCK3

     With BLOCK2, the current block, containing:

         [ INST1, INST2, INST3, ^, INST4, INST5 ]

     With ^ the cursor position. After calling _ir_func_new_block, the IR will look like:

         BLOCK1 -> BLOCK2A -> BLOCK2B -> BLOCK3

     Where:

         BLOCK2A = [ INST1, INST2, INST3 ]
         BLOCK2B = [ ^, INST4, INST5 ]

     This function doesn't know anything about instruction opcodes, so the caller
     must guarantee that the previous block ends with a control flow instruction,
     and that the new block receives a label.
*/

static inline
void _ir_func_new_block(ir_func func) {
    IR_FUNC_ALLOC(block, ir_block, 0)
    block->first_instr = NULL;
    block->current_instr = NULL;
    block->last_instr = NULL;
    block->index = (func->next_block_index)++;
    ir_block after = func->current_block ? func->current_block : func->last_block;
    IR_LL_INSERT_AFTER(func->first_block, func->last_block, after, block);

    ir_block cur = func->current_block;
    if (cur != NULL && cur->current_instr != cur->last_instr) {
        /* Detach all instructions after the current one */
        ir_instr from_instr = cur->current_instr ? cur->current_instr->next : cur->first_instr;
        ir_instr to_instr = cur->last_instr;
        IR_LL_DETACH_CHAIN(cur->first_instr, cur->last_instr, from_instr, to_instr);

        /* Attach into new block */
        block->first_instr = from_instr;
        block->current_instr = NULL;
        block->last_instr = to_instr;

        /* Fix parent pointers */
        ir_instr it = from_instr;
        while (it != NULL) {
            assert(it->parent == cur);
            it->parent = block;
            it = it->next;
        }
    }
    func->current_block = block;
}

/* Remove an instruction from a block */
static inline
void _ir_remove_instr(ir_block b, ir_instr _instr) {
    IR_LL_REMOVE(b->first_instr, b->last_instr, _instr);
    _instr->parent = NULL;
    _instr->prev = _instr->next = NULL;

    /* Remove uses caused by this instruction */
    size_t uses_count;
    ir_use uses = ir_get_uses(_instr, &uses_count);
    for (size_t i = 0; i < uses_count; i++) {
        _ir_clear_use(&uses[i]);
    }
}

/* Open the cursor, and position it immediately after "_instr" in block "b" */
static inline void ir_cursor_open(ir_func func, ir_block b, ir_instr _instr) {
    assert(func->current_block == NULL); /* Cursor already open? */
    assert(b && _instr);
    func->current_block = b;
    b->current_instr = _instr;
}

static inline void ir_cursor_set_pyblock(ir_func func, int current_stack_level, ir_pyblock pb) {
    assert(func->current_pyblock == INVALID_PYBLOCK);
    assert(pb != INVALID_PYBLOCK);
    assert(current_stack_level >= 0);
    func->current_stack_level = current_stack_level;
    func->current_pyblock = pb;
}

static inline void ir_cursor_clear_pyblock(ir_func func) {
    assert(func->current_pyblock != INVALID_PYBLOCK);
    func->current_pyblock = INVALID_PYBLOCK;
    func->current_stack_level = -1;
}

/* Insert an instruction into the function, immediately after the current
   instruction. */
static inline
void ir_cursor_insert(ir_func func, ir_instr _instr) {
    assert(_instr->parent == NULL);
    if (IR_INSTR_OPCODE(_instr) == ir_opcode_alloca) {
        /* alloca instructions ignore the cursor. They are always placed at the
           front of the entry node, after the label. */
        ir_block b = func->entry_block;
        ir_instr after = b->first_instr;
        while (after->next != NULL && IR_INSTR_OPCODE(after->next) == ir_opcode_alloca)
            after = after->next;
        IR_LL_INSERT_AFTER(b->first_instr, b->last_instr, after, _instr);
        _instr->parent = b;
        /* Make sure the cursor stays after the alloca instructions, or else the
           alloca's could get pushed out of position. */
        if (b->current_instr == after) {
            b->current_instr = _instr;
        }
        return;
    }
    ir_block b = func->current_block;
    assert(b != NULL); /* Cursor not open? */
    if (IR_INSTR_OPCODE(_instr) == ir_opcode_label_here) {
        /* We're inserting a label, which must be at the start of a block. */
        /* If we're in the middle of a block, and the last instruction does not
           already terminate the block, branch to the next block. */
        if (b->current_instr != NULL) {
            if (!ir_opcode_is_control_flow(IR_INSTR_OPCODE(b->current_instr))) {
                ir_branch(func, IR_INSTR_DEST(_instr));
            }
            _ir_func_new_block(func);
            b = func->current_block;
        }
        assert(b->current_instr == NULL);
    } else {
        /* If we're immediately after a control flow instruction, but we're not
           inserting a label, then this code is unreachable. Create a dummy label. */
        assert(b->current_instr != NULL);
        if (ir_opcode_is_control_flow(IR_INSTR_OPCODE(b->current_instr))) {
            ir_label unreachable_dummy = ir_label_new(func, "unreachable_dummy");
            ir_label_here(func, unreachable_dummy);
            b = func->current_block;
        }
    }

    if (ir_opcode_needs_to_be_at_front(IR_INSTR_OPCODE(_instr))) {
        assert(b->first_instr != NULL); /* Block needs a label first */
        if (b->current_instr != b->first_instr) {
            ir_label needs_to_be_at_front = ir_label_new(func, "needs_to_be_at_front");
            ir_branch(func, needs_to_be_at_front);
            ir_label_here(func, needs_to_be_at_front);
            b = func->current_block;
        }
        assert(b->current_instr == b->first_instr &&
               IR_INSTR_OPCODE(b->current_instr) == ir_opcode_label_here);
    }

    /* Do actual insert */
    assert(b == func->current_block);
    IR_LL_INSERT_AFTER(b->first_instr, b->last_instr, b->current_instr, _instr);
    _instr->parent = b;
    b->current_instr = _instr;

    /* This insertion might have broken a required invariant of the next
       instruction, but that will be taken care of by ir_cursor_close(). */
}

/* Remove the instruction immediately after the current instruction.
   There must be an open cursor. This does not affect the cursor position.
   Note that this cannot be used to delete labels.
 */
static inline
void ir_cursor_remove_next(ir_func func) {
    ir_block b = func->current_block;
    ir_instr _instr = b->current_instr->next;
    assert(_instr);

    /* Nothing should be using this instruction's value */
    assert(IR_INSTR_DEST(_instr)->use_list == NULL);

    _ir_remove_instr(b, _instr);
}

static inline void ir_cursor_close(ir_func func) {
    assert(func->current_block != NULL);

    /* Check the constraints immediately after the cursor position */
    ir_block b = func->current_block;
    ir_instr _instr = b->current_instr;
    int terminates_block = ir_opcode_is_control_flow(IR_INSTR_OPCODE(_instr));

    if (terminates_block) {
        /* If there is an instruction after a block terminator, it cannot be
           a label, so the code is dead. TODO: Make this an assert. */
        if (_instr->next != NULL) {
            _instr->next = NULL;
            b->last_instr = _instr;
        }
    } else if (_instr->next != NULL &&
               ir_opcode_needs_to_be_at_front(IR_INSTR_OPCODE(_instr->next))) {
        ir_label needs_to_be_at_front = ir_label_new(func, "needs_to_be_at_front");
        ir_branch(func, needs_to_be_at_front);
        ir_label_here(func, needs_to_be_at_front);
    }
    func->current_block = NULL;
}
