typedef enum {
    /* unary ops (see ir_opcode_is_unop)*/
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

    /* shift ops (see ir_opcode_is_shift) */
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

    /* memory ops */
    ir_opcode_get_element_ptr, // &(ptr->member)
    ir_opcode_get_index_ptr,   // &ptr[i]
    ir_opcode_load,            // *ptr
    ir_opcode_store,           // *ptr = value;
    ir_opcode_address_of,      // &value

    /* assign constant value */
    ir_opcode_constant, // new_value = $imm;

    /* cast to register of another type */
    ir_opcode_cast,     // new_value = (cast_type)val;

    /* phi node. Only present in SSA form */
    ir_opcode_phi,

    /* set register. Not present in SSA form */
    ir_opcode_set_value,   // existing_value = value;

    /* labels */
    ir_opcode_label_here,  // label:

    /* flow control */
    ir_opcode_branch,      // goto label;
    ir_opcode_branch_cond, // cond ? goto if_true : goto if_false;
    ir_opcode_jumptable,   // goto *label[i];
    ir_opcode_ret,         // return value;

    /* Python-specific */
    ir_opcode_getlocal,
    ir_opcode_setlocal,
    ir_opcode_incref,
    ir_opcode_decref,
    ir_opcode_stackadj,
    ir_opcode_stack_peek,
    ir_opcode_stack_put,
    ir_opcode_check_eval_breaker,
} ir_opcode;

#define IR_INSTR_AS(kind) \
    ir_instr_##kind instr = (ir_instr_##kind) _instr;

static inline
int ir_instr_opcode_is_flow_control(ir_opcode opcode) {
    return opcode >= ir_opcode_branch &&
           opcode <= ir_opcode_ret;
}

static inline
int ir_opcode_is_python_specific(ir_opcode opcode) {
    return opcode >= ir_opcode_getlocal &&
           opcode <= ir_opcode_check_eval_breaker;
}

IR_PROTOTYPE(ir_instr)
struct ir_instr_t {
    ir_instr prev;
    ir_instr next;
    ir_opcode opcode;
    ir_value dest; /* This is NULL to indicate no destination value. The dest
                      type is considered ir_type_void in this case. */
};

/* Forward declaration. Actual code is at the bottom of this file. */
static inline
void _ir_instr_insert(ir_func func, ir_instr _instr);

#define IR_INSTR_HEADER    ir_instr_t base;
#define IR_INSTR_ALLOC(_type, _extra_size) \
    _type instr = (_type) _ir_alloc(func->context, sizeof(_type ## _t) + (_extra_size), alignof(_type##_t));

#define IR_INSTR_INSERT(_opcode, _ret_type) \
    _ir_instr_insert_helper(func, (ir_instr)instr, (_opcode), ir_value_new(func, (_ret_type)))

/* Append an instruction to the current block of the current function (internal use only) */
static inline
ir_value _ir_instr_insert_helper(ir_func func, ir_instr instr, ir_opcode opcode, ir_value dest) {
    instr->opcode = opcode;
    instr->dest = dest;
    if (dest) {
        dest->def = (opcode == ir_opcode_set_value) ? NULL : instr;
    }
    _ir_instr_insert(func, instr);
    return dest;
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
    ir_value value;
};

static inline
int ir_opcode_is_unop(ir_opcode opcode) {
    return opcode >= ir_opcode_neg &&
           opcode <= ir_opcode_not;
}

static inline
ir_value _ir_insert_unop(ir_func func, ir_opcode opcode, ir_value value) {
    IR_INSTR_ALLOC(ir_instr_unop, 0)
    instr->value = value;
    return IR_INSTR_INSERT(opcode, ir_typeof(value));
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
    ir_value left;
    ir_value right;
};

static inline
int ir_opcode_is_binop(ir_opcode opcode) {
    return opcode >= ir_opcode_add &&
           opcode <= ir_opcode_xor;
}

static inline
ir_value _ir_insert_binop(ir_func func, ir_opcode opcode, ir_value left, ir_value right) {
    IR_INSTR_ALLOC(ir_instr_binop, 0)
    /* Perform int promotion as necessary */
    left = ir_promote(func, left);
    right = ir_promote(func, right);
    assert(ir_type_equal(ir_typeof(left), ir_typeof(right)));
    instr->left = left;
    instr->right = right;
    return IR_INSTR_INSERT(opcode, ir_typeof(left));
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

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_shift)
struct ir_instr_shift_t {
    IR_INSTR_HEADER
    ir_value value;
    ir_value count;
};


static inline
int ir_opcode_is_shift(ir_opcode opcode) {
    return opcode >= ir_opcode_shl &&
           opcode <= ir_opcode_shr;
}

static inline
ir_value _ir_insert_shift(ir_func func, ir_opcode opcode, ir_value value, ir_value count) {
    IR_INSTR_ALLOC(ir_instr_shift, 0)
    /* Perform int promotion as necessary */
    value = ir_promote(func, value);
    count = ir_promote(func, count);
    instr->value = value;
    instr->count = count;
    return IR_INSTR_INSERT(opcode, ir_typeof(value));
}

#define SHIFT_METHOD(name, opcode) \
    static inline \
    ir_value name (ir_func func, ir_value value, ir_value count) { \
        return _ir_insert_shift(func, opcode, value, count); \
    }

SHIFT_METHOD(ir_shl, ir_opcode_shl)
SHIFT_METHOD(ir_shr, ir_opcode_shr)

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_boolean)
struct ir_instr_boolean_t {
    IR_INSTR_HEADER
    ir_value value;
};

static inline
ir_value ir_notbool(ir_func func, ir_value value) {
    IR_INSTR_ALLOC(ir_instr_boolean, 0)
    ir_type type = ir_typeof(value);
    assert(ir_type_is_integral(type) || ir_type_is_pointer(type));
    instr->value = value;
    return IR_INSTR_INSERT(ir_opcode_notbool, ir_type_int);
}

static inline
ir_value ir_bool(ir_func func, ir_value value) {
    IR_INSTR_ALLOC(ir_instr_boolean, 0)
    ir_type type = ir_typeof(value);
    assert(ir_type_is_integral(type) || ir_type_is_pointer(type));
    instr->value = value;
    return IR_INSTR_INSERT(ir_opcode_bool, ir_type_int);
}

/*****************************************************************************/


IR_PROTOTYPE(ir_instr_comparison)
struct ir_instr_comparison_t {
    IR_INSTR_HEADER
    ir_value left;
    ir_value right;
};

static inline
int ir_opcode_is_comparison(ir_opcode opcode) {
    return opcode >= ir_opcode_lt &&
           opcode <= ir_opcode_ge;
}

/* Comparison operators always return an int */
static inline
ir_value _ir_insert_comparison(ir_func func, ir_opcode opcode, ir_value left, ir_value right) {
    IR_INSTR_ALLOC(ir_instr_comparison, 0)
    assert(ir_type_equal(ir_typeof(left), ir_typeof(right)));
    instr->left = left;
    instr->right = right;
    return IR_INSTR_INSERT(opcode, ir_type_int);
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
    ir_value cond;
    ir_value if_true;
    ir_value if_false;
};

static inline
ir_value ir_ternary(ir_func func, ir_value cond, ir_value if_true, ir_value if_false) {
    IR_INSTR_ALLOC(ir_instr_ternary, 0)
    assert(ir_type_equal(ir_typeof(if_true), ir_typeof(if_false)));
    instr->cond = cond;
    instr->if_true = if_true;
    instr->if_false = if_false;
    return IR_INSTR_INSERT(ir_opcode_ternary, ir_typeof(if_true));
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_call)
struct ir_instr_call_t {
    IR_INSTR_HEADER
    ir_value target;
    int arg_count;
    ir_value arg[1];
};

/* target must be a value of function type */
static inline
ir_value ir_call(ir_func func, ir_value target, size_t arg_count, ir_value *args) {
    size_t i;
    IR_INSTR_ALLOC(ir_instr_call, arg_count * sizeof(ir_value))
    ir_type sig = ir_typeof(target);
    ir_type ret_type = sig->param[0];
    assert(sig->kind == ir_type_kind_function);
    assert(sig->param_count == 1 + arg_count);
    instr->target = target;
    instr->arg_count = arg_count;
    /* Check that the arguments match the signature */
    for (i = 0; i < arg_count; i++) {
        assert(ir_type_equal(sig->param[i+1], ir_typeof(args[i])));
        instr->arg[i] = args[i];
    }
    return IR_INSTR_INSERT(ir_opcode_call, ret_type);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_get_element_ptr)
struct ir_instr_get_element_ptr_t {
    IR_INSTR_HEADER
    ir_value ptr;
    size_t offset;
#ifdef IR_DEBUG
    const char *member_name;
#endif
};

static inline
ir_value ir_get_element_ptr(ir_func func, ir_value ptr, size_t offset,
                            ir_type member_type, const char *member_name) {
    IR_INSTR_ALLOC(ir_instr_get_element_ptr, 0)
    assert(ir_typeof(ptr)->kind == ir_type_kind_pointer);
    instr->ptr = ptr;
    instr->offset = offset;
#ifdef IR_DEBUG
    instr->member_name = _ir_strdup(func->context, member_name);
#endif
    return IR_INSTR_INSERT(ir_opcode_get_element_ptr,
                           ir_create_pointer_type(func->context, member_type));
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_get_index_ptr)
struct ir_instr_get_index_ptr_t {
    IR_INSTR_HEADER
    ir_value ptr;
    ir_value index;
};

/* index is allowed to be any integral type */
static inline
ir_value ir_get_index_ptr(ir_func func, ir_value ptr, ir_value index) {
    IR_INSTR_ALLOC(ir_instr_get_index_ptr, 0)
    ir_type ptr_type = ir_typeof(ptr);
    assert(ptr_type->kind == ir_type_kind_pointer);
    assert(ir_type_is_integral(ir_typeof(index)));
    instr->ptr = ptr;
    instr->index = index;
    return IR_INSTR_INSERT(ir_opcode_get_index_ptr, ptr_type);
};

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_load)
struct ir_instr_load_t {
    IR_INSTR_HEADER
    ir_value ptr;
};

static inline
ir_value ir_load(ir_func func, ir_value ptr) {
    IR_INSTR_ALLOC(ir_instr_load, 0)
    ir_type ptr_type = ir_typeof(ptr);
    ir_type base_type = ir_pointer_base(ptr_type);
    instr->ptr = ptr;
    return IR_INSTR_INSERT(ir_opcode_load, base_type);
}

/* Helper to load a field from a structure type */
#define IR_LOAD_FIELD(func, ptr, c_struct_name, c_member_name, member_type) \
    _ir_load_field(func, (ptr), offsetof(c_struct_name, c_member_name), member_type, #c_struct_name, #c_member_name)

static inline
ir_value _ir_load_field(ir_func func, ir_value ptr, size_t offset, ir_type member_type,
                        const char *struct_name, const char *member_name) {
    ir_type ptr_type = ir_typeof(ptr);
    ir_type base_type = ir_pointer_base(ptr_type);
    assert(base_type->kind == ir_type_kind_struct);
    assert(strcmp(base_type->name, struct_name) == 0);
    ir_value addr = ir_get_element_ptr(func, ptr, offset, member_type, member_name);
    return ir_load(func, addr);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_store)
struct ir_instr_store_t {
    IR_INSTR_HEADER
    ir_value ptr;
    ir_value value;
};

static inline
void ir_store(ir_func func, ir_value ptr, ir_value value) {
    IR_INSTR_ALLOC(ir_instr_store, 0)
    ir_type base_type = ir_pointer_base(ir_typeof(ptr));
    assert(ir_type_equal(ir_typeof(value), base_type));
    instr->ptr = ptr;
    instr->value = value;
    IR_INSTR_INSERT(ir_opcode_store, ir_type_void);
}

/* Helper to store a field into a structure type */
#define IR_STORE_FIELD(func, ptr, c_struct_name, c_member_name, member_type, new_value) \
    _ir_store_field(func, (ptr), offsetof(c_struct_name, c_member_name), member_type, #c_struct_name, #c_member_name, (new_value))

static inline
void _ir_store_field(ir_func func, ir_value ptr, size_t offset, ir_type member_type,
                         const char *struct_name, const char *member_name, ir_value new_value) {
    ir_type ptr_type = ir_typeof(ptr);
    ir_type base_type = ir_pointer_base(ptr_type);
    assert(base_type->kind == ir_type_kind_struct);
    assert(strcmp(base_type->name, struct_name) == 0);
    ir_value addr = ir_get_element_ptr(func, ptr, offset, member_type, member_name);
    ir_store(func, addr, new_value);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_address_of)
struct ir_instr_address_of_t {
    IR_INSTR_HEADER
    ir_value value;
};

static inline
ir_value ir_address_of(ir_func func, ir_value value) {
    IR_INSTR_ALLOC(ir_instr_address_of, 0)
    instr->value = value;
    ir_type ret_type = ir_create_pointer_type(func->context, ir_typeof(value));
    return IR_INSTR_INSERT(ir_opcode_address_of, ret_type);
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
        ir_instr_constant_t *instr = (ir_instr_constant_t*)_ir_alloc(func->context, sizeof(ir_instr_constant_t), alignof(ir_instr_constant_t)); \
        (instr->imm) . _fieldname = value; \
        instr->debug_name = debug_name ? _ir_strdup(func->context, debug_name) : NULL; \
        return IR_INSTR_INSERT(ir_opcode_constant, _type); \
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
IR_CONSTANT_METHOD(ir_constant_char_ptr, ir_type_char_ptr, void *, ptr)
IR_CONSTANT_METHOD(ir_constant_intptr, ir_type_intptr, intptr_t, uip)
IR_CONSTANT_METHOD(ir_constant_uintptr, ir_type_uintptr, uintptr_t, uip)
IR_CONSTANT_METHOD(ir_constant_pyssizet, ir_type_pyssizet, Py_ssize_t, pyssizet)

struct _object;
typedef struct _object PyObject;
IR_CONSTANT_METHOD(ir_constant_pyobject_ptr, ir_type_pyobject_ptr, PyObject*, ptr)
IR_CONSTANT_METHOD(ir_constant_pyobject_ptr_ptr, ir_type_pyobject_ptr_ptr, PyObject**, ptr)

/* Arbitrary pointer type */
static inline
ir_value ir_constant_from_ptr(ir_func func, ir_type type, void *value, const char *debug_name) {
    IR_INSTR_ALLOC(ir_instr_constant, 0)
    instr->imm.ptr = value;
    instr->debug_name = debug_name ? _ir_strdup(func->context, debug_name) : NULL;
    return IR_INSTR_INSERT(ir_opcode_constant, type);
}

/*****************************************************************************/

/* C-style cast from one type to another, without safety check. */
IR_PROTOTYPE(ir_instr_cast)
struct ir_instr_cast_t {
    IR_INSTR_HEADER
    ir_value value;
};

static inline
ir_value ir_cast(ir_func func, ir_type type, ir_value value) {
    IR_INSTR_ALLOC(ir_instr_cast, 0)
    instr->value = value;
    return IR_INSTR_INSERT(ir_opcode_cast, type);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_phi_entry)
struct ir_phi_entry_t {
    ir_block from_block;
    ir_value from_reg;
};

IR_PROTOTYPE(ir_instr_phi)
struct ir_instr_phi_t {
    IR_INSTR_HEADER
    unsigned int entry_count;
    ir_phi_entry_t entry[1];
};

// TODO: Implement phi

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_set_value)
struct ir_instr_set_value_t {
    IR_INSTR_HEADER
    ir_value src;
};

static inline
void ir_set_value(ir_func func, ir_value dest, ir_value src) {
    IR_INSTR_ALLOC(ir_instr_set_value, 0)
    assert(ir_type_equal(ir_typeof(dest), ir_typeof(src)));
    instr->src = src;
    /* Bypass automatic value creation by calling _ir_instr_insert_helper directly */
    _ir_instr_insert_helper(func, (ir_instr)instr, ir_opcode_set_value, dest);
}

/*****************************************************************************/
IR_PROTOTYPE(ir_instr_label_here)
struct ir_instr_label_here_t {
    IR_INSTR_HEADER
    ir_label label;
};

static inline
void ir_label_here(ir_func func, ir_label label) {
    IR_INSTR_ALLOC(ir_instr_label_here, 0);
    instr->label = label;
    IR_INSTR_INSERT(ir_opcode_label_here, ir_type_void);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_branch)
struct ir_instr_branch_t {
    IR_INSTR_HEADER
    ir_label target;
};

static inline
void ir_branch(ir_func func, ir_label target) {
    IR_INSTR_ALLOC(ir_instr_branch, 0)
    instr->target = target;
    IR_INSTR_INSERT(ir_opcode_branch, ir_type_void);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_branch_cond)
struct ir_instr_branch_cond_t {
    IR_INSTR_HEADER
    ir_value cond;
    ir_label if_true;
    ir_label if_false;
};

static inline
void ir_branch_cond(ir_func func, ir_value cond, ir_label if_true, ir_label if_false) {
    IR_INSTR_ALLOC(ir_instr_branch_cond, 0)
    instr->cond = cond;
    instr->if_true = if_true;
    instr->if_false = if_false;
    IR_INSTR_INSERT(ir_opcode_branch_cond, ir_type_void);
}

static inline
void ir_branch_if(ir_func func, ir_value cond, ir_label if_true) {
    char namebuf[32];
    sprintf(namebuf, "cond.if_false.%d", ++(func->num_cond_if_false_labels));
    ir_label next_block = ir_label_new(func, namebuf);
    ir_branch_cond(func, cond, if_true, next_block);
    ir_label_here(func, next_block);
}

static inline
void ir_branch_if_not(ir_func func, ir_value cond, ir_label if_not) {
    char namebuf[32];
    sprintf(namebuf, "cond.if_true.%d", ++(func->num_cond_if_true_labels));
    ir_label next_block = ir_label_new(func, namebuf);
    ir_branch_cond(func, cond, next_block, if_not);
    ir_label_here(func, next_block);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_jumptable)
struct ir_instr_jumptable_t {
    IR_INSTR_HEADER
    ir_value index;
    size_t table_size;
    ir_label table[1];
};

static inline
void ir_jumptable(ir_func func, ir_value index, ir_label *table, size_t table_size) {
    IR_INSTR_ALLOC(ir_instr_jumptable, sizeof(ir_label) * table_size)
    instr->index = index;
    instr->table_size = table_size;
    memcpy(&instr->table[0], table, table_size * sizeof(ir_label));
    IR_INSTR_INSERT(ir_opcode_jumptable, ir_type_void);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_ret)
struct ir_instr_ret_t {
    IR_INSTR_HEADER
    ir_value value;
};

static inline
void ir_ret(ir_func func, ir_value value) {
    IR_INSTR_ALLOC(ir_instr_ret, 0)
    instr->value = value;
    IR_INSTR_INSERT(ir_opcode_ret, ir_type_void);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_getlocal)
struct ir_instr_getlocal_t {
    IR_INSTR_HEADER
    size_t index;
};

static inline
ir_value ir_getlocal(ir_func func, size_t index) {
    IR_INSTR_ALLOC(ir_instr_getlocal, 0)
    instr->index = index;
    return IR_INSTR_INSERT(ir_opcode_getlocal, ir_type_pyobject_ptr);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_setlocal)
struct ir_instr_setlocal_t {
    IR_INSTR_HEADER
    size_t index;
    ir_value value;
};

static inline
void ir_setlocal(ir_func func, size_t index, ir_value value) {
    IR_INSTR_ALLOC(ir_instr_setlocal, 0)
    instr->index = index;
    instr->value = value;
    IR_INSTR_INSERT(ir_opcode_setlocal, ir_type_void);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_incref)
struct ir_instr_incref_t {
    IR_INSTR_HEADER
    ir_value obj;
    int is_xincref;
};

static inline
void ir_incref(ir_func func, ir_value obj, int is_xincref) {
    IR_INSTR_ALLOC(ir_instr_incref, 0)
    instr->is_xincref = is_xincref;
    instr->obj = obj;
    IR_INSTR_INSERT(ir_opcode_incref, ir_type_void);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_decref)
struct ir_instr_decref_t {
    IR_INSTR_HEADER
    ir_value obj;
    int is_xdecref;
};

static inline
void ir_decref(ir_func func, ir_value obj, int is_xdecref) {
    IR_INSTR_ALLOC(ir_instr_decref, 0)
    instr->is_xdecref = is_xdecref;
    instr->obj = obj;
    IR_INSTR_INSERT(ir_opcode_decref, ir_type_void);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_stackadj)
struct ir_instr_stackadj_t {
    IR_INSTR_HEADER
    int amount;
};

static inline
void ir_stackadj(ir_func func, int amount) {
    IR_INSTR_ALLOC(ir_instr_stackadj, 0)
    instr->amount = amount;
    IR_INSTR_INSERT(ir_opcode_stackadj, ir_type_void);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_stack_peek)
struct ir_instr_stack_peek_t {
    IR_INSTR_HEADER
    int offset;
};

static inline
ir_value ir_stack_peek(ir_func func, int offset) {
    IR_INSTR_ALLOC(ir_instr_stack_peek, 0)
    instr->offset = offset;
    return IR_INSTR_INSERT(ir_opcode_stack_peek, ir_type_pyobject_ptr);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_stack_put)
struct ir_instr_stack_put_t {
    IR_INSTR_HEADER
    int offset;
    ir_value value;
};

static inline
ir_value ir_stack_put(ir_func func, int offset, ir_value value) {
    IR_INSTR_ALLOC(ir_instr_stack_put, 0)
    instr->offset = offset;
    instr->value = value;
    return IR_INSTR_INSERT(ir_opcode_stack_put, ir_type_void);
}

/*****************************************************************************/

IR_PROTOTYPE(ir_instr_check_eval_breaker)
struct ir_instr_check_eval_breaker_t {
    IR_INSTR_HEADER
    int next_instr_index;
};

static inline
void ir_check_eval_breaker(ir_func func, int next_instr_index) {
    IR_INSTR_ALLOC(ir_instr_check_eval_breaker, 0)
    instr->next_instr_index = next_instr_index;
    IR_INSTR_INSERT(ir_opcode_check_eval_breaker, ir_type_void);
}

char* ir_instr_repr(char *p, ir_instr _instr);

/*****************************************************************************/

/* Insert an insertion into the function. This, along with _ir_instr_remove, should be
   the only code which mutates the instruction linked list.
 */
static inline
void _ir_instr_insert(ir_func func, ir_instr _instr) {
    int is_label = (_instr->opcode == ir_opcode_label_here);
    int terminates_block = ir_instr_opcode_is_flow_control(_instr->opcode);

    if (is_label &&
        func->current_block &&
        func->current_block->current_instr != NULL &&
        func->current_block->current_instr->opcode != ir_opcode_label_here) {
        /* We're inserting a label into the middle of a block. Start a new one.
           Leave the existing block with an unconditional branch.
           Note that this is recursive!
         */
        IR_INSTR_AS(label_here);
        ir_branch(func, instr->label);
    }
    if (terminates_block &&
        func->current_block &&
        func->current_block->current_instr != func->current_block->last_instr) {
        /* We're adding a branch in the middle of an existing block. Yuck!
           We have to split the block at the current instr, and then set the
           insertion position to the top of the new block (at the last label).
         */
        assert(func->current_block->current_instr != NULL);
        ir_block b = func->current_block;
        _ir_func_new_block(func);
        ir_block bnew = func->current_block;
        assert(b->next == bnew);

        /* Detach all instructions after the current one */
        ir_instr from_instr = b->current_instr->next;
        ir_instr to_instr = b->last_instr;
        IR_LL_DETACH_CHAIN(b->first_instr, b->last_instr, from_instr, to_instr);

        /* Attach into new block */
        bnew->first_instr = from_instr;
        bnew->current_instr = NULL;
        bnew->last_instr = to_instr;

        /* Move insertion position back to end of b */
        func->current_block = b;
        b->current_instr = b->last_instr;
    }
    if (func->current_block == NULL) {
        _ir_func_new_block(func);
    }
    ir_block b = func->current_block;
    if (is_label) {
        IR_INSTR_AS(label_here);
        assert(instr->label->block == NULL);
        instr->label->block = b;
    }
    IR_LL_INSERT_AFTER(b->first_instr, b->last_instr, b->current_instr, _instr);
    b->current_instr = _instr;
    if (terminates_block) {
        func->current_block = b->next;
        assert(func->current_block == NULL || func->current_block->current_instr == NULL);
    }
}

static inline
void _ir_instr_remove(ir_func func, ir_block block, ir_instr instr) {
    assert(block->current_instr != instr);
    IR_LL_REMOVE(block->first_instr, block->last_instr, instr);
}
