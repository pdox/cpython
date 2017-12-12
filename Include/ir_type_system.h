typedef enum {
    ir_type_kind_void,

    /* Integral types
       If you change this section, update ir_type_is_integral() below
     */
    ir_type_kind_char,
    ir_type_kind_uchar,
    ir_type_kind_short,
    ir_type_kind_ushort,
    ir_type_kind_int,
    ir_type_kind_uint,
    ir_type_kind_long,
    ir_type_kind_ulong,
    ir_type_kind_longlong,
    ir_type_kind_ulonglong,
    ir_type_kind_intptr,
    ir_type_kind_uintptr,
    ir_type_kind_pyssizet,

    /* Pointer type */
    ir_type_kind_pointer,

    /* Function type */
    ir_type_kind_function,

    /* Struct type */
    ir_type_kind_struct,
} ir_type_kind;

IR_PROTOTYPE(ir_type)
struct ir_type_t {
    const char *name;
    ir_type_kind kind;
    int size;
    size_t param_count;
    ir_type param[1];
};

static inline
int ir_type_is_integral(ir_type type) {
    return type->kind >= ir_type_kind_char &&
           type->kind <= ir_type_kind_pyssizet;
}


static inline
int ir_type_promotes_to_int(ir_type type) {
    return type->kind >= ir_type_kind_char &&
           type->kind <= ir_type_kind_ushort;
}

static inline
int ir_type_is_pointer(ir_type type) {
    return type->kind == ir_type_kind_pointer;
}

static inline
int ir_type_is_function(ir_type type) {
    return type->kind == ir_type_kind_function;
}

static inline
int ir_type_is_struct(ir_type type) {
    return type->kind == ir_type_kind_struct;
}

static inline
int ir_type_is_void(ir_type type) {
    return type->kind == ir_type_kind_void;
}

#define IR_BASE_TYPE_DECL(name) \
    extern ir_type_t _ir_type_ ## name; \
    static const ir_type ir_type_ ## name = &(_ir_type_ ## name);

/* Integral types */
IR_BASE_TYPE_DECL(char)
IR_BASE_TYPE_DECL(uchar)
IR_BASE_TYPE_DECL(short)
IR_BASE_TYPE_DECL(ushort)
IR_BASE_TYPE_DECL(int)
IR_BASE_TYPE_DECL(uint)
IR_BASE_TYPE_DECL(long)
IR_BASE_TYPE_DECL(ulong)
IR_BASE_TYPE_DECL(longlong)
IR_BASE_TYPE_DECL(ulonglong)
IR_BASE_TYPE_DECL(intptr)
IR_BASE_TYPE_DECL(uintptr)
IR_BASE_TYPE_DECL(pyssizet);

/* Base types */
IR_BASE_TYPE_DECL(void)
IR_BASE_TYPE_DECL(pyobject);
IR_BASE_TYPE_DECL(pytypeobject);
IR_BASE_TYPE_DECL(pyframeobject);
IR_BASE_TYPE_DECL(evalcontext);
IR_BASE_TYPE_DECL(pythreadstate);

/* Pointer types */
IR_BASE_TYPE_DECL(void_ptr)
IR_BASE_TYPE_DECL(pyobject_ptr);
IR_BASE_TYPE_DECL(pyobject_ptr_ptr);
IR_BASE_TYPE_DECL(pytypeobject_ptr);
IR_BASE_TYPE_DECL(pyframeobject_ptr);
IR_BASE_TYPE_DECL(evalcontext_ptr);
IR_BASE_TYPE_DECL(pythreadstate_ptr);

IR_BASE_TYPE_DECL(char_ptr);
IR_BASE_TYPE_DECL(uchar_ptr);
IR_BASE_TYPE_DECL(short_ptr);
IR_BASE_TYPE_DECL(ushort_ptr);
IR_BASE_TYPE_DECL(int_ptr);
IR_BASE_TYPE_DECL(uint_ptr);
IR_BASE_TYPE_DECL(long_ptr);
IR_BASE_TYPE_DECL(ulong_ptr);
IR_BASE_TYPE_DECL(longlong_ptr);
IR_BASE_TYPE_DECL(ulonglong_ptr);
IR_BASE_TYPE_DECL(intptr_ptr);
IR_BASE_TYPE_DECL(uintptr_ptr);
IR_BASE_TYPE_DECL(pyssizet_ptr);

/* Typed immediate value */
typedef union {
    char c;
    unsigned char uc;
    short s;
    unsigned short us;
    int i;
    unsigned int ui;
    long l;
    unsigned long ul;
    long long ll;
    unsigned long long ull;
    intptr_t ip;
    uintptr_t uip;
    Py_ssize_t pyssizet;
    void *ptr;
} ir_imm;

static inline
size_t ir_sizeof(ir_type type) {
    return type->size;
}

static inline
ir_type ir_create_function_type(ir_context context, ir_type ret_type, int arg_count, ir_type *arg_type) {
    int i;
    IR_ALLOC(ret, ir_type, sizeof(ir_type) * arg_count)
    ret->name = NULL;
    ret->kind = ir_type_kind_function;
    ret->size = sizeof(void*);
    ret->param_count = 1 + arg_count;
    ret->param[0] = ret_type;
    for (i = 0; i < arg_count; i++) {
        ret->param[i+1] = arg_type[i];
    }
    return ret;
}

static inline
ir_type ir_create_struct_type(ir_context context, const char *name, size_t size) {
    IR_ALLOC(ret, ir_type, 0)
    ret->name = _ir_strdup(context, name);
    ret->kind = ir_type_kind_struct;
    ret->size = size;
    ret->param_count = 0;
    return ret;
}

static inline
ir_type ir_create_pointer_type(ir_context context, ir_type base_type) {
    IR_ALLOC(ret, ir_type, 0)
    ret->name = NULL;
    ret->kind = ir_type_kind_pointer;
    ret->size = sizeof(void*);
    ret->param_count = 1;
    ret->param[0] = base_type;
    return ret;
}

/* Given a pointer type Foo*, grab the base type Foo */
static inline
ir_type ir_pointer_base(ir_type type) {
    assert(type->kind == ir_type_kind_pointer);
    return type->param[0];
}

/* Check if two types are equal */
int ir_type_equal(ir_type a, ir_type b);

/* Generate a user-readable string describing 'type' */
char* ir_type_repr(char *p, ir_type type);
char* ir_imm_repr(char *p, ir_type type, ir_imm imm);
