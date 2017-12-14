IR_PROTOTYPE(ir_mem)
struct ir_mem_t {
    char *start;
    char *pos;
    char *end;
    ir_mem next;
};

/* Needed by ir_context */
IR_PROTOTYPE(ir_func)

IR_PROTOTYPE(ir_context)
struct ir_context_t {
    ir_mem mem;
    ir_func func;
};

/* Create a new context */
ir_context ir_context_new(void);

/* Destroy a context (and free all associated memory) */
void ir_context_destroy(ir_context context);

void _ir_alloc_more(ir_context context, size_t min_size);

/* Allocate memory (as part of this context). Used internally. */
static inline
void* _ir_alloc(ir_context context, size_t size, size_t alignment) {
    char *pos = (char*)ALIGN_UP(context->mem->pos, alignment);
    if (pos + size > context->mem->end) {
        _ir_alloc_more(context, alignment + size);
        pos = (char*)ALIGN_UP(context->mem->pos, alignment);
    }
    assert(pos + size <= context->mem->end);
    context->mem->pos = pos + size;
    return pos;
}

static inline
char* _ir_strdup(ir_context context, const char *s) {
    char *ret = (char*)_ir_alloc(context, strlen(s) + 1, 1);
    strcpy(ret, s);
    return ret;
}

#define IR_ALLOC(varname, name, extra_size) \
    name varname = (name) _ir_alloc(context, sizeof(name ## _t) + (extra_size), _alignof(name##_t));
