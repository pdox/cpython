#include "Python.h"
#include "ir.h"
#define IR_SPACE_SIZE   65536

ir_context ir_context_new(void) {
    ir_context context = malloc(sizeof(ir_context_t));
    assert(context);
    context->mem = NULL;
    context->func = NULL;
    _ir_alloc_more(context, 0);
    return context;
}

void ir_context_destroy(ir_context context) {
    ir_mem m_next;
    ir_mem m;
    for (m = context->mem; m != NULL; m = m_next) {
        m_next = m->next;
        free(m);
    }
    free(context);
}

void _ir_alloc_more(ir_context context, size_t min_size) {
    size_t alloc_size = IR_SPACE_SIZE;

    if (alloc_size < min_size)
        alloc_size = min_size;

    ir_mem m = (ir_mem) malloc(sizeof(ir_mem_t) + alloc_size);
    assert(m);
    m->start = (char*)m + sizeof(ir_mem_t);
    m->pos = m->start;
    m->end = m->start + alloc_size;
    m->next = context->mem;
    context->mem = m;
}
