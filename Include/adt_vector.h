/* An contiguous list of elements which grows as needed. (shrinking not implemented)
   Based on std::vector.
 */

typedef struct {
    size_t element_width;
    size_t alloc_count;
    size_t count;
    char *data;
} adt_vector_t;
typedef adt_vector_t* adt_vector;

static inline char* _adt_vector_slot(adt_vector v, size_t i) {
    assert(i < v->count);
    return v->data + v->element_width * i;
}

static inline adt_vector adt_vector_new(size_t start_count, size_t element_width) {
    adt_vector ret = (adt_vector)malloc(sizeof(adt_vector_t));
    ret->element_width = element_width;
    ret->alloc_count = start_count;
    ret->count = 0;
    ret->data = (char*)malloc(element_width * start_count);
    assert(ret->data);
    return ret;
}

static inline void adt_vector_delete(adt_vector v) {
    free(v->data);
    free(v);
}

static inline void adt_vector_clear(adt_vector v) {
    v->count = 0;
}

static inline size_t adt_vector_size(adt_vector v) {
    return v->count;
}

static inline void *adt_vector_data(adt_vector v) {
    return v->data;
}

static inline int adt_vector_pop_back(adt_vector v, void *out) {
    if (v->count > 0) {
        memcpy(out, _adt_vector_slot(v, v->count - 1), v->element_width);
        v->count--;
        return 1;
    }
    return 0;
}

static inline void adt_vector_push_back(adt_vector v, void *in) {
    if (v->count == v->alloc_count) {
        v->alloc_count *= 2;
        v->data = realloc(v->data, v->alloc_count * v->element_width);
        assert(v->data);
    }
    v->count++;
    memcpy(_adt_vector_slot(v, v->count - 1), in, v->element_width);
}
