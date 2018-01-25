#include "Python.h"
#include "ir.h"

ir_stackmap_index ir_stackmap_index_new(size_t num_stackmaps) {
    ir_stackmap_index index = (ir_stackmap_index)malloc(
        sizeof(ir_stackmap_index_t) +
        sizeof(ir_stackmap_info) * num_stackmaps);
    index->insertion_index = 0;
    index->num_stackmaps = num_stackmaps;
    return index;
}

ir_stackmap_info ir_stackmap_info_new(size_t num_values) {
    ir_stackmap_info info = (ir_stackmap_info)malloc(
        sizeof(ir_stackmap_info_t) +
        sizeof(ir_stackmap_value_t) * num_values);
    info->code_address = NULL;
    info->user_data = NULL;
    info->num_values = 0;
    return info;
}

static int user_data_compare(const void *pa, const void *pb) {
    ir_stackmap_info a = (ir_stackmap_info)pa;
    ir_stackmap_info b = (ir_stackmap_info)pb;
    if (a->user_data < b->user_data) return -1;
    if (a->user_data > b->user_data) return 1;
    return 0;
}

void ir_stackmap_index_add(ir_stackmap_index index, ir_stackmap_info info) {
    assert(index->insertion_index < index->num_stackmaps);
    index->stackmaps[index->insertion_index++] = info;
    if (index->insertion_index == index->num_stackmaps) {
        /* Sort by address, to prepare the index for lookups */
        qsort(index->stackmaps, index->num_stackmaps, sizeof(ir_stackmap_info), user_data_compare);
    }
}

ir_stackmap_info ir_stackmap_index_lookup(ir_stackmap_index index, void *user_data) {
    /* If we didn't fill the index completely, it may not be sorted properly. */
    assert(index->insertion_index == index->num_stackmaps);
    ir_stackmap_info_t key;
    ir_stackmap_info ret;
    key.user_data = user_data;
    ret = (ir_stackmap_info)bsearch(
        &key, index->stackmaps, index->num_stackmaps, sizeof(ir_stackmap_info), user_data_compare);
    return ret;
}

void ir_stackmap_index_free(ir_stackmap_index index) {
    for (size_t i = 0; i < index->insertion_index; i++) {
        free(index->stackmaps[i]);
    }
    free(index);
}
