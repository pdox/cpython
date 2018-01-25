#include "Python.h"
#include "ir.h"

ir_object ir_object_new(void) {
    ir_object ret = (ir_object)malloc(sizeof(ir_object_t));
    assert(ret);
    return ret;
}

void ir_object_free(ir_object obj) {
    if (obj->compiler_free_callback) {
        obj->compiler_free_callback(obj);
    }
    if (obj->stackmap_index) {
        ir_stackmap_index_free(obj->stackmap_index);
    }
    free(obj);
}
