#include "Python.h"
#include "Include/frameobject.h"
#include "ir.h"
#include <stdio.h>

#define IR_INTEGRAL_TYPE(name, ctype) \

/* Define void */
ir_type_t _ir_type_void = {"void", ir_type_kind_void, 0, 0, {NULL}};

/* Define integral types */
#define PROCESS(name, ctype, ps, id, va_arg_type) \
    ir_type_t _ir_type_ ## name = {#ctype, ir_type_kind_ ## name, sizeof(ctype), 0, {NULL}};
#include "ir_integral_types.def"
#undef PROCESS

/* Define struct types */
#define PROCESS(name, ctype) \
    ir_type_t _ir_type_ ## name = {#ctype, ir_type_kind_struct, sizeof(ctype), 0, {NULL}};
#include "ir_struct_types.def"
#undef PROCESS

/* Define pointer types for all built-in types */
#define PROCESS(base_type) \
    ir_type_t _ir_type_ ## base_type ## _ptr = {NULL, ir_type_kind_pointer, sizeof(void*), 1, {&_ir_type_ ## base_type}};
#include "ir_all_types.def"
#undef PROCESS

/* Define pointer to pointers for all built-in types */
#define PROCESS(base_type) \
    ir_type_t _ir_type_ ## base_type ## _ptr_ptr = {NULL, ir_type_kind_pointer, sizeof(void*), 1, {&_ir_type_ ## base_type ## _ptr}};
#include "ir_all_types.def"
#undef PROCESS

int ir_type_equal(ir_type a, ir_type b) {
    if (a == b)
        return 1;
    if (a->kind != b->kind)
        return 0;
    if (a->kind == ir_type_kind_void || ir_type_is_integral(a)) {
        /* void and integral types are fully determined by their 'kind' */
        return 1;
    }
    /* struct types are identified by name */
    if (a->kind == ir_type_kind_struct) {
        return strcmp(a->name, b->name) == 0;
    }

    /* pointer types identified by base type */
    if (a->kind == ir_type_kind_pointer) {
        return ir_type_equal(a->param[0], b->param[0]);
    }

    assert(a->kind == ir_type_kind_function);
    if (a->param_count != b->param_count) {
        return 0;
    }
    size_t i;
    for (i = 0; i < a->param_count; i++) {
        if (!ir_type_equal(a->param[i], b->param[i]))
            return 0;
    }
    return 1;
}

char* ir_type_repr(char *p, ir_type type) {
    size_t i;
    if (type->name) {
        p += sprintf(p, "%s", type->name);
        return p;
    }
    switch (type->kind) {
    case ir_type_kind_pointer:
        p = ir_type_repr(p, type->param[0]);
        *p++ = '*';
        break;
    case ir_type_kind_function:
        /* ret_type(*)(arg1, arg2, ...) */
        p = ir_type_repr(p, type->param[0]);
        p += sprintf(p, "(*)(");
        for (i = 1; i < type->param_count; i++) {
           if (i != 1) {
               *p++ = ',';
               *p++ = ' ';
           }
           p = ir_type_repr(p, type->param[i]);
        }
        p += sprintf(p, ")");
        break;
    default:
        p += sprintf(p, "<invalid_kind_%ld>", (long)type->kind);
        break;
    }
    return p;
}

char* ir_imm_repr(char *p, ir_type type, ir_imm imm) {
    switch (type->kind) {
    case ir_type_kind_void:
        p += sprintf(p, "<void>");
        break;
#define PROCESS(name, ctype, ps, _field, va_arg_type) \
    case ir_type_kind_##name: \
        p += sprintf(p, ps, imm._field); \
        break;
#include "ir_integral_types.def"
#undef PROCESS
    case ir_type_kind_pointer:
    case ir_type_kind_function:
        p += sprintf(p, "%p", imm.ptr);
        break;
    case ir_type_kind_struct:
        p += sprintf(p, "<struct literal>");
        break;
    default:
        p += sprintf(p, "<unknown>");
        break;
    }
    return p;
}
