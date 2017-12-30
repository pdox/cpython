#include "Python.h"
#include "ir.h"
#include <jit/jit.h>

/* Compile an IR using libjit */

jit_context_t gcontext = NULL;

#define SET_DEST(v) do { \
    jit_value_t _v = (v); \
    assert(_instr->dest != NULL); \
    if (jit_values[_instr->dest->index] == NULL) { \
        jit_values[_instr->dest->index] = _v; \
    } else { \
        jit_insn_store(jit_func, jit_values[_instr->dest->index], _v); \
    } \
} while (0)

#define JIT_VALUE(irval) \
    (jit_values[(irval)->index] ? \
     jit_values[(irval)->index] : \
     (jit_values[(irval)->index] = jit_value_create(jit_func, JIT_TYPE(ir_typeof(irval)))))

#define JIT_LABEL(irlabel)   ( \
    assert((irlabel)->block->index < num_blocks), \
    &jit_blocks[(irlabel)->block->index])

#define JIT_TYPE(irtype) _ir_type_to_jit(irtype)

static
jit_type_t _ir_type_to_jit(ir_type type) {
    switch (type->kind) {
    case ir_type_kind_void: return jit_type_void;
    case ir_type_kind_char: return jit_type_sbyte;
    case ir_type_kind_uchar: return jit_type_ubyte;
    case ir_type_kind_short: return jit_type_short;
    case ir_type_kind_ushort: return jit_type_ushort;
    case ir_type_kind_int: return jit_type_int;
    case ir_type_kind_uint: return jit_type_uint;
    case ir_type_kind_long: return jit_type_long;
    case ir_type_kind_ulong: return jit_type_ulong;
    case ir_type_kind_longlong: return jit_type_sys_longlong;
    case ir_type_kind_ulonglong: return jit_type_sys_ulonglong;
    case ir_type_kind_intptr: return jit_type_nint;
    case ir_type_kind_uintptr: return jit_type_nuint;
    case ir_type_kind_sizet: return jit_type_nuint;
    case ir_type_kind_pyssizet: return jit_type_nint;
    case ir_type_kind_pointer: return jit_type_void_ptr;
    case ir_type_kind_function: {
        jit_type_t ret;
        jit_type_t *jit_params = (jit_type_t*)malloc(type->param_count * sizeof(jit_type_t));
        size_t num_args = type->param_count - 1;
        for (size_t i = 0; i < type->param_count; i++) {
            jit_params[i] = _ir_type_to_jit(type->param[i]);
        }
        ret = jit_type_create_signature(jit_abi_cdecl, jit_params[0], &jit_params[1], num_args, 1);
        free(jit_params);
        return ret;
    }
    case ir_type_kind_struct:
        abort(); /* struct literals not supported */
        break;
    }
    Py_UNREACHABLE();
}


static inline
void _emit_instr(jit_function_t jit_func,
                 jit_value_t* jit_values, size_t num_values,
                 jit_label_t* jit_blocks, size_t num_blocks,
                 ir_func func, ir_block block, ir_instr _instr) {
    switch (_instr->opcode) {
    case ir_opcode_neg: {
        IR_INSTR_AS(unop)
        SET_DEST(jit_insn_neg(jit_func, JIT_VALUE(instr->value)));
        break;
    }
    case ir_opcode_not: {
        IR_INSTR_AS(unop)
        SET_DEST(jit_insn_not(jit_func, JIT_VALUE(instr->value)));
        break;
    }

#define EMIT_BINOP(name) \
    case ir_opcode_ ## name: { \
        IR_INSTR_AS(binop) \
        SET_DEST(jit_insn_ ## name (jit_func, JIT_VALUE(instr->left), JIT_VALUE(instr->right))); \
        break; \
    }
    EMIT_BINOP(add)
    EMIT_BINOP(sub)
    EMIT_BINOP(mul)
    EMIT_BINOP(div)
    EMIT_BINOP(rem)
    EMIT_BINOP(and)
    EMIT_BINOP(or)
    EMIT_BINOP(xor)
    EMIT_BINOP(shl)
    EMIT_BINOP(shr)
#undef EMIT_BINOP

    case ir_opcode_notbool: {
        IR_INSTR_AS(boolean)
        SET_DEST(jit_insn_to_not_bool(jit_func, JIT_VALUE(instr->value)));
        break;
    }
    case ir_opcode_bool: {
        IR_INSTR_AS(boolean)
        SET_DEST(jit_insn_to_bool(jit_func, JIT_VALUE(instr->value)));
        break;
    }

#define EMIT_COMPARISON(name) \
    case ir_opcode_ ## name: { \
        IR_INSTR_AS(comparison) \
        SET_DEST(jit_insn_ ## name (jit_func, JIT_VALUE(instr->left), JIT_VALUE(instr->right))); \
        break; \
    }
    EMIT_COMPARISON(lt)
    EMIT_COMPARISON(gt)
    EMIT_COMPARISON(eq)
    EMIT_COMPARISON(ne)
    EMIT_COMPARISON(le)
    EMIT_COMPARISON(ge)
#undef EMIT_COMPARISON

    case ir_opcode_ternary: {
        IR_INSTR_AS(ternary)
        jit_label_t if_false_label = jit_label_undefined;
        jit_label_t after = jit_label_undefined;
        jit_value_t cond = JIT_VALUE(instr->cond);
        jit_value_t value_if_true = JIT_VALUE(instr->if_true);
        jit_value_t value_if_false = JIT_VALUE(instr->if_false);
        jit_value_t output = jit_value_create(jit_func, jit_value_get_type(value_if_true));
        jit_insn_branch_if_not(jit_func, cond, &if_false_label);
        jit_insn_store(jit_func, output, value_if_true);
        jit_insn_branch(jit_func, &after);
        jit_insn_label(jit_func, &if_false_label);
        jit_insn_store(jit_func, output, value_if_false);
        jit_insn_label(jit_func, &after);
        SET_DEST(output);
        break;
    }
    case ir_opcode_call: {
        IR_INSTR_AS(call)
        int num_args = instr->arg_count;
        jit_value_t *args = (jit_value_t*)malloc(num_args * sizeof(jit_value_t));
        int i;
        for (i = 0; i < num_args; i++) {
            args[i] = JIT_VALUE(instr->arg[i]);
        }
        jit_value_t ret;
        /* Optimization: if we're calling a constant function pointer, use jit_insn_call_native */
        if (instr->target->def != NULL && instr->target->def->opcode == ir_opcode_constant) {
            void *native_func = ((ir_instr_constant)(instr->target->def))->imm.ptr;
            ret = jit_insn_call_native(jit_func, NULL, native_func, JIT_TYPE(ir_typeof(instr->target)), args, num_args, JIT_CALL_NOTHROW);
        } else {
            ret = jit_insn_call_indirect(jit_func, JIT_VALUE(instr->target), JIT_TYPE(ir_typeof(instr->target)), args, num_args, JIT_CALL_NOTHROW);
        }
        free(args);
        if (_instr->dest) {
            SET_DEST(ret);
        }
        break;
    }
    case ir_opcode_get_element_ptr: {
        IR_INSTR_AS(get_element_ptr)
        SET_DEST(jit_insn_add_relative(jit_func, JIT_VALUE(instr->ptr), instr->offset));
        break;
    }
    case ir_opcode_get_index_ptr: {
        IR_INSTR_AS(get_index_ptr)
        ir_type base_type = ir_pointer_base(ir_typeof(instr->ptr));
        if (base_type->kind == ir_type_kind_struct) {
            jit_value_t offset = jit_insn_mul(
                jit_func,
                JIT_VALUE(instr->index),
                jit_value_create_nint_constant(jit_func, jit_type_void_ptr, base_type->size));
            SET_DEST(jit_insn_add(jit_func, JIT_VALUE(instr->ptr), offset));
        } else {
            SET_DEST(jit_insn_load_elem_address(jit_func, JIT_VALUE(instr->ptr), JIT_VALUE(instr->index), JIT_TYPE(base_type)));
        }
        break;
    }
    case ir_opcode_load: {
        IR_INSTR_AS(load)
        ir_type base_type = ir_pointer_base(ir_typeof(instr->ptr));
        SET_DEST(jit_insn_load_relative(jit_func, JIT_VALUE(instr->ptr), 0, JIT_TYPE(base_type)));
        break;
    }
    case ir_opcode_store: {
        IR_INSTR_AS(store)
        jit_insn_store_relative(jit_func, JIT_VALUE(instr->ptr), 0, JIT_VALUE(instr->value));
        break;
    }
    case ir_opcode_address_of: {
        IR_INSTR_AS(address_of)
        SET_DEST(jit_insn_address_of(jit_func, JIT_VALUE(instr->value)));
        break;
    }
    case ir_opcode_alloca: {
        IR_INSTR_AS(alloca)
        size_t elem_size = ir_pointer_base(ir_typeof(_instr->dest))->size;
        assert(elem_size > 0);
        jit_value_t alloca_size = jit_insn_mul(
            jit_func,
            jit_insn_convert(jit_func, JIT_VALUE(instr->num_elements), jit_type_nuint, 0),
            jit_value_create_nint_constant(jit_func, jit_type_nuint, elem_size));
        jit_value_t mem = jit_insn_alloca(jit_func, alloca_size);
        SET_DEST(mem);
        break;
    }
    case ir_opcode_constant: {
        IR_INSTR_AS(constant)
        ir_type dest_type = ir_typeof(_instr->dest);
        jit_value_t ret;
        switch (dest_type->kind) {
#define PROCESS(name, ctype, ps, _id, va_arg_type) \
        case ir_type_kind_ ## name: \
            ret = jit_value_create_nint_constant(jit_func, JIT_TYPE(dest_type), instr->imm . _id); \
            break;
#include "ir_integral_types.def"
#undef PROCESS
        case ir_type_kind_pointer:
        case ir_type_kind_function:
            ret = jit_value_create_nint_constant(jit_func, jit_type_void_ptr, (jit_nint)instr->imm.ptr);
            break;
        default:
            abort(); /* Unhandled type */
        }
        SET_DEST(ret);
        break;
    }
    case ir_opcode_cast: {
        IR_INSTR_AS(cast)
        ir_type dest_type = ir_typeof(_instr->dest);
        SET_DEST(jit_insn_convert(jit_func, JIT_VALUE(instr->value), JIT_TYPE(dest_type), 0));
        break;
    }
    case ir_opcode_set_value: {
        IR_INSTR_AS(set_value)
        if (jit_values[_instr->dest->index] == NULL) { \
            jit_values[_instr->dest->index] = jit_value_create(jit_func, JIT_TYPE(ir_typeof(_instr->dest)));
        }
        jit_insn_store(jit_func, jit_values[_instr->dest->index], JIT_VALUE(instr->src));
        break;
    }
    case ir_opcode_label_here: {
        break;
    }
    case ir_opcode_branch: {
        IR_INSTR_AS(branch)
        /* Optimization: Don't emit branch if we're jumping to the next block, just fall through. */
        if (instr->target->block != block->next) {
            jit_insn_branch(jit_func, JIT_LABEL(instr->target));
        }
        break;
    }
    case ir_opcode_branch_cond: {
        IR_INSTR_AS(branch_cond)
        jit_value_t cond = JIT_VALUE(instr->cond);
        /* Optimization: Omit branch if we're jumping to the next block. */
        if (instr->if_true->block != block->next) {
            jit_insn_branch_if(jit_func, cond, JIT_LABEL(instr->if_true));
        }
        if (instr->if_false->block != block->next) {
            jit_insn_branch_if_not(jit_func, cond, JIT_LABEL(instr->if_false));
        }
        break;
    }
    case ir_opcode_jumptable: {
        IR_INSTR_AS(jumptable)
        int num_labels = instr->table_size;
        jit_label_t *labels = (jit_label_t*)malloc(sizeof(jit_label_t) * num_labels);
        int i;
        for (i = 0; i < num_labels; i++) {
            jit_label_t *p = JIT_LABEL(instr->table[i]);
            if (*p == jit_label_undefined) {
                *p = jit_function_reserve_label(jit_func);
            }
            labels[i] = *p;
        }
        jit_insn_jump_table(jit_func, JIT_VALUE(instr->index), labels, num_labels);
        free(labels);
        break;
    }
    case ir_opcode_ret: {
        IR_INSTR_AS(ret)
        jit_insn_return(jit_func, instr->value ? JIT_VALUE(instr->value) : NULL);
        break;
    }
    default:
        abort(); /* unhandled */
    } // end switch
}

void *
ir_libjit_compile(ir_func func) {
    unsigned int i;
    if (gcontext == NULL) {
        gcontext = jit_context_create();
    }

    jit_context_build_start(gcontext);
    jit_type_t jit_sig = JIT_TYPE(func->sig);
    jit_function_t jit_func = jit_function_create(gcontext, jit_sig);

    /* Index and count the values */
    size_t num_values = ir_func_largest_value_index(func);

    /* Index and count blocks */
    size_t num_blocks = ir_func_largest_block_index(func);

    /* Allocate an array for the jit_value_t equivalents */
    jit_value_t *jit_values = (jit_value_t*)malloc(num_values * sizeof(jit_value_t));
    memset(jit_values, 0, num_values * sizeof(jit_value_t));

    /* Setup the values corresponding to the arguments */
    for (i = 0; i < func->sig->param_count - 1; i++) {
        ir_value argi = ir_func_get_argument(func, i);
        jit_values[argi->index] = jit_value_get_param(jit_func, i);
    }

    /* Allocate an array for block labels */
    jit_label_t *jit_blocks = (jit_label_t*)malloc(num_blocks * sizeof(jit_label_t));
    for (i = 0; i < num_blocks; i++) {
        jit_blocks[i] = jit_label_undefined;
    }

    /* Emit each block, one by one */
    ir_block b;
    ir_instr instr;
    for (b = func->first_block; b != NULL; b = b->next) {
        assert(b->index < num_blocks);
        jit_insn_label(jit_func, &jit_blocks[b->index]);

        /* Emit each instruction, one by one */
        for (instr = b->first_instr; instr != NULL; instr = instr->next) {
            _emit_instr(jit_func, jit_values, num_values, jit_blocks, num_blocks, func, b, instr);
        }
    }

    jit_function_set_optimization_level(jit_func, jit_function_get_max_optimization_level());
    int ok = jit_function_compile(jit_func);
    if (!ok) {
        Py_FatalError("libjit compile failed");
    }
    void *ret = jit_function_to_closure(jit_func);
    assert(ret);
    jit_context_build_end(gcontext);
    return ret;
}
