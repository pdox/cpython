#include "Python.h"
#include "ir.h"
#include <jit/jit.h>

/* Compile an IR using libjit */

jit_context_t gcontext = NULL;

#define SET_DEST(v) do { \
    jit_value_t _v = (v); \
    ir_value dest = IR_INSTR_DEST(_instr); \
    assert(dest->index != IR_INVALID_INDEX); \
    assert(dest->type != ir_type_void); \
    assert(jit_modes[dest->index] != 2); \
    if (jit_values[dest->index] == NULL) { \
        jit_values[dest->index] = _v; \
    } else { \
        jit_insn_store(jit_func, jit_values[dest->index], _v); \
    } \
} while (0)

#define SET_DEST_FORCE_COPY(v) do { \
    jit_value_t _v = (v); \
    ir_value dest = IR_INSTR_DEST(_instr); \
    assert(dest->index != IR_INVALID_INDEX); \
    assert(dest->type != ir_type_void); \
    assert(jit_modes[dest->index] != 2); \
    if (jit_values[dest->index] == NULL) { \
        jit_values[dest->index] = jit_value_create(jit_func, JIT_TYPE(dest->type)); \
    } \
    jit_insn_store(jit_func, jit_values[dest->index], _v); \
} while (0)

#define JIT_VALUE_FROM_ALLOCA(irval) ({ \
    ir_value _val = (irval); \
    size_t _index = _val->index; \
    jit_value_t _ret; \
    assert(jit_modes[_index] == 2); \
    if (jit_values[_index]) { \
        _ret = jit_values[_index]; \
    } else { \
        ir_type base = ir_pointer_base(ir_typeof(_val)); \
        _ret = jit_values[_index] = jit_value_create(jit_func, JIT_TYPE(base)); \
    } \
    _ret; \
})

#define JIT_VALUE(irval) ({ \
    ir_value _val = (irval); \
    size_t _index = _val->index; \
    jit_value_t _ret; \
    assert(jit_modes[_index] != 2); \
    if (jit_values[_index]) { \
        _ret = jit_values[_index]; \
    } else { \
        _ret = jit_values[_index] = jit_value_create(jit_func, JIT_TYPE(ir_typeof(_val))); \
    } \
    _ret; \
})

/* ir_value corresponding to operand i */
#define VOP(i)   IR_INSTR_OPERAND(_instr, (i))

/* jit_value_t corresponding to operand i */
#define JOP(i)   JIT_VALUE(VOP(i))

/* get ir_block corresponding to operand i */
#define BOP(i)   IR_LABEL_BLOCK(VOP(i))

#define JIT_LABEL(irblock)   ( \
    assert((irblock)->index < num_blocks), \
    &jit_blocks[(irblock)->index])

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
    case ir_type_kind_label:
    case ir_type_kind_struct:
        abort(); /* struct and label literals not supported */
        break;
    }
    Py_UNREACHABLE();
}


static inline
void _emit_instr(jit_function_t jit_func,
                 jit_value_t* jit_values, unsigned char *jit_modes,
                 size_t num_values,
                 jit_label_t* jit_blocks, size_t num_blocks,
                 ir_func func, ir_block block, ir_instr _instr) {
    switch (IR_INSTR_OPCODE(_instr)) {
    case ir_opcode_neg: {
        SET_DEST(jit_insn_neg(jit_func, JOP(0)));
        break;
    }
    case ir_opcode_not: {
        SET_DEST(jit_insn_not(jit_func, JOP(0)));
        break;
    }

#define EMIT_BINOP(name) \
    case ir_opcode_ ## name: { \
        SET_DEST(jit_insn_ ## name (jit_func, JOP(0), JOP(1))); \
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
        SET_DEST(jit_insn_to_not_bool(jit_func, JOP(0)));
        break;
    }
    case ir_opcode_bool: {
        SET_DEST(jit_insn_to_bool(jit_func, JOP(0)));
        break;
    }

#define EMIT_COMPARISON(name) \
    case ir_opcode_ ## name: { \
        SET_DEST(jit_insn_ ## name (jit_func, JOP(0), JOP(1))); \
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
        jit_label_t if_false_label = jit_label_undefined;
        jit_label_t after = jit_label_undefined;
        jit_value_t cond = JOP(0);
        jit_value_t value_if_true = JOP(1);
        jit_value_t value_if_false = JOP(2);
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
        int num_args = IR_INSTR_NUM_OPERANDS(_instr) - 1;
        jit_value_t *args = (jit_value_t*)malloc(num_args * sizeof(jit_value_t));
        int i;
        for (i = 0; i < num_args; i++) {
            args[i] = JOP(1 + i);
        }
        ir_value target = VOP(0);
        jit_type_t jit_sig = JIT_TYPE(ir_typeof(target));
        jit_value_t ret;
        /* Optimization: if we're calling a constant function pointer, use jit_insn_call_native */
        ir_instr def = IR_VALUE_DEF(target);
        if (IR_INSTR_OPCODE(def) == ir_opcode_constant) {
            void *native_func = ((ir_instr_constant)def)->imm.ptr;
            ret = jit_insn_call_native(jit_func, NULL, native_func, jit_sig, args, num_args, JIT_CALL_NOTHROW);
        } else {
            ret = jit_insn_call_indirect(jit_func, JIT_VALUE(target), jit_sig, args, num_args, JIT_CALL_NOTHROW);
        }
        free(args);
        if (ir_typeof(IR_INSTR_DEST(_instr)) != ir_type_void) {
            SET_DEST(ret);
        }
        break;
    }
    case ir_opcode_patchpoint: {
        /* Patchpoints not yet supported by libjit */
        abort();
        break;
    }
    case ir_opcode_get_element_ptr: {
        IR_INSTR_AS(get_element_ptr)
        SET_DEST(jit_insn_add_relative(jit_func, JOP(0), instr->offset));
        break;
    }
    case ir_opcode_get_index_ptr: {
        /* operands: ptr, index */
        ir_value ptr = VOP(0);
        ir_type base_type = ir_pointer_base(ir_typeof(ptr));
        if (base_type->kind == ir_type_kind_struct) {
            jit_value_t offset = jit_insn_mul(
                jit_func,
                JOP(1),
                jit_value_create_nint_constant(jit_func, jit_type_void_ptr, base_type->size));
            SET_DEST(jit_insn_add(jit_func, JOP(0), offset));
        } else {
            SET_DEST(jit_insn_load_elem_address(jit_func, JOP(0), JOP(1), JIT_TYPE(base_type)));
        }
        break;
    }
    case ir_opcode_load: {
        ir_value ptr = VOP(0);
        if (jit_modes[ptr->index] == 2) {
            SET_DEST_FORCE_COPY(JIT_VALUE_FROM_ALLOCA(ptr));
        } else {
            ir_type base_type = ir_pointer_base(ir_typeof(ptr));
            SET_DEST(jit_insn_load_relative(jit_func, JIT_VALUE(ptr), 0, JIT_TYPE(base_type)));
        }
        break;
    }
    case ir_opcode_store: {
        ir_value ptr = VOP(0);
        if (jit_modes[ptr->index] == 2) {
            jit_insn_store(jit_func, JIT_VALUE_FROM_ALLOCA(ptr), JOP(1));
        } else {
            jit_insn_store_relative(jit_func, JIT_VALUE(ptr), 0, JOP(1));
        }
        break;
    }
    case ir_opcode_alloca: {
        IR_INSTR_AS(alloca)
        ir_value dest = IR_INSTR_DEST(_instr);
        if (jit_modes[dest->index] == 2) {
            /* Nothing to do. The value will be created by JIT_VALUE_FROM_ALLOCA. */
        } else {
            size_t elem_size = ir_pointer_base(ir_typeof(dest))->size;
            assert(elem_size > 0);
            jit_value_t alloca_size =
                jit_value_create_nint_constant(jit_func, jit_type_nuint, elem_size * instr->num_elements);
            jit_value_t mem = jit_insn_alloca(jit_func, alloca_size);
            SET_DEST(mem);
        }
        break;
    }
    case ir_opcode_constant: {
        IR_INSTR_AS(constant)
        ir_type dest_type = ir_typeof(IR_INSTR_DEST(_instr));
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
        ir_type dest_type = ir_typeof(IR_INSTR_DEST(_instr));
        SET_DEST(jit_insn_convert(jit_func, JOP(0), JIT_TYPE(dest_type), 0));
        break;
    }
    case ir_opcode_func_arg: {
        IR_INSTR_AS(func_arg)
        assert(instr->index >= 1);
        SET_DEST(jit_value_get_param(jit_func, instr->index - 1));
        break;
    }
    case ir_opcode_label_here: {
        break;
    }
    case ir_opcode_info_here: {
        break;
    }
    case ir_opcode_branch: {
        ir_block target = BOP(0);
        /* Optimization: Don't emit branch if we're jumping to the next block, just fall through. */
        if (target != block->next) {
            jit_insn_branch(jit_func, JIT_LABEL(target));
        }
        break;
    }
    case ir_opcode_branch_cond: {
        jit_value_t cond = JOP(0);
        ir_block if_true = BOP(1);
        ir_block if_false = BOP(2);
        /* Optimization: Omit branch if we're jumping to the next block. */
        if (if_true != block->next) {
            jit_insn_branch_if(jit_func, cond, JIT_LABEL(if_true));
        }
        if (if_false != block->next) {
            jit_insn_branch_if_not(jit_func, cond, JIT_LABEL(if_false));
        }
        break;
    }
    case ir_opcode_jumptable: {
        int num_labels = IR_INSTR_NUM_OPERANDS(_instr) - 1;
        jit_label_t *labels = (jit_label_t*)malloc(sizeof(jit_label_t) * num_labels);
        int i;
        for (i = 0; i < num_labels; i++) {
            jit_label_t *p = JIT_LABEL(BOP(1+i));
            if (*p == jit_label_undefined) {
                *p = jit_function_reserve_label(jit_func);
            }
            labels[i] = *p;
        }
        jit_insn_jump_table(jit_func, JOP(0), labels, num_labels);
        free(labels);
        break;
    }
    case ir_opcode_ret: {
        jit_insn_return(jit_func, NULL);
        break;
    }
    case ir_opcode_retval: {
        jit_insn_return(jit_func, JOP(0));
        break;
    }
    default:
        abort(); /* unhandled */
    } // end switch
}

ir_object ir_libjit_compile(ir_func func) {
    unsigned int i;
    if (gcontext == NULL) {
        gcontext = jit_context_create();
    }

    jit_context_build_start(gcontext);
    jit_type_t jit_sig = JIT_TYPE(func->sig);
    jit_function_t jit_func = jit_function_create(gcontext, jit_sig);

    /* Index and count the values */
    size_t num_values = ir_func_next_value_index(func);

    /* Index and count blocks */
    size_t num_blocks = ir_func_next_block_index(func);

    /* Allocate an array for the jit_value_t equivalents */
    jit_value_t *jit_values = (jit_value_t*)malloc(num_values * sizeof(jit_value_t));
    memset(jit_values, 0, num_values * sizeof(jit_value_t));

    /* Allocate array for value modes:
       0 = undetermined
       1 = normal value
       2 = alloca translated to jit value
     */
    unsigned char *jit_modes = (unsigned char*)malloc(num_values * sizeof(unsigned char));
    memset(jit_modes, 0, num_values * sizeof(unsigned char));

    /* Allocate an array for block labels */
    jit_label_t *jit_blocks = (jit_label_t*)malloc(num_blocks * sizeof(jit_label_t));
    for (i = 0; i < num_blocks; i++) {
        jit_blocks[i] = jit_label_undefined;
    }

    /* Convert alloca values of constant integer size 1, with only loads and stores,
       into jit values, for libjit to optimize. This is a short-term optimization,
       as there should eventually be a "Mem2Reg" pass. */
    ir_block b;
    ir_instr _instr;
    for (b = func->first_block; b != NULL; b = b->next) {
        for (_instr = b->first_instr; _instr != NULL; _instr = _instr->next) {
            ir_opcode opcode = IR_INSTR_OPCODE(_instr);
            if (opcode != ir_opcode_load &&
                opcode != ir_opcode_store) {
                size_t uses_count;
                ir_use uses = ir_get_uses(_instr, &uses_count);
                for (size_t i = 0; i < uses_count; i++) {
                    jit_modes[IR_USE_VALUE(uses[i])->index] = 1;
                }
            }
            if (opcode == ir_opcode_alloca) {
                IR_INSTR_AS(alloca)
                ir_value dest = IR_INSTR_DEST(_instr);
                if (instr->num_elements == 1 &&
                    jit_modes[dest->index] == 0) {
                    jit_modes[dest->index] = 2;
                }
            }
        }
    }

    /* Emit each block, one by one */
    for (b = func->first_block; b != NULL; b = b->next) {
        assert(b->index < num_blocks);
        jit_insn_label(jit_func, &jit_blocks[b->index]);

        /* Emit each instruction, one by one */
        for (_instr = b->first_instr; _instr != NULL; _instr = _instr->next) {
            _emit_instr(jit_func, jit_values, jit_modes, num_values, jit_blocks, num_blocks, func, b, _instr);
        }
    }

    jit_function_set_optimization_level(jit_func, jit_function_get_max_optimization_level());
    int ok = jit_function_compile(jit_func);
    if (!ok) {
        Py_FatalError("libjit compile failed");
    }
    void *entrypoint = jit_function_to_closure(jit_func);
    assert(entrypoint);
    jit_context_build_end(gcontext);

    ir_object ret = ir_object_new();
    // TODO: Keep track of compiler data, so that when an ir_object is free'd,
    //       the associated compiler module is also freed.
    ret->compiler_data = NULL;
    ret->compiler_free_callback = NULL;
    ret->entrypoint = entrypoint;
    ret->stackmap_index = NULL;
    return ret;
}
