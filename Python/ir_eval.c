#include "Python.h"
#include "ir.h"

#if 0

#define DO_UNOP(type, _id, op) \
        case ir_type_kind_ ## type: \
            dest->rtv._id = op (instr->value->rtv . _id); \
            break;

#define EVAL_UNOP(name, op) \
    case ir_opcode_ ## name: { \
        ir_instr_unop instr = (ir_instr_unop) _instr; \
        switch (instr->value->type->kind) { \
        DO_UNOP(char, c, op) \
        DO_UNOP(uchar, uc, op) \
        DO_UNOP(short, s, op) \
        DO_UNOP(ushort, us, op) \
        DO_UNOP(int, i, op) \
        DO_UNOP(uint, ui, op) \
        DO_UNOP(long, l, op) \
        DO_UNOP(ulong, ul, op) \
        DO_UNOP(longlong, ll, op) \
        DO_UNOP(ulonglong, ull, op) \
        DO_UNOP(intptr, ip, op) \
        DO_UNOP(uintptr, uip, op) \
        DO_UNOP(pyssizet, pyssizet, op) \
        default: \
            abort(); /* Non-integral type? */ \
            break; \
        } \
        break; \
    }

#define DO_BINOP(type, _id, op) \
        case ir_type_kind_ ## type: \
            dest->rtv._id = (instr->left->rtv . _id) op (instr->right->rtv . _id); \
            break;

#define EVAL_BINOP(name, op) \
    case ir_opcode_ ## name: { \
        ir_instr_binop instr = (ir_instr_binop) _instr; \
        switch (instr->left->type->kind) { \
        DO_BINOP(char, c, op) \
        DO_BINOP(uchar, uc, op) \
        DO_BINOP(short, s, op) \
        DO_BINOP(ushort, us, op) \
        DO_BINOP(int, i, op) \
        DO_BINOP(uint, ui, op) \
        DO_BINOP(long, l, op) \
        DO_BINOP(ulong, ul, op) \
        DO_BINOP(longlong, ll, op) \
        DO_BINOP(ulonglong, ull, op) \
        DO_BINOP(intptr, ip, op) \
        DO_BINOP(uintptr, uip, op) \
        DO_BINOP(pyssizet, pyssizet, op) \
        default: \
            abort(); /* Non-integral type? */ \
            break; \
        } \
        break; \
    }

/* Evaluate an instruction, and then return the next block and instruction */
void
ir_eval_instr(ir_func func, ir_block block, ir_instr _instr, ir_block *next_block, ir_instr *next_instr) {
    ir_value dest = _instr->dest;
    if (
    switch (_instr->opcode) {
        
        EVAL_UNOP(neg, -)
        EVAL_UNOP(not, ~)
        EVAL_UNOP(bool, !!)

        EVAL_BINOP(add, +)
        EVAL_BINOP(sub, -)
        EVAL_BINOP(mul, *)
        EVAL_BINOP(div, /)
        EVAL_BINOP(rem, %)
        EVAL_BINOP(shl, <<)
        EVAL_BINOP(shr, >>)
        EVAL_BINOP(and, &)
        EVAL_BINOP(or, |)
        EVAL_BINOP(xor, ^)
        EVAL_BINOP(lt, <)
        EVAL_BINOP(gt, >)
        EVAL_BINOP(eq, ==)
        EVAL_BINOP(ne, !=)
        EVAL_BINOP(le, <=)
        EVAL_BINOP(ge, >=)

        default:
            Py_FatalError("ir_eval unhandled opcode");
            break;
    }
}

static inline
void _ir_eval_va_arg(ir_func func, ir_imm *imm, va_list ap, ir_type type) {
    /* Get an argument from 'ap' using the correct C type, and store it into 'imm' */
    switch (type->kind) {
    case ir_type_kind_void:
        abort(); /* void literal argument makes no sense */
        break;

#define PROCESS(name, ctype, ps, _id, va_arg_type) \
    case ir_type_kind_ ## name: \
        imm-> _id = va_arg(ap, va_arg_type); \
        break;
#include "ir_integral_types.def"
#undef PROCESS
    case ir_type_kind_pointer:
    case ir_type_kind_function:
        imm->ptr = va_arg(ap, void*);
        break;
    case ir_type_kind_struct:
        abort(); /* struct literal arguments not supported */
        break;
    }
}

void ir_eval_func(ir_func func, ...) {
    int i;
    va_list ap;

    /* Copy the arguments into the argument values */
    va_start(ap, func);
    for (i = 1; i < func->sig->param_count; i++) {
        ir_value v = func->param[1 + i];
        _ir_eval_va_arg(func, &v->rtv, ap, ir_typeof(v));
    }
    va_end(ap);

    /* Execute instructions */
    func->first_block->first_instr
}

#endif
