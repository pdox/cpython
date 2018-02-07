#include "Python.h"
#include "ir.h"

#include <iostream>
#include <fstream>
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "SimpleOrcJit.h"

typedef struct {
    llvm::Module *module;
    llvm::Function *llvm_func;
    llvm::BasicBlock *entry_bb;
    llvm::Value** llvm_values;
    char *llvm_modes; /* 0 = not yet assigned, 1 = plain value, 2 = alloca */
    size_t num_values;
    llvm::BasicBlock** llvm_blocks;
    size_t num_blocks;
    llvm::IRBuilder<> *builder;
} _TranslationContext;

#define BUILDER()   (*(ctx.builder))

#define LLVM_BLOCK(label)  ctx.llvm_blocks[(label)->block->index]

#define LLVM_TYPE(irtype)  _ir_type_to_llvm_type(ctx, (irtype))

#define FORCE_ALLOCA_MODE(irval1) do { \
    ir_value v1 = (irval1); \
    llvm::Type *ltype = LLVM_TYPE(ir_typeof(v1)); \
    if (ctx.llvm_values[v1->index] == NULL) { \
        ctx.llvm_values[v1->index] = BUILDER().CreateAlloca(ltype); \
        ctx.llvm_modes[v1->index] = 2; \
    } else { \
        assert(ctx.llvm_modes[v1->index] == 2); \
        assert(ctx.llvm_values[v1->index] != NULL); \
    } \
} while (0)

/* Set the value to alloca mode if this use is not preceded by a
   value creation, in the block and instruction iteration order */
#define ALLOCA_MODE_IF_NOT_DEFINED(irval2) do { \
    ir_value v2 = (irval2); \
    if (ctx.llvm_modes[v2->index] == 0) { \
        FORCE_ALLOCA_MODE(v2); \
    } \
} while (0)

#define RECORD_DEFINE(irval) do { \
    ir_value v = (irval); \
    if (ctx.llvm_modes[v->index] == 0) { \
        ctx.llvm_modes[v->index] = 1; \
    } \
} while (0)


static inline
llvm::Type *
_ir_type_to_llvm_type(_TranslationContext &ctx, ir_type type) {
    auto &context = ctx.module->getContext();
    switch (type->kind) {
    case ir_type_kind_void:
        return llvm::Type::getVoidTy(context);

#define PROCESS(name, ctype, ps, id, ptype) \
    case ir_type_kind_ ## name:
#include "ir_integral_types.def"
#undef PROCESS
        switch (type->size) {
            case 1: return llvm::Type::getInt8Ty(context);
            case 2: return llvm::Type::getInt16Ty(context);
            case 4: return llvm::Type::getInt32Ty(context);
            case 8: return llvm::Type::getInt64Ty(context);
            default: abort(); /* Other integer widths unhandled */
        }
        break;
    case ir_type_kind_pointer: {
        /* LLVM void is weird. There is a LLVM void type, but it cannot be used as the base of a pointer.
           So instead, use i8* */
        ir_type base = ir_pointer_base(type);
        if (ir_type_is_void(base)) {
            base = ir_type_char;
        }
        return llvm::PointerType::get(LLVM_TYPE(base), 0);
    }
    case ir_type_kind_function: {
        llvm::Type** llvm_types = (llvm::Type**)malloc(type->param_count * sizeof(llvm::Type*));
        size_t num_args = type->param_count - 1;
        for (size_t i = 0; i < type->param_count; i++) {
            llvm_types[i] = LLVM_TYPE(type->param[i]);
        }
        llvm::Type* ret = llvm::FunctionType::get(
            llvm_types[0],
            llvm::ArrayRef<llvm::Type*>(&llvm_types[1], num_args),
            /*IsVarArgs=*/false);
        free(llvm_types);
        return llvm::PointerType::get(ret, 0);
    }
    case ir_type_kind_struct: {
        /* Find an existing type in the module, or create a new one */
        llvm::Type *ret = ctx.module->getTypeByName(type->name);
        if (ret == nullptr) {
            ret = llvm::StructType::create(context, type->name);
        }
        return ret;
    }
    default:
        abort();
    }
}

/* Return the llvm::Value* corresponding to a given ir_value */
#define LLVM_VALUE_ADDRESS(_v) ({ \
    ir_value v = (_v); \
    assert(ctx.llvm_modes[v->index] == 2); \
    ctx.llvm_values[v->index]; \
})

#define LLVM_VALUE(__v) ({ \
    ir_value _v = (__v); \
    llvm::Value *rawval = ctx.llvm_values[_v->index]; \
    llvm::Value *ret; \
    assert(rawval != NULL); \
    assert(ctx.llvm_modes[_v->index] != 0); \
    if (ctx.llvm_modes[_v->index] == 1) { \
        ret = rawval; \
    } else { \
        ret = BUILDER().CreateLoad(rawval); \
    } \
    ret; \
})

#define LLVM_VALUE_FROM_USE(use) LLVM_VALUE(IR_USE_VALUE((use)))

/* Given an llvm::Value*, use it as the 'dest' value for the current instruction */
#define SET_DEST(__v) do { \
    llvm::Value *_v = (__v); \
    ir_value dest = IR_INSTR_DEST(_instr); \
    assert(dest->type != ir_type_void); \
    if (ctx.llvm_modes[dest->index] == 1) { \
        assert(ctx.llvm_values[dest->index] == NULL); \
        ctx.llvm_values[dest->index] = _v; \
    } else { \
        BUILDER().CreateStore(_v, ctx.llvm_values[dest->index]); \
    } \
} while (0)

static inline
void _emit_instr(_TranslationContext &ctx, ir_block b, ir_instr _instr) {
    llvm::LLVMContext &context = ctx.llvm_func->getContext();
    switch (_instr->opcode) {
    case ir_opcode_neg: {
        IR_INSTR_AS(unop)
        SET_DEST(BUILDER().CreateNeg(LLVM_VALUE_FROM_USE(instr->value)));
        break;
    }
    case ir_opcode_not: {
        IR_INSTR_AS(unop)
        SET_DEST(BUILDER().CreateNot(LLVM_VALUE_FROM_USE(instr->value)));
        break;
    }
    case ir_opcode_add: { \
        IR_INSTR_AS(binop) \
        SET_DEST(BUILDER().CreateAdd(LLVM_VALUE_FROM_USE(instr->left), LLVM_VALUE_FROM_USE(instr->right))); \
        break; \
    }
    case ir_opcode_sub: {
        IR_INSTR_AS(binop)
        SET_DEST(BUILDER().CreateSub(LLVM_VALUE_FROM_USE(instr->left), LLVM_VALUE_FROM_USE(instr->right)));
        break;
    }
    case ir_opcode_mul: {
        IR_INSTR_AS(binop)
        SET_DEST(BUILDER().CreateMul(LLVM_VALUE_FROM_USE(instr->left), LLVM_VALUE_FROM_USE(instr->right)));
        break;
    }
    case ir_opcode_div: {
        IR_INSTR_AS(binop)
        ir_value left = IR_USE_VALUE(instr->left);
        ir_value right = IR_USE_VALUE(instr->right);
        if (ir_type_is_signed(ir_typeof(left))) {
            SET_DEST(BUILDER().CreateSDiv(LLVM_VALUE(left), LLVM_VALUE(right)));
        } else {
            SET_DEST(BUILDER().CreateUDiv(LLVM_VALUE(left), LLVM_VALUE(right)));
        }
        break;
    }
    case ir_opcode_rem: {
        IR_INSTR_AS(binop)
        ir_value left = IR_USE_VALUE(instr->left);
        ir_value right = IR_USE_VALUE(instr->right);
        if (ir_type_is_signed(ir_typeof(left))) {
            SET_DEST(BUILDER().CreateSRem(LLVM_VALUE(left), LLVM_VALUE(right)));
        } else {
            SET_DEST(BUILDER().CreateURem(LLVM_VALUE(left), LLVM_VALUE(right)));
        }
        break;
    }
    case ir_opcode_and: {
        IR_INSTR_AS(binop)
        SET_DEST(BUILDER().CreateAnd(LLVM_VALUE_FROM_USE(instr->left), LLVM_VALUE_FROM_USE(instr->right)));
        break;
    }
    case ir_opcode_or: {
        IR_INSTR_AS(binop)
        SET_DEST(BUILDER().CreateOr(LLVM_VALUE_FROM_USE(instr->left), LLVM_VALUE_FROM_USE(instr->right)));
        break;
    }
    case ir_opcode_xor: {
        IR_INSTR_AS(binop)
        SET_DEST(BUILDER().CreateXor(LLVM_VALUE_FROM_USE(instr->left), LLVM_VALUE_FROM_USE(instr->right)));
        break;
    }
    case ir_opcode_shl: {
        IR_INSTR_AS(binop)
        SET_DEST(BUILDER().CreateShl(LLVM_VALUE_FROM_USE(instr->left), LLVM_VALUE_FROM_USE(instr->right)));
        break;
    }
    case ir_opcode_shr: {
        IR_INSTR_AS(binop)
        ir_value left = IR_USE_VALUE(instr->left);
        ir_value right = IR_USE_VALUE(instr->right);
        if (ir_type_is_signed(ir_typeof(left))) {
            SET_DEST(BUILDER().CreateAShr(LLVM_VALUE(left), LLVM_VALUE(right)));
        } else {
            SET_DEST(BUILDER().CreateLShr(LLVM_VALUE(left), LLVM_VALUE(right)));
        }
        break;
    }
    case ir_opcode_notbool: {
        IR_INSTR_AS(boolean)
        ir_value value = IR_USE_VALUE(instr->value);
        llvm::Value *v = BUILDER().CreateICmpEQ(
            LLVM_VALUE(value),
            llvm::Constant::getNullValue(LLVM_TYPE(ir_typeof(value))));
        SET_DEST(BUILDER().CreateZExt(v, LLVM_TYPE(ir_type_int)));
        break;
    }
    case ir_opcode_bool: {
        IR_INSTR_AS(boolean)
        ir_value value = IR_USE_VALUE(instr->value);
        llvm::Value *v = BUILDER().CreateICmpNE(
            LLVM_VALUE(value),
            llvm::Constant::getNullValue(LLVM_TYPE(ir_typeof(value))));
        SET_DEST(BUILDER().CreateZExt(v, LLVM_TYPE(ir_type_int)));
        break;
    }
    case ir_opcode_lt: {
        IR_INSTR_AS(comparison)
        ir_value left = IR_USE_VALUE(instr->left);
        ir_value right = IR_USE_VALUE(instr->right);
        llvm::Value *v;
        if (ir_type_is_signed(ir_typeof(left))) {
            v = BUILDER().CreateICmpSLT(LLVM_VALUE(left), LLVM_VALUE(right));
        } else {
            v = BUILDER().CreateICmpULT(LLVM_VALUE(left), LLVM_VALUE(right));
        }
        SET_DEST(BUILDER().CreateZExt(v, LLVM_TYPE(ir_type_int)));
        break;
    }
    case ir_opcode_gt: {
        IR_INSTR_AS(comparison)
        ir_value left = IR_USE_VALUE(instr->left);
        ir_value right = IR_USE_VALUE(instr->right);
        llvm::Value *v;
        if (ir_type_is_signed(ir_typeof(left))) {
            v = BUILDER().CreateICmpSGT(LLVM_VALUE(left), LLVM_VALUE(right));
        } else {
            v = BUILDER().CreateICmpUGT(LLVM_VALUE(left), LLVM_VALUE(right));
        }
        SET_DEST(BUILDER().CreateZExt(v, LLVM_TYPE(ir_type_int)));
        break;
    }
    case ir_opcode_eq: {
        IR_INSTR_AS(comparison)
        llvm::Value *v;
        v = BUILDER().CreateICmpEQ(LLVM_VALUE_FROM_USE(instr->left), LLVM_VALUE_FROM_USE(instr->right));
        SET_DEST(BUILDER().CreateZExt(v, LLVM_TYPE(ir_type_int)));
        break;
    }
    case ir_opcode_ne: {
        IR_INSTR_AS(comparison)
        llvm::Value *v;
        v = BUILDER().CreateICmpNE(LLVM_VALUE_FROM_USE(instr->left), LLVM_VALUE_FROM_USE(instr->right));
        SET_DEST(BUILDER().CreateZExt(v, LLVM_TYPE(ir_type_int)));
        break;
    }
    case ir_opcode_le: {
        IR_INSTR_AS(comparison)
        ir_value left = IR_USE_VALUE(instr->left);
        ir_value right = IR_USE_VALUE(instr->right);
        llvm::Value *v;
        if (ir_type_is_signed(ir_typeof(left))) {
            v = BUILDER().CreateICmpSLE(LLVM_VALUE(left), LLVM_VALUE(right));
        } else {
            v = BUILDER().CreateICmpULE(LLVM_VALUE(left), LLVM_VALUE(right));
        }
        SET_DEST(BUILDER().CreateZExt(v, LLVM_TYPE(ir_type_int)));
        break;
    }
    case ir_opcode_ge: {
        IR_INSTR_AS(comparison)
        ir_value left = IR_USE_VALUE(instr->left);
        ir_value right = IR_USE_VALUE(instr->right);
        llvm::Value *v;
        if (ir_type_is_signed(ir_typeof(left))) {
            v = BUILDER().CreateICmpSGE(LLVM_VALUE(left), LLVM_VALUE(right));
        } else {
            v = BUILDER().CreateICmpUGE(LLVM_VALUE(left), LLVM_VALUE(right));
        }
        SET_DEST(BUILDER().CreateZExt(v, LLVM_TYPE(ir_type_int)));
        break;
    }
    case ir_opcode_ternary: {
        IR_INSTR_AS(ternary)
        llvm::Value *cond = BUILDER().CreateICmpEQ(
            LLVM_VALUE_FROM_USE(instr->cond),
            llvm::Constant::getNullValue(LLVM_TYPE(ir_typeof(IR_USE_VALUE(instr->cond)))));
        SET_DEST(BUILDER().CreateSelect(cond, LLVM_VALUE_FROM_USE(instr->if_false), LLVM_VALUE_FROM_USE(instr->if_true)));
        break;
    }
    case ir_opcode_call: {
        IR_INSTR_AS(call)
        size_t num_args = instr->arg_count;
        llvm::Value** args = (llvm::Value**)malloc(num_args * sizeof(llvm::Value*));
        size_t i;
        for (i = 0; i < num_args; i++) {
            args[i] = LLVM_VALUE_FROM_USE(instr->arg[i]);
        }
        llvm::Value *ret =
            BUILDER().CreateCall(LLVM_VALUE_FROM_USE(instr->target), llvm::ArrayRef<llvm::Value*>(args, num_args));
        free(args);
        if (ir_typeof(IR_INSTR_DEST(_instr)) != ir_type_void) {
            SET_DEST(ret);
        }
        break;
    }
    case ir_opcode_patchpoint: {
        IR_INSTR_AS(patchpoint)
        /* Signature:
           void|i64 @llvm.experimental.patchpoint.void|i64(i64 <id>,
                                                           i32 <numBytes>,
                                                           i8* <target>,
                                                           i32 <numArgs>,
                                                           [Args...],
                                                           [live variables...])
         */
        size_t niargs = 4 + instr->arg_count;
        llvm::Value** iargs = (llvm::Value**)malloc(niargs * sizeof(llvm::Value));
        iargs[0] = BUILDER().getInt64((uintptr_t)instr->user_data); /* stuff the user_data into the id */
        iargs[1] = BUILDER().getInt32(16); /* numBytes in shadow region */
        iargs[2] = BUILDER().CreateBitCast(LLVM_VALUE_FROM_USE(instr->target), BUILDER().getInt8PtrTy());
        iargs[3] = BUILDER().getInt32(instr->real_arg_count);
        size_t i;
        for (i = 0; i < instr->arg_count; i++) {
            iargs[4 + i] = LLVM_VALUE_FROM_USE(instr->arg[i]);
        }
        assert(4 + i == niargs);
        llvm::Function *f = llvm::Intrinsic::getDeclaration(ctx.module, llvm::Intrinsic::experimental_patchpoint_void);
        BUILDER().CreateCall(f, llvm::ArrayRef<llvm::Value*>(iargs, niargs));
        break;
    }
    case ir_opcode_get_element_ptr: {
        IR_INSTR_AS(get_element_ptr)
        llvm::Value *casted_ptr = BUILDER().CreateBitCast(LLVM_VALUE_FROM_USE(instr->ptr), BUILDER().getInt8PtrTy());
        llvm::Value *offset_ptr = BUILDER().CreateGEP(casted_ptr, BUILDER().getInt64(instr->offset));
        ir_type dest_type = ir_typeof(IR_INSTR_DEST(_instr));
        llvm::Value *new_ptr = BUILDER().CreateBitCast(offset_ptr, LLVM_TYPE(dest_type));
        SET_DEST(new_ptr);
        break;
    }
    case ir_opcode_get_index_ptr: {
        IR_INSTR_AS(get_index_ptr)
        ir_type base_type = ir_pointer_base(ir_typeof(IR_USE_VALUE(instr->ptr)));
        ir_type dest_type = ir_typeof(IR_INSTR_DEST(_instr));
        if (base_type->kind == ir_type_kind_struct) {
            llvm::Value *casted_ptr = BUILDER().CreateBitCast(LLVM_VALUE_FROM_USE(instr->ptr), BUILDER().getInt8PtrTy());
            llvm::Value *offset = BUILDER().CreateMul(
                BUILDER().CreateZExt(LLVM_VALUE_FROM_USE(instr->index), BUILDER().getInt64Ty()),
                BUILDER().getInt64(base_type->size));
            llvm::Value *offset_ptr = BUILDER().CreateGEP(casted_ptr, offset);
            llvm::Value *new_ptr = BUILDER().CreateBitCast(offset_ptr, LLVM_TYPE(dest_type));
            SET_DEST(new_ptr);
        } else {
            SET_DEST(BUILDER().CreateGEP(LLVM_VALUE_FROM_USE(instr->ptr), LLVM_VALUE_FROM_USE(instr->index)));
        }
        break;
    }
    case ir_opcode_load: {
        IR_INSTR_AS(load)
        SET_DEST(BUILDER().CreateLoad(LLVM_VALUE_FROM_USE(instr->ptr)));
        break;
    }
    case ir_opcode_store: {
        IR_INSTR_AS(store)
        BUILDER().CreateStore(LLVM_VALUE_FROM_USE(instr->value), LLVM_VALUE_FROM_USE(instr->ptr));
        break;
    }
    case ir_opcode_alloca: {
        IR_INSTR_AS(alloca)
        ir_type elem_type = ir_pointer_base(ir_typeof(IR_INSTR_DEST(_instr)));
        assert(elem_type->size > 0);
        llvm::Value *alloca_size =
                BUILDER().getInt64(instr->num_elements * elem_type->size);
        SET_DEST(BUILDER().CreateAlloca(LLVM_TYPE(elem_type), alloca_size));
        break;
    }
    case ir_opcode_constant: {
        IR_INSTR_AS(constant)
        ir_type dest_type = ir_typeof(IR_INSTR_DEST(_instr));
        llvm::Value *ret;
        switch (dest_type->kind) {
#define PROCESS(name, ctype, ps, _id, va_arg_type) \
        case ir_type_kind_ ## name: \
            ret = BUILDER().getIntN(dest_type->size * 8, instr->imm . _id); \
            break;
#include "ir_integral_types.def"
#undef PROCESS
        case ir_type_kind_pointer:
        case ir_type_kind_function: {
            llvm::Value *ptr_as_int = BUILDER().getIntN(sizeof(void*) * 8, (uint64_t)instr->imm.ptr);
            ret = BUILDER().CreateIntToPtr(ptr_as_int, LLVM_TYPE(dest_type));
            break;
        }
        default:
            abort(); /* Unhandled type */
        }
        SET_DEST(ret);
        break;
    }
    case ir_opcode_cast: {
        IR_INSTR_AS(cast)
        ir_type src_type = ir_typeof(IR_USE_VALUE(instr->value));
        ir_type dest_type = ir_typeof(IR_INSTR_DEST(_instr));
        assert(!ir_type_is_void(src_type));
        assert(!ir_type_is_void(dest_type));
        assert(!ir_type_is_struct(src_type));
        assert(!ir_type_is_struct(dest_type));
        assert(src_type->size != 0);
        assert(dest_type->size != 0);
        llvm::Value *dest;

        if (src_type->size == dest_type->size) {
            int src_is_pointer = ir_type_is_pointer(src_type) || ir_type_is_function(src_type);
            int src_is_integral = ir_type_is_integral(src_type);
            int dest_is_pointer = ir_type_is_pointer(dest_type) || ir_type_is_function(dest_type);
            int dest_is_integral = ir_type_is_integral(dest_type);

            if (src_is_integral && dest_is_integral) {
                /* No casting needed. We collapse integer types to their width for LLVM. */
                dest = LLVM_VALUE_FROM_USE(instr->value);
            } else if (src_is_pointer && dest_is_pointer) {
                dest = BUILDER().CreateBitCast(LLVM_VALUE_FROM_USE(instr->value), LLVM_TYPE(dest_type));
            } else if (src_is_integral && dest_is_pointer) {
                dest = BUILDER().CreateIntToPtr(LLVM_VALUE_FROM_USE(instr->value), LLVM_TYPE(dest_type));
            } else if (src_is_pointer && dest_is_integral) {
                dest = BUILDER().CreatePtrToInt(LLVM_VALUE_FROM_USE(instr->value), LLVM_TYPE(dest_type));
            } else {
                abort(); /* Unhandled case */
            }
        } else if (ir_type_is_integral(src_type) && ir_type_is_integral(dest_type)) {
            /* Integer casting */
            if (src_type->size > dest_type->size) {
                /* Truncation */
                dest = BUILDER().CreateTrunc(LLVM_VALUE_FROM_USE(instr->value), LLVM_TYPE(dest_type));
            } else {
                /* Zero-Extension or Sign-extension */
                if (ir_type_is_signed(src_type)) {
                    dest = BUILDER().CreateSExt(LLVM_VALUE_FROM_USE(instr->value), LLVM_TYPE(dest_type));
                } else {
                    dest = BUILDER().CreateZExt(LLVM_VALUE_FROM_USE(instr->value), LLVM_TYPE(dest_type));
                }
            }
        }
        SET_DEST(dest);
        break;
    }
    case ir_opcode_func_arg: {
        IR_INSTR_AS(func_arg)
        assert(instr->index - 1 < ctx.llvm_func->arg_size());
        SET_DEST(ctx.llvm_func->arg_begin() + (instr->index - 1));
        break;
    }
    case ir_opcode_label_here: {
        break;
    }
    case ir_opcode_info_here: {
        break;
    }
    case ir_opcode_branch: {
        IR_INSTR_AS(branch)
        BUILDER().CreateBr(LLVM_BLOCK(instr->target));
        break;
    }
    case ir_opcode_branch_cond: {
        IR_INSTR_AS(branch_cond)
        llvm::Value *cond =
            BUILDER().CreateICmpNE(
                LLVM_VALUE_FROM_USE(instr->cond),
                llvm::Constant::getNullValue(LLVM_TYPE(ir_typeof(IR_USE_VALUE(instr->cond)))));
        llvm::MDNode* branchWeights =
            llvm::MDBuilder(context).createBranchWeights(instr->if_true_weight, instr->if_false_weight);
        BUILDER().CreateCondBr(
            cond,
            LLVM_BLOCK(instr->if_true),
            LLVM_BLOCK(instr->if_false),
            branchWeights);
        break;
    }
    case ir_opcode_jumptable: {
        IR_INSTR_AS(jumptable)

        auto ip = BUILDER().saveIP();
        llvm::BasicBlock* error_block = llvm::BasicBlock::Create(context, "error_block", ctx.llvm_func);
        BUILDER().SetInsertPoint(error_block);
        BUILDER().CreateUnreachable();
        BUILDER().restoreIP(ip);


        llvm::SwitchInst *sw = BUILDER().CreateSwitch(
            LLVM_VALUE_FROM_USE(instr->index),
            error_block,
            instr->table_size);
        for (size_t i = 0; i < instr->table_size; i++) {
            sw->addCase(BUILDER().getInt32(i), LLVM_BLOCK(instr->table[i]));
        }
        break;
    }
    case ir_opcode_ret: {
        IR_INSTR_AS(ret)
        if (ir_typeof(IR_USE_VALUE(instr->value)) != ir_type_void) {
            BUILDER().CreateRet(LLVM_VALUE_FROM_USE(instr->value));
        } else {
            BUILDER().CreateRetVoid();
        }
        break;
    }
    default:
        abort(); // Unhandled ir instruction
    } // switch
}

void dump_llvm_error(llvm::Error E) {
    llvm::handleAllErrors(std::move(E), [&](const llvm::ErrorInfoBase &EI) {
         llvm::errs() << "Fatal Error: ";
         EI.log(llvm::errs());
         llvm::errs() << "\n";
         llvm::errs().flush();
    });
}

ir_stackmap_index parse_stackmap_section(void *stackmap_addr);

class LLVMState {
  public:
    llvm::LLVMContext context;
    llvm::TargetMachine* targetMachine;
    std::unique_ptr<SimpleOrcJit> jit;
    size_t func_counter;

    LLVMState()
        : context(),
          targetMachine(llvm::EngineBuilder().selectTarget()),
          jit(new SimpleOrcJit(*targetMachine)),
          func_counter(0) { }
};

ir_object
ir_llvm_compile(ir_func func) {
    static LLVMState *state = NULL;
    if (state == NULL) {
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();
        llvm::InitializeNativeTargetAsmParser();
        state = new LLVMState();
        state->targetMachine->setOptLevel(llvm::CodeGenOpt::Aggressive);
    }
    _TranslationContext ctx;

    auto &context = state->context;
    std::unique_ptr<llvm::Module> module_up(new llvm::Module("my_module", context));
    ctx.module = module_up.get();
    ctx.module->setDataLayout(state->targetMachine->createDataLayout());

    llvm::Type *llvm_sig = llvm::cast<llvm::PointerType>(LLVM_TYPE(func->sig))->getElementType();
    char llvm_func_name[64];
    sprintf(llvm_func_name, "__llvm_func_%zu", state->func_counter++);

    ctx.llvm_func = llvm::Function::Create(
        llvm::cast<llvm::FunctionType>(llvm_sig),
        llvm::Function::ExternalLinkage,
        llvm_func_name,
        ctx.module);
    ctx.entry_bb = llvm::BasicBlock::Create(context, "entry", ctx.llvm_func);
    llvm::IRBuilder<> builder(ctx.entry_bb);
    ctx.builder = &builder;

    ctx.num_values = ir_func_next_value_index(func);
    ctx.num_blocks = ir_func_next_block_index(func);

    ctx.llvm_values = (llvm::Value**)malloc(ctx.num_values * sizeof(llvm::Value*));
    memset(ctx.llvm_values, 0, ctx.num_values * sizeof(llvm::Value*));

    ctx.llvm_modes = (char*)malloc(ctx.num_values * sizeof(char));
    memset(ctx.llvm_modes, 0, ctx.num_values * sizeof(char));

    ctx.llvm_blocks = (llvm::BasicBlock**)malloc(ctx.num_blocks * sizeof(llvm::BasicBlock*));
    memset(ctx.llvm_blocks, 0, ctx.num_blocks * sizeof(llvm::BasicBlock*));

    /* Create the LLVM blocks */
    ir_block b;
    ir_instr _instr;
    for (b = func->first_block; b != NULL; b = b->next) {
        if (b == func->first_block) {
            ctx.llvm_blocks[b->index] = ctx.entry_bb;
        } else {
            char namebuf[64];
            sprintf(namebuf, "block_%zu", b->index);
            ctx.llvm_blocks[b->index] = llvm::BasicBlock::Create(context, namebuf, ctx.llvm_func);
        }

        /* Do a first pass through the code to determine which values must be
           stored using 'alloca', and which ones can be bare SSA values.

           Everything defaults to a bare value except those values which are used
             before they are defined (in the block/instruction iteration order)
         */
        for (_instr = b->first_instr; _instr != NULL; _instr = _instr->next) {
            size_t count;
            ir_use uses = ir_get_uses(_instr, &count);
            for (int i = 0; i < count; i++) {
                ALLOCA_MODE_IF_NOT_DEFINED(IR_USE_VALUE(uses[i]));
            }
            if (ir_typeof(IR_INSTR_DEST(_instr)) != ir_type_void) {
                RECORD_DEFINE(IR_INSTR_DEST(_instr));
            }

        }
    }

    /* Translate instructions, block by block */
    for (b = func->first_block; b != NULL; b = b->next) {
        assert(b->index < ctx.num_blocks);
        BUILDER().SetInsertPoint(ctx.llvm_blocks[b->index]);
        for (_instr = b->first_instr; _instr != NULL; _instr = _instr->next) {
            _emit_instr(ctx, b, _instr);
        }
    }
    free(ctx.llvm_blocks);
    free(ctx.llvm_values);
    free(ctx.llvm_modes);

#ifdef Py_DEBUG
    if (llvm::verifyFunction(*ctx.llvm_func, &llvm::errs())) {
        abort();
    }
#endif

    state->jit->submitModule(std::move(module_up));
    llvm::JITSymbol sym = state->jit->findSymbolInJITedCode(state->jit->mangle(llvm_func_name));
    if (!sym) {
        std::cout << "Unable to find symbol in LLVM JIT" << std::endl;
        return nullptr;
    }
    llvm::Expected<llvm::JITTargetAddress> addr = sym.getAddress();
    if (!addr) {
        dump_llvm_error(addr.takeError());
        return nullptr;
    }
    /* Load stackmaps */
    void *stackmap_addr = state->jit->getStackmapSectionAddress();
    ir_stackmap_index stackmap_index = NULL;
    if (stackmap_addr) {
        stackmap_index = parse_stackmap_section(stackmap_addr);
    }
    ir_object ret = ir_object_new();
    ret->compiler_data = nullptr;
    ret->compiler_free_callback = nullptr;
    ret->entrypoint = reinterpret_cast<void*>(*addr);
    ret->stackmap_index = stackmap_index;
    return ret;
}

/* These structures mirror the format described by:
       https://llvm.org/docs/StackMaps.html#stack-map-format
 */

struct StackMapHeader {
    uint8_t version;
    uint8_t reserved1;
    uint16_t reserved2;

    uint32_t num_functions;
    uint32_t num_constants;
    uint32_t num_records;
};

struct StkSizeRecord {
    uint64_t function_address;
    uint64_t stack_size;
    uint64_t record_count;
};

enum LocationKind : uint8_t {
    LocationKindRegister      = 0x1, /* value in register */
    LocationKindDirect        = 0x2, /* value is register + offset */
    LocationKindIndirect      = 0x3, /* value is spilled (on stack) */
    LocationKindConstant      = 0x4, /* value is small constant (stored in offset) */
    LocationKindConstantIndex = 0x5, /* value is large constants (offset is index) */
};

struct Location {
    LocationKind kind;
    uint8_t reserved1;
    uint16_t location_size;
    uint16_t dwarf_regnum;
    uint16_t reserved2;
    int32_t offset_or_small_constant;
};

struct StkMapRecordHeader {
    uint64_t patchpoint_id;
    uint32_t instruction_offset;
    uint16_t reserved;
    uint16_t num_locations;
};

struct StkMapRecordFooter {
    uint16_t padding;
    uint16_t num_live_outs;
};

struct LiveOuts {
    uint16_t dwarf_regnum;
    uint8_t reserved;
    uint8_t size_in_bytes;
};

static inline
void _location_to_stackmap_value(ir_stackmap_value dest, Location *loc, size_t num_constants, uint64_t *constants) {
    switch (loc->kind) {
    case LocationKindRegister:
        dest->location = IR_LOCATION_REGISTER;
        dest->dwarf_index = loc->dwarf_regnum;
        dest->offset = 0;
        break;
    case LocationKindDirect:
        dest->location = IR_LOCATION_REGISTER_PLUS_OFFSET;
        dest->dwarf_index = loc->dwarf_regnum;
        dest->offset = loc->offset_or_small_constant;
        break;
    case LocationKindIndirect:
        dest->location = IR_LOCATION_MEMORY;
        dest->dwarf_index = loc->dwarf_regnum;
        dest->offset = loc->offset_or_small_constant;
        break;
    case LocationKindConstant:
        dest->location = IR_LOCATION_CONSTANT;
        dest->dwarf_index = loc->dwarf_regnum;
        dest->offset = loc->offset_or_small_constant;
        break;
    case LocationKindConstantIndex:
        dest->location = IR_LOCATION_CONSTANT;
        dest->dwarf_index = loc->dwarf_regnum;
        assert(loc->offset_or_small_constant >= 0 &&
               loc->offset_or_small_constant < num_constants);
        dest->offset = constants[loc->offset_or_small_constant];
        break;
    default:
        Py_FatalError("Unhandled location value in LLVM stackmap section");
        break;
    }
}

ir_stackmap_index parse_stackmap_section(void *stackmap_addr) {
    char *cursor = (char*)stackmap_addr;
    StackMapHeader *header = reinterpret_cast<StackMapHeader*>(cursor);
    cursor += sizeof(StackMapHeader);
    if (header->version != 3) {
        Py_FatalError("Incorrect LLVM stackmap version");
    }
    auto num_functions = header->num_functions;
    auto num_constants = header->num_constants;
    auto num_records = header->num_records;
    StkSizeRecord *functions = reinterpret_cast<StkSizeRecord*>(cursor);
    cursor += num_functions * sizeof(StkSizeRecord);
    uint64_t *constants = reinterpret_cast<uint64_t*>(cursor);
    cursor += num_constants * sizeof(uint64_t);

    size_t total_records = 0;
    ir_stackmap_index index = ir_stackmap_index_new(num_records);
    for (size_t i = 0; i < num_functions; i++) {
        StkSizeRecord *func = &functions[i];
        for (size_t j = 0; j < func->record_count; j++) {
            StkMapRecordHeader *record = reinterpret_cast<StkMapRecordHeader*>(cursor);
            cursor += sizeof(StkMapRecordHeader);
            total_records++;
            size_t num_locations = record->num_locations;
            ir_stackmap_info info = ir_stackmap_info_new(num_locations);
            info->code_address = reinterpret_cast<void*>(func->function_address + record->instruction_offset);
            info->user_data = reinterpret_cast<void*>(record->patchpoint_id);
            /* Hacky: Set the first word of the user data to point to the stackmap info */
            *(reinterpret_cast<ir_stackmap_info*>(info->user_data)) = info;
            info->num_values = num_locations;
            for (size_t k = 0; k < num_locations; k++) {
                Location *loc = reinterpret_cast<Location*>(cursor);
                cursor += sizeof(Location);
                _location_to_stackmap_value(&info->values[k], loc, num_constants, constants);
            }

            /* Align up to 8 bytes */
            cursor = (char*)_Py_ALIGN_UP(cursor, 8);

            StkMapRecordFooter *footer = reinterpret_cast<StkMapRecordFooter*>(cursor);
            cursor += sizeof(StkMapRecordFooter);

            /* Skip over live outs */
            cursor += sizeof(LiveOuts) * footer->num_live_outs;

            /* Align up to 8 bytes */
            cursor = (char*)_Py_ALIGN_UP(cursor, 8);

            ir_stackmap_index_add(index, info);
        }
    }
    assert(total_records == num_records);
    return index;
}
