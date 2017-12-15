#include "Python.h"
#include "ir.h"

#include <iostream>
#include <fstream>
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "SimpleOrcJit.h"

#define LLVM_BLOCK(label)  llvm_blocks[(label)->block->index]

#define LLVM_TYPE(irtype)  _ir_type_to_llvm_type(module, (irtype))

static inline
llvm::Type *
_ir_type_to_llvm_type(llvm::Module* module, ir_type type) {
    auto &context = module->getContext();
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
        llvm::Type *ret = module->getTypeByName(type->name);
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
#define LLVM_VALUE_ADDRESS(_v) llvm_values[(_v)->index]
#define LLVM_VALUE(__v)  builder.CreateLoad(LLVM_VALUE_ADDRESS((__v)))

/* Given an llvm::Value*, use it as the 'dest' value for the current instruction */
#define SET_DEST(_v) do { \
    builder.CreateStore((_v), LLVM_VALUE_ADDRESS(_instr->dest)); \
} while (0)

static inline
void _emit_instr(
        llvm::Module *module,
        llvm::Function *llvm_func,
        llvm::Value** llvm_values,
        size_t num_values,
        llvm::BasicBlock** llvm_blocks,
        size_t num_blocks,
        llvm::IRBuilder<> &builder,
        ir_block b,
        ir_instr _instr) {
    llvm::LLVMContext &context = llvm_func->getContext();
    switch (_instr->opcode) {
    case ir_opcode_neg: {
        IR_INSTR_AS(unop)
        SET_DEST(builder.CreateNeg(LLVM_VALUE(instr->value)));
        break;
    }
    case ir_opcode_not: {
        IR_INSTR_AS(unop)
        SET_DEST(builder.CreateNot(LLVM_VALUE(instr->value)));
        break;
    }
    case ir_opcode_add: { \
        IR_INSTR_AS(binop) \
        SET_DEST(builder.CreateAdd(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right))); \
        break; \
    }
    case ir_opcode_sub: {
        IR_INSTR_AS(binop)
        SET_DEST(builder.CreateSub(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right)));
        break;
    }
    case ir_opcode_mul: {
        IR_INSTR_AS(binop)
        SET_DEST(builder.CreateMul(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right)));
        break;
    }
    case ir_opcode_div: {
        IR_INSTR_AS(binop)
        if (ir_type_is_signed(ir_typeof(instr->left))) {
            SET_DEST(builder.CreateSDiv(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right)));
        } else {
            SET_DEST(builder.CreateUDiv(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right)));
        }
        break;
    }
    case ir_opcode_rem: {
        IR_INSTR_AS(binop)
        if (ir_type_is_signed(ir_typeof(instr->left))) {
            SET_DEST(builder.CreateSRem(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right)));
        } else {
            SET_DEST(builder.CreateURem(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right)));
        }
        break;
    }
    case ir_opcode_and: {
        IR_INSTR_AS(binop)
        SET_DEST(builder.CreateAnd(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right)));
        break;
    }
    case ir_opcode_or: {
        IR_INSTR_AS(binop)
        SET_DEST(builder.CreateOr(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right)));
        break;
    }
    case ir_opcode_xor: {
        IR_INSTR_AS(binop)
        SET_DEST(builder.CreateXor(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right)));
        break;
    }
    case ir_opcode_shl: {
        IR_INSTR_AS(binop)
        SET_DEST(builder.CreateShl(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right)));
        break;
    }
    case ir_opcode_shr: {
        IR_INSTR_AS(binop)
        if (ir_type_is_signed(ir_typeof(instr->left))) {
            SET_DEST(builder.CreateAShr(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right)));
        } else {
            SET_DEST(builder.CreateLShr(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right)));
        }
        break;
    }
    case ir_opcode_notbool: {
        IR_INSTR_AS(boolean)
        llvm::Value *v = builder.CreateICmpEQ(
            LLVM_VALUE(instr->value),
            llvm::Constant::getNullValue(LLVM_TYPE(ir_typeof(instr->value))));
        SET_DEST(builder.CreateZExt(v, LLVM_TYPE(ir_type_int)));
        break;
    }
    case ir_opcode_bool: {
        IR_INSTR_AS(boolean)
        llvm::Value *v = builder.CreateICmpNE(
            LLVM_VALUE(instr->value),
            llvm::Constant::getNullValue(LLVM_TYPE(ir_typeof(instr->value))));
        SET_DEST(builder.CreateZExt(v, LLVM_TYPE(ir_type_int)));
        break;
    }
    case ir_opcode_lt: {
        IR_INSTR_AS(comparison)
        llvm::Value *v;
        if (ir_type_is_signed(ir_typeof(instr->left))) {
            v = builder.CreateICmpSLT(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right));
        } else {
            v = builder.CreateICmpULT(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right));
        }
        SET_DEST(builder.CreateZExt(v, LLVM_TYPE(ir_type_int)));
        break;
    }
    case ir_opcode_gt: {
        IR_INSTR_AS(comparison)
        llvm::Value *v;
        if (ir_type_is_signed(ir_typeof(instr->left))) {
            v = builder.CreateICmpSGT(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right));
        } else {
            v = builder.CreateICmpUGT(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right));
        }
        SET_DEST(builder.CreateZExt(v, LLVM_TYPE(ir_type_int)));
        break;
    }
    case ir_opcode_eq: {
        IR_INSTR_AS(comparison)
        llvm::Value *v;
        v = builder.CreateICmpEQ(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right));
        SET_DEST(builder.CreateZExt(v, LLVM_TYPE(ir_type_int)));
        break;
    }
    case ir_opcode_ne: {
        IR_INSTR_AS(comparison)
        llvm::Value *v;
        v = builder.CreateICmpNE(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right));
        SET_DEST(builder.CreateZExt(v, LLVM_TYPE(ir_type_int)));
        break;
    }
    case ir_opcode_le: {
        IR_INSTR_AS(comparison)
        llvm::Value *v;
        if (ir_type_is_signed(ir_typeof(instr->left))) {
            v = builder.CreateICmpSLE(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right));
        } else {
            v = builder.CreateICmpULE(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right));
        }
        SET_DEST(builder.CreateZExt(v, LLVM_TYPE(ir_type_int)));
        break;
    }
    case ir_opcode_ge: {
        IR_INSTR_AS(comparison)
        llvm::Value *v;
        if (ir_type_is_signed(ir_typeof(instr->left))) {
            v = builder.CreateICmpSGE(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right));
        } else {
            v = builder.CreateICmpUGE(LLVM_VALUE(instr->left), LLVM_VALUE(instr->right));
        }
        SET_DEST(builder.CreateZExt(v, LLVM_TYPE(ir_type_int)));
        break;
    }
    case ir_opcode_ternary: {
        IR_INSTR_AS(ternary)
        llvm::Value *cond = builder.CreateICmpEQ(
            LLVM_VALUE(instr->cond),
            llvm::Constant::getNullValue(LLVM_TYPE(ir_typeof(instr->cond))));
        SET_DEST(builder.CreateSelect(cond, LLVM_VALUE(instr->if_false), LLVM_VALUE(instr->if_true)));
        break;
    }
    case ir_opcode_call: {
        IR_INSTR_AS(call)
        size_t num_args = instr->arg_count;
        llvm::Value** args = (llvm::Value**)malloc(num_args * sizeof(llvm::Value*));
        size_t i;
        for (i = 0; i < num_args; i++) {
            args[i] = LLVM_VALUE(instr->arg[i]);
        }
        llvm::Value *ret =
            builder.CreateCall(LLVM_VALUE(instr->target), llvm::ArrayRef<llvm::Value*>(args, num_args));
        free(args);
        if (_instr->dest != NULL) {
            SET_DEST(ret);
        }
        break;
    }
    case ir_opcode_get_element_ptr: {
        IR_INSTR_AS(get_element_ptr)
        llvm::Value *casted_ptr = builder.CreateBitCast(LLVM_VALUE(instr->ptr), builder.getInt8PtrTy());
        llvm::Value *offset_ptr = builder.CreateGEP(casted_ptr, builder.getInt64(instr->offset));
        llvm::Value *new_ptr = builder.CreateBitCast(offset_ptr, LLVM_TYPE(ir_typeof(_instr->dest)));
        SET_DEST(new_ptr);
        break;
    }
    case ir_opcode_get_index_ptr: {
        IR_INSTR_AS(get_index_ptr)
        SET_DEST(builder.CreateGEP(LLVM_VALUE(instr->ptr), LLVM_VALUE(instr->index)));
        break;
    }
    case ir_opcode_load: {
        IR_INSTR_AS(load)
        SET_DEST(builder.CreateLoad(LLVM_VALUE(instr->ptr)));
        break;
    }
    case ir_opcode_store: {
        IR_INSTR_AS(store)
        builder.CreateStore(LLVM_VALUE(instr->value), LLVM_VALUE(instr->ptr));
        break;
    }
    case ir_opcode_address_of: {
        IR_INSTR_AS(address_of)
        SET_DEST(LLVM_VALUE_ADDRESS(instr->value));
        break;
    }
    case ir_opcode_constant: {
        IR_INSTR_AS(constant)
        ir_type dest_type = ir_typeof(_instr->dest);
        llvm::Value *ret;
        switch (dest_type->kind) {
#define PROCESS(name, ctype, ps, _id, va_arg_type) \
        case ir_type_kind_ ## name: \
            ret = builder.getIntN(dest_type->size * 8, instr->imm . _id); \
            break;
#include "ir_integral_types.def"
#undef PROCESS
        case ir_type_kind_pointer:
        case ir_type_kind_function: {
            llvm::Value *ptr_as_int = builder.getIntN(sizeof(void*) * 8, (uint64_t)instr->imm.ptr);
            ret = builder.CreateIntToPtr(ptr_as_int, LLVM_TYPE(dest_type));
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
        ir_type src_type = ir_typeof(instr->value);
        ir_type dest_type = ir_typeof(_instr->dest);
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
                dest = LLVM_VALUE(instr->value);
            } else if (src_is_pointer && dest_is_pointer) {
                dest = builder.CreateBitCast(LLVM_VALUE(instr->value), LLVM_TYPE(dest_type));
            } else if (src_is_integral && dest_is_pointer) {
                dest = builder.CreateIntToPtr(LLVM_VALUE(instr->value), LLVM_TYPE(dest_type));
            } else if (src_is_pointer && dest_is_integral) {
                dest = builder.CreatePtrToInt(LLVM_VALUE(instr->value), LLVM_TYPE(dest_type));
            } else {
                abort(); /* Unhandled case */
            }
        } else if (ir_type_is_integral(src_type) && ir_type_is_integral(dest_type)) {
            /* Integer casting */
            if (src_type->size > dest_type->size) {
                /* Truncation */
                dest = builder.CreateTrunc(LLVM_VALUE(instr->value), LLVM_TYPE(dest_type));
            } else {
                /* Zero-Extension or Sign-extension */
                if (ir_type_is_signed(src_type)) {
                    dest = builder.CreateSExt(LLVM_VALUE(instr->value), LLVM_TYPE(dest_type));
                } else {
                    dest = builder.CreateZExt(LLVM_VALUE(instr->value), LLVM_TYPE(dest_type));
                }
            }
        }
        SET_DEST(dest);
        break;
    }
    case ir_opcode_set_value: {
        IR_INSTR_AS(set_value)
        SET_DEST(LLVM_VALUE(instr->src));
        break;
    }
    case ir_opcode_label_here: {
        break;
    }
    case ir_opcode_branch: {
        IR_INSTR_AS(branch)
        builder.CreateBr(LLVM_BLOCK(instr->target));
        break;
    }
    case ir_opcode_branch_cond: {
        IR_INSTR_AS(branch_cond)
        llvm::Value *cond =
            builder.CreateICmpEQ(
                LLVM_VALUE(instr->cond),
                llvm::Constant::getNullValue(LLVM_TYPE(ir_typeof(instr->cond))));
        builder.CreateCondBr(cond, LLVM_BLOCK(instr->if_false), LLVM_BLOCK(instr->if_true));
        break;
    }
    case ir_opcode_jumptable: {
        IR_INSTR_AS(jumptable)

        auto ip = builder.saveIP();
        llvm::BasicBlock* error_block = llvm::BasicBlock::Create(context, "error_block", llvm_func);
        builder.SetInsertPoint(error_block);
        builder.CreateUnreachable();
        builder.restoreIP(ip);


        llvm::SwitchInst *sw = builder.CreateSwitch(
            LLVM_VALUE(instr->index),
            error_block,
            instr->table_size);
        for (size_t i = 0; i < instr->table_size; i++) {
            sw->addCase(builder.getInt32(i), LLVM_BLOCK(instr->table[i]));
        }
        break;
    }
    case ir_opcode_ret: {
        IR_INSTR_AS(ret)
        if (instr->value) {
            builder.CreateRet(LLVM_VALUE(instr->value));
        } else {
            builder.CreateRetVoid();
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

void *
ir_llvm_compile(ir_func func) {
    static LLVMState *state = NULL;
    if (state == NULL) {
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();
        llvm::InitializeNativeTargetAsmParser();
        state = new LLVMState();
    }
    auto &context = state->context;
    std::unique_ptr<llvm::Module> module_up(new llvm::Module("my_module", context));
    llvm::Module *module = module_up.get();
    module->setDataLayout(state->targetMachine->createDataLayout());

    llvm::Type *llvm_sig = llvm::cast<llvm::PointerType>(LLVM_TYPE(func->sig))->getElementType();
    char llvm_func_name[64];
    sprintf(llvm_func_name, "__llvm_func_%zu", state->func_counter++);

    llvm::Function* llvm_func = llvm::Function::Create(
        llvm::cast<llvm::FunctionType>(llvm_sig),
        llvm::Function::ExternalLinkage,
        llvm_func_name,
        module);
    llvm::BasicBlock* entry_bb = llvm::BasicBlock::Create(context, "entry", llvm_func);
    llvm::IRBuilder<> builder(entry_bb);

    size_t num_values = ir_func_largest_value_index(func);
    size_t num_blocks = ir_func_largest_block_index(func);

    llvm::Value**      llvm_values = (llvm::Value**)malloc(num_values * sizeof(llvm::Value*));
    memset(llvm_values, 0, num_values * sizeof(llvm::Value*));

    /* By convention, the first values correspond to the function arguments */
    llvm::Function::arg_iterator llvm_args = llvm_func->arg_begin();
    for (size_t i = 0; i < func->sig->param_count - 1; i++) {
        llvm_values[i] = builder.CreateAlloca(LLVM_TYPE(func->sig->param[i+1]));
        builder.CreateStore(llvm_args++, llvm_values[i]);
    }

    llvm::BasicBlock** llvm_blocks = (llvm::BasicBlock**)malloc(num_blocks * sizeof(llvm::BasicBlock*));
    memset(llvm_blocks, 0, num_blocks * sizeof(llvm::BasicBlock*));

    /* Create the LLVM blocks */
    ir_block b;
    ir_instr instr;
    for (b = func->first_block; b != NULL; b = b->next) {
        if (b == func->first_block) {
            llvm_blocks[b->index] = entry_bb;
        } else {
            char namebuf[64];
            sprintf(namebuf, "block_%zu", b->index);
            llvm_blocks[b->index] = llvm::BasicBlock::Create(context, namebuf, llvm_func);
        }

        /* Allocate all values up front */
        for (instr = b->first_instr; instr != NULL; instr = instr->next) {
            if (instr->dest && llvm_values[instr->dest->index] == NULL) {
                llvm::Type *ltype = LLVM_TYPE(ir_typeof(instr->dest));
                llvm_values[instr->dest->index] = builder.CreateAlloca(ltype);
            }
        }
    }

    /* Translate instructions, block by block */
    for (b = func->first_block; b != NULL; b = b->next) {
        assert(b->index < num_blocks);
        builder.SetInsertPoint(llvm_blocks[b->index]);
        for (instr = b->first_instr; instr != NULL; instr = instr->next) {
            _emit_instr(module, llvm_func, llvm_values, num_values, llvm_blocks, num_blocks, builder, b, instr);
        }
    }
    free(llvm_blocks);
    free(llvm_values);

#ifdef IR_DEBUG
    if (llvm::verifyFunction(*llvm_func, &llvm::errs())) {
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
    return reinterpret_cast<void*>(*addr);
}
