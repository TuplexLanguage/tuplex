#include "ast_lit.hpp"
#include "llvm_generator.hpp"


using namespace llvm;


Value* TxBoolLitNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context, std::to_string(this->value));
    return ( this->value ? ConstantInt::getTrue(context.llvmContext) : ConstantInt::getFalse(context.llvmContext) );
}


Value* TxCStringLitNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context, '"' << this->value << '"');
    //auto type = context.get_llvm_type(this->get_type());
    std::vector<Constant*> members {
        ConstantInt::get(context.llvmContext, APInt(32, this->arrayLength)),
        ConstantDataArray::getString(context.llvmContext, this->value)
    };
    auto str = ConstantStruct::getAnon(members);
    return str;
}


Value* TxCharacterLitNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context, '\'' << this->value << "' == " << (int)this->value);
    auto value = ConstantInt::get(context.llvmContext, APInt(8, this->value, false));
    return value;
}


Value* TxIntegerLitNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context, this->intValue.value.i64);
    switch (this->intValue.typeId) {
    case BYTE:
        return ConstantInt::get(IntegerType::getInt8Ty(context.llvmContext), this->intValue.value.i64, true);
    case SHORT:
        return ConstantInt::get(IntegerType::getInt16Ty(context.llvmContext), this->intValue.value.i64, true);
    case INT:
        return ConstantInt::get(IntegerType::getInt32Ty(context.llvmContext), this->intValue.value.i64, true);
    case LONG:
        return ConstantInt::get(IntegerType::getInt64Ty(context.llvmContext), this->intValue.value.i64, true);
    case UBYTE:
        return ConstantInt::get(IntegerType::getInt8Ty(context.llvmContext), this->intValue.value.u64, false);
    case USHORT:
        return ConstantInt::get(IntegerType::getInt16Ty(context.llvmContext), this->intValue.value.u64, false);
    case UINT:
        return ConstantInt::get(IntegerType::getInt32Ty(context.llvmContext), this->intValue.value.u64, false);
    case ULONG:
        return ConstantInt::get(IntegerType::getInt64Ty(context.llvmContext), this->intValue.value.u64, false);
    default:
        ASSERT(false, "Unhandled type id " << this->intValue.typeId);
        return nullptr;
    }
//    IntegerType* type = static_cast<IntegerType*>(context.get_llvm_type(this->get_type()));
//    ASSERT (type, "Could not get IntegerType for TxIntegerLitNode " << context.get_llvm_type(this->get_type()));
//    auto value = ConstantInt::get(type, this->intValue.value.u64, this->intValue._signed);
////    ASSERT (value->getSExtValue()==this->value, "LLVM's s-ext-int value " << value->getSExtValue() << "!=" << this->value);
////    //auto value = ConstantInt::get(context.llvmContext, APInt(64, this->value, true));
//    return value;
}

Constant* TxIntegerLitNode::IntConstantProxy::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    return cast<Constant>(this->intNode->code_gen(context, scope));
}


Value* TxFloatingLitNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context, this->value);
    Type* type = context.get_llvm_type(this->get_type());
    ASSERT (type, "Could not get Type for TxFloatingLitNode " << context.get_llvm_type(this->get_type()));
    auto value = ConstantFP::get(type, this->literal);
    //auto value = ConstantFP::get(type, this->value);
    return value;
}
