#include "ast_lit.hpp"

#include "llvm_generator.hpp"

using namespace llvm;

llvm::Value* TxLiteralElementaryValueNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->code_gen_const_value( context );
}

// FUTURE: Implement integer and float constant code-gen using APInt and APFloat.

Constant* TxIntegerLitNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, this->constValue.value.i64 );
    switch ( this->constValue.typeId ) {
    case TXBT_BYTE:
        return ConstantInt::get( IntegerType::getInt8Ty( context.llvmContext ), this->constValue.value.i64, true );
    case TXBT_SHORT:
        return ConstantInt::get( IntegerType::getInt16Ty( context.llvmContext ), this->constValue.value.i64, true );
    case TXBT_INT:
        return ConstantInt::get( IntegerType::getInt32Ty( context.llvmContext ), this->constValue.value.i64, true );
    case TXBT_LONG:
        return ConstantInt::get( IntegerType::getInt64Ty( context.llvmContext ), this->constValue.value.i64, true );
    case TXBT_UBYTE:
        return ConstantInt::get( IntegerType::getInt8Ty( context.llvmContext ), this->constValue.value.u64, false );
    case TXBT_USHORT:
        return ConstantInt::get( IntegerType::getInt16Ty( context.llvmContext ), this->constValue.value.u64, false );
    case TXBT_UINT:
        return ConstantInt::get( IntegerType::getInt32Ty( context.llvmContext ), this->constValue.value.u64, false );
    case TXBT_ULONG:
        return ConstantInt::get( IntegerType::getInt64Ty( context.llvmContext ), this->constValue.value.u64, false );
    default:
        THROW_LOGIC( "Unhandled type id " << this->constValue.typeId << " in " << this );
    }
}

Constant* TxFloatingLitNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, this->constValue.value );
    switch ( this->constValue.typeId ) {
    case TXBT_HALF:
        return ConstantFP::get( Type::getHalfTy( context.llvmContext ), this->constValue.value );
    case TXBT_FLOAT:
        return ConstantFP::get( Type::getFloatTy( context.llvmContext ), this->constValue.value );
    case TXBT_DOUBLE:
        return ConstantFP::get( Type::getDoubleTy( context.llvmContext ), this->constValue.value );
    default:
        THROW_LOGIC( "Unhandled type id " << this->constValue.typeId << " in " << this );
    }
}

Constant* TxBoolLitNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, std::to_string(this->value) );
    return ( this->value ? ConstantInt::getTrue( context.llvmContext ) : ConstantInt::getFalse( context.llvmContext ) );
}

Constant* TxCharacterLitNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, '\'' << this->value << "' == " << (int)this->value );
    auto value = ConstantInt::get( context.llvmContext, APInt( 8, this->value, false ) );
    return value;
}
