#include "ast_lit.hpp"
#include "llvm_generator.hpp"

using namespace llvm;

//uint32_t IntConstant::get_value_UInt() const {
//    if ( this->_signed ) {
//        if ( this->value.i64 >= 0 && this->value.i64 <= UINT32_MAX )
//            return static_cast<uint32_t>( this->value.i64 );
//    }
//    else {
//        if ( this->value.u64 <= UINT32_MAX )
//            return static_cast<uint32_t>( this->value.u64 );
//    }
//    throw std::range_error( "Value of constant integer is not within range of type UInt" );
//}

Constant* IntConstant::code_gen_constant( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, this->value.i64 );
    switch ( this->typeId ) {
    case TXBT_BYTE:
        return ConstantInt::get( IntegerType::getInt8Ty( context.llvmContext ), this->value.i64, true );
    case TXBT_SHORT:
        return ConstantInt::get( IntegerType::getInt16Ty( context.llvmContext ), this->value.i64, true );
    case TXBT_INT:
        return ConstantInt::get( IntegerType::getInt32Ty( context.llvmContext ), this->value.i64, true );
    case TXBT_LONG:
        return ConstantInt::get( IntegerType::getInt64Ty( context.llvmContext ), this->value.i64, true );
    case TXBT_UBYTE:
        return ConstantInt::get( IntegerType::getInt8Ty( context.llvmContext ), this->value.u64, false );
    case TXBT_USHORT:
        return ConstantInt::get( IntegerType::getInt16Ty( context.llvmContext ), this->value.u64, false );
    case TXBT_UINT:
        return ConstantInt::get( IntegerType::getInt32Ty( context.llvmContext ), this->value.u64, false );
    case TXBT_ULONG:
        return ConstantInt::get( IntegerType::getInt64Ty( context.llvmContext ), this->value.u64, false );
    default:
        THROW_LOGIC( "Unhandled type id " << this->typeId << " in " << this );
    }
}

llvm::Value* TxLiteralElementaryValueNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->code_gen_constant( context );
}

Constant* TxIntegerLitNode::code_gen_constant( LlvmGenerationContext& context ) const {
    return this->constValue.code_gen_constant( context );
}

Constant* TxFloatingLitNode::code_gen_constant( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, this->value );
    switch ( this->typeId ) {
    case TXBT_HALF:
        return ConstantFP::get( Type::getHalfTy( context.llvmContext ), this->literal );
    case TXBT_FLOAT:
        return ConstantFP::get( Type::getFloatTy( context.llvmContext ), this->literal );
    case TXBT_DOUBLE:
        return ConstantFP::get( Type::getDoubleTy( context.llvmContext ), this->literal );
    default:
        THROW_LOGIC( "Unhandled type id " << this->typeId << " in " << this );
    }
}

Constant* TxBoolLitNode::code_gen_constant( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, std::to_string(this->value) );
    return ( this->value ? ConstantInt::getTrue( context.llvmContext ) : ConstantInt::getFalse( context.llvmContext ) );
}

Constant* TxCharacterLitNode::code_gen_constant( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, '\'' << this->value << "' == " << (int)this->value );
    auto value = ConstantInt::get( context.llvmContext, APInt( 8, this->value, false ) );
    return value;
}



Constant* TxCStringLitNode::code_gen_constant( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, '"' << this->value << '"' );
    std::vector<Constant*> members {
                                     ConstantInt::get( context.llvmContext, APInt( 32, this->arrayCapacity ) ),
                                     ConstantInt::get( context.llvmContext, APInt( 32, this->arrayCapacity ) ),
                                     ConstantDataArray::getString( context.llvmContext, this->value )
    };
    auto str = ConstantStruct::getAnon( members );
    return str;
}

Value* TxCStringLitNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    // experimental, automatically allocates space for literals, used for e.g. string literals
    auto constant = this->code_gen_value( context, scope );
    auto constInitializer = cast<Constant>( constant );
    return new GlobalVariable( context.llvmModule, constInitializer->getType(), true, GlobalValue::InternalLinkage, constInitializer );
}

Value* TxCStringLitNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->code_gen_constant( context );
}
