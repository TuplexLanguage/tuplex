#include "ast_conv.hpp"
#include "ast_ref.hpp"

#include "llvm_generator.hpp"

using namespace llvm;

Constant* TxMaybeConversionNode::code_gen_const_address( LlvmGenerationContext& context) const {
    TRACE_CODEGEN( this, context );
    return this->get_expr()->code_gen_const_address( context );
}

Constant* TxMaybeConversionNode::code_gen_const_value( LlvmGenerationContext& context) const {
    TRACE_CODEGEN( this, context );
    return this->get_expr()->code_gen_const_value( context );
}

Value* TxMaybeConversionNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return this->get_expr()->code_gen_dyn_address( context, scope );
}

Value* TxMaybeConversionNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return this->get_expr()->code_gen_dyn_value( context, scope );
}

Constant* TxBoolConvNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, " -> " << this->resultType );
    auto origValue = this->expr->code_gen_const_value( context );
    // accepts scalar types and converts to bool: 0 => FALSE, otherwise => TRUE
    // Note: can't cast, since that will simply truncate to the lowest source bit
    if ( origValue->getType()->isIntegerTy() )
        return ConstantExpr::getICmp( ICmpInst::ICMP_NE, cast<Constant>( origValue ), ConstantInt::get( origValue->getType(), 0 ) );
    ASSERT( origValue->getType()->isFloatingPointTy(), "Expected floating point type but was: " << origValue->getType() );
    return ConstantExpr::getFCmp( FCmpInst::FCMP_UNE, cast<Constant>( origValue ), ConstantFP::get( origValue->getType(), 0 ) );
}

Value* TxBoolConvNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context, " -> " << this->resultType );
    auto origValue = this->expr->code_gen_dyn_value( context, scope );
    // accepts scalar types and converts to bool: 0 => FALSE, otherwise => TRUE
    // Note: can't cast, since that will simply truncate to the lowest source bit
    if ( origValue->getType()->isIntegerTy() )
        return scope->builder->CreateICmpNE( origValue, ConstantInt::get( origValue->getType(), 0 ) );
    ASSERT( origValue->getType()->isFloatingPointTy(), "Expected floating point type but was: " << origValue->getType() );
    return ConstantExpr::getFCmp( FCmpInst::FCMP_UNE, cast<Constant>( origValue ), ConstantFP::get( origValue->getType(), 0 ) );
}

Constant* TxScalarConvNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, " -> " << this->resultType );
    auto origValue = this->expr->code_gen_const_value( context );
    auto targetLlvmType = context.get_llvm_type( this->resultType );
    bool srcSigned = false, dstSigned = false;
    if ( auto intType = dynamic_cast<const TxIntegerType*>( this->expr->qualtype()->type()->acttype() ) )
        if ( intType->sign )
            srcSigned = true;
    if ( auto intType = dynamic_cast<const TxIntegerType*>( this->resultType->acttype() ) )
        if ( intType->sign )
            dstSigned = true;
    Instruction::CastOps cop = CastInst::getCastOpcode( origValue, srcSigned, targetLlvmType, dstSigned );
    ASSERT( cop, "No CastOps code found for cast from " << this->expr->qualtype() << " to " << this->resultType );
    ConstantFolder folder;
    return folder.CreateCast( cop, origValue, targetLlvmType );
}

Value* TxScalarConvNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context, " -> " << this->resultType );
    auto origValue = this->expr->code_gen_dyn_value( context, scope );
    auto targetLlvmType = context.get_llvm_type( this->resultType );
    // FUTURE: manually determine cast instruction
    bool srcSigned = false, dstSigned = false;
    if ( auto intType = dynamic_cast<const TxIntegerType*>( this->expr->qualtype()->type()->acttype() ) )
        if ( intType->sign )
            srcSigned = true;
    if ( auto intType = dynamic_cast<const TxIntegerType*>( this->resultType->acttype() ) )
        if ( intType->sign )
            dstSigned = true;
    Instruction::CastOps cop = CastInst::getCastOpcode( origValue, srcSigned, targetLlvmType, dstSigned );
    ASSERT( cop, "No CastOps code found for cast from " << this->expr->qualtype() << " to " << this->resultType );
    return scope->builder->CreateCast( cop, origValue, targetLlvmType, "" );
    /* for reference, copied from Instruction.def:
     HANDLE_CAST_INST(33, Trunc   , TruncInst   )  // Truncate integers
     HANDLE_CAST_INST(34, ZExt    , ZExtInst    )  // Zero extend integers
     HANDLE_CAST_INST(35, SExt    , SExtInst    )  // Sign extend integers
     HANDLE_CAST_INST(36, FPToUI  , FPToUIInst  )  // floating point -> UInt
     HANDLE_CAST_INST(37, FPToSI  , FPToSIInst  )  // floating point -> SInt
     HANDLE_CAST_INST(38, UIToFP  , UIToFPInst  )  // UInt -> floating point
     HANDLE_CAST_INST(39, SIToFP  , SIToFPInst  )  // SInt -> floating point
     HANDLE_CAST_INST(40, FPTrunc , FPTruncInst )  // Truncate floating point
     HANDLE_CAST_INST(41, FPExt   , FPExtInst   )  // Extend floating point
     HANDLE_CAST_INST(42, PtrToInt, PtrToIntInst)  // Pointer -> Integer
     HANDLE_CAST_INST(43, IntToPtr, IntToPtrInst)  // Integer -> Pointer
     HANDLE_CAST_INST(44, BitCast , BitCastInst )  // Type cast
     HANDLE_CAST_INST(45, AddrSpaceCast, AddrSpaceCastInst)  // addrspace cast
     */
}

Value* TxReferenceConvNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context, " -> " << this->resultType );
    ASSERT( this->expr->qualtype()->get_type_class() == TXTC_REFERENCE, "TxReferenceConvNode applied to non-reference type: " << this->expr->qualtype() );

    auto origValueV = this->expr->code_gen_dyn_value( context, scope );
    auto refT = context.get_llvm_type( this->resultType );
    if ( !refT ) {
        THROW_LOGIC( "In reference conversion, LLVM type not found for result type " << this->resultType << " in " << this );
    }
    // (if UINT32_MAX, pointers original target type id is retained)
    uint32_t adapterTypeId = ( this->adapterType ? this->adapterType->get_type_id() : UINT32_MAX );
    //std::cerr << "Ref conversion\n from " << this->expr->get_type(0) << "\n   to " << this->resultType << " = " << refT
    //          << "\n adapterTypeId=" << adapterTypeId << std::endl;
    return gen_ref_conversion( context, scope, origValueV, refT, adapterTypeId );
}

Constant* TxReferenceConvNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, " -> " << this->resultType );
    ASSERT( this->expr->qualtype()->get_type_class() == TXTC_REFERENCE, "TxReferenceConvNode applied to non-reference type: " << this->expr->qualtype() );

    auto origValueC = this->expr->code_gen_const_value( context );
    auto refT = context.get_llvm_type( this->resultType );
    if ( !refT ) {
        THROW_LOGIC( "In reference conversion, LLVM type not found for result type " << this->resultType << " in " << this );
    }
    // (if UINT32_MAX, pointers original target type id is retained)
    uint32_t adapterTypeId = ( this->adapterType ? this->adapterType->get_type_id() : UINT32_MAX );
    //std::cerr << "Ref conversion\n from " << this->expr->get_type(0) << "\n   to " << this->resultType << " = " << refT
    //          << "\n adapterTypeId=" << adapterTypeId << std::endl;
    return gen_ref_conversion( context, origValueC, refT, adapterTypeId );
}
