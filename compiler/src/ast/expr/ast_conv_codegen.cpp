#include "ast_conv.hpp"
#include "ast_ref.hpp"

#include "llvm_generator.hpp"

using namespace llvm;

Constant* TxMaybeConversionNode::code_gen_typeid( LlvmGenerationContext& context) const {
    TRACE_CODEGEN( this, context );
    return this->get_expr()->code_gen_typeid( context );
}

Value* TxMaybeConversionNode::code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return this->get_expr()->code_gen_typeid( context, scope );
}

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
    if ( auto intType = dynamic_cast<const TxIntegerTypeClassHandler*>( this->expr->qtype()->type_class_handler() ) )
        if ( intType->is_signed() )
            srcSigned = true;
    if ( auto intType = dynamic_cast<const TxIntegerTypeClassHandler*>( this->expr->qtype()->type_class_handler() ) )
        if ( intType->is_signed() )
            dstSigned = true;
    Instruction::CastOps cop = CastInst::getCastOpcode( origValue, srcSigned, targetLlvmType, dstSigned );
    ASSERT( cop, "No CastOps code found for cast from " << this->expr->qtype() << " to " << this->resultType );
    ConstantFolder folder;
    return folder.CreateCast( cop, origValue, targetLlvmType );
}

Value* TxScalarConvNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context, " -> " << this->resultType );
    auto origValue = this->expr->code_gen_dyn_value( context, scope );
    auto targetLlvmType = context.get_llvm_type( this->resultType );
    // FUTURE: manually determine cast instruction
    bool srcSigned = false, dstSigned = false;
    if ( auto intType = dynamic_cast<const TxIntegerTypeClassHandler*>( this->expr->qtype()->type_class_handler() ) )
        if ( intType->is_signed() )
            srcSigned = true;
    if ( auto intType = dynamic_cast<const TxIntegerTypeClassHandler*>( this->expr->qtype()->type_class_handler() ) )
        if ( intType->is_signed() )
            dstSigned = true;
    Instruction::CastOps cop = CastInst::getCastOpcode( origValue, srcSigned, targetLlvmType, dstSigned );
    ASSERT( cop, "No CastOps code found for cast from " << this->expr->qtype() << " to " << this->resultType );
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

Value* TxReferenceConvNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context, " -> " << this->resultType );
    ASSERT( this->expr->qtype()->get_type_class() == TXTC_REFERENCE, "TxReferenceConvNode applied to non-reference type: " << this->expr->qtype() );

    auto origValueV = this->expr->code_gen_dyn_value( context, scope );
    auto newRefT = context.get_llvm_type( this->resultType );
    Value* origPtrV = gen_get_ref_pointer( context, scope, origValueV );
    // bitcast from one pointer type to another
    auto newPtrT = cast<StructType>( newRefT )->getElementType( 0 );
    Value* newPtrV = scope->builder->CreateBitCast( origPtrV, newPtrT );
    return newPtrV;
}

Value* TxReferenceConvNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context, " -> " << this->resultType );
    ASSERT( this->expr->qtype()->get_type_class() == TXTC_REFERENCE, "TxReferenceConvNode applied to non-reference type: " << this->expr->qtype() );

    auto origValueV = this->expr->code_gen_dyn_value( context, scope );
    auto refT = context.get_llvm_type( this->resultType );
    if ( !refT ) {
        THROW_LOGIC( "In reference conversion, LLVM type not found for result type " << this->resultType << " in " << this );
    }
    Value* targetTidV = nullptr;
    if ( this->adapterType )
        targetTidV = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->adapterType->get_runtime_type_id() );
    else {
        auto origTargetType = this->expr->qtype()->target_type();
        auto resultTargetType = this->resultType->target_type();
        if ( origTargetType->get_type_class() == TXTC_INTERFACE
             && resultTargetType->get_type_class() != TXTC_INTERFACE ) {
            // from interface (adapter) to non-interface (object): dereference adapter's adaptee-type-id
            const TxActualType* staticBaseType = origTargetType.type();
            auto * runtimeBaseTypeIdV = gen_get_ref_typeid( context, scope, origValueV );
            targetTidV = virtual_field_addr_code_gen( context, scope, staticBaseType, runtimeBaseTypeIdV, "$adTypeId" );
        }
    }
    //std::cerr << "Ref conversion\n from " << this->expr->get_type(0) << "\n   to " << this->resultType << " = " << refT
    //          << "\n adapterTypeId=" << adapterTypeId << std::endl;
    return gen_ref_conversion( context, scope, origValueV, refT, targetTidV );
}

Constant* TxReferenceConvNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, " -> " << this->resultType );
    ASSERT( this->expr->qtype()->get_type_class() == TXTC_REFERENCE, "TxReferenceConvNode applied to non-reference type: " << this->expr->qtype() );

    auto origValueC = this->expr->code_gen_const_value( context );
    auto refT = context.get_llvm_type( this->resultType );
    if ( !refT ) {
        THROW_LOGIC( "In reference conversion, LLVM type not found for result type " << this->resultType << " in " << this );
    }
    Constant* targetTidC = nullptr;
    if ( this->adapterType )
        targetTidC = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->adapterType->get_runtime_type_id() );
    else {
        auto origTargetType = this->expr->qtype()->target_type();
        auto resultTargetType = this->resultType->target_type();
        if ( origTargetType->get_type_class() == TXTC_INTERFACE
             && resultTargetType->get_type_class() != TXTC_INTERFACE ) {
            // from interface (adapter) to non-interface (object): dereference adapter's adaptee-type-id
            // Do we ever encounter interface adapters as constant source values??
            LOG( this->LOGGER(), ALERT, "Encountering unexpected constant from-adapter conversion, in " << this );
//            const TxActualType* staticBaseType = origTargetType.type();
//            auto * runtimeBaseTypeIdC = gen_get_ref_typeid( context, origValueC );
//            targetTidC = virtual_field_addr_code_gen( context, staticBaseType, runtimeBaseTypeIdC, "$adTypeId" );
        }
    }
    //std::cerr << "Ref conversion\n from " << this->expr->get_type(0) << "\n   to " << this->resultType << " = " << refT
    //          << "\n adapterTypeId=" << adapterTypeId << std::endl;
    return gen_ref_conversion( context, origValueC, refT, targetTidC );
}
