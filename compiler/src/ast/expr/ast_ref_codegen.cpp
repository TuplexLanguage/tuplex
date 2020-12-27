#include "ast_ref.hpp"

#include "ast_exprs.hpp"

#include "llvm_generator.hpp"

using namespace llvm;


Value* gen_get_struct_member( LlvmGenerationContext& context, GenScope* scope, Value* structV, unsigned ix ) {
    if ( structV->getType()->isPointerTy() ) {  // address of struct
        auto memberA = scope->builder->CreateStructGEP( structV->getType()->getPointerElementType(), structV, ix );
        return scope->builder->CreateLoad( memberA );
    }
    else {  // direct / "register" struct
        ASSERT( structV->getType()->isStructTy(), "expected value to be a struct: " << structV );
        return scope->builder->CreateExtractValue( structV, ix );
    }
}

Constant* gen_get_struct_member( LlvmGenerationContext& context, Constant* structC, unsigned ix ) {
    return structC->getAggregateElement( ix );
}


Value* gen_get_ref_pointer( LlvmGenerationContext& context, GenScope* scope, Value* refV ) {
    if ( refV->getType()->isPointerTy() ) {  // address of struct
        //std::cerr << "DOES THIS HAPPEN? gen_get_ref_pointer() on address of ref struct: " << refV << std::endl;
        auto refPtrA = scope->builder->CreateStructGEP( refV->getType()->getPointerElementType(), refV, 0 );
        return scope->builder->CreateLoad( refPtrA, "ptr" );
    }
    else {  // direct / "register" struct
        return scope->builder->CreateExtractValue( refV, 0, "ptr" );
    }
}

Value* gen_get_ref_typeid( LlvmGenerationContext& context, GenScope* scope, Value* refV ) {
    if ( refV->getType()->isPointerTy() ) {  // address of struct
        //std::cerr << "DOES THIS HAPPEN? gen_get_ref_typeid() on address of ref struct: " << refV << std::endl;
        auto refPtrA = scope->builder->CreateStructGEP( refV->getType()->getPointerElementType(), refV, 1 );
        return scope->builder->CreateLoad( refPtrA, "tid" );
    }
    else {  // direct / "register" struct
        return scope->builder->CreateExtractValue( refV, 1, "tid" );
    }
}

Value* gen_ref( LlvmGenerationContext& context, GenScope* scope, Type* refT, Value* ptrV, Value* tidV ) {
    Value* refV = UndefValue::get( refT );
    auto castPtrV = scope->builder->CreatePointerCast( ptrV, refT->getStructElementType( 0 ) );
    refV = scope->builder->CreateInsertValue( refV, castPtrV, 0 );
    refV = scope->builder->CreateInsertValue( refV, tidV, 1 );
    return refV;
}

Value* gen_ref( LlvmGenerationContext& context, GenScope* scope, const TxActualType* refType, Value* ptrV ) {
    ASSERT( refType->get_type_class() == TXTC_REFERENCE, "Not a reference type: " << refType );
    auto refT = context.get_llvm_type( refType );
    auto tidV = refType->target_type()->gen_typeid( context );
    return gen_ref( context, scope, refT, ptrV, tidV );
}


Constant* gen_get_ref_pointer( LlvmGenerationContext& context, Constant* refC ) {
    return refC->getAggregateElement( 0U );
}

Constant* gen_get_ref_typeid( LlvmGenerationContext& context, Constant* refC ) {
    return refC->getAggregateElement( 1 );
}

Constant* gen_ref( LlvmGenerationContext& context, Type* refT, Constant* ptrC, Constant* tidC ) {
    return ConstantStruct::get( cast<StructType>(refT), { ConstantExpr::getPointerCast( ptrC, refT->getStructElementType( 0 ) ), tidC } );
}


/** Converts a reference value from one type to another. If targetTypeId is specified, it will replace the original type id. */
Value* gen_ref_conversion( LlvmGenerationContext& context, GenScope* scope, Value* origValue,
                           Type* targetRefT, Value* tidV ) {
    auto newPtrT = cast<StructType>( targetRefT )->getElementType( 0 );
    if ( tidV == nullptr )
        tidV = gen_get_ref_typeid( context, scope, origValue );
    Value* origPtrV = gen_get_ref_pointer( context, scope, origValue );
    // bitcast from one pointer type to another
    Value* newPtrV = scope->builder->CreateBitCast( origPtrV, newPtrT );

    return gen_ref( context, scope, targetRefT, newPtrV, tidV );
}

/** Converts a reference constant from one type to another. If targetTypeId is specified, it will replace the original type id. */
Constant* gen_ref_conversion( LlvmGenerationContext& context, Constant* origValue,
                              Type* targetRefT, Constant* tidC ) {
    auto newPtrT = cast<StructType>( targetRefT )->getElementType( 0 );
    if ( tidC == nullptr )
        tidC = gen_get_ref_typeid( context, origValue );
    Constant* origPtrC = gen_get_ref_pointer( context, origValue );
    // bitcast from one pointer type to another
    Constant* newPtrC = ConstantExpr::getBitCast( origPtrC, newPtrT );
    return gen_ref( context, targetRefT, newPtrC, tidC );
}



Value* TxReferenceToNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    // automatically allocates stack space for the reference value
    auto valueV = this->code_gen_expr( context, scope );

    scope->use_alloca_insertion_point();
    Value* valuePtrV = scope->builder->CreateAlloca( valueV->getType(), nullptr, "ref" );
    scope->use_current_insertion_point();

    scope->builder->CreateStore( valueV, valuePtrV );
    return valuePtrV;
}

Constant* TxReferenceToNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    Constant* ptrC = dyn_cast<Constant>( this->target->code_gen_const_address( context ) );
    auto tidC = this->target->code_gen_typeid( context );
    // Note: Using the statically known type id would have risked producing a too-abstract type here
    //       even if the value expression is statically constant.
    auto refT = context.get_llvm_type( this->qtype() );
    return gen_ref( context, refT, ptrC, tidC );
}

Value* TxReferenceToNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto ptrV = this->target->code_gen_dyn_address( context, scope );
    auto tidV = this->target->code_gen_typeid( context, scope );
    auto refT = context.get_llvm_type( this->qtype() );
    return gen_ref( context, scope, refT, ptrV, tidV );
}


Value* TxReferenceDerefNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    if ( !this->refExprValue ) {
        this->refExprValue = this->reference->code_gen_dyn_value( context, scope );
    }
    return gen_get_ref_pointer( context, scope, this->refExprValue );
}

Value* TxReferenceDerefNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    Value* ptrV = this->code_gen_dyn_address( context, scope );
    return scope->builder->CreateLoad( ptrV );
}

Value* TxReferenceDerefNode::code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    if ( !this->refExprValue ) {
        this->refExprValue = this->reference->code_gen_dyn_value( context, scope );
    }
    return gen_get_ref_typeid( context, scope, this->refExprValue );
}

Constant* TxReferenceDerefNode::code_gen_typeid( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    THROW_LOGIC( "Unsupported: constant code_gen_typeid() for node type " << this );
}


Value* TxDerefAssigneeNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    if ( !this->refExprValue ) {
        this->refExprValue = this->reference->code_gen_dyn_value( context, scope );
    }
    return gen_get_ref_pointer( context, scope, this->refExprValue );
}

Value* TxDerefAssigneeNode::code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    if ( !this->refExprValue ) {
        this->refExprValue = this->reference->code_gen_dyn_value( context, scope );
    }
    return gen_get_ref_typeid( context, scope, this->refExprValue );
}

