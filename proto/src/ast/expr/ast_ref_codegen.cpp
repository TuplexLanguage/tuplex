#include "ast_ref.hpp"

#include "ast_exprs.hpp"

#include "llvm_generator.hpp"

using namespace llvm;


Value* gen_get_struct_member( LlvmGenerationContext& context, GenScope* scope, Value* structV, unsigned ix ) {
    Value* memberV;
    if ( auto structPtrT = dyn_cast<PointerType>( structV->getType() ) ) {  // address of struct
        ASSERT( structPtrT->getPointerElementType()->isStructTy(), "expected pointer element to be a struct: " << structV );
        (void) structPtrT;   // suppresses unused variable warning in release mode
        if ( scope ) {
            auto memberA = scope->builder->CreateStructGEP( structPtrT->getPointerElementType(), structV, ix );
            memberV = scope->builder->CreateLoad( memberA );
        }
        else {
            Value *idxs[] = {
                              ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                              ConstantInt::get( Type::getInt32Ty( context.llvmContext ), ix )
            };
            auto memberA = GetElementPtrInst::CreateInBounds( structV, idxs );
            memberV = new LoadInst( memberA );
        }
    }
    else {  // direct / "register" struct
        ASSERT( structV->getType()->isStructTy(), "expected value to be a struct: " << structV );
        memberV = ( scope ? scope->builder->CreateExtractValue( structV, ix )
                          : ExtractValueInst::Create( structV, ix ) );
        //std::cerr << "gen_get_struct_member(), structV: " << structV << "   ix: " << ix << std::endl;
        //std::cerr << "                         memberV: " << memberV << std::endl;
    }
    return memberV;
}

Value* gen_get_ref_pointer( LlvmGenerationContext& context, GenScope* scope, Value* refV ) {
    Value* ptrV = gen_get_struct_member( context, scope, refV, 0 );
    ASSERT( ptrV->getType()->isPointerTy(), "expected ref.ptr element to be a pointer: " << refV );
    return ptrV;
}

Value* gen_get_ref_typeid( LlvmGenerationContext& context, GenScope* scope, Value* refV ) {
    Value* tidV = gen_get_struct_member( context, scope, refV, 1 );
    return tidV;
}

Value* gen_ref( LlvmGenerationContext& context, GenScope* scope, Type* refT, Value* ptrV, Value* tidV ) {
    Value* refV = UndefValue::get( refT );
    auto castPtrV = scope->builder->CreatePointerCast( ptrV, refT->getStructElementType( 0 ) );
    refV = scope->builder->CreateInsertValue( refV, castPtrV, 0 );
    refV = scope->builder->CreateInsertValue( refV, tidV, 1 );
    return refV;
}


Constant* gen_get_ref_pointer( LlvmGenerationContext& context, Constant* refC ) {
    return refC->getAggregateElement( 0U );
}

Constant* gen_get_ref_typeid( LlvmGenerationContext& context, Constant* refC ) {
    return refC->getAggregateElement( 1 );
}

Constant* gen_ref( LlvmGenerationContext& context, Type* refT, Constant* ptrC, Constant* tidC ) {
    return ConstantStruct::get( cast<StructType>(refT), { ptrC, tidC } );
}


/** Converts a reference value from one type to another. If targetTypeId is specified, it will replace the original type id. */
Value* gen_ref_conversion( LlvmGenerationContext& context, GenScope* scope, Value* origValue, Type* targetRefT, uint32_t targetTypeId ) {
    auto newPtrT = cast<StructType>( targetRefT )->getElementType( 0 );
    Value* tidV = ( targetTypeId == UINT32_MAX ? gen_get_ref_typeid( context, scope, origValue )
                                               : ConstantInt::get( Type::getInt32Ty( context.llvmContext ), targetTypeId ) );
    Value* origPtrV = gen_get_ref_pointer( context, scope, origValue );
    // bitcast from one pointer type to another
    Value* newPtrV = scope->builder->CreateBitCast( origPtrV, newPtrT );

    return gen_ref( context, scope, targetRefT, newPtrV, tidV );
}

/** Converts a reference constant from one type to another. If targetTypeId is specified, it will replace the original type id. */
Constant* gen_ref_conversion( LlvmGenerationContext& context, Constant* origValue, Type* targetRefT, uint32_t targetTypeId ) {
    auto newPtrT = cast<StructType>( targetRefT )->getElementType( 0 );
    Constant* tidC = ( targetTypeId == UINT32_MAX ? gen_get_ref_typeid( context, origValue )
                                                  : ConstantInt::get( Type::getInt32Ty( context.llvmContext ), targetTypeId ) );
    Constant* origPtrC = gen_get_ref_pointer( context, origValue );
    // bitcast from one pointer type to another
    Constant* newPtrC = ConstantExpr::getBitCast( origPtrC, newPtrT );
    return gen_ref( context, targetRefT, newPtrC, tidC );
}



Constant* TxReferenceToNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    Constant* ptrC = dyn_cast<Constant>( this->target->code_gen_const_address( context ) );

    // the reference gets the statically known target type id
    auto tidC = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->target->get_type()->get_type_id() );

    // box the pointer:
    auto refT = this->get_type()->type()->make_llvm_type( context );
    return gen_ref( context, refT, ptrC, tidC );
}

Value* TxReferenceToNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    Value* ptrV = this->target->code_gen_dyn_address( context, scope );

    // the reference gets the statically known target type id
    auto tidC = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->target->get_type()->get_type_id() );

    // box the pointer:
    auto refT = this->get_type()->type()->make_llvm_type( context );
    return gen_ref( context, scope, refT, ptrV, tidC );
}

Value* TxReferenceDerefNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    if ( !this->refExprValue ) {
        this->refExprValue = this->reference->code_gen_dyn_value( context, scope );
        if ( !this->refExprValue )
            return NULL;
    }
    return gen_get_ref_pointer( context, scope, this->refExprValue );
}

Value* TxReferenceDerefNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    Value* ptrV = this->code_gen_dyn_address( context, scope );
    return scope->builder->CreateLoad( ptrV );
}

Value* TxReferenceDerefNode::code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const {
    // dynamic by reading the reference's target type id
    TRACE_CODEGEN( this, context, " TypeID" );
    if ( !this->refExprValue ) {
        this->refExprValue = this->reference->code_gen_dyn_value( context, scope );
        if ( !this->refExprValue )
            return NULL;
    }

    Value* tidV = gen_get_ref_typeid( context, scope, this->refExprValue );
    return tidV;
}


Value* TxDerefAssigneeNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto refval = this->operand->code_gen_dyn_value( context, scope );
    return gen_get_ref_pointer( context, scope, refval );
}
