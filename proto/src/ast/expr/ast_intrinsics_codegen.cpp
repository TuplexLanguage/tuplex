#include "ast_intrinsics.hpp"

#include "ast_ref.hpp"

#include "llvm_generator.hpp"

using namespace llvm;


Value* TxRefAddressNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto refV = this->refExpr->code_gen_expr( context, scope );
    auto ptrV = gen_get_ref_pointer( context, scope, refV );
    auto addrV = scope->builder->CreatePtrToInt( ptrV, Type::getInt64Ty( context.llvmContext ) );
    return addrV;
}

Value* TxRefAddressNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto refPtrV = this->refExpr->code_gen_addr( context, scope );
    auto refPtrT = cast<PointerType>( refPtrV->getType() );
    auto refMembPtrV = scope->builder->CreateStructGEP( refPtrT->getPointerElementType(), refPtrV, 0 );
    auto addrPtrV = scope->builder->CreateBitCast( refMembPtrV, Type::getInt64Ty( context.llvmContext )->getPointerTo() );
    return addrPtrV;
}


Value* TxRefTypeIdNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto refV = this->refExpr->code_gen_dyn_value( context, scope );
    auto tidV = gen_get_ref_typeid( context, scope, refV );
    return tidV;
}

Value* TxRefTypeIdNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto refPtrV = this->refExpr->code_gen_addr( context, scope );
    auto refPtrT = cast<PointerType>( refPtrV->getType() );
    auto refMembPtrV = scope->builder->CreateStructGEP( refPtrT->getPointerElementType(), refPtrV, 1 );
    return refMembPtrV;
}

Constant* TxRefTypeIdNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto refC = this->refExpr->code_gen_const_value( context );
    auto tidC = refC->getAggregateElement( 1 );
    return tidC;
}
