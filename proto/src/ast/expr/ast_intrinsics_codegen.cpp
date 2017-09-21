#include "ast_intrinsics.hpp"

#include "ast_ref.hpp"

#include "llvm_generator.hpp"

using namespace llvm;


Value* TxRefAddressNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto refPtrV = this->refExpr->code_gen_addr( context, scope );
    auto refPtrT = cast<PointerType>( refPtrV->getType() );
    auto refMembPtrV = scope->builder->CreateStructGEP( refPtrT->getPointerElementType(), refPtrV, 0 );
    auto addrPtrV = scope->builder->CreateBitCast( refMembPtrV, Type::getInt64Ty( context.llvmContext )->getPointerTo() );
    return addrPtrV;
}

Value* TxRefAddressNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto ptrV = gen_get_ref_pointer( context, scope, this->refExpr->code_gen_expr( context, scope ) );
    return scope->builder->CreatePtrToInt( ptrV, Type::getInt64Ty( context.llvmContext ) );
}

Constant* TxRefAddressNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto ptrC = gen_get_ref_pointer( context, this->refExpr->code_gen_const_value( context ) );
    return ConstantExpr::getPtrToInt( ptrC, Type::getInt64Ty( context.llvmContext ) );
}


Value* TxRefTypeIdNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto refPtrV = this->refExpr->code_gen_addr( context, scope );
    auto refPtrT = cast<PointerType>( refPtrV->getType() );
    auto refMembPtrV = scope->builder->CreateStructGEP( refPtrT->getPointerElementType(), refPtrV, 1 );
    return refMembPtrV;
}

Value* TxRefTypeIdNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return gen_get_ref_typeid( context, scope, this->refExpr->code_gen_dyn_value( context, scope ) );
}

Constant* TxRefTypeIdNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    return gen_get_ref_typeid( context, this->refExpr->code_gen_const_value( context ) );
}


Value* TxTypeExprTypeIdNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->code_gen_const_value( context );
}

Constant* TxTypeExprTypeIdNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    return ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->typeExpr->qualtype()->get_type_id() );
}


Value* TxSizeofExprNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto typeIdV = this->expr->code_gen_typeid( context, scope );
    return context.gen_get_element_size( scope, this->expr->qualtype()->type()->acttype(), typeIdV );
}

Constant* TxSizeofExprNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto typeIdC = this->expr->code_gen_typeid( context );
    return cast<Constant>( context.gen_get_element_size( nullptr, this->expr->qualtype()->type()->acttype(), typeIdC ) );
}


Value* TxSupertypesExprNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto typeIdV = this->expr->code_gen_dyn_value( context, scope );
    auto arrayTypeIdC = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->qualtype()->type()->target_type()->get_type_id() );
    return context.gen_get_supertypes_array_ref( scope, this->expr->qualtype()->type()->acttype(), typeIdV, arrayTypeIdC );
}

//Value* TxSupertypesExprNode::code_gen_const_value( LlvmGenerationContext& context ) const {
//    TRACE_CODEGEN( this, context );
//    auto typeIdC = this->expr->code_gen_const_value( context );
//    auto arrayTypeIdC = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->qualtype()->type()->target_type()->get_type_id() );
//    return context.gen_get_supertypes_array_ref( this->expr->qualtype()->type()->acttype(), typeIdC, arrayTypeIdC );
//}
