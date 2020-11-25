#include "../ast_entitydecls.hpp"
#include "llvm_generator.hpp"
#include "ast_types.hpp"
#include "ast_funcheader_node.hpp"

using namespace llvm;

void TxNamedTypeNode::code_gen_type( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    //this->exprNode->code_gen_expr( context, nullptr );
}

void TxMemberTypeNode::code_gen_type( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    this->baseTypeExpr->code_gen_type( context );
}

void TxTypeArgumentNode::code_gen_type( LlvmGenerationContext& context ) const {
//        if ( this->typeExprNode )
//            this->typeExprNode->code_gen_type( context );
//        else
//            this->valueExprNode->code_gen_expr( context, nullptr );
}
//void TxTypeArgumentNode::code_gen_type( LlvmGenerationContext& context ) const {
//    TRACE_CODEGEN( this, context );
//    this->type_expr_node()->code_gen_type( context );
//}
//
//void TxTypeArgumentNode::code_gen_type( LlvmGenerationContext& context ) const {
//    TRACE_CODEGEN( this, context );
//    //this->valueExprNode->code_gen_expr( context, nullptr );
//}

void TxGenSpecTypeNode::code_gen_type( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    this->genTypeExpr->code_gen_type( context );
    for ( TxTypeArgumentNode* ta : *this->typeArgs )
        ta->code_gen_type( context );
    // (the type may have implicit constructors, but these are already processed via their own reinterpretation AST)
}

void TxReferenceTypeNode::code_gen_type( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    this->targetTypeNode->code_gen_type( context );
}

void TxArrayTypeNode::code_gen_type( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    if ( this->capacityNode )
        this->capacityNode->code_gen_type( context );
    this->elementTypeNode->code_gen_type( context );
}

void TxDerivedTypeNode::code_gen_builtin_type( LlvmGenerationContext& context ) const {
    this->inner_code_gen_type( context );
}

void TxDerivedTypeNode::inner_code_gen_type( LlvmGenerationContext& context ) const {
    this->baseTypeNode->code_gen_type( context );
    for ( auto interface : *this->interfaces )
        interface->code_gen_type( context );
    if ( this->superRefTypeNode )
        this->superRefTypeNode->code_gen( context );
    for ( auto member : *this->members )
        member->code_gen( context );

    for ( auto implConstr : this->qtype()->get_implicit_constructors() )
        implConstr->code_gen( context );
}

void TxDerivedTypeNode::code_gen_type( LlvmGenerationContext& context ) const {
    if ( this->builtinTypeDefiner )
        return;  // will be generated from built-in type AST instead
    TRACE_CODEGEN( this, context );
    this->inner_code_gen_type( context );
}

void TxFunctionTypeNode::code_gen_type( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
}
