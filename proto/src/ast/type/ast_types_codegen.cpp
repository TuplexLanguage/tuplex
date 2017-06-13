#include "../ast_entitydecls.hpp"
#include "llvm_generator.hpp"
#include "ast_types.hpp"
#include "ast_funcheader_node.hpp"

using namespace llvm;

void TxNamedTypeNode::code_gen_type( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
}

void TxMemberTypeNode::code_gen_type( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    this->baseTypeExpr->code_gen_type( context );
}

void TxTypeTypeArgumentNode::code_gen_type( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    this->typeExprNode->code_gen_type( context );
}

void TxValueTypeArgumentNode::code_gen_type( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    //this->valueExprNode->code_gen_expr( context, nullptr );
}

void TxGenSpecTypeNode::code_gen_type( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    this->genTypeExpr->code_gen_type( context );
    for ( TxTypeArgumentNode* ta : *this->typeArgs )
        ta->code_gen_type( context );
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
    this->baseType->code_gen_type( context );
    for ( auto interface : *this->interfaces )
        interface->code_gen_type( context );
    this->superRefTypeNode->code_gen( context );
    for ( auto member : *this->members )
        member->code_gen( context );
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

void TxModifiableTypeNode::code_gen_type( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    this->baseType->code_gen_type( context );
}

void TxFunctionHeaderNode::code_gen_type( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
}
