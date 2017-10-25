#include "ast_entitydecls.hpp"

#include "llvm_generator.hpp"

using namespace llvm;

void TxTypeDeclNode::code_gen( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    if ( this->context().exp_error() ) {
        LOG_DEBUG( this->LOGGER(), "Skipping codegen for AST of type with ExpErr context: " << this->typeCreatingNode->qtype() );
        return;
    }

    // all "vtable types" except the ones that are type-generic-dependent are generated:
    auto type = this->typeCreatingNode->qtype();
    if ( !type->has_runtime_type_id() || type->get_runtime_type_id() >= this->registry().vtable_types_count() ) {
        LOG_DEBUG( context.LOGGER(), "Skipping codegen for AST of non-vtable type: "
                   << this->typeCreatingNode << " : " << this->typeCreatingNode->qtype() );
        return;
    }
    else if ( type->suppress_code_gen() ) {
        LOG_DEBUG( context.LOGGER(), "Skipping codegen for AST of suppressed-code-gen type: "
                   << this->typeCreatingNode << " : " << this->typeCreatingNode->qtype() );
        return;
    }
    this->typeCreatingNode->code_gen_type( context );
}

void TxFieldDeclNode::code_gen( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    this->fieldDef->code_gen_field( context );
}
