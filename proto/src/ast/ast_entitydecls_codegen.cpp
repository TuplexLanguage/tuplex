#include "ast_entitydecls.hpp"

#include "llvm_generator.hpp"

using namespace llvm;

void TxTypeDeclNode::code_gen( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    if ( this->context().exp_error() ) {
        LOG_DEBUG( this->LOGGER(), "Skipping codegen for AST of type with ExpErr context: " << this->typeCreatingNode->qtype() );
        return;
    }
    auto type = this->typeCreatingNode->qtype();
    if ( type->is_type_generic_dependent() ) {
        LOG_DEBUG( context.LOGGER(), "Skipping codegen for AST of generic-dependent type: "
                   << this->typeCreatingNode << " : " << this->typeCreatingNode->qtype() );
        // Note that this skips codegen for the entire AST of all generic-dependent types,
        // which means none of their members are generated, including any statically declared inner/local types.
        // FUTURE: Evaluate capability for generic types to have global static members (e.g. inner types independent of the outer type parameters).
//        if ( !( type->is_builtin() || !type->has_runtime_type_id()
//                || type->get_runtime_type_id() >= this->registry().vtable_types_count() ) )
//            LOG_NOTE( this->LOGGER(), "Skipping codegen for AST of generic-dependent type that has vtable type id: " << type );
        return;
    }
    if ( !type->has_runtime_type_id() || type->get_runtime_type_id() >= this->registry().vtable_types_count() ) {
        LOG_DEBUG( context.LOGGER(), "Skipping codegen for AST of non-vtable type: "
                   << this->typeCreatingNode << " : " << this->typeCreatingNode->qtype() );
        return;
    }
    this->typeCreatingNode->code_gen_type( context );
}

void TxFieldDeclNode::code_gen( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    this->fieldDef->code_gen_field( context );
}
