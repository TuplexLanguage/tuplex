#include "ast_entitydecls.hpp"

#include "llvm_generator.hpp"

using namespace llvm;

void TxTypeDeclNode::code_gen( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    if ( this->context().exp_error() ) {
        LOG_DEBUG( this->LOGGER(), "Skipping codegen for AST of type with ExpErr context: " << this->typeExpression->qualtype() );
        return;
    }
    if ( this->typeExpression->qualtype()->type()->acttype()->is_type_generic_dependent() ) {
        //if ( this->typeExpression->qualtype()->type()->acttype()->has_vtable_id() )
        //    std::cerr << "Skipping code-gen for type with vtable id: " << this->typeExpression->qualtype() << std::endl;
//        if ( this->typeExpression->qualtype()->get_type_class() == TXTC_ARRAY )
//            std::cerr << "Skipping code-gen for array type " << this->typeExpression->qualtype() << " in " << this << std::endl;
        LOG_DEBUG( context.LOGGER(), "Skipping codegen for AST of generic-dependent type: "
                   << this->typeExpression << " : " << this->typeExpression->qualtype() );
        // Note that this skips codegen for the entire AST of all generic-dependent types,
        // which means none of their members are generated, including any statically declared inner/local types.
        // FUTURE: Evaluate capability for generic types to have global static members (e.g. inner types independent of the outer type parameters).
        return;
    }
    if ( !this->typeExpression->qualtype()->type()->acttype()->has_vtable_id() ) {
        LOG_DEBUG( context.LOGGER(), "Skipping codegen for AST of type without vtable id: "
                    << this->typeExpression << " : " << this->typeExpression->qualtype() );
        return;
    }
    this->typeExpression->code_gen_type( context );
}

void TxFieldDeclNode::code_gen( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    this->fieldDef->code_gen_field( context );
}
