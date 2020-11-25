#include "ast_range.hpp"

#include "llvm_generator.hpp"

using namespace llvm;

Value* TxERangeLitNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return this->stackConstr->code_gen_dyn_address( context, scope );
}

Value* TxERangeLitNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return this->stackConstr->code_gen_dyn_value( context, scope );
}
