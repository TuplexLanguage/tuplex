#include "llvm_generator.hpp"

#include "ast_expr_node.hpp"

using namespace llvm;

Value* TxExpressionNode::code_gen_expr( LlvmGenerationContext& context, GenScope* scope ) const {
    if (this->is_statically_constant())
        return this->code_gen_constant( context );
    else
        return this->code_gen_value( context, scope );
}

Value* TxExpressionNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    THROW_LOGIC( "Unsupported: code_gen_address() for node type " << this );
}

Value* TxExpressionNode::code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->get_type()->type()->gen_typeid( context, scope );
}
