#include "../../symbol/qual_type.hpp"
#include "ast_expr_node.hpp"
#include "tx_error.hpp"

#include "llvm_generator.hpp"

using namespace llvm;

Value* TxExpressionNode::code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->qualtype()->type()->acttype()->gen_typeid( context, scope );
}



Value* TxExpressionNode::code_gen_expr( LlvmGenerationContext& context, GenScope* scope ) const {
    if (this->is_statically_constant())
        return this->code_gen_const_value( context );
    else
        return this->code_gen_dyn_value( context, scope );
}

Value* TxExpressionNode::code_gen_addr( LlvmGenerationContext& context, GenScope* scope ) const {
    if (this->is_statically_constant())
        return this->code_gen_const_address( context );
    else
        return this->code_gen_dyn_address( context, scope );
}



Value* TxExpressionNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    if ( this->is_statically_constant() )
        return this->code_gen_const_address( context );
    else
        CERR_CODECHECK( this, "Can't code-gen address value for node type " << this );
}


Constant* TxExpressionNode::code_gen_const_address( LlvmGenerationContext& context ) const {
    // automatically allocates global storage for static constants, used for e.g. string literals
    auto constant = this->code_gen_const_value( context );
    return new GlobalVariable( context.llvmModule(), constant->getType(), true, GlobalValue::InternalLinkage, constant );
}


Constant* TxExpressionNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    ASSERT(! this->is_statically_constant(), "code_gen_constant() not implemented though is_statically_constant() returns true for " << this );
    THROW_LOGIC( "Unsupported: code_gen_constant() for node type " << this );
}
