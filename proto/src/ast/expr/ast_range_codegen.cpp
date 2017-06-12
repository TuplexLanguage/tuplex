#include "ast_range.hpp"

#include "llvm_generator.hpp"

using namespace llvm;

Value* TxERangeLitNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    // experimental, automatically allocates global space for constants
    //std::cerr << "TxArrayLitNode::code_gen_address " << this << std::endl;
    auto targetVal = this->code_gen_expr( context, scope );
    if ( auto constInitializer = dyn_cast<Constant>( targetVal ) ) {
        return new GlobalVariable( context.llvmModule(), constInitializer->getType(), true, GlobalValue::InternalLinkage, constInitializer );
    }
    else {
        return targetVal;
    }
}

Value* TxERangeLitNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return this->stackConstr->code_gen_value( context, scope );
}

Constant* TxERangeLitNode::code_gen_constant( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    return this->stackConstr->code_gen_constant( context );
}
