#include "ast_modbase.hpp"

#include "ast_entitydecls.hpp"

#include "llvm_generator.hpp"

using namespace llvm;

void TxParsingUnitNode::code_gen( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    this->module->code_gen( context );
}

void TxModuleNode::code_gen( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    if ( this->members ) {
        for ( auto mem : *this->members )
            mem->code_gen( context );
    }
    if ( this->subModules ) {
        for ( auto mod : *this->subModules )
            mod->code_gen( context );
    }
}
