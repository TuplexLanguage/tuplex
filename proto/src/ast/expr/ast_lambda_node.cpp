#include "ast_lambda_node.hpp"

void TxLambdaExprNode::declaration_pass() {
    std::string funcName = ( this->fieldDefNode && this->fieldDefNode->get_declaration() )
                                  ? this->fieldDefNode->get_declaration()->get_unique_name()
                                  : "";
    TxScopeSymbol* funcScope = lexContext.scope()->create_code_block_scope( *this, funcName );
    if ( this->is_instance_method() ) {
        auto entitySym = dynamic_cast<TxEntitySymbol*>( lexContext.scope() );
        if ( entitySym && entitySym->get_type_decl() ) {  // if in type scope
            if ( this->fieldDefNode->get_declaration()->get_decl_flags() & TXD_CONSTRUCTOR ) {
                // this is a constructor
                this->constructedObjTypeDecl = entitySym->get_type_decl();
            }
        }
        else
            CERROR( this, "The scope of instance method must be a type scope: " << lexContext.scope() );

        this->selfRefNode->declare_field( funcScope, TXD_NONE, TXS_STACK );
        this->superRefNode->declare_field( funcScope, TXD_NONE, TXS_STACK );
    }
    this->lexContext._scope = funcScope;
    this->lexContext.enclosingLambda = this;
    // FUTURE: define implicit closure object when in code block
}

bool TxLambdaExprNode::is_suppressed_modifying_method() const {
    return ( this->funcHeaderNode->is_modifying()
             && this->context().reinterpretation_definer()
             && !this->selfTypeNode->resolve_type()->type()->is_mutable() );
}

void TxLambdaExprNode::symbol_resolution_pass() {
    TxExpressionNode::symbol_resolution_pass();
    if ( this->is_instance_method() ) {
        if ( this->is_suppressed_modifying_method() ) {
            // modifying methods in an immutable specialization of a generic type are effectively 'abstract'
            //this->funcHeaderNode->symbol_resolution_pass();  // function header
            return;
        }
        try {
            this->selfRefNode->symbol_resolution_pass();
            this->superRefNode->symbol_resolution_pass();
        }
        catch ( const resolution_error& err ) {
            LOG( this->LOGGER(), DEBUG, "Caught resolution error in self/super of instance method " << this << ": " << err );
        }
    }
    else {
        if ( this->funcHeaderNode->is_modifying() )
            CERROR( this, "Only instance methods can be declared 'modifying': " << this->funcHeaderNode->qualtype() );
        // TODO: review if there are non-method functions with closure that can declare 'modifying'?
    }

    this->funcHeaderNode->symbol_resolution_pass();  // function header
    this->suite->symbol_resolution_pass();  // function body

    if ( this->funcHeaderNode->returnField ) {
        // verify that body always ends with explicit return statement
        if ( !this->suite->ends_with_return_stmt() )
            CERROR( this, "Function has return value, but not all code paths end with a return statement." );
    }
}
