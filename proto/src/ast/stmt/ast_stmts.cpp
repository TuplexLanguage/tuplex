#include "ast_stmts.hpp"

#include "ast/expr/ast_lambda_node.hpp"
#include "parsercontext.hpp"


void TxReturnStmtNode::symbol_resolution_pass() {
    // TODO: Illegal to return reference to STACK dataspace
    auto funcHeader = this->context().enclosing_lambda()->funcHeaderNode;
    if ( funcHeader->returnField ) {
        auto retField = funcHeader->returnField->resolve_field();
        if ( this->expr ) {
            this->expr->insert_conversion( retField->qualtype()->type() );
            this->expr->symbol_resolution_pass();
        }
        else
            CERROR( this, "Return statement has no value expression although function returns " << retField->qualtype() );
    }
    else if ( this->expr )
        CERROR( this, "Return statement has value expression although function has no return type" );
}

void TxSuiteNode::stmt_declaration_pass() {
    if (! dynamic_cast<const TxLambdaExprNode*>(this->parent()))
        this->lexContext._scope = lexContext.scope()->create_code_block_scope( *this, "s" );
}

void TxAssignStmtNode::symbol_resolution_pass() {
    this->lvalue->symbol_resolution_pass();
    auto ltype = this->lvalue->resolve_type()->type();

    // note: similar rules to passing function arg
    if ( is_not_properly_concrete( this, ltype ) ) {
        CERROR( this->lvalue, "Assignee is not concrete: " << ltype );
    }

    if ( !( this->context().enclosing_lambda() && this->context().enclosing_lambda()->get_constructed() ) ) {
        // TODO: only members of constructed object should skip error
        if ( !lvalue->is_mutable() ) {
            // error message already generated
            //CERROR( this, "Assignee or assignee's container is not modifiable (nominal type of assignee is " << ltype << ")" );
        }
    }
    // Note: If the object as a whole is modifiable, it can be assigned to.
    // If it has any "non-modifiable" members, those will still get overwritten.
    // We could add custom check to prevent that scenario for Arrays, but then
    // it would in this regard behave differently than other aggregate objects.

    // if assignee is a reference:
    // TODO: check dataspace rules

    this->rvalue->insert_conversion( ltype );
    this->rvalue->symbol_resolution_pass();
}

void TxExpErrStmtNode::stmt_declaration_pass() {
    //this->lexContext._scope = lexContext.scope()->create_code_block_scope( *this, "EE" );
    this->lexContext.expErrCtx = this->expError;
    if ( !this->context().is_reinterpretation() ) {
        this->get_parse_location().parserCtx->register_exp_err_node( this );
    }
}
