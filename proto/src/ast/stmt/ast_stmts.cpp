#include "ast_stmts.hpp"

#include "ast_panicstmt_node.hpp"
#include "ast/expr/ast_lambda_node.hpp"
#include "ast/expr/ast_constexpr.hpp"
#include "parsercontext.hpp"


void TxReturnStmtNode::resolution_pass() {
    // TODO: Illegal to return reference to STACK dataspace
    auto funcHeader = this->context().enclosing_lambda()->funcHeaderNode;
    if ( funcHeader->returnField ) {
        auto retField = funcHeader->returnField->resolve_field();
        if ( this->expr ) {
            this->expr->insert_conversion( TXP_RESOLUTION, retField->qtype() );
        }
        else
            CERROR( this, "Return statement has no value expression although function returns " << retField->qtype() );
    }
    else if ( this->expr )
        CERROR( this, "Return statement has value expression although function has no return type" );
}

void TxSuiteNode::stmt_declaration_pass() {
    if (! dynamic_cast<const TxLambdaExprNode*>(this->parent()))
        this->lexContext._scope = lexContext.scope()->create_code_block_scope( *this, "s" );
}


void TxAssignStmtNode::resolution_pass() {
    auto ltype = this->lvalue->resolve_type( TXP_RESOLUTION ).type();  // strip qualifiers since this only copies value
    if ( ltype->get_type_class() == TXTC_ARRAY ) {
        // This implementation relaxes the auto-conversion check to allow assignment of arrays with unknown C.
        // We only handle auto-dereferencing of the rvalue:
        auto rtype = this->rvalue->originalExpr->resolve_type( TXP_RESOLUTION );
        if ( rtype->get_type_class() == TXTC_REFERENCE ) {
            this->rvalue->insert_conversion( TXP_RESOLUTION, rtype->target_type() );
            rtype = this->rvalue->qtype();
        }
        else
            this->rvalue->resolve_type( TXP_RESOLUTION );

        // Note: In theory, if this expression is statically constant we could perform the bounds checking in resolution pass.
        // However accessing the cogegen'd value of Array.L isn't guaranteed before the type preparation has been run.
        if ( auto lCapExpr = ltype->capacity() ) {
            if ( lCapExpr->is_statically_constant() ) {
                auto lArrayCap = eval_unsigned_int_constant( lCapExpr );  // capacity is statically known

                if ( auto rCapExpr = rtype->capacity() ) {
                    if ( rCapExpr->is_statically_constant() ) {
                        auto rArrayCap = eval_unsigned_int_constant( rCapExpr );  // capacity is statically known

                        if ( lArrayCap < rArrayCap )
                            CWARNING( this, "Array assignee has lower capacity than assigned value: " << lArrayCap << " < " << rArrayCap );
                    }
                }
            }
        }
    }
    else
        this->rvalue->insert_conversion( TXP_RESOLUTION, ltype );
}

void TxAssignStmtNode::verification_pass() const {
    // note: similar rules to passing function arg
    auto ltype = this->lvalue->qtype();
    if ( ltype->get_type_class() == TXTC_ARRAY ) {
        // special concreteness check since we allow assignment between arrays of different capacities (i.e. sizes)
        if ( ltype->is_type_generic() ) {
            if ( !this->context().is_generic() && !ltype->is_generic_param() )
                CERROR( this->lvalue, "Assignee is not a specific array type: " << ltype );
            else
                LOG_DEBUG( this->LOGGER(), this << " " << this->context().scope()
                           << " (Not error since generic context) Assignee is not a specific array type: " << ltype );
        }
    }
    else if ( is_not_properly_concrete( this, ltype ) ) {
        CERROR( this->lvalue, "Assignee is not concrete: " << ltype );
    }

//    if ( ltype->get_type_class() != TXTC_ELEMENTARY
//            && ltype->get_type_class() != TXTC_REFERENCE
//            && ltype->get_type_class() != TXTC_FUNCTION ) {
//        CERROR( this->lvalue, "Assignee is not Elementary / Reference / Function: " << ltype );
//    }

    if ( !( this->context().enclosing_lambda() && this->context().enclosing_lambda()->get_constructed() ) ) {
        // TODO: only members of constructed object should skip error
        if ( !lvalue->is_mutable() ) {
            // error message already generated
            //CERROR( this, "Assignee or assignee's container is not modifiable (nominal type of assignee is " << ltype << ")" );
        }

        // TODO: We don't want aggregate object assignment to violate potential immutability of members.
        //       So this assignment should only support Elementary and Ref types,
        //       and assignment to aggregates is only allowed via user-defined methods.
        //       How do we support it efficiently for arrays (of mutable elements)?
    }

    // if assignee is a reference:
    // TODO: check dataspace rules
}

//void TxArrayCopyStmtNode::resolution_pass() {
//    // This implementation relaxes the auto-conversion check to allow assignment of arrays with unknown C.
//    // We only handle auto-dereferencing of the rvalue:
//    auto ltype = this->lvalue->resolve_type( TXP_RESOLUTION );
//    auto rtype = this->rvalue->originalExpr->resolve_type( TXP_RESOLUTION );
//    if ( rtype->get_type_class() == TXTC_REFERENCE )
//        this->rvalue->insert_conversion( TXP_RESOLUTION, rtype->target_type() );
//    else
//        this->rvalue->resolve_type( TXP_RESOLUTION );
//}
//
//void TxArrayCopyStmtNode::verification_pass() const {
//    auto ltype = this->lvalue->qtype();
//    auto rtype = this->rvalue->qtype();
//
//    if ( ltype->get_type_class() != TXTC_ARRAY )
//        CERROR( this->lvalue, "Assignee is not an array: " << ltype );
//    if ( rtype->get_type_class() != TXTC_ARRAY )
//        CERROR( this->rvalue, "Assigned value is not an array: " << rtype );
//
//    // special concreteness check since we allow assignment between arrays of different capacities (i.e. sizes)
//    if ( ltype->is_type_generic() ) {
//        if ( !this->context().is_generic() && !ltype->is_generic_param() )
//            CERROR( this->lvalue, "Assignee is not a specific array type: " << ltype );
//        else
//            LOG_DEBUG( this->LOGGER(), this << " " << this->context().scope()
//                       << " (Not error since generic context) Assignee is not a specific array type: " << ltype );
//    }
//
//    if ( !( this->context().enclosing_lambda() && this->context().enclosing_lambda()->get_constructed() ) ) {
//        // TODO: only members of constructed object should skip error
//        if ( !lvalue->is_mutable() ) {
//            // error message already generated
//            //CERROR( this, "Assignee or assignee's container is not modifiable (nominal type of assignee is " << ltype << ")" );
//        }
//    }
//}

void TxExpErrStmtNode::stmt_declaration_pass() {
    //this->lexContext._scope = lexContext.scope()->create_code_block_scope( *this, "EE" );
    this->lexContext.expErrCtx = this->expError;
    if ( !this->context().is_reinterpretation() ) {
        this->get_parse_location().parserCtx->register_exp_err_node( this );
    }
}
