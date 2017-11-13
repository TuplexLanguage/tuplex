#include "ast_stmts.hpp"

#include "ast_panicstmt_node.hpp"
#include "ast/expr/ast_lambda_node.hpp"
#include "ast/expr/ast_constexpr.hpp"
#include "ast/expr/ast_field.hpp"
#include "ast/expr/ast_ref.hpp"
#include "parsercontext.hpp"


void TxFieldStmtNode::stmt_declaration_pass() {
    this->successorScope = lexContext.scope()->create_code_block_scope( *this );
    this->fieldDef->declare_field( this->successorScope, this->declFlags, TXS_STACK );
    // (to prevent init expr from referencing this field, it is processed in the 'outer' scope, not in the new block scope)
}

void TxFieldStmtNode::verification_pass() const {
    if ( !this->fieldDef->initExpression ) {
        // TODO: Allow implicit initialization if type has default constructor?
        // FUTURE: instead check that TXS_STACK fields are initialized before first use?
        CERROR( this, "Field has no initializer: " << this->fieldDef->get_descriptor() );
    }
}

TxSelfSuperFieldsStmtNode::TxSelfSuperFieldsStmtNode( const TxLocation& ploc )
        : TxStatementNode( ploc ), selfRefNode(), superRefNode() {
}

void TxSelfSuperFieldsStmtNode::stmt_declaration_pass() {
    bool modifying = this->context().enclosing_lambda()->funcHeaderNode->is_modifying();

    // 'self' reference:
    auto selfTypeNode = new TxNamedTypeNode( this->ploc, "Self" );
    auto selfRefTargetTypeNode = new TxSetQualTypeExprNode( this->ploc, selfTypeNode, modifying );
    auto selfRefTypeExprN = new TxQualTypeExprNode( new TxReferenceTypeNode( this->ploc, nullptr, selfRefTargetTypeNode ) );
    this->selfRefNode = new TxLocalFieldDefNode( this->ploc, new TxIdentifierNode( this->ploc, "self" ), selfRefTypeExprN, nullptr );

    // 'super' reference
    auto superTypeNode = new TxNamedTypeNode( this->ploc, "Super" );
    auto superRefTargetTypeNode = new TxSetQualTypeExprNode( this->ploc, superTypeNode, modifying );
    auto superRefTypeExprN = new TxQualTypeExprNode( new TxReferenceTypeNode( this->ploc, nullptr, superRefTargetTypeNode ) );
    this->superRefNode = new TxLocalFieldDefNode( this->ploc, new TxIdentifierNode( this->ploc, "super" ), superRefTypeExprN, nullptr );

    this->selfRefNode->declare_field( context().scope(), TXD_NONE, TXS_STACK );
    this->superRefNode->declare_field( context().scope(), TXD_NONE, TXS_STACK );
}

static TxMemProviderNode* make_member_expr( TxIdentifierNode* identifier ) {
    if ( identifier->ident() == "self" || identifier->ident() == "super" )
        return new TxInPlaceAllocNode( identifier->ploc,
                                       new TxReferenceDerefNode( identifier->ploc,
                                                                 new TxFieldValueNode( identifier->ploc, nullptr, identifier ) ) );
    else
        return new TxInPlaceAllocNode( identifier->ploc,
                                       new TxFieldValueNode( identifier->ploc,
                                                             new TxFieldValueNode( identifier->ploc, "self" ), identifier ) );
}


TxMemberInitNode::TxMemberInitNode( const TxLocation& ploc, TxIdentifierNode* identifier, const std::vector<TxExpressionNode*>* argsExprList )
        : TxStatementNode( ploc ), identifier( identifier ), argsExprList( argsExprList ),
          constructorCallExpr( new TxFunctionCallNode(
                ploc, new TxConstructorCalleeExprNode( ploc, make_member_expr( identifier ) ), argsExprList ) ) {
}

TxInitStmtNode::TxInitStmtNode( const TxLocation& ploc, std::vector<TxMemberInitNode*>* initClauseList )
        : TxStatementNode( ploc ), selfSuperStmt( new TxSelfSuperFieldsStmtNode( ploc ) ), initClauseList( initClauseList )  {
}

TxInitStmtNode::TxInitStmtNode( const TxLocation& ploc, std::vector<TxExpressionNode*>* argsExprList )
        : TxInitStmtNode( ploc, new std::vector<TxMemberInitNode*>( {
                new TxMemberInitNode( ploc, new TxIdentifierNode( ploc, "self" ), argsExprList )
            } ) ) {
}

TxInitStmtNode::TxInitStmtNode( const TxLocation& ploc )
        : TxInitStmtNode( ploc, new std::vector<TxMemberInitNode*>(
                { new TxMemberInitNode( ploc, new TxIdentifierNode( ploc, "super" ),
                                        new std::vector<TxExpressionNode*>() ) } ) ) {
}

void TxInitStmtNode::stmt_declaration_pass() {
    // sets the new scope containing 'self' and 'super' both for
    // sub-statements (the initializer list) and successor statements
    this->lexContext._scope = lexContext.scope()->create_code_block_scope( *this );
    this->successorScope = this->lexContext._scope;
}

void TxInitStmtNode::resolution_pass() {
    if ( this->initClauseList->empty()
         || ( this->initClauseList->front()->get_identifier()->ident() != "super"
              && this->initClauseList->front()->get_identifier()->ident() != "self" ) ) {
        auto qtype = this->selfSuperStmt->resolve_type( TXP_RESOLUTION );
        auto superType = qtype->get_base_type();
        if ( superType->get_constructors().size() == 1
                && superType->get_constructors().front()->get_definer()->resolve_type( TXP_RESOLUTION )->argument_types().empty() ) {
            // syntactic sugar to implicitly prepend 'super()' if base type has a single no-args constructor
            LOG_DEBUG( this->LOGGER(), "Implicit invokation of super() from " << this );
            auto implicitSuper = new TxMemberInitNode( ploc, new TxIdentifierNode( ploc, "super" ),
                                                       new std::vector<TxExpressionNode*>() );
            run_declaration_pass( implicitSuper, this->context() );
            run_resolution_pass( implicitSuper );
            this->initClauseList->insert( this->initClauseList->begin(), implicitSuper ) ;
        }
    }
}

void TxInitStmtNode::verification_pass() const {
    if ( !this->context().enclosing_lambda()->get_constructed() ) {
        CERROR( this, "Initializer / constructor invocation statements may not be used in non-constructor functions" );
    }
    // verify initializer list:
    auto qtype = this->selfSuperStmt->qtype();
    if ( !qtype->is_builtin() ) {
        unsigned fieldIx = 0;
        bool hasSuper = false;
        auto initFieldList = qtype->get_instance_fields_to_initialize();
        std::vector<unsigned> initFieldErrIxList;
        for ( unsigned clauseIx = 0; clauseIx < this->initClauseList->size(); ++clauseIx ) {
            auto initNode = this->initClauseList->at( clauseIx );
            auto initName = initNode->get_identifier()->ident();
            if ( initName == "self" ) {
                if ( this->initClauseList->size() > 1)
                    CERROR( initNode, "'self' can only be the sole initializer in an initializer list" );
                else
                    return;
            }
            else if ( initName == "super" ) {
                hasSuper = true;
                if ( clauseIx != 0 )
                    CERROR( initNode, "'super' initializer must be first in initializer list" );
            }
            else {
                bool present = false;
                bool wrongPos = false;
                for ( unsigned i = fieldIx; i < initFieldList.size(); i++ ) {
                    if ( initName == initFieldList.at( i )->get_unique_name() ) {
                        present = true;
                        if ( initFieldList.at( i )->get_definer()->initExpression )
                            CERROR( initNode, "Constructor may not initialize field '" << initName << "' that already has direct initializer" );
                        else if ( wrongPos ) {
                            CERROR( initNode, "Initializer for field '" << initName << "' in wrong position" );
                            initFieldErrIxList.push_back( i );
                        }
                        else
                            ++fieldIx;
                        break;
                    }
                    else if ( initFieldList.at( i )->get_definer()->initExpression ) {
                        if ( !wrongPos )
                            ++fieldIx;
                    }
                    else
                        wrongPos = true;
                }
                if ( !present )
                    CERROR( this, "Unknown / invalid field initializer: " << initName );
            }
        }
        if ( !hasSuper )
            CERROR( this, "Missing 'super' initializer in initializer list" );
        for ( unsigned i = fieldIx; i < initFieldList.size(); i++ ) {
            if ( !initFieldList.at( i )->get_definer()->initExpression ) {
                if ( std::find( initFieldErrIxList.cbegin(), initFieldErrIxList.cend(), i ) == initFieldErrIxList.cend() )
                    CERROR( this, "Missing field initializer in initializer list: " << initFieldList.at( i )->get_unique_full_name() );
            }
        }
    }
}

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
        // (Sufficient capacity is checked in runtime.)
        auto rtype = this->rvalue->originalExpr->resolve_type( TXP_RESOLUTION );
        if ( rtype->get_type_class() == TXTC_REFERENCE ) {
            this->rvalue->insert_conversion( TXP_RESOLUTION, rtype->target_type() );
            rtype = this->rvalue->qtype();
        }
        else
            this->rvalue->resolve_type( TXP_RESOLUTION );
    }
    else
        this->rvalue->insert_conversion( TXP_RESOLUTION, ltype );
}

void TxAssignStmtNode::verification_pass() const {
    // note: similar rules to passing function arg
    if ( auto ltype = this->lvalue->attempt_qtype() ) {
        if ( ltype->get_type_class() == TXTC_ARRAY ) {
            // special check since we allow assignment between arrays of different capacities (i.e. sizes)
            verify_array_assignment( this, ltype.type(), this->rvalue->qtype().type() );
        }
        else if ( is_not_properly_concrete( this, ltype ) ) {
            CERROR( this->lvalue, "Assignee is not concrete: " << ltype );
        }
    }

    auto enclLambda = this->context().enclosing_lambda();
    if ( !( enclLambda && enclLambda->get_constructed()
            && ( enclLambda->get_field_def_node()->get_declaration()->get_decl_flags() & ( TXD_IMPLICIT | TXD_BUILTIN ) ) ) ) {
        // only statements of the built-in and implicit constructors (initializers) may ignore non-modifiability
        if ( !lvalue->is_mutable() ) {
            // Note, error message already generated
            //CERROR( this, "Assignee or assignee's container is not modifiable (nominal type of assignee is " << ltype << ")" );
        }
    }

    // if assignee is a reference:
    // TODO: check dataspace rules
}

void TxExpErrStmtNode::stmt_declaration_pass() {
    //this->lexContext._scope = lexContext.scope()->create_code_block_scope( *this, "EE" );
    this->lexContext.expErrCtx = this->expError;
    if ( !this->context().is_reinterpretation() ) {
        this->get_parse_location().parserCtx->register_exp_err_node( this );
    }
}
