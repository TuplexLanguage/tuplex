#include "ast_stmts.hpp"

#include "ast_panicstmt_node.hpp"
#include "ast/expr/ast_lambda_node.hpp"
#include "ast/expr/ast_constexpr.hpp"
#include "ast/expr/ast_field.hpp"
#include "ast/expr/ast_ref.hpp"
#include "parsercontext.hpp"


void TxFieldStmtNode::stmt_declaration_pass() {
    this->successorScope = lexContext.scope()->create_code_block_scope( *this );
    this->fieldDef->declare_field( this, this->successorScope, this->declFlags, TXS_STACK );
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
    TxQualTypeExprNode* selfRefTargetTypeNode;
    if ( modifying || !this->context().enclosing_lambda()->get_constructed() )
        selfRefTargetTypeNode = new TxSetQualTypeExprNode( this->ploc, selfTypeNode, modifying );
    else
        // constructors are implicitly modifying if their type is mutable
        selfRefTargetTypeNode = new TxFlexModTypeExprNode( this->ploc, selfTypeNode );
    auto selfRefTypeExprN = new TxQualTypeExprNode( new TxReferenceTypeNode( this->ploc, nullptr, selfRefTargetTypeNode ) );
    this->selfRefNode = new TxLocalFieldDefNode( this->ploc, new TxIdentifierNode( this->ploc, "self" ), selfRefTypeExprN, nullptr );

    // 'super' reference
    auto superTypeNode = new TxNamedTypeNode( this->ploc, "Super" );
    TxQualTypeExprNode* superRefTargetTypeNode;
    if ( modifying || !this->context().enclosing_lambda()->get_constructed() )
        superRefTargetTypeNode = new TxSetQualTypeExprNode( this->ploc, superTypeNode, modifying );
    else
        // constructors are implicitly modifying if their type is mutable
        superRefTargetTypeNode = new TxFlexModTypeExprNode( this->ploc, superTypeNode );
    auto superRefTypeExprN = new TxQualTypeExprNode( new TxReferenceTypeNode( this->ploc, nullptr, superRefTargetTypeNode ) );
    this->superRefNode = new TxLocalFieldDefNode( this->ploc, new TxIdentifierNode( this->ploc, "super" ), superRefTypeExprN, nullptr );

    this->selfRefNode->declare_field( this, context().scope(), TXD_NONE, TXS_STACK );
    this->superRefNode->declare_field( this, context().scope(), TXD_NONE, TXS_STACK );
}

static TxMemProviderNode* make_member_expr( TxIdentifierNode* identifier ) {
    auto loc = identifier->ploc;
    if ( identifier->ident() == "self" || identifier->ident() == "super" )
        return new TxInPlaceAllocNode( loc, new TxReferenceDerefNode(
                loc, new TxNamedFieldNode( loc, new TxFieldValueNode( loc, nullptr, identifier ) ) ) );
    else {
        auto selfBase = new TxFieldValueNode( loc, nullptr, new TxIdentifierNode( loc, "self" ) );
        return new TxInPlaceAllocNode( loc, new TxNamedFieldNode( loc, new TxFieldValueNode( loc, selfBase, identifier ) ) );
    }
}


TxMemberInitNode::TxMemberInitNode( const TxLocation& ploc, TxIdentifierNode* identifier, const std::vector<TxExpressionNode*>* argsExprList )
        : TxStatementNode( ploc ), identifier( identifier ), argsExprList( argsExprList ),
          constructorCallExpr( new TxFunctionCallNode(
                ploc, new TxConstructorCalleeExprNode( ploc, make_member_expr( identifier ) ), argsExprList ) ) {
}

void TxMemberInitNode::visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) {
    if ( this->context().is_type_generic() || this->context().is_type_gen_dep_bindings() ) {
        // if the member to construct is TYPE parameter dependent, skip resolution and verification
        if ( this->constructorCallExpr->callee->is_context_set() ) {
            auto constrCallee = static_cast<TxConstructorCalleeExprNode*>( this->constructorCallExpr->callee );
            try {
                if ( constrCallee->get_constructed_type( TXP_TYPE )->is_generic_param() ) {
                    //std::cerr << this << " skipping visit of " << this->constructorCallExpr << std::endl;
                    return;
                }
            }
            catch ( const resolution_error& err ) {
                //LOG(this->LOGGER(), DEBUG, "Caught resolution error in " << this << ": " << err);
            }
        }
    }
    this->constructorCallExpr->visit_ast( visitor, thisCursor, "initializer", context );
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
    auto qtype = this->selfSuperStmt->resolve_type( TXP_RESOLUTION );
    if ( qtype->is_builtin() )
        return;

    if ( !this->initClauseList->empty() && this->initClauseList->front()->get_identifier()->ident() == "self" ) {
        if ( this->initClauseList->size() > 1)
            CERROR( this->initClauseList->front(), "self() can only be the sole initializer in an initializer list" );
        return;
    }

    bool superError = false;
    auto initClauseI = this->initClauseList->begin();
    if ( initClauseI == this->initClauseList->end() || (*initClauseI)->get_identifier()->ident() != "super" ) {
        auto superType = qtype->get_base_type();
        if ( superType->get_constructors().size() == 1
             && superType->get_constructors().front()->get_definer()->resolve_type( TXP_RESOLUTION )->argument_types().empty() ) {
            // syntactic sugar to implicitly prepend 'super()' if base type has a single no-args constructor
            LOG_DEBUG( this->LOGGER(), "Implicit invokation of super() from " << this );
            auto implicitSuper = new TxMemberInitNode( ploc, new TxIdentifierNode( ploc, "super" ),
                                                       new std::vector<TxExpressionNode*>() );
            run_declaration_pass( implicitSuper, this->context() );
            run_resolution_pass( implicitSuper );
            initClauseI = this->initClauseList->insert( initClauseI, implicitSuper );
            ++initClauseI;  // skips super() in loop
        }
        else {
            // look through other initializers to see if malpositioned or missing
            auto posI = std::find_if( this->initClauseList->cbegin(), this->initClauseList->cend(),
                                      []( TxMemberInitNode* init ) {
                                          return init->get_identifier()->ident() == "super";
                                      } );
            if ( posI != this->initClauseList->cend() )
                CERROR( *posI, "super() initializer is not in the first position" );
            else
                CERROR( this, "Missing super() initializer in initializer list" );
            superError = true;
        }
    }
    else
        ++initClauseI;  // skips super() in loop

    auto initFieldList = qtype->get_instance_fields_to_initialize();
    for ( auto fieldDecl : initFieldList ) {
        if ( fieldDecl->get_definer()->initExpression ) {
            if ( initClauseI != this->initClauseList->end()
                    && (*initClauseI)->get_identifier()->ident() == fieldDecl->get_unique_name() ) {
                CERROR( (*initClauseI), "Constructor may not initialize field '"
                        << (*initClauseI)->get_identifier()->ident() << "' that already has direct initializer" );
                ++initClauseI;
            }
            else {
                // insert implicit initializer for field that has direct initialization expression
                auto & iloc = fieldDecl->get_definer()->initExpression->ploc;
                auto initExpr = new TxExprWrapperNode( fieldDecl->get_definer()->initExpression );
                auto implicitInit = new TxMemberInitNode( iloc, new TxIdentifierNode( iloc, fieldDecl->get_unique_name() ),
                                                          new std::vector<TxExpressionNode*>( { initExpr } ) );
                run_declaration_pass( implicitInit, this->context() );
                run_resolution_pass( implicitInit );
                initClauseI = this->initClauseList->insert( initClauseI, implicitInit );
                ++initClauseI;
            }
        }
        else {
            if ( initClauseI != this->initClauseList->end()
                      && (*initClauseI)->get_identifier()->ident() == fieldDecl->get_unique_name() ) {
                ++initClauseI;
            }
            else {
                // look through other initializers to see if malpositioned or missing
                auto posI = std::find_if( this->initClauseList->cbegin(), this->initClauseList->cend(),
                                          [fieldDecl]( TxMemberInitNode* init ) {
                                              return init->get_identifier()->ident() == fieldDecl->get_unique_name();
                                          } );
                if ( posI != this->initClauseList->cend() )
                    CERROR( *posI, "Initializer for field '" << (*posI)->get_identifier()->ident() << "' in wrong position" );
                else
                    CERROR( this, "Missing field initializer in initializer list: " << fieldDecl->get_unique_full_name() );
            }
        }
    }
    for ( ; initClauseI != this->initClauseList->end(); ++initClauseI ) {
        auto initName = (*initClauseI)->get_identifier()->ident();
        if ( initName == "super" ) {
            if ( !superError )
                CERROR( (*initClauseI), "super() initializer not in first position" );
        }
        else if ( initName == "self" ) {
            CERROR( this->initClauseList->front(), "self() can only be the sole initializer in an initializer list" );
        }
        else {
            auto posI = std::find_if( initFieldList.cbegin(), initFieldList.cend(),
                                      [initName]( const TxFieldDeclaration* fieldDecl ) {
                                          return initName == fieldDecl->get_unique_name();
                                      } );
            if ( posI == initFieldList.cend() )
                CERROR( (*initClauseI), "Unknown / invalid field initializer: " << initName );
        }
    }
}

void TxInitStmtNode::verification_pass() const {
    if ( !this->context().enclosing_lambda()->get_constructed() ) {
        CERROR( this, "Initializer / constructor invocation statements may not be used in non-constructor functions" );
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
