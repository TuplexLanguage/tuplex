#include "ast_lambda_node.hpp"

#include "ast/stmt/ast_flow.hpp"


TxLambdaExprNode::TxLambdaExprNode( const TxLocation& ploc, TxFunctionHeaderNode* funcHeaderNode, TxStatementNode* body,
                                    bool isMethodSyntax )
        : TxExpressionNode( ploc ), funcHeaderNode( funcHeaderNode ), body( body ), isMethodSyntax( isMethodSyntax ) {
}

void TxLambdaExprNode::declaration_pass() {
    std::string funcName = ( this->fieldDefNode && this->fieldDefNode->get_declaration() )
                                  ? this->fieldDefNode->get_declaration()->get_unique_name()
                                  : "";
    TxScopeSymbol* funcScope = lexContext.scope()->create_code_block_scope( *this, funcName );
    if ( this->is_instance_method() ) {
        auto entitySym = dynamic_cast<TxEntitySymbol*>( lexContext.scope() );
        if ( entitySym && entitySym->get_type_decl() ) {  // if in type scope
            this->selfTypeNode = new TxNamedTypeNode( this->ploc, "Self" );
            if ( this->fieldDefNode->get_declaration()->get_decl_flags() & TXD_CONSTRUCTOR ) {
                // this is a constructor
                this->constructedObjTypeDecl = entitySym->get_type_decl();
            }
            else {
                // non-constructor instance method
                this->selfSuperStmt = new TxSelfSuperFieldsStmtNode( this->ploc );
            }
        }
        else
            CERROR( this, "The scope of instance method must be a type scope: " << lexContext.scope() );
    }
    this->lexContext._scope = funcScope;
    this->lexContext.enclosingLambda = this;
    // FUTURE: define implicit closure object when in code block
}

bool TxLambdaExprNode::is_suppressed_modifying_method() const {
    return ( this->funcHeaderNode->is_modifying()
             && this->context().reinterpretation_definer()
             && this->selfTypeNode && !this->selfTypeNode->resolve_type( TXP_TYPE )->is_mutable() );
}

static unsigned check_initializer_count( const TxStatementNode* stmt, unsigned count ) {
    if ( auto suite = dynamic_cast<const TxSuiteNode*>( stmt ) ) {
        for ( auto subStmt : *suite->suite ) {
            count += check_initializer_count( subStmt, count );
        }
    }
    else if ( auto ifStmt = dynamic_cast<const TxIfStmtNode*>( stmt ) ) {
        unsigned trueCount = check_initializer_count( ifStmt->get_true_body(), count );
        unsigned falseCount = ( ifStmt->get_else_body() ? check_initializer_count( ifStmt->get_else_body(), count ) : 0 );
        if ( trueCount != falseCount )
            CERROR( stmt, "Mismatching number of invocations of other initializers/constructors in the if/else code paths" );
        count += std::max( trueCount, falseCount );
    }
    else if ( dynamic_cast<const TxInitStmtNode*>( stmt ) ) {
        if ( count )
            CERROR( stmt, "Constructor body may not invoke another initializer/constructor more than once" );
        count += 1;
    }
    else if ( auto errStmt = dynamic_cast<const TxExpErrStmtNode*>( stmt ) ) {
        count += check_initializer_count( errStmt->body, count );
    }
    return count;
}

void TxLambdaExprNode::verification_pass() const {
    if ( !this->is_instance_method() ) {
        if ( this->funcHeaderNode->is_modifying() )
            CERROR( this, "Only instance methods can be declared 'modifying': " << this->funcHeaderNode->qtype() );
        // TODO: review if there are non-method functions with closure that can declare 'modifying'?
    }
    else if ( this->constructedObjTypeDecl ) {
        if ( !( this->fieldDefNode->get_declaration()->get_decl_flags() & ( TXD_BUILTIN | TXD_IMPLICIT ) ) ) {
            // this is a user-defined constructor
            // verify that exactly one #init or #self statement will be run:
            auto initCount = check_initializer_count( this->body, 0 );
            if ( !initCount )
                CERROR( this, "Constructor body does not invoke any implicit initializer or other user-defined constructor" );
        }
    }
    if ( this->funcHeaderNode->returnField ) {
        // verify that body always ends with explicit return statement
        if ( !this->body->ends_with_return_stmt() )
            CERROR( this, "Function has return value, but not all code paths end with a return statement." );
    }
}

void TxLambdaExprNode::visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) {
    this->funcHeaderNode->visit_ast( visitor, thisCursor, "functype", context );
    if ( this->selfTypeNode )
        this->selfTypeNode->visit_ast( visitor, thisCursor, "selftype", context );
    if ( this->selfSuperStmt ) {
        if ( this->selfSuperStmt->is_context_set() && this->is_suppressed_modifying_method() )
            return;
        this->selfSuperStmt->visit_ast( visitor, thisCursor, "selffield", context );
    }
    this->body->visit_ast( visitor, thisCursor, "body", context );
}
