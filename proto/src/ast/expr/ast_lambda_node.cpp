#include "ast_lambda_node.hpp"

//#include "ast/type/ast_qualtypes.hpp"


TxLambdaExprNode::TxLambdaExprNode( const TxLocation& ploc, TxFunctionHeaderNode* funcHeaderNode, TxSuiteNode* suite,
                                    bool isMethodSyntax, bool suppressSuper )
        : TxExpressionNode( ploc ), funcHeaderNode( funcHeaderNode ), suite( suite ), isMethodSyntax( isMethodSyntax ) {
    if ( isMethodSyntax ) {
        // 'self' reference:
        this->selfTypeNode = new TxNamedTypeNode( this->ploc, "Self" );
        auto selfRefTargetTypeNode = new TxSetQualTypeExprNode( this->ploc, this->selfTypeNode, this->funcHeaderNode->is_modifying() );
        auto selfRefTypeExprN = new TxQualTypeExprNode( new TxReferenceTypeNode( this->ploc, nullptr, selfRefTargetTypeNode ) );
        this->selfRefNode = new TxLocalFieldDefNode( this->ploc, new TxIdentifierNode( this->ploc, "self" ), selfRefTypeExprN, nullptr );

        // 'super' reference
        if ( !suppressSuper ) {
            auto superTypeNode = new TxNamedTypeNode( this->ploc, "Super" );
            auto superRefTargetTypeNode = new TxSetQualTypeExprNode( this->ploc, superTypeNode, this->funcHeaderNode->is_modifying() );
            auto superRefTypeExprN = new TxQualTypeExprNode( new TxReferenceTypeNode( this->ploc, nullptr, superRefTargetTypeNode ) );
            this->superRefNode = new TxLocalFieldDefNode( this->ploc, new TxIdentifierNode( this->ploc, "super" ), superRefTypeExprN, nullptr );
        }
    }
}

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
        if ( this->superRefNode )
            this->superRefNode->declare_field( funcScope, TXD_NONE, TXS_STACK );
    }
    this->lexContext._scope = funcScope;
    this->lexContext.enclosingLambda = this;
    // FUTURE: define implicit closure object when in code block
}

bool TxLambdaExprNode::is_suppressed_modifying_method() const {
    return ( this->funcHeaderNode->is_modifying()
             && this->context().reinterpretation_definer()
             && !this->selfTypeNode->resolve_type( TXP_TYPE )->is_mutable() );
}

//void TxLambdaExprNode::resolution_pass() {
//    TxExpressionNode::resolution_pass();
//    if ( this->is_instance_method() ) {
//        if ( this->is_suppressed_modifying_method() ) {
//            // modifying methods in an immutable specialization of a generic type are effectively 'abstract'
//            //this->funcHeaderNode->resolution_pass();  // function header
//            return;
//        }
//        try {
//            this->selfRefNode->resolution_pass();
//            if ( this->superRefNode )
//                this->superRefNode->resolution_pass();
//        }
//        catch ( const resolution_error& err ) {
//            LOG( this->LOGGER(), DEBUG, "Caught resolution error in self/super of instance method " << this << ": " << err );
//        }
//    }
//    this->funcHeaderNode->resolution_pass();  // function header
//    this->suite->resolution_pass();  // function body
//}

void TxLambdaExprNode::verification_pass() const {
    if ( !this->is_instance_method() ) {
        if ( this->funcHeaderNode->is_modifying() )
            CERROR( this, "Only instance methods can be declared 'modifying': " << this->funcHeaderNode->qtype() );
        // TODO: review if there are non-method functions with closure that can declare 'modifying'?
    }
    if ( this->funcHeaderNode->returnField ) {
        // verify that body always ends with explicit return statement
        if ( !this->suite->ends_with_return_stmt() )
            CERROR( this, "Function has return value, but not all code paths end with a return statement." );
    }
}

void TxLambdaExprNode::visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) {
    this->funcHeaderNode->visit_ast( visitor, thisCursor, "functype", context );
    if ( this->is_instance_method() ) {
        if ( this->selfRefNode->is_context_set() && this->is_suppressed_modifying_method() )
            return;
        this->selfRefNode->visit_ast( visitor, thisCursor, "selfref", context );
        if ( this->superRefNode )
            this->superRefNode->visit_ast( visitor, thisCursor, "superref", context );
    }
    this->suite->visit_ast( visitor, thisCursor, "suite", context );
}
