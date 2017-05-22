#include "ast_array.hpp"
#include "ast_wrappers.hpp"
#include "ast_lit.hpp"

// Note: similar to helper function in function call node
static std::vector<TxMaybeConversionNode*>* make_args_vec( const std::vector<TxExpressionNode*>* argsExprList ) {
    std::vector<TxMaybeConversionNode*>* copyVec = new std::vector<TxMaybeConversionNode*>( argsExprList->size() );
    std::transform( argsExprList->cbegin(), argsExprList->cend(), copyVec->begin(),
                    []( TxExpressionNode* n ) -> TxMaybeConversionNode* {return new TxMaybeConversionNode( n );} );
    return copyVec;
}

TxArrayLitNode::TxArrayLitNode( const TxLocation& parseLocation, TxTypeExpressionNode* elementTypeExpr,
                                const std::vector<TxExpressionNode*>* elemExprList,
                                TxExpressionNode* lengthExpr )
        : TxExpressionNode( parseLocation ), origElemExprList( elemExprList ),
          elementTypeNode( elementTypeExpr ? new TxTypeTypeArgumentNode( elementTypeExpr ) : nullptr ),
          lengthExpr( lengthExpr ? new TxMaybeConversionNode( lengthExpr ) : nullptr ), elemExprList( make_args_vec( elemExprList ) )
{
}

TxArrayLitNode::TxArrayLitNode( const TxLocation& parseLocation, const std::vector<TxExpressionNode*>* elemExprList )
        : TxArrayLitNode( parseLocation, nullptr, elemExprList )
{
}

TxArrayLitNode::TxArrayLitNode( const TxLocation& parseLocation, const std::vector<TxMaybeConversionNode*>* elemExprList )
        : TxArrayLitNode( parseLocation, nullptr, elemExprList )
{
}

TxArrayLitNode::TxArrayLitNode( const TxLocation& parseLocation, TxTypeExpressionNode* elementTypeExpr,
                                const std::vector<TxMaybeConversionNode*>* elemExprList )
        : TxExpressionNode( parseLocation ), origElemExprList( nullptr ),
          elementTypeNode( elementTypeExpr ? new TxTypeTypeArgumentNode( elementTypeExpr ) : nullptr ),
          lengthExpr( nullptr ), elemExprList( elemExprList ) {
}

const TxType* TxArrayLitNode::define_type() {
    const TxType* expectedArgType;
    const TxType* arrayType = nullptr;
    if ( this->lengthExpr ) {
        auto elemTypeNode = this->elementTypeNode;
        if ( !elemTypeNode ) {
            elemTypeNode = new TxTypeTypeArgumentNode( new TxTypeExprWrapperNode( elemExprList->front()->originalExpr ) );
            run_declaration_pass( elemTypeNode, this, "elem-type" );
        }
        this->lengthExpr->insert_conversion( this->registry().get_builtin_type( ARRAY_SUBSCRIPT_TYPE_ID ) );
        auto lengthNode = new TxValueTypeArgumentNode( this->lengthExpr );
        lengthNode->node_declaration_pass( this );
        arrayType = this->registry().get_array_type( this, elemTypeNode, lengthNode );
        if ( this->elemExprList->size() == 1
             && get_reinterpretation_degree( arrayType, this->elemExprList->front()->originalExpr->resolve_type() ) >= 0 ) {
            // treat as array to array assignment
            this->_directArrayArg = true;
            expectedArgType = arrayType;
        }
        else
            expectedArgType = arrayType->element_type();
    }
    else {
        if ( this->elemExprList->size() == 1 ) {
            auto singleArgType = this->elemExprList->front()->originalExpr->resolve_type();
            if ( singleArgType->get_type_class() == TXTC_REFERENCE ) {
                singleArgType = singleArgType->target_type();  // auto-dereferencing
            }
            if ( singleArgType->get_type_class() == TXTC_ARRAY ) {
                auto argElemType = singleArgType->element_type();
                if ( this->elementTypeNode && argElemType->is_assignable_to( *this->elementTypeNode->typeExprNode->resolve_type() ) ) {
                    if ( static_cast<const TxArrayType*>( singleArgType->type() )->length() ) {
                        // concrete array length - treat as array to array assignment
                        this->_directArrayArg = true;
                        arrayType = singleArgType;
                        expectedArgType = arrayType;
                    }
                }
            }
        }

        if ( !arrayType ) {
            auto elemTypeNode = this->elementTypeNode;
            if ( !elemTypeNode ) {
                elemTypeNode = new TxTypeTypeArgumentNode( new TxTypeExprWrapperNode( elemExprList->front()->originalExpr ) );
                run_declaration_pass( elemTypeNode, this, "elem-type" );
            }
            auto tmpLengthExpr = new TxIntegerLitNode( this->parseLocation, elemExprList->size(), false, TXBT_UINT );
            auto lengthNode = new TxValueTypeArgumentNode( tmpLengthExpr );
            run_declaration_pass( lengthNode, this, "length" );
            arrayType = this->registry().get_array_type( this, elemTypeNode, lengthNode );
            expectedArgType = arrayType->element_type();
        }
    }

    for ( auto elemExpr : *this->elemExprList )
        elemExpr->insert_conversion( expectedArgType );
    return arrayType;
}

void TxArrayLitNode::symbol_resolution_pass() {
    TxExpressionNode::symbol_resolution_pass();
    if ( this->elementTypeNode )
        this->elementTypeNode->symbol_resolution_pass();
    for ( auto elemExpr : *this->elemExprList )
        elemExpr->symbol_resolution_pass();

    if ( this->lengthExpr ) {
        this->lengthExpr->symbol_resolution_pass();

        if ( this->lengthExpr->is_statically_constant() ) {
            if ( this->elemExprList->size() == 1 && this->elemExprList->front()->get_type()->is_assignable_to( *this->get_type() ) ) {
                // array to array assignment
            }
            else if ( eval_UInt_constant( this->lengthExpr ) != this->elemExprList->size() )
                CERROR( this, "length expression of array literal equals " << eval_UInt_constant( this->lengthExpr )
                        << ", but number of elements is " << this->elemExprList->size() );
        }
        else  // TODO: support dynamic array literals (which requires supporting partially filled arrays)
            CERROR( this, "Length expression of array literal is not statically constant" );
    }
    this->_constant = true;
    for ( auto arg : *this->elemExprList ) {
        if ( !arg->is_statically_constant() ) {
            this->_constant = false;
            break;
        }
    }
}
