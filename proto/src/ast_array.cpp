#include "ast_array.hpp"
#include "ast_wrappers.hpp"
#include "ast_lit.hpp"


// Note: similar to helper function in function call node
static std::vector<TxMaybeConversionNode*>* make_args_vec( const std::vector<TxExpressionNode*>* argsExprList ) {
    std::vector<TxMaybeConversionNode*>* copyVec = new std::vector<TxMaybeConversionNode*>( argsExprList->size() );
    std::transform( argsExprList->cbegin(), argsExprList->cend(), copyVec->begin(),
                    []( TxExpressionNode* n ) -> TxMaybeConversionNode*  {  return new TxMaybeConversionNode( n );  } );
    return copyVec;
}


TxArrayLitNode::TxArrayLitNode( const TxLocation& parseLocation, TxTypeExpressionNode* elementTypeExpr,
                                const std::vector<TxExpressionNode*>* elemExprList, TxExpressionNode* lengthExpr )
        : TxExpressionNode( parseLocation ), origElemExprList( elemExprList ),
          elementTypeNode( elementTypeExpr ? new TxTypeTypeArgumentNode( elementTypeExpr ) : nullptr ),
          lengthExpr( lengthExpr ), elemExprList( make_args_vec( elemExprList ) )
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



void TxArrayLitNode::symbol_declaration_pass( LexicalContext& lexContext) {
    this->set_context( lexContext);
    if (this->elementTypeNode)
        this->elementTypeNode->symbol_declaration_pass( lexContext, lexContext );
    if (this->origElemExprList) {
        // if this node owns the element nodes, perform declaration pass on them:
        for (auto elemExpr : *this->elemExprList)
            elemExpr->symbol_declaration_pass( lexContext);
    }
}


const TxType* TxArrayLitNode::define_type() {
    const TxType* expectedArgType;
    const TxType* arrayType = nullptr;
    if (this->lengthExpr) {
        auto elemTypeNode = this->elementTypeNode ? this->elementTypeNode
                                : new TxTypeTypeArgumentNode( new TxTypeExprWrapperNode( elemExprList->front()->originalExpr ) );
        auto lengthNode = new TxValueTypeArgumentNode( this->lengthExpr );
        lengthNode->symbol_declaration_pass( this->context(), this->context() );
        arrayType = this->registry().get_array_type( this, elemTypeNode, lengthNode );
        if (this->elemExprList->size() == 1 && get_reinterpretation_degree( arrayType, this->elemExprList->front()->originalExpr->resolve_type() ) >= 0) {
            // treat as array to array assignment
            this->_directArrayArg = true;
            expectedArgType = arrayType;
        }
        else
            expectedArgType = arrayType->element_type();
    }
    else {
        if (this->elemExprList->size() == 1) {
            auto singleArgType = this->elemExprList->front()->originalExpr->resolve_type();
            if (singleArgType->get_type_class() == TXTC_REFERENCE) {
                singleArgType = singleArgType->target_type();  // auto-dereferencing
            }
            if (singleArgType->get_type_class() == TXTC_ARRAY) {
                auto argElemType = singleArgType->element_type();
                if (this->elementTypeNode && argElemType->is_assignable_to( *this->elementTypeNode->typeExprNode->resolve_type() )) {
                    if (static_cast<const TxArrayType*>(singleArgType->type())->length()) {
                        // concrete array length - treat as array to array assignment
                        this->_directArrayArg = true;
                        arrayType = singleArgType;
                        expectedArgType = arrayType;
                    }
                }
            }
        }

        if (! arrayType) {
            auto elemTypeNode = this->elementTypeNode;
            if (!elemTypeNode) {
                elemTypeNode = new TxTypeTypeArgumentNode( new TxTypeExprWrapperNode( elemExprList->front()->originalExpr ) );
                elemTypeNode->symbol_declaration_pass( this->context(), this->context() );
            }
            auto tmpLengthExpr = new TxIntegerLitNode( this->parseLocation, elemExprList->size(), false, TXBT_UINT );
            auto lengthNode = new TxValueTypeArgumentNode( tmpLengthExpr );
            lengthNode->symbol_declaration_pass( this->context(), this->context() );
            arrayType = this->registry().get_array_type( this, elemTypeNode, lengthNode );
            expectedArgType = arrayType->element_type();
        }
    }

    for ( auto elemExpr : *this->elemExprList )
        elemExpr->insert_conversion( expectedArgType );
    return arrayType;
}


static bool is_unsigned_integer( BuiltinTypeId typeId ) {
    switch ( typeId ) {
    case TXBT_UNSIGNED:
    case TXBT_UBYTE:
    case TXBT_USHORT:
    case TXBT_UINT:
    case TXBT_ULONG:
        return true;
    default:
        return false;
    }
}

void TxArrayLitNode::symbol_resolution_pass() {
    TxExpressionNode::symbol_resolution_pass();
    if (this->elementTypeNode)
        this->elementTypeNode->symbol_resolution_pass();
    for (auto elemExpr : *this->elemExprList)
        elemExpr->symbol_resolution_pass();

    if (this->lengthExpr) {
        this->lengthExpr->symbol_resolution_pass();

        if (!this->lengthExpr->is_statically_constant())
            CERROR(this, "Length expression of array literal is not statically constant");
        else if (!is_unsigned_integer( (BuiltinTypeId)this->lengthExpr->get_type()->get_type_id() ))
            CERROR(this, "Length expression of array literal is not an unsigned integer type: " << this->lengthExpr->get_type());
        else if (auto constProxy = this->lengthExpr->get_static_constant_proxy()) {
            if (this->elemExprList->size() == 1 && this->elemExprList->front()->get_type()->is_assignable_to( *this->get_type() )) {
                // array to array assignment
            }
            else if (constProxy->get_value_UInt() != this->elemExprList->size())
                CERROR(this, "length expression of array literal equals " << constProxy->get_value_UInt()
                             << ", but number of elements is " << this->elemExprList->size());
        }
    }
    this->_constant = true;
    for (auto arg : *this->elemExprList) {
        if (!arg->is_statically_constant()) {
            this->_constant = false;
            break;
        }
    }
}
