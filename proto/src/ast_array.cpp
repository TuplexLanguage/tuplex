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
        : TxExpressionNode( parseLocation ), origElemExprList( elemExprList ), lengthExpr( lengthExpr ), elemExprList( make_args_vec( elemExprList ) )
{
    this->elementTypeNode = new TxTypeTypeArgumentNode( elementTypeExpr );

    if (! lengthExpr)
        lengthExpr = new TxIntegerLitNode( this->parseLocation, elemExprList->size(), false, UINT );
    this->lengthNode = new TxValueTypeArgumentNode( lengthExpr );
}

TxArrayLitNode::TxArrayLitNode( const TxLocation& parseLocation, const std::vector<TxExpressionNode*>* elemExprList )
        : TxArrayLitNode( parseLocation, new TxTypeExprWrapperNode( elemExprList->at(0) ), elemExprList )
{
}

TxArrayLitNode::TxArrayLitNode( const TxLocation& parseLocation, const std::vector<TxMaybeConversionNode*>* elemExprList )
        : TxExpressionNode( parseLocation ), origElemExprList( nullptr ), lengthExpr( nullptr ), elemExprList( elemExprList ) {
    ASSERT(elemExprList && !elemExprList->empty(), "Array literals must have at least one element");

    TxTypeExprWrapperNode* elementTypeExpr = new TxTypeExprWrapperNode( elemExprList->at( 0 )->originalExpr );
    this->elementTypeNode = new TxTypeTypeArgumentNode( elementTypeExpr );

    TxExpressionNode* lengthExpr = new TxIntegerLitNode( this->parseLocation, elemExprList->size(), false, UINT );
    this->lengthNode = new TxValueTypeArgumentNode( lengthExpr );
}


const TxType* TxArrayLitNode::define_type() {
    auto elemType = this->elementTypeNode->typeExprNode->resolve_type();
    for ( auto elemExpr : *this->elemExprList )
        elemExpr->insert_conversion( elemType );
    return this->registry().get_array_type( this, this->elementTypeNode, this->lengthNode );
}


static bool is_unsigned_integer( BuiltinTypeId typeId ) {
    switch ( typeId ) {
    case UNSIGNED:
    case UBYTE:
    case USHORT:
    case UINT:
    case ULONG:
        return true;
    default:
        return false;
    }
}

void TxArrayLitNode::symbol_resolution_pass() {
    TxExpressionNode::symbol_resolution_pass();
    this->elementTypeNode->symbol_resolution_pass();
    this->lengthNode->symbol_resolution_pass();
    for (auto elemExpr : *this->elemExprList)
        elemExpr->symbol_resolution_pass();

    if (this->lengthExpr) {
        if (!this->lengthExpr->is_statically_constant())
            CERROR(this, "Length expression of array literal is not statically constant");
        else if (!is_unsigned_integer( (BuiltinTypeId)this->lengthExpr->get_type()->get_type_id() ))
            CERROR(this, "Length expression of array literal is not an unsigned integer type: " << this->lengthExpr->get_type());
        else if (auto constProxy = this->lengthExpr->get_static_constant_proxy()) {
            if (constProxy->get_value_UInt() != this->elemExprList->size())
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
