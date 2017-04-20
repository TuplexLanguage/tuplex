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


TxArrayLitNode::TxArrayLitNode( const TxLocation& parseLocation, const std::vector<TxExpressionNode*>* elemExprList )
        : TxExpressionNode( parseLocation ), origElemExprList( elemExprList ), elemExprList( make_args_vec( elemExprList ) ) {
    ASSERT(elemExprList && !elemExprList->empty(), "Array literals must have at least one element");

    TxTypeExprWrapperNode* elementTypeExpr = new TxTypeExprWrapperNode( elemExprList->at( 0 ) );
    this->elementTypeNode = new TxTypeTypeArgumentNode( elementTypeExpr );

    TxExpressionNode* lengthExpr = new TxIntegerLitNode( this->parseLocation, elemExprList->size(), false, UINT );
    this->lengthNode = new TxValueTypeArgumentNode( lengthExpr );
}

TxArrayLitNode::TxArrayLitNode( const TxLocation& parseLocation, const std::vector<TxMaybeConversionNode*>* elemExprList )
        : TxExpressionNode( parseLocation ), origElemExprList( nullptr ), elemExprList( elemExprList ) {
    ASSERT(elemExprList && !elemExprList->empty(), "Array literals must have at least one element");

    TxTypeExprWrapperNode* elementTypeExpr = new TxTypeExprWrapperNode( elemExprList->at( 0 )->originalExpr );
    this->elementTypeNode = new TxTypeTypeArgumentNode( elementTypeExpr );

    TxExpressionNode* lengthExpr = new TxIntegerLitNode( this->parseLocation, elemExprList->size(), false, UINT );
    this->lengthNode = new TxValueTypeArgumentNode( lengthExpr );
}
