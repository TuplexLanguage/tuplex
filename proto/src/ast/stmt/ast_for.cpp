#include "ast_for.hpp"

#include "../expr/ast_maybe_conv_node.hpp"
#include "../expr/ast_field.hpp"
#include "../expr/ast_exprs.hpp"

TxInClauseNode::TxInClauseNode( const TxLocation& parseLocation, const std::string& valueName, const std::string& iterName, TxExpressionNode* seqExpr )
        : TxNode( parseLocation ), valueName( valueName ), iterName( iterName ), origSeqExpr( seqExpr ) {
    auto & loc = this->parseLocation;
    auto iterInitExpr = new TxFunctionCallNode( loc, new TxFieldValueNode( loc, new TxMaybeConversionNode( seqExpr ), "sequencer" ),
                                                new std::vector<TxExpressionNode*>() );
    this->iterField = new TxFieldDefNode( loc, this->iterName, nullptr, iterInitExpr );

    this->nextCond = new TxFunctionCallNode( loc, new TxFieldValueNode( loc, new TxFieldValueNode( loc, nullptr, this->iterName ), "has_next" ),
                                             new std::vector<TxExpressionNode*>() );

    auto nextValueExpr = new TxFunctionCallNode( loc, new TxFieldValueNode( loc, new TxFieldValueNode( loc, nullptr, this->iterName ), "next" ),
                                                 new std::vector<TxExpressionNode*>() );
    this->valueField = new TxFieldDefNode( loc, this->valueName, nullptr, nextValueExpr );
}
