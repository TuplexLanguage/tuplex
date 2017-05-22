#pragma once

class TxExpressionNode;

uint32_t eval_UInt_constant( const TxExpressionNode* exprNode );

int32_t eval_Int_constant( const TxExpressionNode* exprNode );

double eval_Floatingpoint_constant( const TxExpressionNode* exprNode );
