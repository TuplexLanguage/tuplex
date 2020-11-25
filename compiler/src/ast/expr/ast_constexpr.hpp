#pragma once

class TxExpressionNode;

/** Returns the result value of a statically constant expression of unsigned integer or boolean type.
 * It is the caller's responsibility to ensure the expression is statically constant and of the proper type. */
uint64_t eval_unsigned_int_constant( const TxExpressionNode* exprNode );

/** Returns the result value of a statically constant expression of signed integer or boolean type.
 * It is the caller's responsibility to ensure the expression is statically constant and of the proper type. */
int64_t eval_signed_int_constant( const TxExpressionNode* exprNode );

/** Returns the result value of a statically constant expression of floating point type.
 * It is the caller's responsibility to ensure the expression is statically constant and of the proper type. */
double eval_floatingpoint_constant( const TxExpressionNode* exprNode );

/** Returns true if both expressions are statically constant and have equal values.
 * This can only compare elementary values (integer, floating-point, boolean). */
bool is_static_equal( const TxExpressionNode* exprA, const TxExpressionNode* exprB );
