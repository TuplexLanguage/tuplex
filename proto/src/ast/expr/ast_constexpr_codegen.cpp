#include <stdint.h>
#include <limits.h>

#include "../../symbol/qual_type.hpp"
#include "llvm_generator.hpp"

#include "ast_constexpr.hpp"
#include "ast_expr_node.hpp"

#include "parsercontext.hpp"

using namespace llvm;

uint64_t eval_unsigned_int_constant( const TxExpressionNode* exprNode ) {
    Constant* value = exprNode->code_gen_const_value( *exprNode->get_parser_context()->get_llvm_gen_context() );
    ConstantInt* CI = cast<ConstantInt>( value );
    uint64_t constUIntValue = CI->getZExtValue();
    return constUIntValue;
}

int64_t eval_signed_int_constant( const TxExpressionNode* exprNode ) {
    Constant* value = exprNode->code_gen_const_value( *exprNode->get_parser_context()->get_llvm_gen_context() );
    ConstantInt* CI = cast<ConstantInt>( value );
    int64_t constIntValue = CI->getSExtValue();
    return constIntValue;
}

double eval_floatingpoint_constant( const TxExpressionNode* exprNode ) {
    Constant* value = exprNode->code_gen_const_value( *exprNode->get_parser_context()->get_llvm_gen_context() );
    ConstantFP* CF = cast<ConstantFP>( value );
    double constDoubleValue = CF->getValueAPF().convertToDouble();
    return constDoubleValue;
}

bool is_static_equal( const TxExpressionNode* exprA, const TxExpressionNode* exprB ) {
    if ( exprA->is_statically_constant() && exprB->is_statically_constant() ) {
        auto atype = exprA->qualtype()->type()->acttype();
        auto btype = exprB->qualtype()->type()->acttype();
        if ( !( atype->has_type_id() && btype->has_type_id() ) )
            return false;  // can currently only compare constant values of built-in types

        if ( ( is_concrete_uinteger_type( (BuiltinTypeId)atype->get_type_id() ) && is_concrete_uinteger_type( (BuiltinTypeId)btype->get_type_id() ) )
                || ( atype->get_type_id() == TXBT_BOOL && btype->get_type_id() == TXBT_BOOL ) )
            return ( eval_unsigned_int_constant( exprA ) == eval_unsigned_int_constant( exprB ) );

        if ( is_concrete_sinteger_type( (BuiltinTypeId)atype->get_type_id() ) && is_concrete_sinteger_type( (BuiltinTypeId)btype->get_type_id() ) )
            return ( eval_signed_int_constant( exprA ) == eval_signed_int_constant( exprB ) );

        if ( is_concrete_floating_type( (BuiltinTypeId)atype->get_type_id() ) && is_concrete_floating_type( (BuiltinTypeId)btype->get_type_id() ) )
            return ( eval_floatingpoint_constant( exprA ) == eval_floatingpoint_constant( exprB ) );
    }
    return false;
}
