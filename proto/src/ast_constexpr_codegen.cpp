#include <stdint.h>
#include <limits.h>
#include "ast_constexpr.hpp"

#include "llvm_generator.hpp"

using namespace llvm;

uint32_t eval_UInt_constant( const TxExpressionNode* exprNode ) {
    Constant* value = exprNode->code_gen_constant( exprNode->get_parser_context()->llvm_context() );
    ConstantInt* CI = cast<ConstantInt>( value );
    uint64_t constUIntValue = CI->getZExtValue();
    return constUIntValue;
}

int32_t eval_Int_constant( const TxExpressionNode* exprNode ) {
    Constant* value = exprNode->code_gen_constant( exprNode->get_parser_context()->llvm_context() );
    ConstantInt* CI = cast<ConstantInt>( value );
    int64_t constIntValue = CI->getSExtValue();
    return constIntValue;
}

double eval_Floatingpoint_constant( const TxExpressionNode* exprNode ) {
    Constant* value = exprNode->code_gen_constant( exprNode->get_parser_context()->llvm_context() );
    ConstantFP* CF = cast<ConstantFP>( value );
    double constDoubleValue = CF->getValueAPF().convertToDouble();
    return constDoubleValue;
}
