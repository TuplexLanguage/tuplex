#include "ast_op_exprs.hpp"

#include "ast_ref.hpp"
#include "ast/ast_fielddef_node.hpp"

#include "llvm_generator.hpp"

using namespace llvm;

typedef struct {
    TxOperation op;
    unsigned l_si_op;
    unsigned l_ui_op;
    unsigned l_f_op;
} OpMapping;

static const OpMapping OP_MAPPING[] = {
                                        { TXOP_NONE, 0, 0, 0 },
                                        { TXOP_PLUS, Instruction::Add, Instruction::Add, Instruction::FAdd },
                                        { TXOP_MINUS, Instruction::Sub, Instruction::Sub, Instruction::FSub },
                                        { TXOP_MUL, Instruction::Mul, Instruction::Mul, Instruction::FMul },
                                        { TXOP_DIV, Instruction::SDiv, Instruction::UDiv, Instruction::FDiv },
                                        { TXOP_EQ, CmpInst::Predicate::ICMP_EQ, CmpInst::Predicate::ICMP_EQ, CmpInst::Predicate::FCMP_OEQ },
                                        { TXOP_NE, CmpInst::Predicate::ICMP_NE, CmpInst::Predicate::ICMP_NE, CmpInst::Predicate::FCMP_ONE },
                                        { TXOP_GT, CmpInst::Predicate::ICMP_SGT, CmpInst::Predicate::ICMP_UGT, CmpInst::Predicate::FCMP_OGT },
                                        { TXOP_GE, CmpInst::Predicate::ICMP_SGE, CmpInst::Predicate::ICMP_UGE, CmpInst::Predicate::FCMP_OGE },
                                        { TXOP_LT, CmpInst::Predicate::ICMP_SLT, CmpInst::Predicate::ICMP_ULT, CmpInst::Predicate::FCMP_OLT },
                                        { TXOP_LE, CmpInst::Predicate::ICMP_SLE, CmpInst::Predicate::ICMP_ULE, CmpInst::Predicate::FCMP_OLE },
                                        { TXOP_AND, Instruction::And, Instruction::And, 0 },
                                        { TXOP_OR, Instruction::Or, Instruction::Or, 0 },
};

static unsigned get_llvm_op( TxOperationClass op_class, TxOperation op, const TxType* resType, const TxType* operandType, bool* float_operation ) {
    unsigned llvm_op;
    if ( op_class == TXOC_ARITHMETIC ) {
        auto resultType = resType->type();
        if ( auto intType = dynamic_cast<const TxIntegerType*>( resultType ) ) {
            llvm_op = intType->sign ? OP_MAPPING[op].l_si_op : OP_MAPPING[op].l_ui_op;
        }
        else if ( dynamic_cast<const TxFloatingType*>( resultType ) ) {
            llvm_op = OP_MAPPING[op].l_f_op;
            *float_operation = true;
        }
        else {
            ASSERT( false, "Unsupported binary operand type: " << (resultType?resultType->str().c_str():"NULL") );
            llvm_op = 0;
        }
    }
    else {  // TXOC_EQUALITY, TXOC_COMPARISON, TXOC_BOOLEAN
        if ( dynamic_cast<const TxFloatingType*>( operandType->type() ) ) {
            llvm_op = OP_MAPPING[op].l_f_op;
            *float_operation = true;
        }
        else if ( auto intType = dynamic_cast<const TxIntegerType*>( operandType->type() ) ) {
            llvm_op = intType->sign ? OP_MAPPING[op].l_si_op : OP_MAPPING[op].l_ui_op;
        }
        else {  // Bool or Ref operands
            llvm_op = OP_MAPPING[op].l_ui_op;  // as unsigned integers
        }
    }
    return llvm_op;
}

llvm::Constant* TxBinaryOperatorNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto lval = this->lhs->code_gen_const_value( context );
    auto rval = this->rhs->code_gen_const_value( context );

    // pick field's plain name, if available, for the expression value:
    const std::string fieldName = ( this->fieldDefNode ? this->fieldDefNode->get_identifier() : "" );

    auto op_class = get_op_class( this->op );
    bool float_operation = false;
    unsigned llvm_op = get_llvm_op( op_class, this->op, this->get_type(), this->lhs->resolve_type(), &float_operation );

    if ( op_class == TXOC_ARITHMETIC || op_class == TXOC_BOOLEAN ) {
        ASSERT( Instruction::isBinaryOp( llvm_op ), "Not a valid LLVM binary op: " << llvm_op );
        Instruction::BinaryOps binop_instr = (Instruction::BinaryOps) llvm_op;
        return ConstantExpr::get( binop_instr, lval, rval );
    }
    else { // if (op_class == TXOC_EQUALITY || op_class == TXOC_COMPARISON) {
        CmpInst::Predicate cmp_pred = (CmpInst::Predicate) llvm_op;
        return ConstantExpr::getCompare( cmp_pred, lval, rval );
    }
}

Value* TxBinaryOperatorNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    ASSERT( scope, "NULL scope in non-const binary expression: " << this );
    auto lval = this->lhs->code_gen_dyn_value( context, scope );
    auto rval = this->rhs->code_gen_dyn_value( context, scope );

    // pick field's plain name, if available, for the expression value:
    const std::string fieldName = ( this->fieldDefNode ? this->fieldDefNode->get_identifier() : "" );

    auto op_class = get_op_class( this->op );
    bool float_operation = false;
    unsigned llvm_op = get_llvm_op( op_class, this->op, this->get_type(), this->lhs->get_type(), &float_operation );

    if ( op_class == TXOC_ARITHMETIC || op_class == TXOC_BOOLEAN ) {
        ASSERT( Instruction::isBinaryOp( llvm_op ), "Not a valid LLVM binary op: " << llvm_op );
        Instruction::BinaryOps binop_instr = (Instruction::BinaryOps) llvm_op;
        return scope->builder->CreateBinOp( binop_instr, lval, rval, fieldName );
    }

    else { // if (op_class == TXOC_EQUALITY || op_class == TXOC_COMPARISON) {
        CmpInst::Predicate cmp_pred = (CmpInst::Predicate) llvm_op;
        if ( float_operation ) {
            ASSERT( CmpInst::isFPPredicate( cmp_pred ), "Not a valid LLVM FP comparison predicate: " << llvm_op );
            return scope->builder->CreateFCmp( cmp_pred, lval, rval, fieldName );
        }
        else {
            ASSERT( CmpInst::isIntPredicate( cmp_pred ), "Not a valid LLVM Int comparison predicate: " << llvm_op );
            if ( this->lhs->get_type()->get_type_class() == TXTC_REFERENCE ) {
                // both operands are references, compare their pointer values
                lval = gen_get_ref_pointer( context, scope, lval );
                rval = gen_get_ref_pointer( context, scope, rval );
            }
            return scope->builder->CreateICmp( cmp_pred, lval, rval, fieldName );
        }
    }
}

llvm::Constant* TxUnaryMinusNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto operand = this->operand->code_gen_const_value( context );
    if ( dynamic_cast<TxIntegerLitNode*>( this->operand->originalExpr ) ) {
        return operand;  // negation has been applied directly to the literal
    }
    auto opType = this->get_type()->type();
    if ( dynamic_cast<const TxIntegerType*>( opType ) ) {
        return ConstantExpr::getNeg( operand  );
    }
    else if ( dynamic_cast<const TxFloatingType*>( opType ) ) {
        return ConstantExpr::getFNeg( operand );
    }
    THROW_LOGIC( "Invalid unary minus operand type: " << opType << " in " << this );
}

Value* TxUnaryMinusNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto operand = this->operand->code_gen_dyn_value( context, scope );
    if ( dynamic_cast<TxIntegerLitNode*>( this->operand->originalExpr ) ) {
        return operand;  // negation has been applied directly to the literal
    }
    auto opType = this->get_type()->type();
    if ( dynamic_cast<const TxIntegerType*>( opType ) ) {
        return scope->builder->CreateNeg( operand );
    }
    else if ( dynamic_cast<const TxFloatingType*>( opType ) ) {
        return scope->builder->CreateFNeg( operand );
    }
    THROW_LOGIC( "Invalid unary minus operand type: " << opType << " in " << this );
}

llvm::Constant* TxUnaryLogicalNotNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto operand = this->operand->code_gen_const_value( context );
    return ConstantExpr::getNot( operand );
}

Value* TxUnaryLogicalNotNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto operand = this->operand->code_gen_dyn_value( context, scope );
    return scope->builder->CreateNot( operand );
}
