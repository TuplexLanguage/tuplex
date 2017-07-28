#include "ast_op_exprs.hpp"

#include "ast_ref.hpp"
#include "ast/ast_fielddef_node.hpp"

#include "tx_logging.hpp"
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
//                                        { TXOP_EQ, CmpInst::Predicate::ICMP_EQ, CmpInst::Predicate::ICMP_EQ, CmpInst::Predicate::FCMP_OEQ },
//                                        { TXOP_NE, CmpInst::Predicate::ICMP_NE, CmpInst::Predicate::ICMP_NE, CmpInst::Predicate::FCMP_ONE },
                                        { TXOP_GT, CmpInst::Predicate::ICMP_SGT, CmpInst::Predicate::ICMP_UGT, CmpInst::Predicate::FCMP_OGT },
                                        { TXOP_GE, CmpInst::Predicate::ICMP_SGE, CmpInst::Predicate::ICMP_UGE, CmpInst::Predicate::FCMP_OGE },
                                        { TXOP_LT, CmpInst::Predicate::ICMP_SLT, CmpInst::Predicate::ICMP_ULT, CmpInst::Predicate::FCMP_OLT },
                                        { TXOP_LE, CmpInst::Predicate::ICMP_SLE, CmpInst::Predicate::ICMP_ULE, CmpInst::Predicate::FCMP_OLE },
                                        { TXOP_AND, Instruction::And, Instruction::And, 0 },
                                        { TXOP_OR, Instruction::Or, Instruction::Or, 0 },
                                        { TXOP_XOR, Instruction::Xor, Instruction::Xor, 0 },
                                        { TXOP_LSHIFT, Instruction::Shl, Instruction::Shl, 0 },
                                        { TXOP_RSHIFT, Instruction::LShr, Instruction::LShr, 0 },
                                        { TXOP_ARSHIFT, Instruction::AShr, Instruction::AShr, 0 },
};

// Note: In LLVM and in common CPUs, for an integer type of N bits, the result of shifting by >= N is undefined.

static unsigned get_llvm_op( TxOperationClass op_class, TxOperation op, const TxActualType* computeType, bool* float_operation ) {
    unsigned llvm_op;
    if ( auto intType = dynamic_cast<const TxIntegerType*>( computeType ) ) {
        llvm_op = intType->sign ? OP_MAPPING[op].l_si_op : OP_MAPPING[op].l_ui_op;
    }
    else if ( dynamic_cast<const TxFloatingType*>( computeType ) ) {
        llvm_op = OP_MAPPING[op].l_f_op;
        *float_operation = true;
    }
    else {  // Bool or Ref operands
        ASSERT( op_class != TXOC_ARITHMETIC, "Unsupported binary operand type: " << computeType );
        llvm_op = OP_MAPPING[op].l_ui_op;  // as unsigned integers
    }
    return llvm_op;
}

llvm::Constant* TxBinaryElemOperatorNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto lval = this->lhs->code_gen_const_value( context );
    auto rval = this->rhs->code_gen_const_value( context );

    auto op_class = get_op_class( this->op );
    auto computeType = ( op_class == TXOC_ARITHMETIC ? this->qualtype()->type()->acttype() : this->lhs->resolve_type()->type()->acttype() );
    bool float_operation = false;
    unsigned llvm_op = get_llvm_op( op_class, this->op, computeType, &float_operation );

    if ( op_class == TXOC_ARITHMETIC || op_class == TXOC_LOGICAL || op_class == TXOC_SHIFT ) {
        ASSERT( Instruction::isBinaryOp( llvm_op ), "Not a valid LLVM binary op: " << llvm_op );
        Instruction::BinaryOps binop_instr = (Instruction::BinaryOps) llvm_op;
        return ConstantExpr::get( binop_instr, lval, rval );
    }
    else { // if (op_class == TXOC_COMPARISON) {
        CmpInst::Predicate cmp_pred = (CmpInst::Predicate) llvm_op;
        return ConstantExpr::getCompare( cmp_pred, lval, rval );
    }
}

Value* TxBinaryElemOperatorNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto lval = this->lhs->code_gen_dyn_value( context, scope );
    auto rval = this->rhs->code_gen_dyn_value( context, scope );

    // pick field's plain name, if available, for the expression value:
    const std::string fieldName = ( this->fieldDefNode ? this->fieldDefNode->get_descriptor() : "" );

    auto op_class = get_op_class( this->op );
    auto computeType = ( op_class == TXOC_ARITHMETIC ? this->qualtype()->type()->acttype() : this->lhs->resolve_type()->type()->acttype() );
    bool float_operation = false;
    unsigned llvm_op = get_llvm_op( op_class, this->op, computeType, &float_operation );

    if ( op_class == TXOC_ARITHMETIC || op_class == TXOC_LOGICAL || op_class == TXOC_SHIFT ) {
        ASSERT( Instruction::isBinaryOp( llvm_op ), "Not a valid LLVM binary op: " << llvm_op );
        Instruction::BinaryOps binop_instr = (Instruction::BinaryOps) llvm_op;
        return scope->builder->CreateBinOp( binop_instr, lval, rval, fieldName );
    }

    else { // if (op_class == TXOC_COMPARISON) {
        CmpInst::Predicate cmp_pred = (CmpInst::Predicate) llvm_op;
        if ( float_operation ) {
            ASSERT( CmpInst::isFPPredicate( cmp_pred ), "Not a valid LLVM FP comparison predicate: " << llvm_op );
            return scope->builder->CreateFCmp( cmp_pred, lval, rval, fieldName );
        }
        else {
            // integer or bool
            ASSERT( CmpInst::isIntPredicate( cmp_pred ), "Not a valid LLVM Int comparison predicate: " << llvm_op );
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
    const TxActualType* opType = this->qualtype()->type()->acttype();
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
    const TxActualType* opType = this->qualtype()->type()->acttype();
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


llvm::Constant* TxEqualityOperatorNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto lval = this->lhs->code_gen_const_value( context );
    auto rval = this->rhs->code_gen_const_value( context );

    auto lhsType = this->lhs->resolve_type()->type()->acttype();
    auto lhsTypeclass = lhsType->get_type_class();

    if ( lhsTypeclass == TXTC_ELEMENTARY ) {
        if ( dynamic_cast<const TxFloatingType*>( lhsType ) ) {
            return ConstantExpr::getFCmp( CmpInst::Predicate::FCMP_OEQ, lval, rval );
        }
        else {  // integer or boolean
            return ConstantExpr::getICmp( CmpInst::Predicate::ICMP_EQ, lval, rval );
        }
    }
    else if ( lhsTypeclass == TXTC_REFERENCE ) {
        // both operands are references, compare their pointer values
        lval = gen_get_ref_pointer( context, lval );
        rval = gen_get_ref_pointer( context, rval );
        return ConstantExpr::getICmp( CmpInst::Predicate::ICMP_EQ, lval, rval );
    }
    else if ( lhsTypeclass == TXTC_ARRAY ) {
        uint32_t lenIxs[] = { 1 };
        auto lvalLengthC = ConstantExpr::getExtractValue( lval, lenIxs );
        auto rvalLengthC = ConstantExpr::getExtractValue( rval, lenIxs );
        auto lenCondC = ConstantExpr::getICmp( CmpInst::Predicate::ICMP_EQ, lvalLengthC, rvalLengthC );
        uint64_t lenCond = cast<ConstantInt>( lenCondC )->getZExtValue();
        if ( lenCond ) {
            uint64_t length = cast<ConstantInt>( lvalLengthC )->getZExtValue();
            auto elemT = lval->getType()->getContainedType( 2 )->getArrayElementType();
            CmpInst::Predicate eqPred = ( elemT->isFloatingPointTy() ? CmpInst::Predicate::FCMP_OEQ : CmpInst::Predicate::ICMP_EQ );
            for ( unsigned ix = 0; ix < length; ++ix ) {
                uint32_t elemIxs[] = { 2, ix };
                auto lvalElemC = ConstantExpr::getExtractValue( lval, elemIxs );
                auto rvalElemC = ConstantExpr::getExtractValue( rval, elemIxs );
                auto cmpCondC = ConstantExpr::getCompare( eqPred, lvalElemC, rvalElemC );
                if ( !cast<ConstantInt>( cmpCondC )->getZExtValue() )
                    return cmpCondC;   // false i.e. unequal
            }
        }
        return lenCondC;
    }
//    else if ( lhsTypeclass == TXTC_TUPLE ) {
//    }
    else {
        CERR_CODECHECK( this, "Constant equality operator not yet supported for type class " << lhsTypeclass << ": " << this->lhs->qualtype() );
    }
}

Value* TxEqualityOperatorNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    ASSERT( scope, "NULL scope in non-const binary expression: " << this );

    // pick field's plain name, if available, for the expression value:
    const std::string fieldName = ( this->fieldDefNode ? this->fieldDefNode->get_descriptor() : "" );

    auto lhsType = this->lhs->resolve_type()->type()->acttype();
    auto lhsTypeclass = lhsType->get_type_class();

    if ( lhsTypeclass == TXTC_ELEMENTARY ) {
        auto lval = this->lhs->code_gen_dyn_value( context, scope );
        auto rval = this->rhs->code_gen_dyn_value( context, scope );
        if ( dynamic_cast<const TxFloatingType*>( lhsType ) ) {
            return scope->builder->CreateFCmp( CmpInst::Predicate::FCMP_OEQ, lval, rval, fieldName );
        }
        else {  // integer or boolean
            return scope->builder->CreateICmp( CmpInst::Predicate::ICMP_EQ, lval, rval, fieldName );
        }
    }
    else if ( lhsTypeclass == TXTC_REFERENCE ) {
        // both operands are references, compare their pointer values
        auto lvalPtr = gen_get_ref_pointer( context, scope, this->lhs->code_gen_dyn_value( context, scope ) );
        auto rvalPtr = gen_get_ref_pointer( context, scope, this->rhs->code_gen_dyn_value( context, scope ) );
        return scope->builder->CreateICmp( CmpInst::Predicate::ICMP_EQ, lvalPtr, rvalPtr, fieldName );
    }
    else if ( lhsTypeclass == TXTC_ARRAY ) {
        auto lvalA = this->lhs->code_gen_dyn_address( context, scope );
        auto rvalA = this->rhs->code_gen_dyn_address( context, scope );
        // TODO: When the operands are dereferenced references to generic arrays (&Array), the references' types
        //       must be examined in runtime for *array element type* equality!

        Value* eqResultA = this->registry().get_builtin_type( TXBT_BOOL )->acttype()->gen_alloca( context, scope, "arrEq");

        auto parentFunc = scope->builder->GetInsertBlock()->getParent();
        BasicBlock* lenEqBlock   = BasicBlock::Create( context.llvmContext, "if_ArrLenEq", parentFunc );
        BasicBlock* arrEqBlock   = BasicBlock::Create( context.llvmContext, "if_ArrEq",    parentFunc );
        BasicBlock* arrUneqBlock = BasicBlock::Create( context.llvmContext, "if_ArrUneq",  parentFunc );
        BasicBlock* postBlock    = BasicBlock::Create( context.llvmContext, "if_ArrPost",  parentFunc );

        Value* lenIxs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                            ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 1 ) };
        auto lvalLengthV = scope->builder->CreateLoad( scope->builder->CreateInBoundsGEP( lvalA, lenIxs ) );
        {
            auto rvalLengthV = scope->builder->CreateLoad( scope->builder->CreateInBoundsGEP( rvalA, lenIxs ) );
            auto lenCondV = scope->builder->CreateICmpEQ( lvalLengthV, rvalLengthV );
            scope->builder->CreateCondBr( lenCondV, lenEqBlock, arrUneqBlock );
        }
        {
            scope->builder->SetInsertPoint( lenEqBlock );

            auto lvalTypeIdV = this->lhs->code_gen_typeid( context, scope );
            auto elemSizeV = context.gen_get_element_size( scope, lvalTypeIdV );
            auto elemSize64V = scope->builder->CreateZExtOrBitCast( elemSizeV, Type::getInt64Ty( context.llvmContext ) );
            auto lvalLength64V = scope->builder->CreateZExtOrBitCast( lvalLengthV, Type::getInt64Ty( context.llvmContext ) );
            auto dataSizeV = scope->builder->CreateMul( elemSize64V, lvalLength64V, "datasize" );

            Value* dataIxs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                                 ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 2 ) };
            auto lvalDataA = scope->builder->CreatePointerCast( scope->builder->CreateInBoundsGEP( lvalA, dataIxs ), context.get_voidPtrT() );
            auto rvalDataA = scope->builder->CreatePointerCast( scope->builder->CreateInBoundsGEP( rvalA, dataIxs ), context.get_voidPtrT() );
            std::vector<Value*> args( { lvalDataA, rvalDataA, dataSizeV } );
            auto memcmpFuncA = context.llvmModule().getFunction( "memcmp" );
            ASSERT( memcmpFuncA, "memcmp() function not found in " << this );
            auto callV = scope->builder->CreateCall( memcmpFuncA, args );

            auto condV = scope->builder->CreateICmpEQ( callV, ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ) );
            scope->builder->CreateCondBr( condV, arrEqBlock, arrUneqBlock );
        }
        {
            scope->builder->SetInsertPoint( arrEqBlock );
            scope->builder->CreateStore( ConstantInt::get( Type::getInt1Ty( context.llvmContext ), 1 ), eqResultA );
            scope->builder->CreateBr( postBlock );
        }
        {
            scope->builder->SetInsertPoint( arrUneqBlock );
            scope->builder->CreateStore( ConstantInt::get( Type::getInt1Ty( context.llvmContext ), 0 ), eqResultA );
            scope->builder->CreateBr( postBlock );
        }

        scope->builder->SetInsertPoint( postBlock );
        auto eqResultV = scope->builder->CreateLoad( eqResultA );
        return eqResultV;
    }
//    else if ( lhsTypeclass == TXTC_TUPLE ) {
//    }
    else {
        CERR_CODECHECK( this, "Equality operator not yet supported for type class " << lhsTypeclass << ": " << this->lhs->qualtype() );
    }
}
