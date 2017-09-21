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
    auto lvalC = this->lhs->code_gen_const_value( context );
    auto rvalC = this->rhs->code_gen_const_value( context );

    auto lhsType = this->lhs->qualtype()->type()->acttype();
    auto lhsTypeclass = lhsType->get_type_class();

    if ( lhsTypeclass == TXTC_ELEMENTARY ) {
        if ( dynamic_cast<const TxFloatingType*>( lhsType ) ) {
            return ConstantExpr::getFCmp( CmpInst::Predicate::FCMP_OEQ, lvalC, rvalC );
        }
        else {  // integer or boolean
            return ConstantExpr::getICmp( CmpInst::Predicate::ICMP_EQ, lvalC, rvalC );
        }
    }
    else if ( lhsTypeclass == TXTC_REFERENCE ) {
        return ConstantExpr::getICmp( CmpInst::Predicate::ICMP_EQ,
                                      gen_get_ref_pointer( context, lvalC ),
                                      gen_get_ref_pointer( context, rvalC ) );
    }
    else if ( lhsTypeclass == TXTC_FUNCTION ) {
        return ConstantExpr::getAnd( ConstantExpr::getICmp( CmpInst::Predicate::ICMP_EQ,
                                                            gen_get_struct_member( context, lvalC, 0 ),
                                                            gen_get_struct_member( context, rvalC, 0 ) ),
                                     ConstantExpr::getICmp( CmpInst::Predicate::ICMP_EQ,
                                                            gen_get_ref_pointer( context, gen_get_struct_member( context, lvalC, 1 ) ),
                                                            gen_get_ref_pointer( context, gen_get_struct_member( context, rvalC, 1 ) ) ) );
    }
    else if ( lhsTypeclass == TXTC_ARRAY ) {
        uint32_t lenIxs[] = { 1 };
        auto lvalLengthC = ConstantExpr::getExtractValue( lvalC, lenIxs );
        auto rvalLengthC = ConstantExpr::getExtractValue( rvalC, lenIxs );
        auto lenCondC = ConstantExpr::getICmp( CmpInst::Predicate::ICMP_EQ, lvalLengthC, rvalLengthC );
        uint64_t lenCond = cast<ConstantInt>( lenCondC )->getZExtValue();
        if ( lenCond ) {
            uint64_t length = cast<ConstantInt>( lvalLengthC )->getZExtValue();
            auto elemT = lvalC->getType()->getContainedType( 2 )->getArrayElementType();
            CmpInst::Predicate eqPred = ( elemT->isFloatingPointTy() ? CmpInst::Predicate::FCMP_OEQ : CmpInst::Predicate::ICMP_EQ );
            for ( unsigned ix = 0; ix < length; ++ix ) {
                uint32_t elemIxs[] = { 2, ix };
                auto lvalElemC = ConstantExpr::getExtractValue( lvalC, elemIxs );
                auto rvalElemC = ConstantExpr::getExtractValue( rvalC, elemIxs );
                auto cmpCondC = ConstantExpr::getCompare( eqPred, lvalElemC, rvalElemC );
                if ( !cast<ConstantInt>( cmpCondC )->getZExtValue() )
                    return cmpCondC;   // false i.e. unequal
            }
        }
        return lenCondC;
    }
    else {
        CERR_CODECHECK( this, "Constant equality operator not supported for type class " << lhsTypeclass << ": " << this->lhs->qualtype() );
    }
}

Value* TxEqualityOperatorNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    ASSERT( scope, "NULL scope in non-const binary expression: " << this );

    // pick field's plain name, if available, for the expression value:
    const std::string fieldName = ( this->fieldDefNode ? this->fieldDefNode->get_descriptor() : "" );

    auto lhsType = this->lhs->qualtype()->type()->acttype();
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

    else if ( lhsTypeclass == TXTC_FUNCTION ) {
        auto lval = this->lhs->code_gen_dyn_value( context, scope );
        auto lFunctionPtrV = gen_get_struct_member( context, scope, lval, 0 );
        auto lClosurePtrV = gen_get_ref_pointer( context, scope, gen_get_struct_member( context, scope, lval, 1 ) );
        auto rval = this->rhs->code_gen_dyn_value( context, scope );
        auto rFunctionPtrV = gen_get_struct_member( context, scope, rval, 0 );
        auto rClosurePtrV = gen_get_ref_pointer( context, scope, gen_get_struct_member( context, scope, rval, 1 ) );
        auto funcEqV = scope->builder->CreateICmp( CmpInst::Predicate::ICMP_EQ, lFunctionPtrV, rFunctionPtrV );
        auto closEqV = scope->builder->CreateICmp( CmpInst::Predicate::ICMP_EQ, lClosurePtrV, rClosurePtrV );
        return scope->builder->CreateAnd( funcEqV, closEqV );
    }

    else if ( lhsTypeclass == TXTC_ARRAY ) {
        // In current implementation we know that either at least one operand is Any,
        // or both operands' elements will be of the same type class.
        auto lvalA = this->lhs->code_gen_dyn_address( context, scope );
        auto rvalA = this->rhs->code_gen_dyn_address( context, scope );
        auto lvalTypeIdV = this->lhs->code_gen_typeid( context, scope );

        auto rhsType = this->rhs->qualtype()->type()->acttype();
        auto lelemtype = static_cast<const TxArrayType*>( lhsType )->element_type()->type()->acttype();
        auto relemtype = static_cast<const TxArrayType*>( rhsType )->element_type()->type()->acttype();

        if ( lelemtype->get_type_class() == TXTC_ELEMENTARY && relemtype->get_type_class() == TXTC_ELEMENTARY
                && lelemtype->is_concrete() && relemtype->is_concrete()
                && lelemtype->get_runtime_type_id() == relemtype->get_runtime_type_id() ) {
            // statically known that both arrays' elements are the same elementary, concrete types - invoke built-in comparison
            Type* i32T = IntegerType::getInt32Ty( context.llvmContext );
            StructType* genArrayT = StructType::get( i32T, i32T, ArrayType::get( StructType::get( context.llvmContext ), 0 ), nullptr );
            PointerType* genArrayPtrT = PointerType::getUnqual( genArrayT );
            std::vector<Value*> args( { scope->builder->CreatePointerCast( lvalA, genArrayPtrT ),
                                        scope->builder->CreatePointerCast( rvalA, genArrayPtrT ),
                                        lvalTypeIdV } );
            auto arrEqFuncA = context.llvmModule().getFunction( "$array_elementary_equals" );
            ASSERT( arrEqFuncA, "$array_elementary_equals() function not found in " << this );
            return scope->builder->CreateCall( arrEqFuncA, args );
        }

        else if ( lelemtype->get_type_class() == TXTC_ANY || lelemtype->get_type_class() == TXTC_ELEMENTARY ||
                  relemtype->get_type_class() == TXTC_ANY || relemtype->get_type_class() == TXTC_ELEMENTARY ) {
            // When one or both the operands are dereferenced references to generic arrays (&Array),
            // or at least one operand's element type is elementary but both operands' concrete types aren't statically known,
            // the types must be examined in runtime for array element type class equality,
            // and if both elementary also for element type equality.
            auto rvalTypeIdV = this->lhs->code_gen_typeid( context, scope );
            std::vector<Value*> args( { lvalA, rvalA, lvalTypeIdV, rvalTypeIdV } );
            auto arrEqFuncA = context.llvmModule().getFunction( "$array_any_equals" );
            ASSERT( arrEqFuncA, "$array_any_equals() function not found in " << this );
            return scope->builder->CreateCall( arrEqFuncA, args );
        }

        else {
            // invoke the standard Array.equals() method which compares each element
            auto rvalTypeIdV = this->lhs->code_gen_typeid( context, scope );
            return context.gen_equals_invocation( scope, lvalA, lvalTypeIdV, rvalA, rvalTypeIdV );
        }
    }

    else {
        // invoke the standard Any.equals() method
        auto lvalA = this->lhs->code_gen_dyn_address( context, scope );
        auto rvalA = this->rhs->code_gen_dyn_address( context, scope );
        auto lvalTypeIdV = this->lhs->code_gen_typeid( context, scope );
        auto rvalTypeIdV = this->lhs->code_gen_typeid( context, scope );
        return context.gen_equals_invocation( scope, lvalA, lvalTypeIdV, rvalA, rvalTypeIdV );
    }
}

Constant* TxRefEqualityOperatorNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );

    // both operands are references, compare their pointer values
    auto lval = gen_get_ref_pointer( context, this->lhs->code_gen_const_value( context ) );
    auto rval = gen_get_ref_pointer( context, this->rhs->code_gen_const_value( context ) );
    return ConstantExpr::getICmp( CmpInst::Predicate::ICMP_EQ, lval, rval );
}

Value* TxRefEqualityOperatorNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );

    // pick field's plain name, if available, for the expression value:
    const std::string fieldName = ( this->fieldDefNode ? this->fieldDefNode->get_descriptor() : "" );

    // both operands are references, compare their pointer values
    auto lvalPtr = gen_get_ref_pointer( context, scope, this->lhs->code_gen_dyn_value( context, scope ) );
    auto rvalPtr = gen_get_ref_pointer( context, scope, this->rhs->code_gen_dyn_value( context, scope ) );
    return scope->builder->CreateICmp( CmpInst::Predicate::ICMP_EQ, lvalPtr, rvalPtr, fieldName );
}
