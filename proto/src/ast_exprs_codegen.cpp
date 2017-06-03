#include "ast_exprs.hpp"
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

unsigned get_llvm_op( TxOperationClass op_class, TxOperation op, const TxType* resType, const TxType* operandType, bool* float_operation ) {
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

llvm::Constant* TxBinaryOperatorNode::code_gen_constant( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto lval = this->lhs->code_gen_constant( context );
    auto rval = this->rhs->code_gen_constant( context );

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

Value* TxBinaryOperatorNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    ASSERT( scope, "NULL scope in non-const binary expression: " << this );
    auto lval = this->lhs->code_gen_value( context, scope );
    auto rval = this->rhs->code_gen_value( context, scope );

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

llvm::Constant* TxUnaryMinusNode::code_gen_constant( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto operand = this->operand->code_gen_constant( context );
    auto opType = this->get_type()->type();
    if ( dynamic_cast<const TxIntegerType*>( opType ) ) {
        return ConstantExpr::getNeg( operand  );
    }
    else if ( dynamic_cast<const TxFloatingType*>( opType ) ) {
        return ConstantExpr::getFNeg( operand );
    }
    THROW_LOGIC( "Invalid unary minus operand type: " << opType << " in " << this );
}

Value* TxUnaryMinusNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto operand = this->operand->code_gen_value( context, scope );
    auto opType = this->get_type()->type();
    if ( dynamic_cast<const TxIntegerType*>( opType ) ) {
        return scope->builder->CreateNeg( operand );
    }
    else if ( dynamic_cast<const TxFloatingType*>( opType ) ) {
        return scope->builder->CreateFNeg( operand );
    }
    THROW_LOGIC( "Invalid unary minus operand type: " << opType << " in " << this );
}

llvm::Constant* TxUnaryLogicalNotNode::code_gen_constant( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto operand = this->operand->code_gen_constant( context );
    return ConstantExpr::getNot( operand );
}

Value* TxUnaryLogicalNotNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto operand = this->operand->code_gen_value( context, scope );
    return scope->builder->CreateNot( operand );
}

Value* gen_get_struct_member( LlvmGenerationContext& context, GenScope* scope, Value* structV, unsigned ix ) {
    Value* memberV;
    if ( auto structPtrT = dyn_cast<PointerType>( structV->getType() ) ) {  // address of struct
        ASSERT( structPtrT->getPointerElementType()->isStructTy(), "expected pointer element to be a struct: " << structV );
        (void) structPtrT;   // suppresses unused variable warning in release mode
        if ( scope ) {
            auto memberA = scope->builder->CreateStructGEP( structPtrT->getPointerElementType(), structV, ix );
            memberV = scope->builder->CreateLoad( memberA );
        }
        else {
            Value *idxs[] = {
                              ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                              ConstantInt::get( Type::getInt32Ty( context.llvmContext ), ix )
            };
            auto memberA = GetElementPtrInst::CreateInBounds( structV, idxs );
            memberV = new LoadInst( memberA );
        }
    }
    else {  // direct / "register" struct
        ASSERT( structV->getType()->isStructTy(), "expected value to be a struct: " << structV );
        memberV = ( scope ? scope->builder->CreateExtractValue( structV, ix )
                          : ExtractValueInst::Create( structV, ix ) );
        //std::cerr << "gen_get_struct_member(), structV: " << structV << "   ix: " << ix << std::endl;
        //std::cerr << "                         memberV: " << memberV << std::endl;
    }
    return memberV;
}

Value* gen_get_ref_pointer( LlvmGenerationContext& context, GenScope* scope, Value* refV ) {
    Value* ptrV = gen_get_struct_member( context, scope, refV, 0 );
    ASSERT( ptrV->getType()->isPointerTy(), "expected ref.ptr element to be a pointer: " << refV );
    return ptrV;
}

Value* gen_get_ref_typeid( LlvmGenerationContext& context, GenScope* scope, Value* refV ) {
    Value* tidV = gen_get_struct_member( context, scope, refV, 1 );
    return tidV;
}

Value* gen_ref( LlvmGenerationContext& context, GenScope* scope, Type* refT, Value* ptrV, Value* tidV ) {
    if ( scope ) {
        Value* refV = UndefValue::get( refT );
        auto castPtrV = scope->builder->CreatePointerCast( ptrV, refT->getStructElementType( 0 ) );
        refV = scope->builder->CreateInsertValue( refV, castPtrV, 0 );
        refV = scope->builder->CreateInsertValue( refV, tidV, 1 );
        return refV;
    }
    else {
        ASSERT( false, "Not yet supported to construct reference to global: " << ptrV );  // TODO
        return nullptr;
    }
}

Value* gen_lambda( LlvmGenerationContext& context, GenScope* scope, Type* lambdaT, Value* funcV, Value* closureRefV ) {
    if ( scope ) {
        Value* lambdaV = UndefValue::get( lambdaT );
        auto castFuncV = scope->builder->CreatePointerCast( funcV, lambdaT->getStructElementType( 0 ) );
        lambdaV = scope->builder->CreateInsertValue( lambdaV, castFuncV, 0 );
        lambdaV = scope->builder->CreateInsertValue( lambdaV, closureRefV, 1 );
        return lambdaV;
    }
    else {
        ASSERT( false, "Not yet supported to construct global lambda" );  // TODO
        return nullptr;
    }
}

Value* TxReferenceToNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    Value* ptrV = nullptr;
    TxExpressionNode* targetNode = this->target;
    ptrV = targetNode->code_gen_address( context, scope );

    // the reference gets the statically known target type id
    auto tidV = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), targetNode->get_type()->get_type_id() );

    // box the pointer:
    auto refT = this->get_type()->type()->make_llvm_type( context );
    return gen_ref( context, scope, refT, ptrV, tidV );
}

Value* TxReferenceDerefNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    if ( !this->refExprValue ) {
        this->refExprValue = this->reference->code_gen_value( context, scope );
        if ( !this->refExprValue )
            return NULL;
    }
    return gen_get_ref_pointer( context, scope, this->refExprValue );
}

Value* TxReferenceDerefNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    Value* ptrV = this->code_gen_address( context, scope );
    if ( scope )
        return scope->builder->CreateLoad( ptrV );
    else
        return new LoadInst( ptrV );
}

Value* TxReferenceDerefNode::code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const {
    // dynamic by reading the reference's target type id
    TRACE_CODEGEN( this, context, " TypeID" );
    if ( !this->refExprValue ) {
        this->refExprValue = this->reference->code_gen_value( context, scope );
        if ( !this->refExprValue )
            return NULL;
    }

    Value* tidV = gen_get_ref_typeid( context, scope, this->refExprValue );
    return tidV;
}

static Value* gen_elem_address( LlvmGenerationContext& context, GenScope* scope, Value* arrayPtrV, Value* subscriptV ) {
    ASSERT( subscriptV->getType()->isIntegerTy(), "expected subscript to be an integer: " << subscriptV );
    ASSERT( arrayPtrV->getType()->isPointerTy(), "expected array-operand to be a pointer: " << arrayPtrV );
    ASSERT( arrayPtrV->getType()->getPointerElementType()->isStructTy(), "expected array-operand to be a pointer to struct: " << arrayPtrV );

// semantic pass adds AST nodes performing the bounds check
//    { // bounds check
//        Value* lenIxs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
//                            ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 1 ) };
//        auto lengthPtrV = GetElementPtrInst::CreateInBounds( arrayPtrV, lenIxs );
//        auto lengthV = new LoadInst( lengthPtrV );
//
//    }

    if ( auto arrayPtrC = dyn_cast<Constant>( arrayPtrV ) ) {
        // address of global constant
        if ( auto intC = dyn_cast<ConstantInt>( subscriptV ) ) {
            Constant* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                                ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 2 ),
                                intC };
            return ConstantExpr::getInBoundsGetElementPtr( arrayPtrC->getType()->getPointerElementType(), arrayPtrC, ixs );
        }
    }

    Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                     ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 2 ),
                     subscriptV };
    if ( scope )
        return scope->builder->CreateInBoundsGEP( arrayPtrV, ixs );
    else
        return GetElementPtrInst::CreateInBounds( arrayPtrV, ixs );
}

Value* TxElemDerefNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return gen_elem_address( context, scope, this->array->code_gen_address( context, scope ),
                             this->subscript->code_gen_value( context, scope ) );
}

Value* TxElemDerefNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    Value* elemPtr = gen_elem_address( context, scope, this->array->code_gen_address( context, scope ),
                                       this->subscript->code_gen_value( context, scope ) );
    if ( scope )
        return scope->builder->CreateLoad( elemPtr );
    else
        return new LoadInst( elemPtr );
}

Constant* TxElemDerefNode::code_gen_constant( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto arrayC = this->array->code_gen_constant( context );
    auto subscriptC = cast<ConstantInt>( this->subscript->code_gen_constant( context ) );
    ASSERT( arrayC->getType()->isStructTy(), "Can't create constant array elem deref expression with array value that is: "
            << arrayC << "  type: " << arrayC->getType() );
    uint32_t ixs[] = { 2, (uint32_t) subscriptC->getLimitedValue( UINT32_MAX ) };
    return ConstantExpr::getExtractValue( arrayC, ixs );
}

Value* TxElemAssigneeNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return gen_elem_address( context, scope, this->array->code_gen_address( context, scope ), this->subscript->code_gen_value( context, scope ) );
}

Value* TxFieldAssigneeNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return this->field->code_gen_address( context, scope );
}

Value* TxDerefAssigneeNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto refval = this->operand->code_gen_value( context, scope );
    return gen_get_ref_pointer( context, scope, refval );
}

static Value* gen_call( const TxFunctionCallNode* node, LlvmGenerationContext& context, GenScope* scope, Value* functionPtrV, Value* closureRefV,
                        const std::string& exprLabel ) {
    std::vector<Value*> args;
    args.push_back( closureRefV );
    for ( auto argDef : *node->argsExprList ) {
        args.push_back( argDef->code_gen_expr( context, scope ) );
    }

    if ( scope )
        return scope->builder->CreateCall( functionPtrV, args, exprLabel );
    else {
        // FUTURE: support calling functions outside of code block (statically constant or instance initialization)
        LOG( context.LOGGER(), ERROR, "calling functions outside of code block not currently supported" );
        return nullptr;
    }
}

static Value* gen_call( const TxFunctionCallNode* node, LlvmGenerationContext& context, GenScope* scope, const std::string& exprLabel ) {
    auto lambdaV = node->callee->code_gen_value( context, scope );
    //std::cout << "callee: " << lambdaV << std::endl;
    auto functionPtrV = gen_get_struct_member( context, scope, lambdaV, 0 );
    auto closureRefV = gen_get_struct_member( context, scope, lambdaV, 1 );
    return gen_call( node, context, scope, functionPtrV, closureRefV, exprLabel );
}

Constant* TxFunctionCallNode::code_gen_constant( LlvmGenerationContext& context ) const {
    ASSERT( this->inlinedExpression, "invoked code_gen_constant() on function call that has no inlined expression: " << this );
    return this->inlinedExpression->code_gen_constant( context );
}

Value* TxFunctionCallNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    if ( this->inlinedExpression )
        return this->inlinedExpression->code_gen_value( context, scope );
    else {
        // pick field's plain name, if available, for the expression value:
        const std::string fieldName = ( this->fieldDefNode ? this->fieldDefNode->get_identifier() : "" );
        return gen_call( this, context, scope, fieldName );
    }
}

Value* TxConstructorCalleeExprNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    Value* funcPtrV = this->gen_func_ptr( context, scope );
    auto allocType = this->objectExpr->get_type();
    Constant* instanceTypeIdV = allocType->type()->gen_typeid( context, scope );
    // construct the lambda object:
    auto closureRefT = context.get_voidRefT();
    auto closureRefV = gen_ref( context, scope, closureRefT, this->gen_obj_ptr( context, scope ), instanceTypeIdV );
    auto lambdaT = cast<StructType>( context.get_llvm_type( this->get_type() ) );
    return gen_lambda( context, scope, lambdaT, funcPtrV, closureRefV );
}

Value* TxConstructorCalleeExprNode::gen_obj_ptr( LlvmGenerationContext& context, GenScope* scope ) const {
    if ( !this->objectPtrV ) {
        this->objectPtrV = this->objectExpr->code_gen_address( context, scope );
        ASSERT( this->objectPtrV->getType()->isPointerTy(), "Expected baseValue to be of pointer type but was: " << this->objectPtrV->getType() );
    }
    return this->objectPtrV;
}

Value* TxConstructorCalleeExprNode::gen_func_ptr( LlvmGenerationContext& context, GenScope* scope ) const {
    // constructors are similar to instance methods, but they are not virtual (and not in vtable)

    // find the constructor
    // (constructors aren't inherited, but we bypass equivalent specializations to find the code-generated constructor)
    auto uniqueName = this->declaration->get_unique_name();
    const TxActualType* allocType = this->objectExpr->get_type()->type();
    while ( allocType->is_equivalent_derivation() )  // as we don't generate code for equivalent specializations
        allocType = allocType->get_semantic_base_type();
    auto uniqueFullName = allocType->get_declaration()->get_unique_full_name() + "." + uniqueName;
    //std::cerr << "Code-generated constructor name: " << uniqueFullName << " (from: " << this->get_spec(0).declaration->get_unique_full_name() << ")" << std::endl;

    Value* funcPtrV = context.lookup_llvm_value( uniqueFullName );
    if ( !funcPtrV ) {
        if ( auto txType = this->get_type()->type() ) {
            // forward declaration situation
            if ( auto txFuncType = dynamic_cast<const TxFunctionType*>( txType ) ) {
                LOG_DEBUG( context.LOGGER(), "Forward-declaring constructor function " << uniqueFullName << ": " << txFuncType );
                StructType *lambdaT = cast<StructType>( context.get_llvm_type( txFuncType ) );
                FunctionType *funcT = cast<FunctionType>( cast<PointerType>( lambdaT->getElementType( 0 ) )->getPointerElementType() );
                auto funcName = uniqueFullName;
                funcPtrV = context.llvmModule().getOrInsertFunction( funcName, funcT );
            }
            else
                LOG( context.LOGGER(), ERROR, "No LLVM type defined for " << txType );
        }
    }
    return funcPtrV;
}

Value* TxHeapAllocNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    Type* objT = context.get_llvm_type( this->get_type() );
    return context.gen_malloc( scope, objT );
}

Value* TxStackAllocNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return this->get_type()->type()->gen_alloca( context, scope );
}

Value* TxNewConstructionNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    // new constructor returns the constructed object by reference
    TRACE_CODEGEN( this, context );

    Value* objAllocV = static_cast<TxConstructorCalleeExprNode*>( this->constructorCall->callee )->gen_obj_ptr( context, scope );
    // initialize the object
    if ( this->initializationExpression ) {
        auto initValue = this->initializationExpression->code_gen_value( context, scope );
        ASSERT( scope, "new expression not supported in global/static scope: " << this->parse_loc_string() );
        scope->builder->CreateStore( initValue, objAllocV );
    }
    else {
        this->constructorCall->code_gen_value( context, scope );
    }

    Type* objRefT = context.get_llvm_type( this->get_type() );
    if ( !objRefT )
        return nullptr;
    Constant* objTypeIdV = this->get_object_type()->type()->gen_typeid( context, scope );
    auto objRefV = gen_ref( context, scope, objRefT, objAllocV, objTypeIdV );
    return objRefV;
}

Value* TxStackConstructionNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    // stack constructor returns the constructed object by value, not by reference
    TRACE_CODEGEN( this, context );
    if ( this->initializationExpression ) {
        // if inlined, the stack constructor doesn't need to actually allocate storage on stack
        // (the receiver of this expression value might do this, if it needs to)
        return this->initializationExpression->code_gen_value( context, scope );
    }
    else {
        Value* objAllocV = static_cast<TxConstructorCalleeExprNode*>( this->constructorCall->callee )->gen_obj_ptr( context, scope );
        this->constructorCall->code_gen_value( context, scope );
        return objAllocV;
    }
}

