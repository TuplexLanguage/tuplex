#include "ast_array.hpp"
#include "llvm_generator.hpp"

using namespace llvm;

Value* TxArrayLitNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    // experimental, automatically allocates global space for constants
    std::cerr << "TxArrayLitNode::code_gen_address " << this << std::endl;
    auto targetVal = this->code_gen_expr( context, scope );
    if ( auto constInitializer = dyn_cast<Constant>( targetVal ) ) {
        return new GlobalVariable( context.llvmModule(), constInitializer->getType(), true, GlobalValue::InternalLinkage, constInitializer );
    }
    else {
        return targetVal;
    }
}

Value* TxFilledArrayLitNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );

    if ( this->_directArrayArg ) {
        return this->elemExprList->front()->code_gen_value( context, scope );
    }

    {
        ASSERT( scope, "Expected non-constant array literal to be codegen'd within a scope: " << this );
        Type* arrayObjT = this->get_type()->type()->make_llvm_type( context );
        Value* arrayObjV = UndefValue::get( arrayObjT );
        auto capacityC = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->elemExprList->size() );
        arrayObjV = scope->builder->CreateInsertValue( arrayObjV, capacityC, 0 );
        arrayObjV = scope->builder->CreateInsertValue( arrayObjV, capacityC, 1 );  // length equals capacity

        if ( this->elemExprList->empty() ) {
            Constant* emptyArrayC = ConstantArray::get( cast<ArrayType>( arrayObjT->getContainedType( 2 ) ), ArrayRef<Constant*>() );
            arrayObjV = scope->builder->CreateInsertValue( arrayObjV, emptyArrayC, 2 );
        }
        else {
            for ( unsigned i = 0; i < this->elemExprList->size(); i++ ) {
                auto elemV = this->elemExprList->at( i )->code_gen_value( context, scope );
                arrayObjV = scope->builder->CreateInsertValue( arrayObjV, elemV, std::vector<unsigned>( { 2, i } ) );
            }
        }
        return arrayObjV;
    }

    /*
    Value* arrayPtrV = this->get_type()->type()->gen_alloca( context, scope, "array_lit" );
    { // initialize length field:
        Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                         ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 1 ) };
        auto lenField = scope->builder->CreateInBoundsGEP( arrayPtrV, ixs );
        auto lenVal = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->elemExprList->size() );
        scope->builder->CreateStore( lenVal, lenField );
    }
    for ( unsigned i = 0; i < this->elemExprList->size(); i++ ) {
        Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                         ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 2 ),
                         ConstantInt::get( Type::getInt32Ty( context.llvmContext ), i ) };
        auto elemAddr = scope->builder->CreateInBoundsGEP( arrayPtrV, ixs );
        scope->builder->CreateStore( this->elemExprList->at( i )->code_gen_value( context, scope ), elemAddr );
    }
    return arrayPtrV;
    */
}

Constant* TxFilledArrayLitNode::code_gen_constant( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );

    if ( this->_directArrayArg ) {
        return this->elemExprList->front()->code_gen_constant( context );
    }
    // FUTURE: optimize for arrays of scalars
    //    if (this->elementTypeNode->typeExprNode->get_type()->type()->is_scalar()) {
    //        Constant* dataArray = ConstantDataArray::get( context.llvmContext, nullptr );
    //        std::vector<Constant*> members {
    //            ConstantInt::get( context.llvmContext, APInt( 32, this->argsExprList->size() ) ),
    //            dataArray
    //        };
    //        return ConstantStruct::getAnon(members);
    //    }
    std::vector<Constant*> values;
    for ( auto elemExpr : *this->elemExprList )
        values.push_back( elemExpr->code_gen_constant( context ) );
    ArrayRef<Constant*> data( values );

    Type* elemT = context.get_llvm_type( this->get_type()->element_type() );
    uint64_t arrayLen = this->elemExprList->size();
    ArrayType* arrayT = ArrayType::get( elemT, arrayLen );
    Constant* dataArrayC = ConstantArray::get( arrayT, data );
    std::vector<Constant*> objMembers {
                                        ConstantInt::get( context.llvmContext, APInt( 32, arrayLen ) ),
                                        ConstantInt::get( context.llvmContext, APInt( 32, arrayLen ) ),
                                        dataArrayC
    };
    return ConstantStruct::getAnon( objMembers );
}


static Value* unfilled_array_code_gen_value( LlvmGenerationContext& context, GenScope* scope, const TxArrayType* txArrayType ) {
    Type* arrayObjT = txArrayType->make_llvm_type( context );
    Value* arrayObjV = UndefValue::get( arrayObjT );
    Value* arrayCapV = txArrayType->capacity()->code_gen_expr( context, scope );

    arrayObjV = scope->builder->CreateInsertValue( arrayObjV, arrayCapV, 0 );
    arrayObjV = scope->builder->CreateInsertValue( arrayObjV, ConstantInt::get( context.llvmContext, APInt( 32, 0 ) ), 1 );

    auto arrayT = cast<ArrayType>( arrayObjT->getContainedType( 2 ) );
    Constant* emptyArrayC = ConstantArray::get( arrayT, ArrayRef<Constant*>() );
    arrayObjV = scope->builder->CreateInsertValue( arrayObjV, emptyArrayC, 2 );
    return arrayObjV;
}

static Constant* unfilled_array_code_gen_constant( LlvmGenerationContext& context, const TxArrayType* txArrayType  ) {
    Type* arrayObjT = txArrayType->make_llvm_type( context );
    auto arrayT = cast<ArrayType>( arrayObjT->getContainedType( 2 ) );
    auto cap = arrayT->getArrayNumElements();
    //std::cerr << txArrayType << " code_gen_constant() capacity=" << cap << std::endl;
    std::vector<Constant*> objMembers {
                                        ConstantInt::get( context.llvmContext, APInt( 32, cap ) ),
                                        ConstantInt::get( context.llvmContext, APInt( 32, 0 ) ),
                                        ConstantArray::get( arrayT, ArrayRef<Constant*>() )
    };
    return ConstantStruct::getAnon( objMembers );
}

Value* TxUnfilledArrayLitNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto txArrayType = static_cast<const TxArrayType*>( this->get_type()->type() );
    return unfilled_array_code_gen_value( context, scope, txArrayType );
}

Constant* TxUnfilledArrayLitNode::code_gen_constant( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto txArrayType = static_cast<const TxArrayType*>( this->get_type()->type() );
    return unfilled_array_code_gen_constant( context, txArrayType );
}

Value* TxUnfilledArrayCompLitNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto txArrayType = static_cast<const TxArrayType*>( this->get_type()->type() );
    return unfilled_array_code_gen_value( context, scope, txArrayType );
}

Constant* TxUnfilledArrayCompLitNode::code_gen_constant( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto txArrayType = static_cast<const TxArrayType*>( this->get_type()->type() );
    return unfilled_array_code_gen_constant( context, txArrayType );
}
