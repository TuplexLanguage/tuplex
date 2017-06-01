#include "ast_array.hpp"
#include "llvm_generator.hpp"

using namespace llvm;

//Value* TxArrayLitNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
//    // experimental, automatically allocates global space for constants
//    std::cerr << "TxArrayLitNode::code_gen_address " << this << std::endl;
//    auto targetVal = this->code_gen_expr( context, scope );
//    if ( auto constInitializer = dyn_cast<Constant>( targetVal ) ) {
//        return new GlobalVariable( context.llvmModule, constInitializer->getType(), true, GlobalValue::InternalLinkage, constInitializer );
//    }
//    else {
//        return targetVal;
//    }
//}

Value* TxArrayLitNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );

    if ( this->_directArrayArg ) {
        return this->elemExprList->front()->code_gen_value( context, scope );
    }

    {
        ASSERT( scope, "Expected non-constant array literal to be codegen'd within a scope: " << this );
        Type* arrayT = this->get_type()->type()->make_llvm_type( context );
        Value* arrayV = UndefValue::get( arrayT );
        auto capacityC = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->elemExprList->size() );
        arrayV = scope->builder->CreateInsertValue( arrayV, capacityC, 0 );
        arrayV = scope->builder->CreateInsertValue( arrayV, capacityC, 1 );  // length equals capacity

        if ( this->elemExprList->empty() ) {
            Constant* emptyArrayC = ConstantArray::get( cast<ArrayType>( arrayT->getContainedType( 2 ) ), ArrayRef<Constant*>() );
            arrayV = scope->builder->CreateInsertValue( arrayV, emptyArrayC, 2 );
        }
        else {
            for ( unsigned i = 0; i < this->elemExprList->size(); i++ ) {
                auto elemV = this->elemExprList->at( i )->code_gen_value( context, scope );
                arrayV = scope->builder->CreateInsertValue( arrayV, elemV, std::vector<unsigned>( { 2, i } ) );
            }
        }
        return arrayV;
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

Constant* TxArrayLitNode::code_gen_constant( LlvmGenerationContext& context ) const {
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

    Type* elemType = context.get_llvm_type( this->get_type()->element_type()->type() );
    uint64_t arrayLen = this->elemExprList->size();
    ArrayType* arrayType = ArrayType::get( elemType, arrayLen );
    Constant* dataArray = ConstantArray::get( arrayType, data );
    std::vector<Constant*> objMembers {
                                        ConstantInt::get( context.llvmContext, APInt( 32, arrayLen ) ),
                                        ConstantInt::get( context.llvmContext, APInt( 32, arrayLen ) ),
                                        dataArray
    };
    return ConstantStruct::getAnon( objMembers );
}
