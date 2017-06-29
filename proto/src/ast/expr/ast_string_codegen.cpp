#include "ast_string.hpp"

#include "ast_ref.hpp"

#include "llvm_generator.hpp"

using namespace llvm;


Constant* TxStringLitNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, this->literal );

    auto stringObjT = cast<StructType>( context.get_llvm_type( this->get_type() ) );
    auto arrayTIdC = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->arrayTypeNode->get_type()->get_type_id() );
    auto arrayRefObjT = cast<StructType>( stringObjT->getTypeAtIndex( 0U ) );

    std::vector<Constant*> arrayMembers {
        ConstantInt::get( context.llvmContext, APInt( 32, this->utf8data.size() ) ),
        ConstantInt::get( context.llvmContext, APInt( 32, this->utf8data.size() ) ),
        ConstantDataArray::get( context.llvmContext, ArrayRef<uint8_t>( this->utf8data.data(), this->utf8data.size() ) )
    };
    auto arrayC = ConstantStruct::getAnon( arrayMembers );
    auto arrayGlobalPtr = new GlobalVariable( context.llvmModule(), arrayC->getType(), true, GlobalValue::InternalLinkage, arrayC );

    // String datatype member is a reference to unknown array length (represented by length 0 in LLVM array type)
    auto arrayGenPtr = ConstantExpr::getBitCast( arrayGlobalPtr, arrayRefObjT->getTypeAtIndex( 0U ) );
    auto arrayRefObjC = gen_ref( context, arrayRefObjT, arrayGenPtr, arrayTIdC );
    auto tupleC = ConstantStruct::get( stringObjT, { arrayRefObjC } );
    return tupleC;
}

Value* TxStringLitNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->code_gen_const_value( context );
}


Value* TxConcatenateStringsNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return this->stackConstr->code_gen_dyn_value( context, scope );
}

Value* TxConcatenateStringsNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return this->stackConstr->code_gen_dyn_address( context, scope );
}


Value* TxStringFormatNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return this->stackConstr->code_gen_dyn_value( context, scope );
}

Value* TxStringFormatNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return this->stackConstr->code_gen_dyn_address( context, scope );
}


//Value* TxStringFormatterNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
//    TRACE_CODEGEN( this, context );
//    return this->stackConstr->code_gen_dyn_value( context, scope );
//}
//
//Value* TxStringFormatterNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
//    TRACE_CODEGEN( this, context );
//    return this->stackConstr->code_gen_dyn_address( context, scope );
//}


Constant* TxCStringLitNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, '"' << this->value << '"' );
    std::vector<Constant*> members {
                                     ConstantInt::get( context.llvmContext, APInt( 32, this->arrayCapacity ) ),
                                     ConstantInt::get( context.llvmContext, APInt( 32, this->arrayCapacity ) ),
                                     ConstantDataArray::getString( context.llvmContext, this->value )
    };
    auto str = ConstantStruct::getAnon( members );
    return str;
}

Value* TxCStringLitNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->code_gen_const_value( context );
}
