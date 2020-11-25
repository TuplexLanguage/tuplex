#include "ast_string.hpp"

#include "ast_ref.hpp"

#include "llvm_generator.hpp"

using namespace llvm;


Constant* TxStringLitNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, this->literal );

    StructType* stringObjT = cast<StructType>( context.get_llvm_type( this->qtype() ) );
    Constant* arrayTIdC = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->arrayTypeNode->qtype()->get_runtime_type_id() );
    auto arrayRefObjT = cast<StructType>( stringObjT->getTypeAtIndex( 0U ) );

    auto arrayGlobalPtr = context.gen_const_byte_array_address( this->utf8data );

    // String datatype member is a reference to unknown array length (represented by length 0 in LLVM array type)
    auto arrayGenPtr = ConstantExpr::getBitCast( arrayGlobalPtr, arrayRefObjT->getTypeAtIndex( 0U ) );
    auto arrayRefObjC = gen_ref( context, arrayRefObjT, arrayGenPtr, arrayTIdC );
    auto tupleC = ConstantStruct::get( stringObjT, { arrayRefObjC } );
    return tupleC;
}

Value* TxStringLitNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->code_gen_const_value( context );
}

Constant* TxStringLitNode::code_gen_const_address( LlvmGenerationContext& context ) const {
    // allocates global storage for string literals, a single shared instance for each unique value
    StructType* stringObjT = cast<StructType>( context.get_llvm_type( this->qtype() ) );
    Constant* arrayTIdC = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->arrayTypeNode->qtype()->get_runtime_type_id() );
    return context.gen_const_string_obj_address( stringObjT, arrayTIdC, this->utf8data );
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


Constant* TxCStringLitNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, '"' << this->value << '"' );
    auto arrayCapacity = this->value.length() + 1;  // note: array capacity includes the null terminator
    std::vector<Constant*> members {
                                     ConstantInt::get( context.llvmContext, APInt( 32, arrayCapacity ) ),
                                     ConstantInt::get( context.llvmContext, APInt( 32, arrayCapacity ) ),
                                     ConstantDataArray::getString( context.llvmContext, this->value )
    };
    auto str = ConstantStruct::getAnon( members );
    return str;
}

Value* TxCStringLitNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->code_gen_const_value( context );
}

Constant* TxCStringLitNode::code_gen_const_address( LlvmGenerationContext& context ) const {
    // allocates global storage for string literals, a single shared instance for each unique value
    return context.gen_const_cstring_address( this->value );
}
