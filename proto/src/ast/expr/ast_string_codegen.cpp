#include "ast_string.hpp"

#include "ast_ref.hpp"

#include "llvm_generator.hpp"

using namespace llvm;


Constant* TxStringLitNode::code_gen_constant( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, this->literal );

    auto stringObjT = cast<StructType>( context.get_llvm_type( this->stringTypeNode->get_type() ) );
    auto arrayTIdC = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->arrayTypeNode->get_type()->get_type_id() );
    auto arrayRefObjT = cast<StructType>( stringObjT->getTypeAtIndex( 0U ) );

    std::vector<Constant*> arrayMembers {
        ConstantInt::get( context.llvmContext, APInt( 32, this->utf8data.size() ) ),
        ConstantInt::get( context.llvmContext, APInt( 32, this->utf8data.size() ) ),
        ConstantDataArray::get( context.llvmContext, ArrayRef<uint8_t>( this->utf8data.data(), this->utf8data.size() ) )
    };
    auto arrayC = ConstantStruct::getAnon( arrayMembers );
    auto arrayGlobal = new GlobalVariable( context.llvmModule(), arrayC->getType(), true, GlobalValue::InternalLinkage, arrayC );

    // String datatype member is a reference to unknown array length (represented by length 0 in LLVM array type):
    auto arrayGlobalPtrC = ConstantExpr::getBitCast( arrayGlobal, arrayRefObjT->getTypeAtIndex( 0U ) );

    auto arrayRefObjC = gen_ref( context, arrayRefObjT, arrayGlobalPtrC, arrayTIdC );
    auto tupleC = ConstantStruct::get( stringObjT, { arrayRefObjC } );
    return tupleC;
}

Value* TxStringLitNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->code_gen_constant( context );
}

Value* TxStringLitNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    // experimental, automatically allocates space for literals, used for e.g. string literals
    auto constant = this->code_gen_value( context, scope );
    auto constInitializer = cast<Constant>( constant );
    return new GlobalVariable( context.llvmModule(), constInitializer->getType(), true, GlobalValue::InternalLinkage, constInitializer );
}
