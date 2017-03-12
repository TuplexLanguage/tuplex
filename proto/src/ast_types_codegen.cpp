#include "ast_types.hpp"
#include "llvm_generator.hpp"


using namespace llvm;


Value* TxTypeTypeArgumentNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    return this->typeExprNode->code_gen(context, scope);
}

Value* TxValueTypeArgumentNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    return this->valueExprNode->code_gen(context, scope);
}

Value* TxGenSpecTypeNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    for (TxTypeArgumentNode* ta : *this->typeArgs)
        ta->code_gen(context, scope);
    return nullptr;
}

Value* TxIdentifiedTypeNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    return nullptr;
}


Value* TxReferenceTypeNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    this->targetTypeNode->code_gen(context, scope);
    return nullptr;
}


Value* TxArrayTypeNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    if (this->lengthNode)
        this->lengthNode->code_gen(context, scope);
    this->elementTypeNode->code_gen(context, scope);
    return nullptr;
}


Value* TxDerivedTypeNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    for (auto type : *this->baseTypes)
        type->code_gen(context, scope);
    for (auto member : *this->members)
        member->code_gen(context, scope);
    return nullptr;
}


Value* TxSuperTypeNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    this->derivedTypeNode->code_gen(context, scope);
    return nullptr;
}


Value* TxFunctionTypeNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    for (auto argDef : *this->arguments)
        argDef->code_gen(context, scope);
    if (this->returnField)
        this->returnField->code_gen(context, scope);
    return nullptr;
}


Value* TxModifiableTypeNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    this->baseType->code_gen(context, scope);
    return nullptr;
}
