#include "ast_types.hpp"
#include "llvm_generator.hpp"


llvm::Value* TxTypeArgumentNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    if (this->typeDeclNode)
        return this->typeDeclNode->code_gen(context, scope);
    else
        return this->fieldDeclNode->code_gen(context, scope);
}

llvm::Value* TxSpecializedTypeNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    for (TxTypeArgumentNode* ta : *this->typeArgs)
        ta->code_gen(context, scope);
    return nullptr;
}


llvm::Value* TxDerivedTypeNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    for (auto type : *this->baseTypes)
        type->code_gen(context, scope);
    for (auto member : *this->staticMembers)
        member->code_gen(context, scope);
    for (auto member : *this->instanceMembers)
        member->code_gen(context, scope);
    return nullptr;
}
