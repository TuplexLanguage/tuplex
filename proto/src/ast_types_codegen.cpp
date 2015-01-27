#include "ast_types.hpp"
#include "llvm_generator.hpp"


llvm::Value* TxDerivedTypeNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    for (auto type : *this->baseTypes)
        type->codeGen(context, scope);
    for (auto member : *this->staticMembers)
        member->codeGen(context, scope);
    for (auto member : *this->instanceMembers)
        member->codeGen(context, scope);
    return nullptr;
}
