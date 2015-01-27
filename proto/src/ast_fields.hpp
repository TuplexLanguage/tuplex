#pragma once

#include "ast_base.hpp"


class TxFieldValueNode : public TxExpressionNode {
    mutable std::vector<const TxSymbolScope*> memberPath;
    mutable TxFieldEntity const * entity = nullptr;
public:
    TxExpressionNode* base;
    const TxIdentifierNode* member;

    TxFieldValueNode(const yy::location& parseLocation, TxExpressionNode* base, const TxIdentifierNode* member)
        : TxExpressionNode(parseLocation), base(base), member(member) {
//        if (base)
//            std::cout << "Created TxFieldValueNode(NULL, " << *member << ")" << std::endl;
//        else
//            std::cout << "Created TxFieldValueNode(base, " << *member << ")" << std::endl;
    }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        if (base)
            base->symbol_table_pass(lexContext);
    }

    virtual const TxFieldEntity* get_entity() const {
        // FUTURE: support overloaded field resolution in non-call expressions (e.g. overloadedFunc(Int,Float).typeid )
        if (! this->entity) {
            if (! this->memberPath.empty())
                return nullptr;  // has already been attempted and failed
            if (base) {
                // (lookup is similar to that of TxFieldEntity)
                if (auto symbol = this->base->get_type()->lookup_instance_member(memberPath, this->member->ident)) {
                    this->entity = this->context().scope()->resolve_field(symbol, this->appliedFuncArgTypes);
                    if (this->entity && memberPath.back() != this->entity)
                        memberPath[memberPath.size()-1] = this->entity;
                }
            }
            else
                this->entity = this->context().scope()->resolve_field(memberPath, this->member->ident, this->appliedFuncArgTypes);
        }
        return this->entity;
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        auto ent = this->get_entity();
        return ent ? ent->get_type() : nullptr;
    }

    /** returns int value, if this is a statically constant integer expression */
    virtual long get_int_value() const {
        if (! this->is_context_set())
            return 0; // FIXME
        if (! this->is_statically_constant())
            parser_error(this->parseLocation, "Non-constant field used in constant expression: %s", this->member->to_string().c_str());
        return 0;  // FIXME
    }

    virtual bool is_statically_constant() const {
        if (auto ent = this->get_entity())
            return ent->is_statically_constant();
        return false;
    }

    virtual void semantic_pass() {
        if (base)
            base->semantic_pass();
        if (! this->get_entity())
            parser_error(this->parseLocation, "No such field: %s (from %s)", this->member->ident.to_string().c_str(), this->context().to_string().c_str());
    }

//    virtual bool has_address() const {
//        auto entity = this->get_entity();
//        return (entity->get_storage() != TXS_NOSTORAGE);
//    }

    virtual llvm::Value* codeGen(LlvmGenerationContext& context, GenScope* scope) const;
};


class TxFieldAssigneeNode : public TxAssigneeNode {
    mutable std::vector<const TxSymbolScope*> memberPath;
    mutable TxFieldEntity const * entity = nullptr;
public:
    TxExpressionNode* base;
    const TxIdentifierNode* member;

    TxFieldAssigneeNode(const yy::location& parseLocation, TxExpressionNode* base, const TxIdentifierNode* member)
        : TxAssigneeNode(parseLocation), base(base), member(member) {
//        if (base)
//            std::cout << "Created TxFieldAssigneeNode(NULL, " << *member << ")" << std::endl;
//        else
//            std::cout << "Created TxFieldAssigneeNode(base, " << *member << ")" << std::endl;
    }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        if (base)
            base->symbol_table_pass(lexContext);
    }

    virtual const TxFieldEntity* get_entity() const {
        //return this->context().scope()->resolve_field(this->member->ident);
        // FUTURE: support overloaded field resolution in assignment (e.g. overloadedFuncPointer(Int,Float) = func )
        if (! this->entity) {
            if (! this->memberPath.empty())
                return nullptr;  // has already been attempted and failed
            if (base) {
                // (lookup is similar to that of TxFieldEntity)
                this->entity = dynamic_cast<const TxFieldEntity*>(this->base->get_type()->lookup_instance_member(memberPath, this->member->ident));
            }
            else
                this->entity = this->context().scope()->resolve_field(memberPath, this->member->ident);
        }
        return this->entity;
    }

    virtual const TxType* get_type() const {
        auto entity = this->get_entity();
        return entity ? entity->get_type() : nullptr;
    }

    virtual void semantic_pass() {
        if (base)
            base->semantic_pass();
        auto entity = this->get_entity();
        if (! entity)
            parser_error(this->parseLocation, "No such field: %s (from %s)", this->member->ident.to_string().c_str(), this->context().to_string().c_str());
        else if (entity->get_storage() == TXS_NOSTORAGE)
            parser_error(this->parseLocation, "Assignee %s is not an L-value / has no storage.", member->to_string().c_str());
    }

//    virtual bool hasAddress() const {
//        auto entity = this->get_entity();
//        return (entity->get_storage() != TXS_NOSTORAGE);
//    }

    virtual llvm::Value* codeGen(LlvmGenerationContext& context, GenScope* scope) const;
};
