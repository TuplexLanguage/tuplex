#pragma once

#include "ast_base.hpp"


class TxFieldValueNode : public TxExpressionNode {
    mutable std::vector<const TxSymbolScope*> memberPath;
    mutable TxFieldEntity const * cachedEntity = nullptr;

    class FieldTypeDefiner : public TxTypeDefiner {
        TxFieldValueNode* fieldNode;
    public:
        FieldTypeDefiner(TxFieldValueNode* fieldNode) : fieldNode(fieldNode) { }
        virtual const TxType* attempt_get_type() const override { return fieldNode->get_type(); }
        virtual const TxType* get_type() const override { return fieldNode->get_type(); }
    } fieldTypeDefiner;

public:
    TxExpressionNode* base;
    const TxIdentifierNode* member;

    TxFieldValueNode(const yy::location& parseLocation, TxExpressionNode* base, const TxIdentifierNode* member)
        : TxExpressionNode(parseLocation), fieldTypeDefiner(this), base(base), member(member) {
//        if (base)
//            std::cout << "Created TxFieldValueNode(NULL, " << *member << ")" << std::endl;
//        else
//            std::cout << "Created TxFieldValueNode(base, " << *member << ")" << std::endl;
    }

    virtual bool has_predefined_type() const override { return true; }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        if (base)
            base->symbol_table_pass(lexContext);
    }

    virtual const TxFieldEntity* get_entity() const {
        // FUTURE: support overloaded field resolution in non-call expressions (e.g. overloadedFunc(Int,Float).typeid )
        if (! this->cachedEntity) {
            if (! this->memberPath.empty())
                return nullptr;  // has already been attempted and failed
            if (base) {
                // (lookup is similar to that of TxFieldEntity)
                if (auto symbol = this->base->get_type()->lookup_instance_member(memberPath, this->member->ident)) {
                    this->cachedEntity = this->context().scope()->resolve_symbol_as_field(symbol, this->appliedFuncArgTypes);
                    if (this->cachedEntity && memberPath.back() != this->cachedEntity)
                        memberPath[memberPath.size()-1] = this->cachedEntity;
                }
            }
            else
                this->cachedEntity = this->context().scope()->resolve_field(memberPath, this->member->ident, this->appliedFuncArgTypes);

            if (this->cachedEntity) {
                if (this->cachedEntity->get_decl_flags() & TXD_GENPARAM) {
                    // alias for a generic type parameter
                    LOGGER().debug("%s: Resolving '%s' as GENPARAM %s", this->parse_loc_string().c_str(), this->member->ident.to_string().c_str(), this->cachedEntity->to_string().c_str());
//                    auto typeName = this->context().scope()->get_unique_name("$alias");
//                    auto typeAliasEnt = this->context().scope()->declare_type(typeName, &this->fieldTypeDefiner, TXD_PUBLIC | TXD_IMPLICIT);
//                    if (!typeAliasEnt)
//                        cerror("Failed to declare implicit type %s", typeName.c_str());
//                    typeAliasEnt->set_alias();
//                    LOGGER().debug("%s: Declared '%s' as alias for GENPARAM %s", this->parse_loc_string().c_str(), this->member->ident.to_string().c_str(), this->cachedEntity->to_string().c_str());
                }
                else if (this->cachedEntity->get_static_constant_proxy())
                    LOGGER().debug("%s: Resolving '%s' as statically constant %s", this->parse_loc_string().c_str(), this->member->ident.to_string().c_str(), this->cachedEntity->to_string().c_str());
            }
        }
        return this->cachedEntity;
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        auto ent = this->get_entity();
        return ent ? ent->get_type() : nullptr;
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override {
        if (auto ent = this->get_entity())
            if (auto constProxy = ent->get_static_constant_proxy()) {
                std::cout << "Returning static constant proxy for field " << ent->get_full_name() << std::endl;
                return constProxy;
            }
        return nullptr;
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
            cerror("No such field: %s (from %s)", this->member->ident.to_string().c_str(), this->context().to_string().c_str());
    }

//    virtual bool has_address() const {
//        auto entity = this->get_entity();
//        return (entity->get_storage() != TXS_NOSTORAGE);
//    }

    virtual llvm::Value* code_gen_address(LlvmGenerationContext& context, GenScope* scope) const;
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
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
            cerror("No such field: %s (from %s)", this->member->ident.to_string().c_str(), this->context().to_string().c_str());
        else if (entity->get_storage() == TXS_NOSTORAGE)
            cerror("Assignee %s is not an L-value / has no storage.", member->to_string().c_str());
    }

//    virtual bool hasAddress() const {
//        auto entity = this->get_entity();
//        return (entity->get_storage() != TXS_NOSTORAGE);
//    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};
