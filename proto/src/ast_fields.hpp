#pragma once

#include "ast_base.hpp"


class TxFieldValueNode : public TxExpressionNode {
    std::vector<TxSymbolScope*> memberPath;
    TxFieldEntity * cachedEntity = nullptr;

    class FieldTypeDefiner : public TxEntityDefiner {
        TxFieldValueNode* fieldNode;
    public:
        FieldTypeDefiner(TxFieldValueNode* fieldNode) : fieldNode(fieldNode) { }
        virtual const TxType* symbol_resolution_pass(ResolutionContext& resCtx) override {
            return fieldNode->symbol_resolution_pass(resCtx);
        }
        virtual const TxType* attempt_get_type() const override { return fieldNode->get_type(); }
        virtual const TxType* get_type() const override { return fieldNode->get_type(); }
    } fieldTypeDefiner;

    inline const TxFieldEntity* get_entity() const { return this->cachedEntity; }

protected:
    virtual const TxType* resolve_expression(ResolutionContext& resCtx) override {
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
                    LOGGER().warning("%s: Resolving '%s' as GENPARAM %s from scope %s", this->parse_loc_string().c_str(),
                            this->member->ident.to_string().c_str(), this->cachedEntity->to_string().c_str(), this->context().scope()->get_full_name().to_string().c_str());
//                    if (declEnt->get_name() == make_generic_binding_name(cachedEntity->get_full_name().to_string())) {
//                        // if type-arg resolves to ancestor type's type parameter, it is unspecified in current scope
//                        // (need to catch this, lest we get an infinite alias lookup loop or spurious name resolution)
//                        if (auto outerType = dynamic_cast<const TxTypeEntity*>(scope->get_parent())) {
//                            // since we declare base types under the subtype's scope,
//                            // we may have to lookup via outer (the subtype's) scope
//                            return inner_define_type(outerType, errorMsg);
//                        }
//    //                    else
//    //                        LOGGER().warning("%s: type '%s' as alias for GENPARAM %s", this->parse_loc_string().c_str(),
//    //                                         declEnt->get_full_name().to_string().c_str(), identifiedEntity->to_string().c_str());
//                    }
                }
//                else if (this->cachedEntity->get_static_constant_proxy())
//                    LOGGER().debug("%s: Resolving '%s' as statically constant %s", this->parse_loc_string().c_str(), this->member->ident.to_string().c_str(), this->cachedEntity->to_string().c_str());
            }
            else
                cerror("No such field: %s (from %s)", this->member->ident.to_string().c_str(), this->context().to_string().c_str());
        }
        return this->cachedEntity ? this->cachedEntity->symbol_resolution_pass(resCtx) : nullptr;
    }

public:
    TxExpressionNode* base;
    const TxIdentifierNode* member;

    TxFieldValueNode(const yy::location& parseLocation, TxExpressionNode* base, const TxIdentifierNode* member)
        : TxExpressionNode(parseLocation), fieldTypeDefiner(this), base(base), member(member) {
    }

    virtual bool has_predefined_type() const override { return true; }

    virtual void symbol_registration_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        if (base)
            base->symbol_registration_pass(lexContext);
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override {
        this->LOGGER().trace("Getting static constant proxy for field %s", this->member->ident.to_string().c_str());
        if (auto ent = this->get_entity())
            if (auto constProxy = ent->get_static_constant_proxy()) {
                this->LOGGER().debug("Returning static constant proxy for field %s", ent->get_full_name().to_string().c_str());
                return constProxy;
            }
        return nullptr;
    }

    virtual bool is_statically_constant() const override {
        if (auto ent = this->get_entity())
            return ent->is_statically_constant();
        return false;
    }

    virtual void semantic_pass() override {
        if (base)
            base->semantic_pass();
        this->get_entity();
    }

//    virtual bool has_address() const {
//        auto entity = this->get_entity();
//        return (entity->get_storage() != TXS_NOSTORAGE);
//    }

    virtual llvm::Value* code_gen_address(LlvmGenerationContext& context, GenScope* scope) const;
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};


class TxFieldAssigneeNode : public TxAssigneeNode {
    std::vector<TxSymbolScope*> memberPath;
    TxFieldEntity * entity = nullptr;

    const TxFieldEntity* get_entity() {
        //return this->context().scope()->resolve_field(this->member->ident);
        // FUTURE: support overloaded field resolution in assignment (e.g. overloadedFuncPointer(Int,Float) = func )
        return this->entity;
    }

protected:
    virtual const TxType* resolve_expression(ResolutionContext& resCtx) override {
        if (! this->entity) {
            if (! this->memberPath.empty())
                return nullptr;  // has already been attempted and failed
            if (base)
                // (lookup is similar to that of TxFieldEntity)
                this->entity = dynamic_cast<TxFieldEntity*>(this->base->get_type()->lookup_instance_member(memberPath, this->member->ident));
            else
                this->entity = this->context().scope()->resolve_field(memberPath, this->member->ident);
            if (! this->entity) {
                cerror("No such field: %s (from %s)", this->member->ident.to_string().c_str(), this->context().to_string().c_str());
                return nullptr;
            }
            return this->entity->symbol_resolution_pass(resCtx);
        }
        else
            return this->entity->symbol_resolution_pass(resCtx);
    }

public:
    TxExpressionNode* base;
    const TxIdentifierNode* member;

    TxFieldAssigneeNode(const yy::location& parseLocation, TxExpressionNode* base, const TxIdentifierNode* member)
        : TxAssigneeNode(parseLocation), base(base), member(member) { }

    virtual void symbol_registration_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        if (base)
            base->symbol_registration_pass(lexContext);
    }

    virtual void semantic_pass() {
        if (base)
            base->semantic_pass();
        auto entity = this->get_entity();
        if (entity && entity->get_storage() == TXS_NOSTORAGE)
            cerror("Assignee %s is not an L-value / has no storage.", member->to_string().c_str());
    }

//    virtual bool hasAddress() const {
//        auto entity = this->get_entity();
//        return (entity->get_storage() != TXS_NOSTORAGE);
//    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};
