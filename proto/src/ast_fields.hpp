#pragma once

#include "ast_base.hpp"


// unused
class TxSymbolIdentifierNode : public TxNode {
    bool resolved = false;
    std::vector<TxSymbolScope*> memberPath;
    TxSymbolScope* cachedSymbol = nullptr;

protected:
    std::vector<TxSymbolScope*>& get_member_path() {
        return (this->base ? this->base->get_member_path() : this->memberPath );
    }

    virtual TxSymbolScope* inner_resolve_symbol() {
        TxIdentifier ident(this->name);
        if (this->base) {
            if (TxSymbolScope* baseSym = this->base->resolve_symbol())
                return baseSym->lookup_member(this->base->get_member_path(), ident);
            return nullptr;
        }
        else
            return this->context().scope()->start_lookup_symbol(memberPath, ident);
    }

public:
    TxSymbolIdentifierNode* base;
    const std::string name;

    TxSymbolIdentifierNode(const yy::location& parseLocation, TxSymbolIdentifierNode* base, const std::string& name)
        : TxNode(parseLocation), base(base), name(name)  { }

    TxIdentifier get_full_ident() const {
        if (this->base) {
            auto baseIdent = this->base->get_full_ident();
            baseIdent.append(this->name);
            return baseIdent;
        }
        else
            return TxIdentifier(this->name);
    }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        if (this->base)
            this->base->symbol_declaration_pass(lexContext);
    }

    virtual TxSymbolScope* resolve_symbol() final {
        if (! this->resolved) {
            LOGGER().trace("resolving symbol of %s", this->to_string().c_str());
            this->resolved = true;
            this->cachedSymbol = this->inner_resolve_symbol();
            if (! this->cachedSymbol)
                cerror("No such symbol: %s", this->name.c_str());
        }
        return cachedSymbol;
    }

    //inline const TxSymbolScope* get_symbol() const { return this->cachedSymbol; }
};



class TxFieldValueNode : public TxExpressionNode {
    std::vector<TxSymbolScope*> memberPath;
    TxFieldEntity * cachedEntity = nullptr;

    inline const TxFieldEntity* get_entity() const { return this->cachedEntity; }

protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        // FUTURE: support overloaded field resolution in non-call expressions (e.g. overloadedFunc(Int,Float).typeid )
        if (! this->cachedEntity) {
            if (! this->memberPath.empty())
                return nullptr;  // has already been attempted and failed
            if (this->baseExpr) {
                // (lookup is similar to that of TxFieldEntity)
                if (auto baseType = this->baseExpr->resolve_type(resCtx))
                    if (auto symbol = baseType->lookup_instance_member(this->memberPath, this->member->ident)) {
                        this->cachedEntity = this->context().scope()->resolve_field_lookup(resCtx, symbol, this->appliedFuncArgTypes);
                        if (this->cachedEntity && this->memberPath.back() != this->cachedEntity)
                            this->memberPath[this->memberPath.size()-1] = this->cachedEntity;
                    }
            }
            else
                this->cachedEntity = this->context().scope()->lookup_field(resCtx, this->memberPath, this->member->ident, this->appliedFuncArgTypes);

            if (this->cachedEntity) {
                if (this->cachedEntity->get_decl_flags() & TXD_GENPARAM) {
                    LOGGER().debug("%s: Resolving field value '%s' as GENPARAM %s from scope %s", this->parse_loc_string().c_str(),
                            this->member->ident.to_string().c_str(), this->cachedEntity->to_string().c_str(), this->context().scope()->get_full_name().to_string().c_str());
                }
            }
            else
                cerror("No such field: %s (from %s)", this->member->ident.to_string().c_str(), this->context().to_string().c_str());
        }
        return this->cachedEntity ? this->cachedEntity->resolve_symbol_type(resCtx) : nullptr;
    }

public:
    TxExpressionNode* baseExpr;
    const TxIdentifierNode* member;

    TxFieldValueNode(const yy::location& parseLocation, TxExpressionNode* base, const TxIdentifierNode* member)
        : TxExpressionNode(parseLocation), baseExpr(base), member(member) {
    }

    virtual bool has_predefined_type() const override { return true; }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        if (this->baseExpr)
            this->baseExpr->symbol_declaration_pass(lexContext);
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
        if (this->baseExpr)
            this->baseExpr->semantic_pass();
        this->get_entity();
    }

//    virtual bool has_address() const {
//        auto entity = this->get_entity();
//        return (entity->get_storage() != TXS_NOSTORAGE);
//    }

    virtual llvm::Value* code_gen_address(LlvmGenerationContext& context, GenScope* scope, bool foldStatics=false) const;
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
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
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        if (! this->entity) {
            if (! this->memberPath.empty())
                return nullptr;  // has already been attempted and failed
            if (baseExpr) {
                // (lookup is similar to that of TxFieldEntity)
                if (auto baseType = this->baseExpr->resolve_type(resCtx))
                    this->entity = dynamic_cast<TxFieldEntity*>(baseType->lookup_instance_member(memberPath, this->member->ident));
            }
            else
                this->entity = this->context().scope()->lookup_field(resCtx, memberPath, this->member->ident);
            if (! this->entity) {
                cerror("No such field: %s (from %s)", this->member->ident.to_string().c_str(), this->context().to_string().c_str());
                return nullptr;
            }
            return this->entity->resolve_symbol_type(resCtx);
        }
        else
            return this->entity->resolve_symbol_type(resCtx);
    }

public:
    TxExpressionNode* baseExpr;
    const TxIdentifierNode* member;

    TxFieldAssigneeNode(const yy::location& parseLocation, TxExpressionNode* base, const TxIdentifierNode* member)
        : TxAssigneeNode(parseLocation), baseExpr(base), member(member) { }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
        if (this->baseExpr)
            this->baseExpr->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxAssigneeNode::symbol_resolution_pass(resCtx);
        if (this->baseExpr)
            this->baseExpr->symbol_resolution_pass(resCtx);
        auto entity = this->get_entity();
        if (entity && entity->get_storage() == TXS_NOSTORAGE)
            cerror("Assignee %s is not an L-value / has no storage.", member->to_string().c_str());
    }

    virtual void semantic_pass() override {
        if (this->baseExpr)
            this->baseExpr->semantic_pass();
    }

//    virtual bool hasAddress() const {
//        auto entity = this->get_entity();
//        return (entity->get_storage() != TXS_NOSTORAGE);
//    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxNewExprNode;

/** Substitute for TxFieldValueNode for calling constructors. */
class TxConstructorCalleeExprNode : public TxExpressionNode {
    const TxNewExprNode* newExpr;
    TxFieldEntity* constructorEntity = nullptr;

    mutable llvm::Value* objectPtrV = nullptr;
    friend TxNewExprNode;

protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override;

public:
    TxConstructorCalleeExprNode(const yy::location& parseLocation, const TxNewExprNode* newExpr)
            : TxExpressionNode(parseLocation), newExpr(newExpr) { }

    virtual bool has_predefined_type() const override { return true; }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
    }

    virtual void semantic_pass() override { }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};
