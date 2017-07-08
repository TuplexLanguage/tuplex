#pragma once

#include "ast_node.hpp"

class TxQualType;
class TxField;

namespace llvm {
class Constant;
}

/** Root class for AST nodes that resolve a program entity - a type or a field.
 * To resolve means to either produce a new entity (define a new type or field)
 * or to use one produced elsewhere.
 *
 * Type and field definitions, as well as value expressions, produce and/or use an entity.
 * If the node resolves a field entity, it will also resolve the type of that field.
 * This means all instances of this node will resolve a type, both type-defining and field-defining ones.
 * The entity is produced/resolved during the resolution pass.
 */
class TxEntityDefiningNode : public TxNode {
public:
    TxEntityDefiningNode( const TxLocation& ploc )
            : TxNode( ploc ) {
    }

    virtual TxEntityDefiningNode* make_ast_copy() const override = 0;

    /** Resolves and returns the qual-type of the entity/value this node produces/uses.
     * If this node's entity has not already been resolved, it will be resolved in this invocation.
     * If the resolution fails, an error message will have been generated and resolution_error exception is thrown.
     * This method never returns NULL. */
    virtual const TxQualType* resolve_type() = 0;

    /** Returns the qual-type of the entity/value this node produces/uses.
     * This node must have been resolved before this call.
     * This method never returns NULL, provided this node has been successfully resolved. */
    virtual const TxQualType* qualtype() const = 0;

    /** Returns the qual-type of the entity/value this node produces/uses if already successfully resolved,
     * otherwise NULL. */
    virtual const TxQualType* attempt_qualtype() const = 0;
};

class TxTypeDefiningNode : public TxEntityDefiningNode {
    const TxQualType* type = nullptr;
    bool startedRslv = false;  // guard against recursive resolution
    bool hasResolved = false;  // to prevent multiple identical error messages

protected:
    /** Defines the type of this expression (as specific as can be known), constructing/obtaining the TxTypeUsage instance.
     * The implementation should only traverse the minimum nodes needed to define the type
     * (e.g. not require the actual target type of a reference to be defined).
     * This should only be invoked once, from the TxTypeDefiningNode class.
     * @return a valid type pointer (exception must be thrown upon failure) */
    virtual const TxQualType* define_type() = 0;

public:
    TxTypeDefiningNode( const TxLocation& ploc )
            : TxEntityDefiningNode( ploc ) {
    }

    virtual TxTypeDefiningNode* make_ast_copy() const override = 0;

    /** Returns the qual-type of the value this node produces/uses.
     * @return a valid type pointer (exception is thrown upon failure) */
    virtual const TxQualType* resolve_type() override final;

    virtual const TxQualType* qualtype() const override {
        ASSERT( this->type, "entity definer not resolved: " << this );
        return this->type;
    }

    virtual const TxQualType* attempt_qualtype() const override {
        return this->type;
    }
};

class TxExpressionNode;

class TxFieldDefiningNode : public TxEntityDefiningNode {
    const TxQualType* type = nullptr;
    const TxField* field = nullptr;
    bool startedRslv = false;  // guard against recursive resolution
    bool hasResolved = false;  // to prevent multiple identical error messages

protected:
    /** Defines the type of this field (as specific as can be known), constructing/obtaining the TxTypeUsage instance.
     * The implementation should only traverse the minimum nodes needed to define the type
     * (e.g. not require the actual target type of a reference to be defined).
     * This should only be invoked once, from the TxFieldDefiningNode class.
     * @return a valid type pointer (exception must be thrown upon failure) */
    virtual const TxQualType* define_type() = 0;

    /** Defines the field of this node, constructing/obtaining the TxField instance.
     * This should only be invoked once, from the TxFieldDefiningNode class. */
    virtual const TxField* define_field() = 0;

public:
    TxFieldDefiningNode( const TxLocation& ploc )
            : TxEntityDefiningNode( ploc ) {
    }

    virtual TxFieldDefiningNode* make_ast_copy() const override = 0;

    /** Resolves the type and returns the field entity of this field-defining node.
     * @return a valid field pointer (exception is thrown upon failure) */
    virtual const TxField* resolve_field() final;

    /** Returns the type (as specific as can be known) of the value this field-defining node produces/uses.
     * @return a valid type pointer (exception is thrown upon failure) */
    virtual const TxQualType* resolve_type() final {
        this->resolve_field();
        return this->type;
    }

    virtual const TxQualType* qualtype() const override final {
        ASSERT( this->type, "entity definer not resolved: " << this );
        return this->type;
    }

    virtual const TxQualType* attempt_qualtype() const override final {
        return this->type;
    }

    virtual const TxField* attempt_get_field() const final {
        return this->field;
    }
    virtual const TxField* get_field() const final {
        ASSERT( this->field, "entity definer not resolved: " << this );
        return this->field;
    }

    virtual TxExpressionNode* get_init_expression() const = 0;

    /** Returns true if this field has a constant initialization expression that can be evaluated at compile time. */
    virtual bool is_statically_constant() const;

    /** Generates / retrieves the code generated constant value of this field's init expression,
     * if it has one and it is constant.
     * May be called multiple times, it caches the result to ensures the constant value is only generated once.
     * Only valid to call on nodes for which is_statically_constant() returns true. */
    virtual llvm::Constant* code_gen_const_init_value( LlvmGenerationContext& context ) const { return nullptr ; }
};
