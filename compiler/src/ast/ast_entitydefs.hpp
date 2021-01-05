#pragma once

#include "ast_node.hpp"
#include "symbol/qual_type.hpp"

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
class TxEntityResolvingNode : public TxNode {
protected:
    TxQualType _type;

public:
    explicit TxEntityResolvingNode( const TxLocation& ploc )
            : TxNode( ploc ) {
    }

    TxEntityResolvingNode* make_ast_copy() const override = 0;

    /** Resolves and returns the type of the entity/value this node produces/uses.
     * If this node's entity has not already been resolved, it will be resolved in this invocation.
     * If the resolution fails, an error message will have been generated and resolution_error exception is thrown.
     * This method never returns NULL. */
    virtual TxQualType resolve_type( TxTypeResLevel typeResLevel ) = 0;

    /** Returns the type of the entity/value this node produces/uses.
     * This node must have been resolved before this call.
     * This method never returns NULL, provided this node has been successfully resolved. */
    TxQualType qtype() const {
        ASSERT( this->_type, "entity not resolved: " << this );
        return this->_type;
    }

    /** Returns the type of the entity/value this node produces/uses if already successfully resolved,
     * otherwise the returned TxQualType is null-valued . */
    TxQualType attempt_qtype() const {
        return this->_type;
    }
};

class TxTypeResolvingNode : public TxEntityResolvingNode {
    bool startedRslv = false;  // guard against recursive resolution
    bool hasResolved = false;  // to prevent multiple identical error messages

protected:
    /** Defines the type of this expression (as specific as can be known), constructing/obtaining the TxTypeUsage instance.
     * The implementation should only traverse the minimum nodes needed to define the type
     * (e.g. not require the actual target type of a reference to be defined).
     * This should only be invoked once, from the TxTypeDefiningNode class.
     * @return a valid type pointer (exception must be thrown upon failure) */
    virtual TxQualType define_type( TxTypeResLevel typeResLevel ) = 0;

    void resolution_pass() override {
        this->resolve_type( TXR_FULL_RESOLUTION );
    }

public:
    explicit TxTypeResolvingNode( const TxLocation& ploc )
            : TxEntityResolvingNode( ploc ) {
    }

    TxTypeResolvingNode* make_ast_copy() const override = 0;

    /** Returns true if this node resolves/evaluates to a value. */
    virtual bool is_value() const {
        return false;
    }

    /** Returns the type of the value this node produces/uses.
     * @return a valid type pointer (exception is thrown upon failure) */
    TxQualType resolve_type( TxTypeResLevel typeResLevel ) override;
};
