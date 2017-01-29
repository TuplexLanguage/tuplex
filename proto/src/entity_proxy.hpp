#pragma once

#include "util/assert.hpp"
#include "tx_error.hpp"


class TxTypeDefiningNode;
class TxExpressionNode;
class TxType;
class TxField;
typedef unsigned TxSpecializationIndex;


/** Proxy interface that provides a layer of indirection to a type reference.
 * This is needed for two reasons:
 * - Before the symbol table pass has completed, resolving the actual type may not be possible
 * - Resolving the type may be context-dependent (e.g. type parameter resolution depends
 *   on subtype context)
 */
class TxTypeProxy {
public:
    virtual ~TxTypeProxy() = default;

    /** Gets the TxType instance this type proxy represents.
     * The contract is that it shall return the same instance every invocation.
     * If called before symbol resolution has completed the result is undefined
     * (may assert or return null).
     */
    virtual const TxType* get_type() const = 0;
};


class TxEntityDefiner : public TxTypeProxy, public virtual TxParseOrigin {
public:
    /** Returns a type if this type definer "is ready" (has a defined type), otherwise NULL. */
    virtual const TxType* attempt_get_type() const = 0;

    virtual const TxType* resolve_type() = 0;
};

class TxTypeDefiner : public TxEntityDefiner {
public:
    /** Returns the node defining the type. */
    virtual TxTypeDefiningNode* get_node() const = 0;

    /** Returns the specialization index for which the node defined the type. */
    virtual TxSpecializationIndex get_six() const = 0;
};

class TxFieldDefiner : public TxEntityDefiner {
public:
    /** Gets the TxExpressionNode that defines the initialization value for this field.
     * Returns nullptr if there is no initializer.
     */
    virtual const TxExpressionNode* get_init_expression() const = 0;

    virtual const TxField* resolve_field() = 0;

    virtual const TxField* get_field() const = 0;


    virtual const TxType* resolve_type() override;
};
