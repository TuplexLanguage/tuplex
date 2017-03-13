#pragma once

#include <unordered_map>

#include "util/assert.hpp"

#include "tx_lang_defs.hpp"
#include "identifier.hpp"

#include "declaration.hpp"
#include "symbol.hpp"


class TxConstantProxy;


/** Represents the definition of a type or a field. */
class TxEntity : public virtual TxParseOrigin, public Printable {
    static Logger& _LOG;

    const TxEntityDeclaration* declaration;

protected:
    TxEntity(const TxEntityDeclaration* declaration) : declaration(declaration)  {
        // TODO: Make all entities have a declaration. ASSERT(declaration, "Assumed non-null declaration but was null");
    }

public:
    inline Logger* LOGGER() const { return &this->_LOG; }

    virtual inline const TxEntityDeclaration* get_declaration() const { return this->declaration; }

    virtual const TxLocation& get_parse_location() const override;

    virtual ExpectedErrorClause* exp_err_ctx() const override;

    TxScopeSymbol* get_symbol() const {
        return (this->declaration ? this->declaration->get_symbol() : nullptr);
    }

    TxDeclarationFlags get_decl_flags() const {
        return (this->declaration ? this->declaration->get_decl_flags() : TXD_NONE);
    }
};



/** Represents a field definition. */
class TxField : public TxEntity {
    const TxType* type;

    TxField( const TxFieldDeclaration* declaration, const TxType* type )
            : TxEntity(declaration), type(type) {
//        ASSERT(declaration, "Fields must be named (have non-null declaration)");
//        ASSERT(type, "NULL type for field " << declaration);
    }

    const TxTypeDeclaration* get_outer_type_decl() const {
        if (auto outerEntity = dynamic_cast<TxEntitySymbol*>(get_symbol()->get_outer()))
            return outerEntity->get_type_decl();
        return nullptr;
    }

public:
    /** Constructs a new field after applying some validation checks. If validation fails, null is returned. */
    static TxField* make_field( const TxFieldDeclaration* declaration, const TxType* type );


    virtual inline const TxFieldDeclaration* get_declaration() const override {
        return static_cast<const TxFieldDeclaration*>(TxEntity::get_declaration());
    }

    inline TxFieldStorage get_storage() const { return this->get_declaration()->get_storage(); }

    inline const TxType* get_type() const { return this->type; }

    /** Gets the storage "index" of this field within its declaration scope's data tuple.
     * This field must not have global or stack storage class.
     */
    int get_decl_storage_index() const;

    /** Returns true if this field is statically constant. */
    bool is_statically_constant() const;

    /** If this field's initializer can be statically evaluated,
     * a TxConstantProxy representing its value is returned, otherwise nullptr.
     * In future, this should return non-null for all expressions for which is_statically_constant() returns true.
     */
    virtual const TxConstantProxy* get_static_constant_proxy() const;

    bool is_modifiable() const;

    inline const std::string get_unique_name() const {
        return this->get_declaration()->get_unique_name();
    }

    virtual std::string str() const {
        return std::string("FIELD ") + ::to_string(this->get_decl_flags()) + " " + this->get_symbol()->get_full_name().str();
    }
};
