#pragma once

#include <unordered_map>

#include "util/assert.hpp"

#include "tx_lang_defs.hpp"
#include "identifier.hpp"

#include "declaration.hpp"
#include "symbol.hpp"

class TxConstantProxy;

/** Represents a resolved program entity - a type or a field.
 * Entities are produced by entity-defining AST nodes (that inherit from TxEntityDefiningNode)
 * during the resolution pass.
 * An entity typically has a declaration (however for specializations of generic types there are exceptions)
 * and the declaration associates the entity with a qualified name (a symbol table entry)
 * and with declaration flags (attributes).
 */
class TxEntity : public virtual TxParseOrigin, public Printable {
    static Logger& _LOG;

    const TxEntityDeclaration* declaration;

protected:
    TxEntity( const TxEntityDeclaration* declaration )
            : declaration( declaration ) {
        // Note: Type entities might not have a declaration. Their underlying actual type will always have a declaration though.
    }

public:
    inline Logger* LOGGER() const {
        return &this->_LOG;
    }

    virtual inline const TxEntityDeclaration* get_declaration() const {
        return this->declaration;
    }

    virtual const TxNode* get_origin_node() const override;

    TxEntitySymbol* get_symbol() const {
        return ( this->declaration ? this->declaration->get_symbol() : nullptr );
    }

    TxDeclarationFlags get_decl_flags() const {
        return ( this->declaration ? this->declaration->get_decl_flags() : TXD_NONE );
    }
};

/** Represents a field definition. */
class TxField : public TxEntity {
    const TxType* type;

    TxField( const TxFieldDeclaration* declaration, const TxType* type )
            : TxEntity( declaration ), type( type ) {
//        ASSERT(declaration, "Fields must be named (have non-null declaration)");
//        ASSERT(type, "NULL type for field " << declaration);
    }

//    const TxTypeDeclaration* get_outer_type_decl() const {
//        if ( auto outerEntity = dynamic_cast<TxEntitySymbol*>( this->get_symbol()->get_outer() ) )
//            return outerEntity->get_type_decl();
//        return nullptr;
//    }

public:
    /** Constructs a new field after applying some validation checks. If validation fails, resolution exception is thrown. */
    static TxField* make_field( const TxFieldDeclaration* declaration, const TxType* type );

    virtual inline const TxFieldDeclaration* get_declaration() const override {
        return static_cast<const TxFieldDeclaration*>( TxEntity::get_declaration() );
    }

    inline TxFieldStorage get_storage() const {
        return this->get_declaration()->get_storage();
    }

    inline const TxType* get_type() const {
        return this->type;
    }

//    /** Gets the storage "index" of this field within its declaration scope's data tuple.
//     * This field must not have global or stack storage class.
//     */
//    int get_decl_storage_index() const;

    bool is_modifiable() const;

    inline const std::string get_unique_name() const {
        return this->get_declaration()->get_unique_name();
    }

    inline const std::string get_unique_full_name() const {
        return this->get_declaration()->get_unique_full_name();
    }

    virtual std::string str() const override {
        return this->get_declaration()->get_unique_full_name();
    }
};
