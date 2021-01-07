#pragma once

#include "identifier.hpp"
#include "tx_declaration_flags.hpp"
#include "tx_field_storage.hpp"

#include "ast/ast_entitydefs.hpp"
#include "ast/ast_fielddef_node.hpp"
#include "ast/type/ast_typecreating_node.hpp"
#include "ast/type/ast_qualtypes.hpp"


// (types are implicitly static)
const TxDeclarationFlags LEGAL_TYPE_DECL_FLAGS
= TXD_EXTERNC | TXD_FINAL | TXD_PUBLIC | TXD_PROTECTED | TXD_ABSTRACT
  | TXD_BUILTIN | TXD_IMPLICIT | TXD_GENPARAM | TXD_GENBINDING | TXD_EXPERROR;

const TxDeclarationFlags LEGAL_FIELD_DECL_FLAGS
= TXD_EXTERNC | TXD_VIRTUAL | TXD_FINAL | TXD_OVERRIDE | TXD_PUBLIC | TXD_PROTECTED | TXD_ABSTRACT
  | TXD_BUILTIN | TXD_IMPLICIT | TXD_GENPARAM | TXD_GENBINDING | TXD_CONSTRUCTOR | TXD_INITIALIZER | TXD_EXPERROR;

/** Represents a declaration of a program entity - a type or a field.
 * A declaration constitutes the association between an entity-defining AST node
 * (that inherits from TxEntityDefiningNode), and the entity's qualified name (symbol table entry)
 * and declaration flags (attributes).
 *
 * Declarations are made in the declaration pass.
 * The entity is produced by the definer during the resolution pass.
 */
class TxEntityDeclaration : public Printable {
    TxEntitySymbol* const symbol;
    const TxDeclarationFlags declFlags;

public:
    TxEntityDeclaration( TxEntitySymbol* symbol, TxDeclarationFlags declFlags )
            : symbol( symbol ), declFlags( declFlags ) {
        ASSERT( symbol, "NULL symbol" );
        (void) LEGAL_TYPE_DECL_FLAGS;   // suppresses unused variable warning in release mode
        (void) LEGAL_FIELD_DECL_FLAGS;  // suppresses unused variable warning in release mode
    }

    virtual ~TxEntityDeclaration() = default;

    inline TxEntitySymbol* get_symbol() const {
        return this->symbol;
    }

    inline TxDeclarationFlags get_decl_flags() const {
        return this->declFlags;
    }

    virtual TxEntityResolvingNode* get_definer() const = 0;

    /** Returns a globally unique full name for this declaration.
     * This will augment the "fully qualified name" for overloaded symbols.
     * This should not be called before declaration phase has completed.
     */
    virtual std::string get_unique_full_name() const;

    /** Returns a unique name (within its outer scope) for this declaration.
     * This will augment the name for overloaded symbols.
     * This should not be called before declaration phase has completed.
     */
    virtual std::string get_unique_name() const;

    virtual std::string str() const override;
};

class TxFieldDeclaration : public TxEntityDeclaration {
    TxFieldDefiningNode* const fieldDefiner;
    const TxFieldStorage storage;
    const TxIdentifier dataspace;

    unsigned get_overload_index() const;

public:
    TxFieldDeclaration( TxEntitySymbol* symbol, TxDeclarationFlags declFlags, TxFieldDefiningNode* fieldDefiner,
                        TxFieldStorage storage /*, const TxIdentifier& dataspace*/ )
            : TxEntityDeclaration( symbol, declFlags ), fieldDefiner( fieldDefiner ),
              storage( storage ),
              dataspace( /*dataspace*/ ) {
        ASSERT( ( declFlags | LEGAL_FIELD_DECL_FLAGS ) == LEGAL_FIELD_DECL_FLAGS, "Illegal field declFlags: " << declFlags );
        ASSERT( storage != TXS_NOSTORAGE && storage != TXS_UNBOUND_STACK, "Illegal field storage: " << storage );
    }

    virtual TxFieldDefiningNode* get_definer() const override {
        return this->fieldDefiner;
    }

    inline TxFieldStorage get_storage() const {
        return this->storage;
    }

    inline const TxIdentifier& get_dataspace() const {
        return this->dataspace;
    }

    virtual std::string get_unique_full_name() const override;
    virtual std::string get_unique_name() const override;
};

class TxTypeDeclaration : public TxEntityDeclaration {
    TxTypeCreatingNode* const typeDefiner;

public:
    TxTypeDeclaration( TxEntitySymbol* symbol, TxDeclarationFlags declFlags, TxTypeCreatingNode* typeDefiner )
            : TxEntityDeclaration( symbol, declFlags ), typeDefiner( typeDefiner ) {
        ASSERT( ( declFlags | LEGAL_TYPE_DECL_FLAGS ) == LEGAL_TYPE_DECL_FLAGS, "Illegal type declFlags: " << declFlags );
    }

    virtual TxTypeCreatingNode* get_definer() const override {
        return this->typeDefiner;
    }
};
