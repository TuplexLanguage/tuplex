#pragma once

#include "identifier.hpp"
#include "tx_declaration_flags.hpp"
#include "tx_field_storage.hpp"
#include "entity_proxy.hpp"


class TxEntitySymbol;


// (types are implicitly static)
const TxDeclarationFlags LEGAL_TYPE_DECL_FLAGS = TXD_ABSTRACT | TXD_FINAL | TXD_PUBLIC | TXD_PROTECTED | TXD_BUILTIN | TXD_IMPLICIT | TXD_GENPARAM;
const TxDeclarationFlags LEGAL_FIELD_DECL_FLAGS = TXD_STATIC | TXD_FINAL | TXD_OVERRIDE | TXD_PUBLIC | TXD_PROTECTED | TXD_BUILTIN | TXD_IMPLICIT | TXD_GENPARAM | TXD_CONSTRUCTOR;


class TxEntityDeclaration : public Printable {
    TxEntitySymbol* symbol;
    TxDeclarationFlags declFlags;

public:
    TxEntityDeclaration(TxEntitySymbol* symbol, TxDeclarationFlags declFlags)
        : symbol(symbol), declFlags(declFlags)  { }

    virtual ~TxEntityDeclaration() = default;

    TxEntitySymbol* get_symbol() const { return this->symbol; }

    TxDeclarationFlags get_decl_flags() const { return this->declFlags; }

    virtual bool validate() const = 0;

    /** Returns a globally unique full name for this declaration.
     * This will augment the "fully qualified name" for overloaded symbols.
     * This should not be called before declaration phase has completed.
     */
    virtual std::string get_unique_full_name() const = 0;

    /** Returns a unique name (within its outer scope) for this declaration.
     * This will augment the name for overloaded symbols.
     * This should not be called before declaration phase has completed.
     */
    virtual std::string get_unique_name() const = 0;

    virtual std::string to_string() const override;
};

class TxFieldDeclaration : public TxEntityDeclaration {
    TxFieldDefiner* fieldDefiner;
    TxFieldStorage storage;
    const TxIdentifier dataspace;

    unsigned get_overload_index() const;

public:
    TxFieldDeclaration(TxEntitySymbol* symbol, TxDeclarationFlags declFlags, TxFieldDefiner* fieldDefiner,
                       TxFieldStorage storage, const TxIdentifier& dataspace)
            : TxEntityDeclaration(symbol, declFlags), fieldDefiner(fieldDefiner), storage(storage), dataspace(dataspace)  {
        ASSERT((declFlags | LEGAL_FIELD_DECL_FLAGS) == LEGAL_FIELD_DECL_FLAGS, "Illegal field declFlags: " << declFlags);
    }

    TxFieldDefiner* get_field_definer() const { return this->fieldDefiner; }

    TxFieldStorage get_storage() const { return this->storage; }

    const TxIdentifier& get_dataspace() const { return this->dataspace; }

    virtual bool validate() const override;

    virtual std::string get_unique_full_name() const override;
    virtual std::string get_unique_name() const override;
};

class TxTypeDeclaration : public TxEntityDeclaration {
    TxTypeDefiner* typeDefiner;

public:
    TxTypeDeclaration(TxEntitySymbol* symbol, TxDeclarationFlags declFlags, TxTypeDefiner* typeDefiner)
            : TxEntityDeclaration(symbol, declFlags), typeDefiner(typeDefiner)  {
        ASSERT((declFlags | LEGAL_TYPE_DECL_FLAGS) == LEGAL_TYPE_DECL_FLAGS, "Illegal type declFlags: " << declFlags);
    }

    TxTypeDefiner* get_type_definer() const { return this->typeDefiner; }

    virtual bool validate() const override;

    virtual std::string get_unique_full_name() const override;
    virtual std::string get_unique_name() const override;
};
