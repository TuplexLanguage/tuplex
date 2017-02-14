#pragma once

#include "identifier.hpp"
#include "tx_declaration_flags.hpp"
#include "tx_field_storage.hpp"

#include "ast_base.hpp"


// (types are implicitly static)
const TxDeclarationFlags LEGAL_TYPE_DECL_FLAGS
        = TXD_ABSTRACT | TXD_FINAL | TXD_PUBLIC | TXD_PROTECTED | TXD_ABSTRACT
          | TXD_BUILTIN | TXD_IMPLICIT | TXD_GENPARAM | TXD_GENBINDING | TXD_EXPERRBLOCK;
const TxDeclarationFlags LEGAL_FIELD_DECL_FLAGS
        = TXD_STATIC | TXD_FINAL | TXD_OVERRIDE | TXD_PUBLIC | TXD_PROTECTED | TXD_ABSTRACT
          | TXD_BUILTIN | TXD_IMPLICIT | TXD_GENPARAM | TXD_GENBINDING | TXD_CONSTRUCTOR | TXD_EXPERRBLOCK;


class TxEntityDeclaration : public Printable {
    TxEntitySymbol* const symbol;
    const TxDeclarationFlags declFlags;

protected:
    bool in_exp_err_block() const;

public:
    TxEntityDeclaration(TxEntitySymbol* symbol, TxDeclarationFlags declFlags)
            : symbol(symbol), declFlags(declFlags)  {
        ASSERT(symbol, "NULL symbol");
    }

    virtual ~TxEntityDeclaration() = default;

    inline TxEntitySymbol* get_symbol() const { return this->symbol; }

    inline TxDeclarationFlags get_decl_flags() const { return this->declFlags; }

    virtual TxEntityDefiningNode* get_definer() const = 0;

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
    TxFieldDefiningNode* const fieldDefiner;
    const TxFieldStorage storage;
    const TxIdentifier dataspace;

    static inline TxFieldStorage determine_storage(TxFieldStorage storage, TxDeclarationFlags declFlags) {
        if ( storage == TXS_STATIC
             && ( declFlags & (TXD_PUBLIC | TXD_PROTECTED) )  // private fields are non-virtual
             // if final but doesn't override, its effectively non-virtual:
             && ( ( declFlags & (TXD_OVERRIDE | TXD_FINAL)) != TXD_FINAL ) )
            return TXS_VIRTUAL;
        else
            return storage;
    }

    unsigned get_overload_index() const;

public:
    TxFieldDeclaration(TxEntitySymbol* symbol, TxDeclarationFlags declFlags, TxFieldDefiningNode* fieldDefiner,
                       TxFieldStorage storage, const TxIdentifier& dataspace)
            : TxEntityDeclaration(symbol, declFlags), fieldDefiner(fieldDefiner),
              storage(determine_storage(storage, declFlags)), dataspace(dataspace)  {
        ASSERT((declFlags | LEGAL_FIELD_DECL_FLAGS) == LEGAL_FIELD_DECL_FLAGS, "Illegal field declFlags: " << declFlags);
    }

    virtual TxFieldDefiningNode* get_definer() const override { return this->fieldDefiner; }

    inline TxFieldStorage get_storage() const { return this->storage; }

    inline const TxIdentifier& get_dataspace() const { return this->dataspace; }

    virtual bool validate() const override;

    virtual std::string get_unique_full_name() const override;
    virtual std::string get_unique_name() const override;
};

class TxTypeDeclaration : public TxEntityDeclaration {
    TxTypeDefiningNode* const typeDefiner;

public:
    TxTypeDeclaration(TxEntitySymbol* symbol, TxDeclarationFlags declFlags, TxTypeDefiningNode* typeDefiner)
            : TxEntityDeclaration(symbol, declFlags), typeDefiner(typeDefiner)  {
        ASSERT((declFlags | LEGAL_TYPE_DECL_FLAGS) == LEGAL_TYPE_DECL_FLAGS, "Illegal type declFlags: " << declFlags);
    }

    virtual TxTypeDefiningNode* get_definer() const override { return this->typeDefiner; }

    virtual bool validate() const override;

    virtual std::string get_unique_full_name() const override;
    virtual std::string get_unique_name() const override;
};
