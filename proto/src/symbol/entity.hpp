#pragma once

#include <unordered_map>

#include "util/assert.hpp"

#include "tx_lang_defs.hpp"
#include "identifier.hpp"

#include "declaration.hpp"
#include "symbol.hpp"
#include "qual_type.hpp"

namespace llvm {
class Value;
}

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
    const TxQualType _qtype;

    /** The code-gen value for this field. Generated exactly once and stored here. */
    mutable llvm::Value* llvmValue = nullptr;

    TxField( const TxFieldDeclaration* declaration, TxQualType qtype )
            : TxEntity( declaration ), _qtype( qtype ) {
    }

public:
    /** Constructs a new field after applying some validation checks. If validation fails, resolution exception is thrown. */
    static TxField* make_field( const TxFieldDeclaration* declaration, TxQualType type );

    virtual inline const TxFieldDeclaration* get_declaration() const override {
        return static_cast<const TxFieldDeclaration*>( TxEntity::get_declaration() );
    }

    inline TxFieldStorage get_storage() const {
        return this->get_declaration()->get_storage();
    }

    inline TxQualType qtype() const {
        return this->_qtype;
    }

    inline bool is_modifiable() const {
        return this->_qtype.is_modifiable();
    }

    inline const std::string get_unique_name() const {
        return this->get_declaration()->get_unique_name();
    }

    inline const std::string get_unique_full_name() const {
        return this->get_declaration()->get_unique_full_name();
    }

    virtual std::string str() const override {
        return this->get_declaration()->get_unique_full_name();
    }

    /** Generates this field, potentially only as a declaration without initializer. Invoked from code referencing this field. */
    llvm::Value* code_gen_field_decl( LlvmGenerationContext& context ) const;

    inline bool has_llvm_value() const { return this->llvmValue; }

    inline llvm::Value* get_llvm_value() const {
        ASSERT( this->llvmValue, "LLVM value NOT set for field " << this );
        return this->llvmValue;
    }

    inline void set_llvm_value( llvm::Value* llvmValue ) const {
        ASSERT( !this->llvmValue, "LLVM value already set for field " << this << ": " << this->llvmValue << "; new value: " << llvmValue );
        this->llvmValue = llvmValue;
    }
};
