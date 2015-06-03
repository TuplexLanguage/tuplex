#pragma once

#include <unordered_map>

#include "txassert.hpp"

#include "tx_lang_defs.hpp"
#include "identifier.hpp"

#include "scope.hpp"
#include "declaration.hpp"


class TxConstantProxy;


/** Represents the definition of a type or a field. */
class TxEntity : public TxTypeProxy, public Printable {
    Logger& LOG;

    const TxEntityDeclaration* declaration;

protected:
    TxEntity(const TxEntityDeclaration* declaration)
        : LOG(Logger::get("ENTITY")), declaration(declaration)  { }

    inline Logger& LOGGER() const { return this->LOG; }

public:
    virtual inline const TxEntityDeclaration* get_declaration() const { return this->declaration; }

    TxScopeSymbol* get_symbol() const {
        //ASSERT(this->declaration, "entity has NULL declaration");
        return (this->declaration ? this->declaration->get_symbol() : nullptr);
    }

    TxDeclarationFlags get_decl_flags() const {
        //ASSERT(this->declaration, "entity has NULL declaration");
        return (this->declaration ? this->declaration->get_decl_flags() : TXD_NONE);
    }

    virtual bool validate() const = 0;

//    virtual TxSymbolScope* resolve_generic(TxSymbolScope* vantageScope) override;

//    virtual const TxType* resolve_symbol_type(ResolutionContext& resCtx) = 0;
//
//    virtual const TxType* get_type() const = 0;
};



/** Represents a field definition. */
class TxField : public TxEntity {
//    /** the type (if static) or field (if instance) that this field is part of (or null if global or local) */
//    const TxEntity* outer;

    const TxType* type;

    const TxFieldStorage storage;

    static inline TxFieldStorage determine_storage(TxFieldStorage storage, TxDeclarationFlags declFlags) {
        if ( storage == TXS_STATIC
             && ( declFlags & (TXD_PUBLIC | TXD_PROTECTED) )  // private fields are non-virtual
             // if final but doesn't override, its effectively non-virtual:
             && ( ( declFlags & (TXD_OVERRIDE | TXD_FINAL)) != TXD_FINAL ) )
            return TXS_VIRTUAL;
        else
            return storage;
    }

//    inline const TxExpressionNode* get_init_expression() const {
//        return static_cast<const TxFieldDefiner*>(this->entityDefiner)->get_init_expression();
//    }

    const TxScopeSymbol* get_outer() const {
        return get_symbol()->get_outer();
    }

    const TxTypeDeclaration* get_outer_type_decl() const {
        if (auto outerEntity = dynamic_cast<TxEntitySymbol*>(get_symbol()->get_outer()))
            return outerEntity->get_type_decl();
        return nullptr;
    }

public:
    TxField(const TxFieldDeclaration* declaration, const TxType* type)
            : TxEntity(declaration), type(type),
              storage(determine_storage(declaration->get_storage(), declaration->get_decl_flags())) {
        ASSERT(declaration, "Fields must be named (have non-null declaration)");
    }

    virtual inline const TxFieldDeclaration* get_declaration() const override {
        return static_cast<const TxFieldDeclaration*>(TxEntity::get_declaration());
    }

    inline TxFieldStorage get_storage() const { return this->storage; }

    inline const TxType* get_type() const { return this->type; }

    /** Gets the storage "index" of this field within its data tuple.
     * This field must have instance storage class.
     * (This should maybe be moved elsewhere as it is specific to low-level code generation.)
     */
    int get_instance_field_index() const;

    /** Gets the storage "index" of this field within its data tuple.
     * This field must have static virtual storage class.
     * (This should maybe be moved elsewhere as it is specific to low-level code generation.)
     */
    int get_virtual_field_index() const;

    /** Gets the storage "index" of this field within its data tuple.
     *  This field must have static non-virtual storage class.
     * (This should maybe be moved elsewhere as it is specific to low-level code generation.)
     */
    int get_static_field_index() const;

    /** Returns true if this field is statically constant. */
    bool is_statically_constant() const;

    /** If this field's initializer can be statically evaluated,
     * a TxConstantProxy representing its value is returned, otherwise nullptr.
     * In future, this should return non-null for all expressions for which is_statically_constant() returns true.
     */
    virtual const TxConstantProxy* get_static_constant_proxy() const;

//    /** Returns true if this field is a binding for a generic base type's type parameter. */
//    bool is_generic_param_binding() const {
//        return (this->get_name().find_last_of('#') != std::string::npos);
//    }

    bool is_modifiable() const;

//    virtual TxSymbolScope* lookup_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) override {
//        // field symbols don't have member symbols - lookup is via type instead
//        ResolutionContext resCtx;
//        auto type = this->resolve_symbol_type(resCtx);
//        //std::cout << "field '" << this->get_full_name() << "' resolves to type " << type << std::endl;
//        return type->lookup_instance_member(path, ident);
//    }

    inline const std::string get_unique_name() const {
        return this->get_declaration()->get_unique_name();
    }

    virtual bool validate() const override { return true; }

    virtual std::string to_string() const {
        return std::string("FIELD ") + ::to_string(this->get_decl_flags()) + " " + this->get_symbol()->get_full_name().to_string();
    }
};

/* * Represents a single declared type. * /
class TxTypeEntity : public TxDistinctEntity {
    mutable bool hasCheckedInstanceFields = false;
    mutable bool declaresInstanceFields = false;

    TxSymbolScope* inner_lookup_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident, bool static_lookup);

public:
    TxTypeEntity(TxSymbolScope* parent, const std::string& name, TxTypeDefiner* entityDefiner, TxDeclarationFlags declFlags)
            : TxDistinctEntity(parent, name, entityDefiner, declFlags) {
        // types are implicitly static; it's not legal to specify them in source
        ASSERT ((declFlags | LEGAL_TYPE_DECL_FLAGS) == LEGAL_TYPE_DECL_FLAGS, "Illegal type declFlags: " << declFlags);
    }

    virtual TxTypeEntity* make_copy(const std::string& newName) const {
        return new TxTypeEntity(this->get_outer(), newName, this->entityDefiner, this->get_decl_flags());
    }


    virtual bool validate_symbol(ResolutionContext& resCtx) override;


//    virtual const TxType* resolve_symbol_type(ResolutionContext& resCtx) override {
//        auto type = TxDistinctEntity::resolve_symbol_type(resCtx);
//        this->define_data_layout(resCtx, type);
//        return type;
//    }

    virtual const TxType* get_type() const override {
        return TxDistinctEntity::get_type();
    }

    / ** match against this entity's static members (from statically known type, up through its base types) * /
    virtual TxSymbolScope* lookup_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) override;

    / ** match against this entity's instance & static members (from statically known type, up through its base types) * /
    virtual TxScopeSymbol* lookup_instance_member(std::vector<TxScopeSymbol*>& path, const TxIdentifier& ident);


    / ** Returns true if this type declares any instance fields (i.e. in addition to base types' members.) * /
    bool declares_instance_fields() const {
        // note: this check needs to be shallow - not traverse all type defs - to prevent risk of infinite recursion
        if (! this->hasCheckedInstanceFields) {
            this->hasCheckedInstanceFields = true;
            this->declaresInstanceFields = std::any_of( this->symbols_cbegin(), this->symbols_cend(),
                                [](const SymbolMap::value_type & p) {
                                    if (auto field = dynamic_cast<TxFieldEntity*>(p.second))
                                        return (field->get_storage() == TXS_INSTANCE || field->get_storage() == TXS_INSTANCEMETHOD);
                                    return false; } );
        }
        return this->declaresInstanceFields;
    }


    virtual std::string to_string() const {
        return std::string("TYPE  ") + ::to_string(this->get_decl_flags()) + " " + this->get_full_name().to_string();
    }
};
*/
