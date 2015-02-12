#pragma once

#include <unordered_map>

#include "txassert.hpp"

#include "tx_lang_defs.hpp"
#include "type.hpp"
#include "identifier.hpp"

#include "scope.hpp"
#include "tx_declaration_flags.hpp"


/** Specifies the storage type for a field entity.
 * GLOBAL is compile-time-allocated.
 * Unmodifiable STATIC is compile-time-allocated.
 * Modifiable STATIC is (effectively) thread-local (what is referenced to might not be, within dataspace constraints)
 * INSTANCE is an object instance member (storage o/c the same as the object instance).
 * STACK is stack-allocated (unless promoted to register).
 */
enum TxFieldStorage : int { TXS_NOSTORAGE, TXS_GLOBAL, TXS_STATIC, TXS_INSTANCE, TXS_STACK };

// types are implicitly static
static const TxDeclarationFlags LEGAL_TYPE_DECL_FLAGS = TXD_ABSTRACT | TXD_FINAL | TXD_PUBLIC | TXD_PROTECTED | TXD_BUILTIN | TXD_IMPLICIT | TXD_GENPARAM;
static const TxDeclarationFlags LEGAL_FIELD_DECL_FLAGS = TXD_STATIC | TXD_FINAL | TXD_OVERRIDE | TXD_PUBLIC | TXD_PROTECTED | TXD_BUILTIN | TXD_IMPLICIT | TXD_GENPARAM;


/** A symbol that represents a declared source code entity (type or field), or several overloaded ones. */
class TxEntity : public TxSymbolScope, public TxTypeProxy {
protected:
    TxEntity(TxSymbolScope* parent, const std::string& name) : TxSymbolScope(parent, name)  { }

public:
    virtual const TxType* get_type() const override = 0;

    virtual std::string to_string() const {
        return "<entity>        " + this->get_full_name().to_string();
    }
};


///** Represents a declared entity that is an alias for another entity. */
//class TxAliasEntity : public TxEntity {
//protected:
//    const TxDeclarationFlags declFlags;
//    const TxIdentifier aliasedName;
//
//    virtual bool declare_symbol(TxSymbolScope* symbol) override {
//        this->LOGGER().error("Can't add member symbol (%s) directly to an alias symbol: %s",
//                             symbol->to_string().c_str(), this->get_full_name().to_string().c_str());
//        return false;
//    }
//
//public:
//    TxAliasEntity(TxSymbolScope* parent, const std::string& name, TxDeclarationFlags declFlags, const TxIdentifier& aliasedName)
//            : TxEntity(parent, name), declFlags(declFlags), aliasedName(aliasedName) {
//    }
//
//    TxDeclarationFlags get_decl_flags() const { return this->declFlags; }
//
//    const TxIdentifier& get_aliased_name() const { return this->aliasedName; }
//
//    virtual std::string to_string() const {
//        return "<alias>         " + this->get_full_name().to_string() + " = " + this->aliasedName.to_string();
//    }
//};


/** Represents a single declared source code entity - a field or a type. */
class TxDistinctEntity : public TxEntity {
    //TxDeclarationFlags alias = TXD_NONE;
    TxDeclarationFlags declFlags;
protected:
    mutable bool gettingType = false;  // during development - guard against recursive calls to get_type()
    TxTypeDefiner const * const typeDefiner;

    TxDistinctEntity(TxSymbolScope* parent, const std::string& name, const TxTypeDefiner* typeDefiner, TxDeclarationFlags declFlags)
            : TxEntity(parent, name), declFlags(declFlags), typeDefiner(typeDefiner) {
    }

public:
    virtual TxDistinctEntity* make_copy(const std::string& newName) const = 0;

    virtual const TxType* get_type() const override {
        ASSERT(!this->gettingType, "Recursive call to get_type() of " << this->get_full_name());
        this->gettingType = true;
        auto type = this->typeDefiner->get_type();
        this->gettingType = false;
        return type;
    }

    TxDeclarationFlags get_decl_flags() const { return this->declFlags; }

    virtual const TxSymbolScope* resolve_generic(const TxSymbolScope* vantageScope) const override;

    void set_alias() { this->declFlags = (this->declFlags | TXD_ALIAS); }
    virtual bool is_alias() const override { return (this->declFlags & TXD_ALIAS); }
    virtual const TxIdentifier* get_alias() const override;

    virtual bool prepare_symbol() override {
        bool valid = TxEntity::prepare_symbol();
        if (valid) {
            if (auto type = this->get_type()) {
                std::string errorMsg = type->validate();
                if (! errorMsg.empty()) {
                    this->LOGGER().error("%s", errorMsg.c_str());
                    valid = false;
                }
            }
            else
                valid = false;
        }
        return valid;
    }
};


/** Represents a single declared field. */
class TxFieldEntity : public TxDistinctEntity {
    const TxFieldStorage storage;
    const TxIdentifier dataspace;
    const TxExpressionNode* initializerExpr;

protected:
    virtual bool declare_symbol(TxSymbolScope* symbol) override {
        this->LOGGER().error("Can't add member symbol (%s) directly to a field symbol: %s",
                             symbol->to_string().c_str(), this->get_full_name().to_string().c_str());
        return false;
    }

public:
    TxFieldEntity(TxSymbolScope* parent, const std::string& name, const TxTypeDefiner* typeDefiner, TxDeclarationFlags declFlags,
                  TxFieldStorage storage, const TxIdentifier& dataspace, const TxExpressionNode* initializerExpr)
            : TxDistinctEntity(parent, name, typeDefiner, declFlags), storage(storage), dataspace(dataspace), initializerExpr(initializerExpr) {
        ASSERT ((declFlags | LEGAL_FIELD_DECL_FLAGS) == LEGAL_FIELD_DECL_FLAGS, "Illegal field declFlags: " << declFlags);
    }

    virtual TxFieldEntity* make_copy(const std::string& newName) const {
        return new TxFieldEntity(this->get_parent(), newName, this->typeDefiner, this->get_decl_flags(), this->storage, this->dataspace, this->initializerExpr);
    }

    inline TxFieldStorage get_storage() const { return this->storage; }

    /** Gets the "index" of this field under its parent. This field must have instance storage class.
     * (This should maybe be moved elsewhere as it is specific to low-level code generation.)
     */
    int get_instance_field_index() const;

    /** Gets the "index" of this field under its parent. This field must have instance storage class.
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

    bool is_modifiable() const {
        return this->get_type()->is_modifiable();
    }

    virtual const TxSymbolScope* lookup_member(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const override {
        // field symbols don't have member symbols - lookup is via type instead
        return this->get_type()->lookup_instance_member(path, ident);
    }

    virtual std::string to_string() const {
        return std::string("FIELD ") + ::toString(this->get_decl_flags()) + " " + this->get_full_name().to_string();
    }
};

/** Represents a single declared type. */
class TxTypeEntity : public TxDistinctEntity {
    // FUTURE: overhaul initialization order so mutable no longer needed for data layout
    mutable bool dataLaidOut = false;
    mutable bool startedLayout = false;
    mutable std::unordered_map<const std::string*, int> staticFields;
    mutable std::unordered_map<const std::string*, int> instanceFields;
    mutable std::vector<const TxType*> staticFieldTypes;
    mutable std::vector<const TxType*> instanceFieldTypes;

    void define_data_layout() const {
        ASSERT(!this->dataLaidOut, "Data of " << *this << " already laid out");
        ASSERT(!this->startedLayout, "Recursive call to define_data_layout() of " << *this);
        this->startedLayout = true;
        for (auto siter = this->symbols_cbegin(); siter != this->symbols_cend(); siter++) {
            if (auto field = dynamic_cast<const TxFieldEntity*>(siter->second)) {
                auto fieldType = field->get_type();
                if (field->get_storage() == TXS_INSTANCE) {
                    this->instanceFields.emplace(&field->get_name(), this->instanceFields.size());
                    instanceFieldTypes.push_back(fieldType);
                }
                else {
                    ASSERT(field->get_storage() == TXS_STATIC, "Invalid storage class " << field->get_storage() << " for field member " << *field);
                    this->staticFields.emplace(&field->get_name(), this->staticFields.size());
                    staticFieldTypes.push_back(fieldType);
                }
            }
        }
        this->dataLaidOut = true;
    }

public:
    TxTypeEntity(TxSymbolScope* parent, const std::string& name, const TxTypeDefiner* typeDefiner, TxDeclarationFlags declFlags)
            : TxDistinctEntity(parent, name, typeDefiner, declFlags) {
        // types are implicitly static; it's not legal to specify them in source
        ASSERT ((declFlags | LEGAL_TYPE_DECL_FLAGS) == LEGAL_TYPE_DECL_FLAGS, "Illegal type declFlags: " << declFlags);
    }

    virtual TxTypeEntity* make_copy(const std::string& newName) const {
        return new TxTypeEntity(this->get_parent(), newName, this->typeDefiner, this->get_decl_flags());
    }

    virtual const TxType* get_type() const override {
        auto type = TxDistinctEntity::get_type();
        if (! type)
            this->LOGGER().warning("In get_type() of entity %s: type is NULL", this->to_string().c_str());
            //ASSERT(type, "Type of entity " << this << " is NULL");
        else
            ASSERT(type->entity()==this || this->is_alias(), "Type (" << type << ") does not belong to this entity "
                    << this->get_full_name() << ", it belongs to " << type->entity());
        return type;
    }

    /** match against this entity's static members (from statically known type, up through its base types) */
    virtual const TxSymbolScope* lookup_member(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const override;

    /** match against this entity's instance/static members (from statically known type, up through its base types) */
    virtual const TxSymbolScope* lookup_instance_member(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const {
        //if (auto member = TxDistinctEntity::lookup_member(path, ident)) bloody h*ll
        if (auto member = lookup_member(path, ident))
            return member;
        else
            return this->get_type()->lookup_inherited_instance_member(path, ident);
    }

    /** Returns true if this type declares any instance fields. (Does not consider base types' members.) */
    bool has_instance_fields() const {
        // note: this check needs to be shallow - not traverse all type defs - to prevent risk of infinite recursion
        if (! this->dataLaidOut) {
            return std::any_of( this->symbols_cbegin(), this->symbols_cend(),
                                [](const SymbolMap::value_type & p) {
                                    if (auto field = dynamic_cast<TxFieldEntity*>(p.second))
                                        return (field->get_storage() == TXS_INSTANCE);
                                    return false; } );
        }
        return ! this->instanceFieldTypes.empty();
    }


    /*--- data layout ---*/

    const std::vector<const TxType*> get_instance_field_types() const {
        if (! this->dataLaidOut)
            this->define_data_layout();
        return this->instanceFieldTypes;
    }

    const std::vector<const TxType*> get_static_field_types() const {
        if (! this->dataLaidOut)
            this->define_data_layout();
        return this->staticFieldTypes;
    }

    /** Gets the data layout index of the named instance field in this type. */
    int get_instance_field_index(const std::string& name) const {
        if (! this->dataLaidOut)
            this->define_data_layout();
        return this->instanceFields.at(&name);
    }

    /** Gets the data layout index of the named static field in this type. */
    int get_static_field_index(const std::string& name) const {
        if (! this->dataLaidOut)
            this->define_data_layout();
        return this->staticFields.at(&name);
    }


    virtual bool prepare_symbol() override {
        bool valid = TxDistinctEntity::prepare_symbol();
        if (! valid)
            return valid;
        else if (this->get_alias()) {
            // do something?
            //this->LOGGER().debug("Alias: %s", alias->to_string().c_str());
        }
        else if (auto type = this->get_type()) {
            if (this->has_instance_fields()) {
                if (auto tupleType = dynamic_cast<const TxTupleType*>(type)) {
                    for (auto basetype = tupleType->get_base_type(); basetype;
                              basetype = basetype->get_base_type()) {
                        if (basetype->entity() && basetype->entity()->has_instance_fields()) {
                            this->LOGGER().error("Not yet supported to extend base types with additional instance members: %s", type->to_string().c_str());
                            valid = false;
                        }
                    }
                }
                else {
                    this->LOGGER().error("Can't declare instance members in a non-tuple type: %s", type->to_string().c_str());
                    valid = false;
                }
            }
        }
        else
            valid = false;
        return valid;
    }

    virtual std::string to_string() const {
        return std::string("TYPE  ") + ::toString(this->get_decl_flags()) + " " + this->get_full_name().to_string();
    }
};


/** A symbol that has been overloaded and represents multiple declared entities. */
class TxOverloadedEntity : public TxEntity {
    TxTypeEntity* typeEntity;
    std::vector<TxFieldEntity*> fieldEntities;

protected:
    virtual bool declare_symbol(TxSymbolScope* symbol) override {
        this->LOGGER().error("Can't add member symbol (%s) directly to an overloaded symbol: %s",
                             symbol->to_string().c_str(), this->get_full_name().to_string().c_str());
        return false;
    }

public:
    TxOverloadedEntity(TxDistinctEntity* entity) : TxEntity(entity->get_parent(), entity->get_name()), typeEntity() {
        this->add(entity);
    }

    /** Adds a distinct entity declaration to this overloaded symbol, after performing some validity checking.
     * Note that the provided entity does not get this overloaded symbol as its parent scope.
     */
    bool add(TxDistinctEntity* entity) {
        // verify that the names are equal except for the internal "$..." suffix:
        auto entName = entity->get_full_name().to_string();
        ASSERT(this->get_full_name().to_string() == entName.substr(0, this->get_full_name().to_string().length()),
               "Overloaded entity declarations must have the same qualified name (except for $ suffix): "
               << entName << "!=" << this->get_full_name().to_string());

        if (auto typeEnt = dynamic_cast<TxTypeEntity*>(entity)) {
            if (this->typeEntity) {
                this->LOGGER().error("Can't overload several type declarations on the same name: %s", this->get_full_name().to_string().c_str());
                return false;
            }
            this->typeEntity = typeEnt;
        }
        else if (auto fieldEnt = dynamic_cast<TxFieldEntity*>(entity))
            this->fieldEntities.push_back(fieldEnt);
        else {  // shouldn't happen...
            this->LOGGER().error("Unknown TxDistinctEntity type: %s", entity->to_string().c_str());
            return false;
        }
        return true;
    }

    inline TxTypeEntity* get_type_declaration() const { return typeEntity; }

    inline std::vector<TxFieldEntity*>::const_iterator fields_cbegin() const noexcept { return this->fieldEntities.cbegin(); }
    inline std::vector<TxFieldEntity*>::const_iterator fields_cend() const noexcept { return this->fieldEntities.cend(); }

    inline size_t field_count() const { return this->fieldEntities.size(); }
    inline size_t count() const { return this->fieldEntities.size() + (this->typeEntity ? 1 : 0); }

    virtual const TxType* get_type() const {
        return this->typeEntity ? this->typeEntity->get_type() : nullptr;
// previous code for returning an "overloaded function name" type:
//        auto groupType = new TxFunctionGroupType(nullptr);
//        for (auto decl : this->fieldEntities) {
//            auto entType = decl->get_type();
//            if (auto funcType = dynamic_cast<const TxFunctionType*>(entType))
//                groupType->add(funcType);
//            else
//                throw std::logic_error("Illegal overloaded entity type: " + entType->to_string());
//        }
//        return groupType;
    }


    virtual const TxSymbolScope* lookup_member(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const override {
        // for now: match against this overloaded symbol's type entity, if present, otherwise fail
        // (if/when functions can have "members", or non-function fields can be overloaded, this would need to change)
        if (this->typeEntity) {
            ASSERT(this == path.back(), "Expected this to equal last entity in path: " << *this << " != " << *path.back());
            path[path.size()-1] = this->typeEntity;
            return this->typeEntity->lookup_member(path, ident);
        }
        this->LOGGER().warning("Attempted to lookup member %s in overloaded fields symbol %s", ident.to_string().c_str(), this->get_full_name().to_string().c_str());
        return TxEntity::lookup_member(path, ident);
    }

    virtual bool prepare_symbol() override {
        // Note: The 'specific' entity instances overloaded here are prepared via their unique'd symbol names,
        // so we don't prepare them here.
        bool valid = TxEntity::prepare_symbol();
        std::vector<const TxFunctionType*> functionTypes;
        for (auto fieldDeclI = this->fields_cbegin(); fieldDeclI != this->fields_cend(); fieldDeclI++) {
            auto type = (*fieldDeclI)->get_type();
            if (auto funcType = dynamic_cast<const TxFunctionType*>(type)) {
                // TODO: check that no two signatures are exactly equal
                functionTypes.push_back(funcType);
            }
            else {
                this->LOGGER().error("Illegal overload of symbol %s with type %s", (*fieldDeclI)->to_string().c_str(),
                                     (type ? type->to_string().c_str() : "NULL"));
                valid = false;
            }
        }
        return valid;
    }

    virtual std::string to_string() const {
        return std::string("overloaded ") + this->get_full_name().to_string();
    }
};
