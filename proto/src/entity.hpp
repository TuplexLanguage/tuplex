#pragma once

#include "txassert.hpp"

#include "tx_lang_defs.hpp"
#include "type.hpp"
#include "identifier.hpp"

#include "scope.hpp"
#include "tx_declaration_flags.hpp"


/** Specifies the storage type for a field entity.
 * GLOBAL is compile-time-allocated.
 * Unmodifiable STATIC is?
 * Modifiable STATIC, and INSTANCE are heap-allocated.
 * Modifiable STATIC is thread-local (what is referenced to might not be, within dataspace constraints)
 * STACK is stack-allocated (unless promoted to register).
 */
enum TxFieldStorage : int { TXS_NOSTORAGE, TXS_GLOBAL, TXS_STATIC, TXS_INSTANCE, TXS_STACK };

static const TxDeclarationFlags LEGAL_TYPE_DECL_FLAGS = TXD_ABSTRACT | TXD_FINAL | TXD_PUBLIC | TXD_PROTECTED;
static const TxDeclarationFlags LEGAL_FIELD_DECL_FLAGS = TXD_STATIC | TXD_FINAL | TXD_OVERRIDE | TXD_PUBLIC | TXD_PROTECTED;


/** A symbol that represents a declared source code entity (type or field), or several overloaded ones. */
class TxEntity : public TxSymbolScope, public TxTypeProxy {
protected:
    TxEntity(TxSymbolScope* parent, const std::string& name) : TxSymbolScope(parent, name)  { }

public:
    virtual const TxType* get_type() const override = 0;

    virtual std::string to_string() const {
        return "<entity> " + this->get_full_name().to_string();
    }
};


/** Represents a single declared source code entity - a field or a type. */
class TxDistinctEntity : public TxEntity {
protected:
    TxTypeProxy const * const typeDefiner;
    const TxDeclarationFlags declFlags;

    TxDistinctEntity(TxSymbolScope* parent, const std::string& name, const TxTypeProxy* typeDefiner, TxDeclarationFlags declFlags)
            : TxEntity(parent, name), typeDefiner(typeDefiner), declFlags(declFlags) {
    }

public:
    virtual TxDistinctEntity* make_copy(const std::string& newName) const = 0;

    virtual const TxType* get_type() const override { return this->typeDefiner->get_type(); }

    TxDeclarationFlags get_decl_flags() const { return this->declFlags; }
};

/** Represents a single declared field. */
class TxFieldEntity : public TxDistinctEntity {
    const TxFieldStorage storage;
    const TxIdentifier dataspace;

protected:
    virtual bool declare_symbol(TxSymbolScope* symbol) override {
        this->LOGGER().error("Can't add member symbol (%s) directly to a field symbol: %s",
                             symbol->to_string().c_str(), this->get_full_name().to_string().c_str());
        return false;
    }

public:
    TxFieldEntity(TxSymbolScope* parent, const std::string& name, const TxTypeProxy* typeDefiner, TxDeclarationFlags declFlags,
                  TxFieldStorage storage, const TxIdentifier& dataspace)
            : TxDistinctEntity(parent, name, typeDefiner, declFlags), storage(storage), dataspace(dataspace) {
        ASSERT ((declFlags | LEGAL_FIELD_DECL_FLAGS) == LEGAL_FIELD_DECL_FLAGS, "Illegal field declFlags: " << declFlags);
    }

    virtual TxFieldEntity* make_copy(const std::string& newName) const {
        return new TxFieldEntity(this->get_parent(), newName, this->typeDefiner, this->declFlags, this->storage, this->dataspace);
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

    bool is_statically_constant() const {
        return ( this->get_storage() == TXS_GLOBAL
                 || (this->get_storage() == TXS_STATIC && this->get_type()->is_immutable()));
                // TODO: also return true if storage is static and expression.is_statically_constant()
    }

    bool is_modifiable() const {
        return this->get_type()->is_modifiable();
    }

    virtual const TxSymbolScope* lookup_member(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const override {
        // field symbols don't have member symbols - lookup is via type instead
        return this->get_type()->lookup_instance_member(path, ident);
    }

    virtual std::string to_string() const {
        return std::string("<field> ") + ::toString(this->declFlags) + " " + this->get_full_name().to_string();
    }
};

/** Represents a single declared type. */
class TxTypeEntity : public TxDistinctEntity {
    // FUTURE: overhaul initialization order so mutable no longer needed
    mutable bool dataLaidOut = false;
    mutable std::map<const std::string*, int> staticFields;
    mutable std::map<const std::string*, int> instanceFields;
    mutable std::vector<const TxType*> staticFieldTypes;
    mutable std::vector<const TxType*> instanceFieldTypes;

    void define_data_layout() const {
        ASSERT(!this->dataLaidOut, "Data of " << *this << " already laid out");
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
    TxTypeEntity(TxSymbolScope* parent, const std::string& name, const TxTypeProxy* typeDefiner, TxDeclarationFlags declFlags)
            : TxDistinctEntity(parent, name, typeDefiner, declFlags) {
        ASSERT ((declFlags | LEGAL_TYPE_DECL_FLAGS) == LEGAL_TYPE_DECL_FLAGS, "Illegal type declFlags: " << declFlags);
    }

    virtual TxTypeEntity* make_copy(const std::string& newName) const {
        return new TxTypeEntity(this->get_parent(), newName, this->typeDefiner, this->declFlags);
    }

    virtual const TxType* get_type() const override {
        auto type = this->typeDefiner->get_type();
        ASSERT(type, "Type of entity " << this << " is NULL");
        ASSERT(type->entity()==this, "Type (" << type << ") does not belong to this entity " << this->get_full_name());
        return type;
    }

    /** match against this entity's static members (from statically known type, up through its base types) */
    virtual const TxSymbolScope* lookup_member(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const override {
        // static lookup, so if instance-member, return its type instead
        auto memberName = ident.segment(0);
        if (auto member = this->get_symbol(memberName)) {
            if (auto fieldMember = dynamic_cast<const TxFieldEntity*>(member))
                if (fieldMember->get_storage() == TXS_INSTANCE) {
                    auto fieldType = fieldMember->get_type();
                    if (fieldType->is_pure_modifiable())
                         member = fieldType->get_base_type_spec().type->entity();
                    else
                         member = fieldType->entity();
                    if (! member)
                        return nullptr;
                }

            path.push_back(member);
            if (ident.is_plain())
                return member;
            else
                return member->lookup_member(path, TxIdentifier(ident, 1));
        }
        // TODO: this causes infinite recursion when run before symbol table pass has completed:
        //std::cout << "LOOKING UP " << ident << " in " << this->get_full_name() << "; trying inherited members" << std::endl;
        return nullptr; // this->get_type()->lookup_inherited_member(path, ident);
    }

    /** match against this entity's instance/static members (from statically known type, up through its base types) */
    virtual const TxSymbolScope* lookup_instance_member(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const {
        if (auto member = TxDistinctEntity::lookup_member(path, ident))
            return member;
        else
            return this->get_type()->lookup_inherited_instance_member(path, ident);
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


    virtual std::string to_string() const {
        return std::string("<type>  ") + ::toString(this->declFlags) + " " + this->get_full_name().to_string();
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
        auto groupType = new TxFunctionGroupType();
        for (auto decl : this->fieldEntities) {
            auto entType = decl->get_type();
            if (auto funcType = dynamic_cast<const TxFunctionType*>(entType))
                groupType->add(funcType);
            else
                throw std::logic_error("Illegal overloaded entity type: " + entType->to_string());
        }
        return groupType;
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

    bool prepare_symbol() override {
        bool valid = TxEntity::prepare_symbol();
        std::vector<const TxFunctionType*> functionTypes;
        for (auto fieldDeclI = this->fields_cbegin(); fieldDeclI != this->fields_cend(); fieldDeclI++) {
            auto type = (*fieldDeclI)->get_type();
            if (auto funcType = dynamic_cast<const TxFunctionType*>(type)) {
                // TODO: check that no two signatures are exactly equal
                functionTypes.push_back(funcType);
            }
            else {
                this->LOGGER().error("Illegal overload of symbol %s with type %s", (*fieldDeclI)->to_string().c_str(), type->to_string().c_str());
                valid = false;
            }
        }
        return valid;
    }

    virtual std::string to_string() const {
        return std::string("overloaded ") + this->get_full_name().to_string();
    }
};
