#pragma once

#include <unordered_map>

#include "txassert.hpp"

#include "tx_lang_defs.hpp"
#include "type.hpp"
#include "identifier.hpp"

#include "scope.hpp"
#include "tx_declaration_flags.hpp"


/** Specifies the storage type for a field entity.
 * GLOBAL are globally declared fields, i.e. outside of any type definition.
 * STATIC and VIRTUAL are statically allocated fields within a type definition.
 * VIRTUAL fields are like STATIC but subject to polymorphic lookup.
 * INSTANCEMETHOD is a special case, where the function pointer is static/virtual and the 'self' ref is provided in runtime
 * INSTANCE fields are members of type instances (i.e. object members).
 * STACK fields are regular "auto" variables, including function arguments.
 * GLOBAL, STATIC, VIRTUAL are compile-time-allocated.
 */
enum TxFieldStorage : int { TXS_NOSTORAGE, TXS_GLOBAL, TXS_STATIC, TXS_VIRTUAL, TXS_INSTANCEMETHOD, TXS_INSTANCE, TXS_STACK };

// types are implicitly static
static const TxDeclarationFlags LEGAL_TYPE_DECL_FLAGS = TXD_ABSTRACT | TXD_FINAL | TXD_PUBLIC | TXD_PROTECTED | TXD_BUILTIN | TXD_IMPLICIT | TXD_GENPARAM;
static const TxDeclarationFlags LEGAL_FIELD_DECL_FLAGS = TXD_STATIC | TXD_FINAL | TXD_OVERRIDE | TXD_PUBLIC | TXD_PROTECTED | TXD_BUILTIN | TXD_IMPLICIT | TXD_GENPARAM | TXD_CONSTRUCTOR;


class DataTupleDefinition {
public:
    // we need to be able to look up fields both via index and plain name:
    /** map from plain name to field index (note, contains fewer entries than fieldTypes when parent field name is hidden) */
    std::unordered_map<std::string, uint32_t> fields;
    /** the field types */
    std::vector<const TxType*> fieldTypes;

    void add_field(const std::string& name, const TxType* type) {
        this->fields[name] = this->fieldTypes.size();  // (inserts new or overwrites existing entry)
        this->fieldTypes.push_back(type);
    }

    inline bool has_field(const std::string& name) const { return this->fields.count(name); }

    inline uint32_t get_field_index(const std::string& name) const { return this->fields.at(name); }

    inline uint32_t get_field_count() const { return this->fieldTypes.size(); }
};



/** A symbol that represents a declared source code entity (type or field), or several overloaded ones. */
class TxEntity : public TxSymbolScope { //, public TxTypeProxy {
protected:
    TxEntity(TxSymbolScope* parent, const std::string& name) : TxSymbolScope(parent, name)  { }

public:
    virtual const TxType* resolve_symbol_type(ResolutionContext& resCtx) = 0;

    virtual const TxType* get_type() const = 0;
};


/** Represents a single declared source code entity - a field or a type. */
class TxDistinctEntity : public TxEntity {
    TxDeclarationFlags declFlags;
    bool resolved = false;  // during development
protected:
    mutable bool gettingType = false;  // during development - guard against recursive calls to get_type()
    TxTypeDefiner * const entityDefiner;

    TxDistinctEntity(TxSymbolScope* parent, const std::string& name, TxTypeDefiner* entityDefiner, TxDeclarationFlags declFlags)
            : TxEntity(parent, name), declFlags(declFlags), entityDefiner(entityDefiner) {
    }

public:
    virtual TxDistinctEntity* make_copy(const std::string& newName) const = 0;


    virtual bool validate_symbol(ResolutionContext& resCtx) override {
        bool valid = TxEntity::validate_symbol(resCtx);
        if (auto type = this->resolve_symbol_type(resCtx)) {
            std::string errorMsg = type->validate(resCtx);
            if (! errorMsg.empty()) {
                this->LOGGER().error("Invalid type definition for %s: %s", this->get_full_name().to_string().c_str(), errorMsg.c_str());
                valid = false;
            }
        }
        else {
            this->LOGGER().warning("NULL type for entity: %s", this->to_string().c_str());
            valid = false;
        }
        return valid;
    }

    virtual const TxType* resolve_symbol_type(ResolutionContext& resCtx) override {
        resolved = true;
        return this->entityDefiner->resolve_type(resCtx);
    }

    virtual const TxType* attempt_get_type() const {
        return this->entityDefiner->attempt_get_type();
    }

    virtual const TxType* get_type() const {
        auto type = this->entityDefiner->get_type();
        ASSERT(type || resolved, "Called get_type() before symbols resolved of entity " << this);
        return type;
    }
//    virtual const TxType* get_type() const override {
//        ASSERT(!this->gettingType, "Recursive call to get_type() of " << this->get_full_name());
//        //ASSERT(resolved, "Called get_type() before symbols resolved of entity " << this);
//        this->gettingType = true;
//        auto type = this->entityDefiner->get_type();
//        this->gettingType = false;
//        return type;
//    }

    TxDeclarationFlags get_decl_flags() const { return this->declFlags; }

    virtual TxSymbolScope* resolve_generic(TxSymbolScope* vantageScope) override;
};


/** Represents a single declared field. */
class TxFieldEntity : public TxDistinctEntity {
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

    inline const TxExpressionNode* get_init_expression() const {
        return static_cast<const TxFieldDefiner*>(this->entityDefiner)->get_init_expression();
    }

protected:
    virtual bool declare_symbol(TxSymbolScope* symbol) override {
        this->LOGGER().error("Can't add member symbol (%s) directly to a field symbol: %s",
                             symbol->to_string().c_str(), this->get_full_name().to_string().c_str());
        return false;
    }

public:
    TxFieldEntity(TxSymbolScope* parent, const std::string& name, TxFieldDefiner* entityDefiner, TxDeclarationFlags declFlags,
                  TxFieldStorage storage, const TxIdentifier& dataspace)
            : TxDistinctEntity(parent, name, entityDefiner, declFlags),
              storage(determine_storage(storage, declFlags)), dataspace(dataspace) {
        ASSERT ((declFlags | LEGAL_FIELD_DECL_FLAGS) == LEGAL_FIELD_DECL_FLAGS, "Illegal field declFlags: " << declFlags);
    }

    virtual TxFieldEntity* make_copy(const std::string& newName) const {
        return new TxFieldEntity(this->get_outer(), newName, static_cast<TxFieldDefiner*>(this->entityDefiner),
                                 this->get_decl_flags(), this->storage, this->dataspace);
    }

    inline TxFieldStorage get_storage() const { return this->storage; }

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

    /** Returns true if this field is a binding for a generic base type's type parameter. */
    bool is_generic_param_binding() const {
        return (this->get_name().find_last_of('#') != std::string::npos);
    }

    bool is_modifiable() const {
        return this->get_type()->is_modifiable();
    }

    virtual TxSymbolScope* lookup_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) override {
        // field symbols don't have member symbols - lookup is via type instead
        ResolutionContext resCtx;  // FIXME
        auto type = this->resolve_symbol_type(resCtx);
        //std::cout << "field '" << this->get_full_name() << "' resolves to type " << type << std::endl;
        return type->lookup_instance_member(path, ident);
    }

    virtual std::string to_string() const {
        return std::string("FIELD ") + ::to_string(this->get_decl_flags()) + " " + this->get_full_name().to_string();
    }
};

/** Represents a single declared type. */
class TxTypeEntity : public TxDistinctEntity, public TxTypeProxy {
    bool dataLaidOut = false;
    bool startedLayout = false;
    bool declaresInstanceFields = false;
    DataTupleDefinition staticFields;
    DataTupleDefinition virtualFields;
    DataTupleDefinition instanceMethods;
    DataTupleDefinition instanceFields;

    void define_data_layout(ResolutionContext& resCtx, const TxType* type);

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


    virtual const TxType* resolve_symbol_type(ResolutionContext& resCtx) override {
        auto type = TxDistinctEntity::resolve_symbol_type(resCtx);
        this->define_data_layout(resCtx, type);
        return type;
    }

    virtual const TxType* get_type() const override {
        return TxDistinctEntity::get_type();
    }
//    virtual const TxType* get_type() const override {
//        auto type = TxDistinctEntity::get_type();
//        if (! type)
//            this->LOGGER().warning("In get_type() of entity %s: type is NULL", this->to_string().c_str());
//            //ASSERT(type, "Type of entity " << this << " is NULL");
//        else
//            ASSERT(type->entity()==this, "Type (" << type << ") does not belong to this entity "
//                    << this->get_full_name() << ", it belongs to " << type->entity());
//        return type;
//    }

    /** match against this entity's static members (from statically known type, up through its base types) */
    virtual TxSymbolScope* lookup_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) override;

    /** match against this entity's instance & static members (from statically known type, up through its base types) */
    virtual TxSymbolScope* lookup_instance_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident);


    /** Returns true if this type declares any instance fields (i.e. in addition to base types' members.) */
    bool declares_instance_fields() const {
        // note: this check needs to be shallow - not traverse all type defs - to prevent risk of infinite recursion
        if (! this->dataLaidOut) {
            return std::any_of( this->symbols_cbegin(), this->symbols_cend(),
                                [](const SymbolMap::value_type & p) {
                                    if (auto field = dynamic_cast<TxFieldEntity*>(p.second))
                                        return (field->get_storage() == TXS_INSTANCE || field->get_storage() == TXS_INSTANCEMETHOD);
                                    return false; } );
        }
        return this->declaresInstanceFields;
    }


    /*--- data layout ---*/

    const DataTupleDefinition& get_instance_fields() const {
        ASSERT(this->dataLaidOut, "Data not laid out in " << this);
        return this->instanceFields;
    }

    const DataTupleDefinition& get_instance_methods() const {
        ASSERT(this->dataLaidOut, "Data not laid out in " << this);
        return this->instanceMethods;
    }

    const DataTupleDefinition& get_virtual_fields() const {
        ASSERT(this->dataLaidOut, "Data not laid out in " << this);
        return this->virtualFields;
    }

    const DataTupleDefinition& get_static_fields() const {
        ASSERT(this->dataLaidOut, "Data not laid out in " << this);
        return this->staticFields;
    }



    virtual std::string to_string() const {
        return std::string("TYPE  ") + ::to_string(this->get_decl_flags()) + " " + this->get_full_name().to_string();
    }
};



/** Represents a declared entity that is an alias for another entity. */
class TxAliasEntity : public TxEntity {
    const TxDeclarationFlags declFlags;
    TxDistinctEntity* aliasedEntity;
    //const TxIdentifier aliasedName;
protected:
    virtual bool declare_symbol(TxSymbolScope* symbol) override {
        this->LOGGER().error("Can't add member symbol (%s) directly to an alias symbol: %s",
                             symbol->to_string().c_str(), this->get_full_name().to_string().c_str());
        return false;
    }

public:
//    TxAliasEntity(TxSymbolScope* parent, const std::string& name, TxDeclarationFlags declFlags, const TxIdentifier& aliasedName)
//            : TxEntity(parent, name), declFlags(declFlags), aliasedName(aliasedName) { }
    TxAliasEntity(TxSymbolScope* parent, const std::string& name, TxDeclarationFlags declFlags, TxDistinctEntity* aliasedEntity)
            : TxEntity(parent, name), declFlags(declFlags), aliasedEntity(aliasedEntity) { }

    TxDeclarationFlags get_decl_flags() const { return this->declFlags; }

    const TxIdentifier& get_aliased_name() const { return this->aliasedEntity->get_full_name(); }

    TxDistinctEntity* get_aliased_entity() const { return this->aliasedEntity; }

    TxSymbolScope* resolve_generic(TxSymbolScope* vantageScope) override;

    virtual const TxType* resolve_symbol_type(ResolutionContext& resCtx) override {
        return this->aliasedEntity->resolve_symbol_type(resCtx);
    }

    virtual const TxType* get_type() const override {
        return this->aliasedEntity->get_type();
    }

    virtual std::string to_string() const {
        return "<alias>          " + this->get_full_name().to_string() + " = " + this->get_aliased_name().to_string();
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
    TxOverloadedEntity(TxDistinctEntity* entity) : TxEntity(entity->get_outer(), entity->get_name()), typeEntity() {
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

    inline TxFieldEntity* get_first_field() const { return (this->fieldEntities.empty() ? nullptr : this->fieldEntities.front()); }

    inline std::vector<TxFieldEntity*>::const_iterator fields_cbegin() const noexcept { return this->fieldEntities.cbegin(); }
    inline std::vector<TxFieldEntity*>::const_iterator fields_cend() const noexcept { return this->fieldEntities.cend(); }

    inline size_t field_count() const { return this->fieldEntities.size(); }
    inline size_t count() const { return this->fieldEntities.size() + (this->typeEntity ? 1 : 0); }


    virtual const TxType* resolve_symbol_type(ResolutionContext& resCtx) override {
        return this->typeEntity ? this->typeEntity->resolve_symbol_type(resCtx) : nullptr;
    }

    virtual const TxType* get_type() const override {
        return this->typeEntity ? this->typeEntity->get_type() : nullptr;
    }


    virtual TxSymbolScope* lookup_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) override {
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

    virtual bool validate_symbol(ResolutionContext& resCtx) override {
        bool valid = TxEntity::validate_symbol(resCtx);

        if (this->typeEntity) {
            valid &= this->typeEntity->validate_symbol(resCtx);
        }

        // Note: The 'specific' field entity instances overloaded here are prepared via their unique'd symbol names,
        // so we don't prepare them here. Here we just check overloading rules.
        std::vector<const TxFunctionType*> functionTypes;
        for (auto fieldDeclI = this->fields_cbegin(); fieldDeclI != this->fields_cend(); fieldDeclI++) {
            auto type = (*fieldDeclI)->resolve_symbol_type(resCtx);
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

    virtual std::string to_string() const override {
        return "<overloaded>      " + this->get_full_name().to_string();
    }
};
