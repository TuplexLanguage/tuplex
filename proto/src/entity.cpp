#include "entity.hpp"

#include "ast_base.hpp"


int TxFieldEntity::get_instance_field_index() const {
    ASSERT(this->storage == TXS_INSTANCE, "Only fields of instance storage class have an instance field index: " << *this);
    auto scope = dynamic_cast<const TxTypeEntity*>(this->get_outer());
    ASSERT(scope, "Field's scope is not a type: " << *this->get_outer());
    return scope->get_instance_fields().get_field_index(this->get_name());
}

int TxFieldEntity::get_virtual_field_index() const {
    ASSERT(this->storage == TXS_VIRTUAL || this->storage == TXS_INSTANCEMETHOD,
           "Only fields of static virtual storage class have an virtual field index: " << *this);
    auto scope = dynamic_cast<const TxTypeEntity*>(this->get_outer());
    ASSERT(scope, "Field's scope is not a type: " << *this->get_outer());
    if (this->storage == TXS_VIRTUAL)
        return scope->get_virtual_fields().get_field_index(this->get_name()) + scope->get_instance_methods().get_field_count();
    else
        return scope->get_instance_methods().get_field_index(this->get_name());
}

int TxFieldEntity::get_static_field_index() const {
    ASSERT(this->storage == TXS_STATIC, "Only fields of static non-virtual storage class have a static field index: " << *this);
    auto scope = dynamic_cast<const TxTypeEntity*>(this->get_outer());
    ASSERT(scope, "Field's scope is not a type: " << *this->get_outer());
    return scope->get_static_fields().get_field_index(this->get_name());
}


void TxTypeEntity::define_data_layout(ResolutionContext& resCtx, const TxType* type) {
    if (this->dataLaidOut)
        return;
    ASSERT(!this->startedLayout, "Recursive call to define_data_layout() of " << *this);
    this->startedLayout = true;
    this->LOGGER().debug("Laying out data of type %s", this->get_full_name().to_string().c_str());

    // copy base type's virtual and instance field tuples (to which we will add and override fields):
    const TxType* baseType = type->get_base_type();
    while (baseType && !baseType->entity())
        baseType = baseType->get_base_type();
    if (baseType) {
        baseType->entity()->resolve_symbol_type(resCtx);
        this->virtualFields = baseType->entity()->virtualFields;
        this->instanceMethods = baseType->entity()->instanceMethods;
        this->instanceFields = baseType->entity()->instanceFields;
    }

    for (auto symname = this->symbol_names_cbegin(); symname != this->symbol_names_cend(); symname++) {
        auto symbol = this->get_symbol(*symname);
        TxFieldEntity* field = dynamic_cast<TxFieldEntity*>(symbol);
        if (! field)
            if (auto overloadedField = dynamic_cast<TxOverloadedEntity*>(symbol))
                field = overloadedField->get_first_field();
        if (field) {
            auto fieldType = field->resolve_symbol_type(resCtx);
            if (field->get_storage() == TXS_INSTANCE) {
                this->LOGGER().debug("Laying out instance field %-40s  %s  %s", field->get_full_name().to_string().c_str(),
                        ::to_string(this->get_decl_flags()).c_str(), fieldType->to_string(true).c_str());
                this->instanceFields.add_field(field->get_name(), fieldType);
                this->declaresInstanceFields = true;
            }
            else if (field->get_storage() == TXS_INSTANCEMETHOD) {
                if (field->get_decl_flags() & TXD_CONSTRUCTOR) {
                    // skip, constructors aren't virtual
                }
                else if (this->virtualFields.has_field(field->get_name())) {
                    this->LOGGER().error("A non-static method may not override a static parent field: %s", field->to_string().c_str());
                }
                else if (this->instanceMethods.has_field(field->get_name())) {
                    if (! (field->get_decl_flags() & TXD_OVERRIDE))
                        this->LOGGER().warning("Field overrides but isn't declared 'override': %s", field->to_string().c_str());
                }
                else {
                    if (field->get_decl_flags() & TXD_OVERRIDE)
                        this->LOGGER().warning("Field doesn't override but is declared 'override': %s", field->to_string().c_str());
                    this->instanceMethods.add_field(field->get_name(), fieldType);
                }
                this->declaresInstanceFields = true;
            }
            else if (field->get_storage() == TXS_VIRTUAL) {
                if (this->instanceMethods.has_field(field->get_name())) {
                    this->LOGGER().error("A static field may not override a non-static parent method: %s", field->to_string().c_str());
                }
                else if (this->virtualFields.has_field(field->get_name())) {
                    if (! (field->get_decl_flags() & TXD_OVERRIDE))
                        this->LOGGER().warning("Field overrides but isn't declared 'override': %s", field->to_string().c_str());
                }
                else {
                    if (field->get_decl_flags() & TXD_OVERRIDE)
                        this->LOGGER().warning("Field doesn't override but is declared 'override': %s", field->to_string().c_str());
                    this->virtualFields.add_field(field->get_name(), fieldType);
                }
            }
            else {
                ASSERT(field->get_storage() == TXS_STATIC, "Invalid storage class " << field->get_storage() << " for field member " << *field);
                if (field->get_decl_flags() & TXD_OVERRIDE)
                    this->LOGGER().warning("Field doesn't override but is declared 'override': %s", field->to_string().c_str());
                this->staticFields.add_field(field->get_name(), fieldType);
            }
        }
    }

    this->dataLaidOut = true;
}


bool TxFieldEntity::is_statically_constant() const {
    if ( (this->get_decl_flags() & TXD_GENPARAM) || this->is_generic_param_binding() )
        // (The second condition might be removable in future, but now needed to avoid expecting e.g.
        // tx#Array#L to be statically constant)
        return false;
    if ( this->get_storage() == TXS_GLOBAL )
        return true;
    ResolutionContext resCtx;
    if ( auto type = const_cast<TxFieldEntity*>(this)->resolve_symbol_type(resCtx) )
        if (auto initExpr = this->get_init_expression() )
            return ( ! type->is_modifiable() && initExpr->is_statically_constant() );
    return false;
}

const TxConstantProxy* TxFieldEntity::get_static_constant_proxy() const {
    if (this->is_statically_constant())
        if (auto initExpr = this->get_init_expression())
            return initExpr->get_static_constant_proxy();
    return nullptr;
}


TxSymbolScope* TxDistinctEntity::resolve_generic(TxSymbolScope* vantageScope) {
    std::vector<TxSymbolScope*> tmpPath;
    if (this->get_decl_flags() & TXD_GENPARAM) {
        std::string bindingName = this->get_full_name().to_string();
        std::replace(bindingName.begin(), bindingName.end(), '.', '#');
        this->LOGGER().trace("Trying to resolve generic parameter %s = %s from %s", this->get_full_name().to_string().c_str(), bindingName.c_str(), vantageScope->get_full_name().to_string().c_str());
        if (auto boundSym = vantageScope->start_lookup_symbol(tmpPath, bindingName)) {
            this->LOGGER().debug("Substituting generic parameter %s with %s", this->to_string().c_str(), boundSym->to_string().c_str());
            return boundSym->resolve_generic(vantageScope);
        }
        else {
            // unbound symbols are not resolved against, unless they're defined by an outer scope -
            // meaning they're type parameters pertaining to the current lexical context
            if (vantageScope->get_full_name().begins_with(this->get_outer()->get_full_name()))
                this->LOGGER().debug("Scope of generic parameter %s encompasses current vantage scope %s", this->to_string().c_str(), vantageScope->get_full_name().to_string().c_str());
            else
                this->LOGGER().debug("Generic parameter %s unbound within vantage scope %s", this->to_string().c_str(), vantageScope->get_full_name().to_string().c_str());
        }
    }
    /*
    else if (auto alias = this->get_alias()) {
        this->LOGGER().warning("Trying to resolve alias %s = %s from %s", this->get_full_name().to_string().c_str(), this->get_alias()->to_string().c_str(), vantageScope->get_full_name().to_string().c_str());
        if (auto boundSym = vantageScope->start_lookup_symbol(tmpPath, *alias)) {
            this->LOGGER().debug("Substituting alias %s with %s", this->to_string().c_str(), boundSym->to_string().c_str());
            return boundSym->resolve_generic(vantageScope);
        }
        else {
            this->LOGGER().warning("Symbol is unknown alias: %s", this->to_string().c_str());
        }
    }
    */
    return this;
}

TxSymbolScope* TxAliasEntity::resolve_generic(TxSymbolScope* vantageScope) {
    this->LOGGER().alert("Substituting alias %s with %s (vantage %s)", this->get_full_name().to_string().c_str(),
                         this->get_aliased_name().to_string().c_str(), vantageScope->get_full_name().to_string().c_str());
    return this->get_aliased_entity()->resolve_generic(vantageScope);
}

TxSymbolScope* TxTypeEntity::inner_lookup_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident, bool static_lookup) {
    auto memberName = ident.segment(0);
    if (auto member = this->get_symbol(memberName)) {
        if (auto fieldMember = dynamic_cast<const TxFieldEntity*>(member)) {
            // if static lookup and instance member, return its type instead
            if (static_lookup && fieldMember->get_storage() == TXS_INSTANCE) {
                member = nullptr;
                if (auto fieldType = fieldMember->attempt_get_type()) {
                    if (fieldType->is_modifiable())
                        fieldType = fieldType->get_base_type();
                    member = fieldType->entity();
                }
                if (! member) {
                    this->LOGGER().debug("No type obtained for instance field '%s'", fieldMember->get_full_name().to_string().c_str());
                    return nullptr;
                }
            }
        }

        // if the identified member is a type parameter/alias, attempt to resolve it by substituting it for its binding:
        if (! path.empty()) {  // (can be empty upon expression-based member access: <expr>.member)
            TxSymbolScope* vantageScope = path.back();
            member = member->resolve_generic(vantageScope);
        }

        path.push_back(member);
        if (ident.is_plain())
            return member;
        else
            return member->lookup_member(path, TxIdentifier(ident, 1));
    }

    if (auto type = this->entityDefiner->attempt_get_type()) {
        // Without the guard this lookup can cause infinite recursion or runtime/assertion errors
        // when run before symbol table pass has completed.
        // The root cause is predef type name lookup is done *from within the scope of the new type being declared*.
        // This is necessary since in this kind of expression:
        // type Sub<T> derives Super<T>
        // The second T needs to resolve T in the namespace of Sub, but of course the supertypes of Sub
        // aren't defined at that point yet.
        this->LOGGER().trace("Looking up '%s' among inherited members of %s", ident.to_string().c_str(), this->get_full_name().to_string().c_str());
        if (static_lookup)
            return type->lookup_inherited_member(path, ident);
        else
            return type->lookup_inherited_instance_member(path, ident);
    }
    else {
        this->LOGGER().trace("Skipping looking up '%s' among inherited members of %s", ident.to_string().c_str(), this->get_full_name().to_string().c_str());
        return nullptr;
    }
}

TxSymbolScope* TxTypeEntity::lookup_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) {
    return this->inner_lookup_member(path, ident, true);
}

TxSymbolScope* TxTypeEntity::lookup_instance_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) {
    return this->inner_lookup_member(path, ident, false);
}


bool TxTypeEntity::validate_symbol(ResolutionContext& resCtx) {
    bool valid = TxDistinctEntity::validate_symbol(resCtx);
    if (! valid)
        return valid;
    else if (auto type = this->get_type()) {
        if (! this->dataLaidOut) {
            this->LOGGER().error("Data not laid out for type: %s", type->to_string().c_str());
            valid = false;
        }
        else {
            auto tupleType = dynamic_cast<const TxTupleType*>(type);
            for (auto symname = this->symbol_names_cbegin(); symname != this->symbol_names_cend(); symname++) {
                if (auto field = dynamic_cast<TxFieldEntity*>(this->get_symbol(*symname))) {
                    if (auto fieldType = field->resolve_symbol_type(resCtx)) {
                        if (! fieldType->is_concrete()) {
                            this->LOGGER().error("Can't declare a field of non-concrete type: %s", field->to_string().c_str());
                            valid = false;
                        }
                        else if (field->get_storage() == TXS_INSTANCE) {
                            //std::cout << "Concrete INSTANCE field " << field << std::endl;
                            if (! fieldType->is_statically_sized()) {
                                this->LOGGER().error("Instance fields that don't have statically determined size not yet supported: %s", field->to_string().c_str());
                                valid = false;
                            }
                            else if (! (tupleType || (field->get_decl_flags() & (TXD_GENPARAM | TXD_IMPLICIT)))) {
                                this->LOGGER().error("Can't declare instance member in non-tuple type: %s", field->to_string().c_str());
                                valid = false;
                            }
                        }
                        else {  // TXS_STATIC
                            //std::cout << "Concrete STATIC field " << field << std::endl;
                            if (! fieldType->is_statically_sized()) {
                                // since static fields are per generic base type, and not per specialization:
                                this->LOGGER().error("Static fields must have statically determined size: %s", field->to_string().c_str());
                                valid = false;
                            }
                        }
                    }
                }
            }
        }
    }
    else
        valid = false;
    return valid;
}
