#include "entity.hpp"

#include "ast_base.hpp"


//const TxIdentifier& TxDistinctEntity::get_alias() const {
//    auto type = this->get_type();
//    if (! type)
//        this->LOGGER().warning("In get_alias() of entity %s: type is NULL", this->to_string().c_str());
//    else if (type->entity() && (static_cast<const TxDistinctEntity*>(type->entity()) != this) && (type->entity()->get_decl_flags() & TXD_GENPARAM))
//        return &type->entity()->get_full_name();
//    return nullptr;
//}


int TxFieldEntity::get_instance_field_index() const {
    ASSERT(this->storage == TXS_INSTANCE, "Only fields of instance storage class have an instance field index: " << *this);
    auto parentType = dynamic_cast<const TxTypeEntity*>(this->get_outer());
    ASSERT(parentType, "Field's parent is not a type: " << *this->get_outer());
    return parentType->get_instance_field_index(this->get_name());
}

/** Gets the "index" of this field under its parent. This field must have instance storage class.
 * (This should maybe be moved elsewhere as it is specific to low-level code generation.)
 */
int TxFieldEntity::get_static_field_index() const {
    ASSERT(this->storage == TXS_STATIC, "Only fields of instance storage class have an instance field index: " << *this);
    auto parentType = dynamic_cast<const TxTypeEntity*>(this->get_outer());
    ASSERT(parentType, "Field's parent is not a type: " << *this->get_outer());
    return parentType->get_static_field_index(this->get_name());
}

bool TxFieldEntity::is_statically_constant() const {
    if ( (this->get_decl_flags() & TXD_GENPARAM) || this->is_generic_param_binding() )
        // (The second condition might be removable in future, but now needed to avoid expecting e.g.
        // tx#Array#L to be statically constant)
        return false;
    return ( this->get_storage() == TXS_GLOBAL
             || ( this->get_storage() == TXS_STATIC
                  && ( (! this->get_type()->is_modifiable() )
                       || ( this->initializerExpr && this->initializerExpr->is_statically_constant() ) ) )
             || ( // STACK or INSTANCE
                  (! this->get_type()->is_modifiable() )
                  && this->initializerExpr && this->initializerExpr->is_statically_constant() ) );
}

const TxConstantProxy* TxFieldEntity::get_static_constant_proxy() const {
    if (this->is_statically_constant() && this->initializerExpr)
        return this->initializerExpr->get_static_constant_proxy();
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
                auto fieldType = fieldMember->get_type();
                if (fieldType->is_modifiable())
                    fieldType = fieldType->get_base_type();
                member = fieldType->entity();
                if (! member) {
                    this->LOGGER().debug("No TxTypeEntity for type '%s' of field '%s'", fieldType->to_string().c_str(), fieldMember->get_full_name().to_string().c_str());
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
