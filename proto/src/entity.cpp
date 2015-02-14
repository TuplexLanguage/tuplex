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
    if ( this->get_decl_flags() & TXD_GENPARAM )
        return false;
    return ( this->get_storage() == TXS_GLOBAL
             || ( this->get_storage() == TXS_STATIC
                  && ( this->get_type()->is_immutable()
                       || ( this->initializerExpr && this->initializerExpr->is_statically_constant() ) ) ) );
}

const TxConstantProxy* TxFieldEntity::get_static_constant_proxy() const {
    return ( this->initializerExpr ? this->initializerExpr->get_static_constant_proxy() : nullptr );
}


TxSymbolScope* TxDistinctEntity::resolve_generic(TxSymbolScope* vantageScope) {
    std::vector<TxSymbolScope*> tmpPath;
    if (this->get_decl_flags() & TXD_GENPARAM) {
        std::string bindingName = this->get_full_name().to_string();
        std::replace(bindingName.begin(), bindingName.end(), '.', '#');
        this->LOGGER().trace("Trying to resolve type parameter %s = %s from %s", this->get_full_name().to_string().c_str(), bindingName.c_str(), vantageScope->get_full_name().to_string().c_str());
        if (auto boundSym = vantageScope->resolve_symbol(tmpPath, bindingName)) {
            this->LOGGER().warning("Substituting type parameter %s with %s", this->to_string().c_str(), boundSym->to_string().c_str());
            return boundSym->resolve_generic(vantageScope);
        }
        else {
            // unbound symbols are not resolved against, unless they're defined by an outer scope -
            // meaning they're type parameters pertaining to the current lexical context
            if (vantageScope->get_full_name().begins_with(this->get_outer()->get_full_name()))
                this->LOGGER().warning("symbol is being-defined type parameter %s from vantage scope %s", this->to_string().c_str(), vantageScope->get_full_name().to_string().c_str());
            else
                this->LOGGER().warning("symbol is unbound type parameter %s in vantage scope %s", this->to_string().c_str(), vantageScope->get_full_name().to_string().c_str());
        }
    }
    else if (this->is_alias()) {
        this->LOGGER().trace("Trying to resolve alias %s = %s from %s", this->get_full_name().to_string().c_str(), this->get_alias()->to_string().c_str(), vantageScope->get_full_name().to_string().c_str());
        if (auto boundSym = vantageScope->resolve_symbol(tmpPath, *this->get_alias())) {
            this->LOGGER().warning("Substituting alias %s with %s", this->to_string().c_str(), boundSym->to_string().c_str());
            return boundSym->resolve_generic(vantageScope);
        }
        else {
            this->LOGGER().warning("Symbol is unknown alias: %s", this->to_string().c_str());
        }
    }
    return this;
}

TxSymbolScope* TxTypeEntity::lookup_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) {
    auto memberName = ident.segment(0);
    if (auto member = this->get_symbol(memberName)) {
        if (auto fieldMember = dynamic_cast<const TxFieldEntity*>(member)) {
            // static lookup, so if instance-member, return its type instead
            if (fieldMember->get_storage() == TXS_INSTANCE) {
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

        // if the identified member is a type parameter, attempt to resolve it by substituting it for its binding:
        TxSymbolScope* vantageScope = path.back();
        member = member->resolve_generic(vantageScope);

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
        return type->lookup_inherited_member(path, ident);
    }
    else {
        this->LOGGER().trace("Skipping looking up '%s' among inherited members of %s", ident.to_string().c_str(), this->get_full_name().to_string().c_str());
        return nullptr;
    }
}
