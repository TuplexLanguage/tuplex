#include "entity.hpp"

#include "ast_base.hpp"


int TxFieldEntity::get_instance_field_index() const {
    ASSERT(this->storage == TXS_INSTANCE, "Only fields of instance storage class have an instance field index: " << *this);
    auto parentType = dynamic_cast<const TxTypeEntity*>(this->get_parent());
    ASSERT(parentType, "Field's parent is not a type: " << *this->get_parent());
    return parentType->get_instance_field_index(this->get_name());
}

/** Gets the "index" of this field under its parent. This field must have instance storage class.
 * (This should maybe be moved elsewhere as it is specific to low-level code generation.)
 */
int TxFieldEntity::get_static_field_index() const {
    ASSERT(this->storage == TXS_STATIC, "Only fields of instance storage class have an instance field index: " << *this);
    auto parentType = dynamic_cast<const TxTypeEntity*>(this->get_parent());
    ASSERT(parentType, "Field's parent is not a type: " << *this->get_parent());
    return parentType->get_static_field_index(this->get_name());
}


const TxSymbolScope* TxTypeEntity::lookup_member(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const {
    // static lookup, so if instance-member, return its type instead
    auto memberName = ident.segment(0);
    if (auto member = this->get_symbol(memberName)) {
        if (auto fieldMember = dynamic_cast<const TxFieldEntity*>(member))
            if (fieldMember->get_storage() == TXS_INSTANCE) {
                auto fieldType = fieldMember->get_type();
                if (fieldType->is_modifiable())
                    fieldType = fieldType->get_base_type_spec().type;
                member = fieldType->entity();
                if (! member) {
                    this->LOGGER().debug("No TxTypeEntity for type '%s' of field '%s'", fieldType->to_string().c_str(), fieldMember->get_full_name().to_string().c_str());
                    return nullptr;
                }
            }

        path.push_back(member);
        if (ident.is_plain())
            return member;
        else
            return member->lookup_member(path, TxIdentifier(ident, 1));
    }

    if (! this->gettingType) {
        // Without the guard this lookup can cause infinite recursion or runtime/assertion errors
        // when run before symbol table pass has completed.
        // The root cause is predef type name lookup is done *from within the scope of the new type being declared*.
        // Changing this requires changing how the lexical context is passed within the type expression nodes.
        // (If changing, note that looking up bindings for type parameters still needs to work.)
        this->LOGGER().debug("Looking up '%s' among inherited members of %s", ident.to_string().c_str(), this->get_full_name().to_string().c_str());
        return this->get_type()->lookup_inherited_member(path, ident);
    }
    else
        return nullptr;
}
