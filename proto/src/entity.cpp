#include "entity.hpp"


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
