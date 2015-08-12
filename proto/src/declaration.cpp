#include "declaration.hpp"

#include "assert.hpp"
#include "entity.hpp"
#include "symbol.hpp"
#include "type.hpp"


bool TxEntityDeclaration::in_exp_err_block() const {
    return (this->declFlags & TXD_EXPERRBLOCK) || this->symbol->in_exp_err_block();
}

std::string TxEntityDeclaration::to_string() const {
    return ::to_string(this->declFlags) + " " + this->get_unique_full_name();
}


unsigned TxFieldDeclaration::get_overload_index() const {
    unsigned i = 0;
    for (auto iter = this->get_symbol()->fields_cbegin(); iter != this->get_symbol()->fields_cend(); iter++, i++)
        if ((*iter) == this)
            return i;
    ASSERT(false, "could not find this field decl within its symbol: " << this->to_string());
}

bool TxFieldDeclaration::validate() const {
    if (auto field = this->get_definer()->get_field()) {
        bool valid = field->validate();
        return valid || this->in_exp_err_block();
    }
    else {
        if (this->in_exp_err_block())
            return true;
        this->get_symbol()->LOGGER().warning("In validation of %s: No field definition", this->to_string().c_str());
        return false;
    }
}

std::string TxFieldDeclaration::get_unique_full_name() const {
    if (this->get_symbol()->is_overloaded())
        return this->get_symbol()->get_full_name().to_string() + "$" + std::to_string(this->get_overload_index());
    else
        return this->get_symbol()->get_full_name().to_string();
}

std::string TxFieldDeclaration::get_unique_name() const {
    if (this->get_symbol()->is_overloaded())
        return this->get_symbol()->get_name() + "$" + std::to_string(this->get_overload_index());
    else
        return this->get_symbol()->get_name();
}

bool TxTypeDeclaration::validate() const {
    if (auto type = this->get_definer()->get_type()) {
        bool valid = type->validate();
        return valid || this->in_exp_err_block();
    }
    else {
        if (this->in_exp_err_block())
            return true;
        this->get_symbol()->LOGGER().warning("In validation of %s: No type definition", this->to_string().c_str());
        return false;
    }
}

std::string TxTypeDeclaration::get_unique_full_name() const {
    return this->get_symbol()->get_full_name().to_string();
}
std::string TxTypeDeclaration::get_unique_name() const {
    return this->get_symbol()->get_name();
}
