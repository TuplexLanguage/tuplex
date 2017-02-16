#include "entity.hpp"

#include "ast.hpp"


Logger& TxEntity::LOG = Logger::get("ENTITY");


const TxLocation& TxEntity::get_parse_location() const {
    ASSERT(this->declaration, "NULL declaration in " << this);
    return this->declaration->get_definer()->get_parse_location();
}

ExpectedErrorClause* TxEntity::exp_err_ctx() const {
    ASSERT(this->declaration, "NULL declaration in " << this);
    return this->declaration->get_definer()->exp_err_ctx();
}


int TxField::get_decl_storage_index() const {
    if (auto typeDecl = this->get_outer_type_decl()) {
        if (auto outerType = typeDecl->get_definer()->get_type()) {  // assumes already resolved
            switch (this->get_storage()) {
            case TXS_STATIC:
                return outerType->get_static_fields().get_field_index(this->get_unique_name());
            case TXS_VIRTUAL:
            case TXS_INSTANCEMETHOD:
                ASSERT(! (this->get_decl_flags() & TXD_CONSTRUCTOR), "constructor does not have an instance method index: " << this);
                return outerType->get_virtual_fields().get_field_index(this->get_unique_name());
            case TXS_INSTANCE:
                return outerType->get_instance_fields().get_field_index(this->get_unique_name());
            default:
                //ASSERT(false, "Only fields of static/virtual/instancemethod/instance storage classes have a storage index: " << *this);
                return -1;
            }
        }
    }
    return -1;
}

bool TxField::is_statically_constant() const {
    /*
    bool genericParamBinding = (this->get_unique_name().find_last_of('#') != std::string::npos);
    if ( (this->get_decl_flags() & TXD_GENPARAM) || genericParamBinding )
        // (The second condition might be removable in future, but now needed to avoid expecting e.g.
        // tx#Array#L to be statically constant)
        return false;
    if ( this->get_storage() == TXS_GLOBAL )
        return true;
    */
    // A field is statically constant if it is unmodifiable and has a statically constant initializer.
    if (auto initExpr = this->get_declaration()->get_definer()->get_init_expression())
        return ( ! this->get_type()->is_modifiable() && initExpr->is_statically_constant() );
    return false;
}

const TxConstantProxy* TxField::get_static_constant_proxy() const {
    if (this->is_statically_constant())
        if (auto initExpr = this->get_declaration()->get_definer()->get_init_expression())
            return initExpr->get_static_constant_proxy();
    return nullptr;
}

bool TxField::is_modifiable() const {
    return this->get_type()->is_modifiable();
}
