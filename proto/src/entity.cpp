#include "entity.hpp"

#include "ast_base.hpp"



const TxType* TxFieldDefiner::resolve_type(ResolutionContext& resCtx) {
    if (auto field = this->resolve_field(resCtx))
        return field->get_type();
    return nullptr;
}



Logger& TxEntity::LOG = Logger::get("ENTITY");


TxDriver* TxEntity::get_driver() const {
    return (this->declaration ? &this->declaration->get_symbol()->get_root_scope()->driver() : nullptr);
}
const yy::location& TxEntity::get_parse_location() const {
    return (this->declaration ? this->declaration->get_definer()->get_parse_location() : NULL_LOC);
}


const TxType* TxField::get_outer_type() const {
    auto typeDecl = this->get_outer_type_decl();
    ASSERT(typeDecl, "Field's scope is not a type: " << *this->get_symbol()->get_outer());
    return typeDecl->get_definer()->get_type();  // assumes already resolved
}

int TxField::get_storage_index() const {
    switch (this->get_storage()) {
    case TXS_STATIC:
        return this->get_outer_type()->get_static_fields().get_field_index(this->get_unique_name());
    case TXS_VIRTUAL:
    case TXS_INSTANCEMETHOD:
        ASSERT(! (this->get_decl_flags() & TXD_CONSTRUCTOR), "constructor does not have an instance method index: " << this);
        return this->get_outer_type()->get_virtual_fields().get_field_index(this->get_unique_name());
    case TXS_INSTANCE:
        return this->get_outer_type()->get_instance_fields().get_field_index(this->get_unique_name());
    default:
        //ASSERT(false, "Only fields of static/virtual/instancemethod/instance storage classes have a storage index: " << *this);
        return -1;
    }
}

int TxField::get_instance_field_index() const {
    ASSERT(this->get_storage() == TXS_INSTANCE, "Only fields of instance storage class have an instance field index: " << *this);
    auto typeDecl = this->get_outer_type_decl();
    ASSERT(typeDecl, "Field's scope is not a type: " << *this->get_symbol()->get_outer());
    auto type = typeDecl->get_definer()->get_type();  // assumes already resolved
    return type->get_instance_fields().get_field_index(this->get_unique_name());
}

int TxField::get_virtual_field_index() const {
    ASSERT(this->get_storage() == TXS_VIRTUAL || this->get_storage() == TXS_INSTANCEMETHOD,
           "Only fields of static virtual storage class have an virtual field index: " << *this);
    auto typeDecl = this->get_outer_type_decl();
    ASSERT(typeDecl, "Field's scope is not a type: " << *this->get_symbol()->get_outer());
    auto type = typeDecl->get_definer()->get_type();  // assumes already resolved
    ASSERT(! (this->get_decl_flags() & TXD_CONSTRUCTOR), "constructor does not have an instance method index: " << this);
    return type->get_virtual_fields().get_field_index(this->get_unique_name());
}

int TxField::get_static_field_index() const {
    ASSERT(this->get_storage() == TXS_STATIC, "Only fields of static non-virtual storage class have a static field index: " << *this);
    auto typeDecl = this->get_outer_type_decl();
    ASSERT(typeDecl, "Field's scope is not a type: " << *this->get_symbol()->get_outer());
    auto type = typeDecl->get_definer()->get_type();  // assumes already resolved
    return type->get_static_fields().get_field_index(this->get_unique_name());
}


bool TxField::is_statically_constant() const {
    bool genericParamBinding = (this->get_unique_name().find_last_of('#') != std::string::npos);
    if ( (this->get_decl_flags() & TXD_GENPARAM) || genericParamBinding )
        // (The second condition might be removable in future, but now needed to avoid expecting e.g.
        // tx#Array#L to be statically constant)
        return false;
    if ( this->get_storage() == TXS_GLOBAL )
        return true;
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

/*
TxSymbolScope* TxEntity::resolve_generic(TxSymbolScope* vantageScope) {
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
                        fieldType = fieldType->get_semantic_base_type();
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
*/
