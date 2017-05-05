#include "entity.hpp"

#include "ast.hpp"

Logger& TxEntity::_LOG = Logger::get( "ENTITY" );

const TxLocation& TxEntity::get_parse_location() const {
    ASSERT( this->declaration, "NULL declaration in " << this );
    return this->declaration->get_definer()->get_parse_location();
}

ExpectedErrorClause* TxEntity::exp_err_ctx() const {
    ASSERT( this->declaration, "NULL declaration in " << this );
    return this->declaration->get_definer()->exp_err_ctx();
}

TxField* TxField::make_field( const TxFieldDeclaration* fieldDeclaration, const TxType* fieldType ) {
    ASSERT( fieldDeclaration, "Fields must be named (have non-null declaration)" );
    ASSERT( fieldType, "NULL type for field " << fieldDeclaration );
    auto symbol = fieldDeclaration->get_symbol();
    if ( fieldType->get_type_class() == TXTC_FUNCTION ) {
        auto funcArgTypes = fieldType->argument_types();
        // check that no two signatures are exactly equal
        for ( auto prevFieldDeclI = symbol->fields_cbegin(); prevFieldDeclI != symbol->fields_cend(); prevFieldDeclI++ ) {
            if ( ( *prevFieldDeclI ) == fieldDeclaration )
                continue;
            // we only check against the previous fields that have already been resolved at this point:
            if ( auto prevFieldType = ( *prevFieldDeclI )->get_definer()->attempt_get_type() ) {
                if ( prevFieldType->get_type_class() == TXTC_FUNCTION ) {
                    auto prevFuncArgTypes = prevFieldType->argument_types();
                    if ( funcArgTypes.size() == prevFuncArgTypes.size()
                         && equal( funcArgTypes.begin(), funcArgTypes.end(), prevFuncArgTypes.begin(),
                                   [](const TxType* t1, const TxType* t2) {return *t1 == *t2;} ) ) {
                        CERR_THROWRES( fieldDeclaration->get_definer(), "Can't overload two functions with identical argument types:\n\t"
                                       << symbol->get_full_name() << ": " << fieldType );
                        return nullptr;
                    }
                }
                else {
                    CERR_THROWRES( fieldDeclaration->get_definer(),
                                   "Can't overload symbol that has a non-function " << (*prevFieldDeclI) << " of type "
                                   << prevFieldType );
                    return nullptr;
                }
            }
        }
    }
    else if ( symbol->field_count() > 1 ) {
        CERR_THROWRES( fieldDeclaration->get_definer(), "Can't overload symbol with non-function " << fieldDeclaration << " of type " << fieldType );
        return nullptr;
    }

    return new TxField( fieldDeclaration, fieldType );
}

int TxField::get_decl_storage_index() const {
    if ( auto typeDecl = this->get_outer_type_decl() ) {
        if ( auto outerType = typeDecl->get_definer()->get_type() ) {  // assumes already resolved
            switch ( this->get_storage() ) {
            case TXS_STATIC:
                return outerType->type()->get_static_fields().get_field_index( this->get_unique_name() );
            case TXS_VIRTUAL:
                case TXS_INSTANCEMETHOD:
                ASSERT( !( this->get_decl_flags() & TXD_CONSTRUCTOR ), "constructor does not have an instance method index: " << this );
                return outerType->type()->get_virtual_fields().get_field_index( this->get_unique_name() );
            case TXS_INSTANCE:
                return outerType->type()->get_instance_fields().get_field_index( this->get_unique_name() );
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
    if ( auto initExpr = this->get_declaration()->get_definer()->get_init_expression() )
        return ( !this->get_type()->is_modifiable() && initExpr->is_statically_constant() );
    return false;
}

const TxConstantProxy* TxField::get_static_constant_proxy() const {
    if ( this->is_statically_constant() )
        if ( auto initExpr = this->get_declaration()->get_definer()->get_init_expression() )
            return initExpr->get_static_constant_proxy();
    return nullptr;
}

bool TxField::is_modifiable() const {
    return this->get_type()->is_modifiable();
}
