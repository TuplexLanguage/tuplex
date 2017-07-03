#include "entity.hpp"
#include "entity_type.hpp"
#include "ast/expr/ast_expr_node.hpp"
#include "qual_type.hpp"

Logger& TxEntity::_LOG = Logger::get( "ENTITY" );

const TxNode* TxEntity::get_origin_node() const {
    ASSERT( this->declaration, "NULL declaration in " << this );
    return this->declaration->get_definer();
}

TxField* TxField::make_field( const TxFieldDeclaration* fieldDeclaration, const TxQualType* fieldType ) {
    ASSERT( fieldDeclaration, "Fields must be named (have non-null declaration)" );
    ASSERT( fieldType, "NULL type for field " << fieldDeclaration );
    auto symbol = fieldDeclaration->get_symbol();
    if ( fieldType->get_type_class() == TXTC_FUNCTION ) {
        auto funcArgTypes = fieldType->type()->argument_types();
        // check that no two signatures are exactly equal
        for ( auto prevFieldDeclI = symbol->fields_cbegin(); prevFieldDeclI != symbol->fields_cend(); prevFieldDeclI++ ) {
            if ( ( *prevFieldDeclI ) == fieldDeclaration )
                continue;
            // we only check against the previous fields that have already been resolved at this point:
            if ( auto prevFieldType = ( *prevFieldDeclI )->get_definer()->attempt_qualtype() ) {
                if ( prevFieldType->get_type_class() == TXTC_FUNCTION ) {
                    auto prevFuncArgTypes = prevFieldType->type()->argument_types();
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

//int TxField::get_decl_storage_index() const {
//    if ( auto typeDecl = this->get_outer_type_decl() ) {
//        if ( auto outerType = typeDecl->get_definer()->get_type() ) {  // assumes already resolved
//            switch ( this->get_storage() ) {
//            case TXS_STATIC:
//                return outerType->type()->get_static_fields().get_field_index( this->get_unique_name() );
//            case TXS_VIRTUAL:
//            case TXS_INSTANCEMETHOD:
//                ASSERT( !( this->get_decl_flags() & TXD_CONSTRUCTOR ), "constructor does not have an instance method index: " << this );
//                return outerType->type()->get_virtual_fields().get_field_index( this->get_unique_name() );
//            case TXS_INSTANCE:
//                return outerType->type()->get_instance_fields().get_field_index( this->get_unique_name() );
//            default:
//                //ASSERT(false, "Only fields of static/virtual/instancemethod/instance storage classes have a storage index: " << *this);
//                return -1;
//            }
//        }
//    }
//    return -1;
//}

bool TxField::is_modifiable() const {
    return this->get_type()->is_modifiable();
}
