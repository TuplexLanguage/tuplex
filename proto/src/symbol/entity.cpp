#include "entity.hpp"
#include "type_base.hpp"
#include "ast/ast_fielddef_node.hpp"
#include "ast/expr/ast_expr_node.hpp"

Logger& TxEntity::_LOG = Logger::get( "ENTITY" );

const TxNode* TxEntity::get_origin_node() const {
    ASSERT( this->declaration, "NULL declaration in " << this );
    return this->declaration->get_definer();
}

TxField* TxField::make_field( const TxFieldDeclaration* fieldDeclaration, TxQualType fieldType ) {
    ASSERT( fieldDeclaration, "Fields must be named (have non-null declaration)" );
    ASSERT( fieldType, "NULL type for field " << fieldDeclaration );
    auto symbol = fieldDeclaration->get_symbol();
    if ( fieldType->is_initialized() && fieldType->get_type_class() == TXTC_FUNCTION ) {
        auto & funcArgTypes = fieldType->argument_types();
        // check that no two signatures are exactly equal
        for ( auto prevFieldDeclI = symbol->fields_cbegin(); prevFieldDeclI != symbol->fields_cend(); prevFieldDeclI++ ) {
            if ( ( *prevFieldDeclI ) == fieldDeclaration )
                continue;
            // we only check against the previous fields that have already been resolved at this point:
            if ( auto prevFieldQType = ( *prevFieldDeclI )->get_definer()->attempt_qtype() ) {
                if ( prevFieldQType->get_type_class() == TXTC_FUNCTION ) {
                    auto & prevFuncArgTypes = prevFieldQType->argument_types();
                    if ( funcArgTypes.size() == prevFuncArgTypes.size()
                         && equal( funcArgTypes.begin(), funcArgTypes.end(), prevFuncArgTypes.begin(),
                                   [](const TxActualType* t1, const TxActualType* t2) {return *t1 == *t2;} ) ) {
                        CERR_THROWRES( fieldDeclaration->get_definer(), "Can't overload two functions with identical argument types:\n\t"
                                       << symbol->get_full_name() << ": " << fieldType );
                        return nullptr;
                    }
                }
                else {
                    CERR_THROWRES( fieldDeclaration->get_definer(),
                                   "Can't overload symbol that has a non-function " << (*prevFieldDeclI) << " of type "
                                   << prevFieldQType );
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

llvm::Value* TxField::code_gen_field_decl( LlvmGenerationContext& context ) const {
    return static_cast<TxFieldDefiningNode*>( this->get_declaration()->get_definer() )->code_gen_field_decl( context );
}
