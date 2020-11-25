#include "entity.hpp"
#include "type_base.hpp"
#include "ast/ast_fielddef_node.hpp"
#include "ast/expr/ast_expr_node.hpp"

Logger& TxEntity::_LOG = Logger::get( "ENTITY" );

const TxNode* TxEntity::get_origin_node() const {
    ASSERT( this->declaration, "NULL declaration in " << this );
    return this->declaration->get_definer();
}

static bool equal_signature( const TxActualType* funcTypeA, const TxActualType* funcTypeB ) {
    if ( funcTypeA->modifiable_closure() != funcTypeB->modifiable_closure() )
        return false;
    auto & argTypesA = funcTypeA->argument_types();
    auto & argTypesB = funcTypeB->argument_types();
    if ( argTypesA.size() != argTypesB.size() )
        return false;
    return equal( argTypesA.begin(), argTypesA.end(), argTypesB.begin(),
                  [](const TxActualType* t1, const TxActualType* t2) { return *t1 == *t2; } );
}

TxField* TxField::make_field( const TxFieldDeclaration* fieldDeclaration, TxQualType fieldQType ) {
    ASSERT( fieldDeclaration, "Fields must be named (have non-null declaration)" );
    ASSERT( fieldQType, "NULL type for field " << fieldDeclaration );
    auto symbol = fieldDeclaration->get_symbol();
    if ( fieldQType->is_initialized() && fieldQType->get_type_class() == TXTC_FUNCTION ) {
        // check that no two signatures are exactly equal
        for ( auto prevFieldDeclI = symbol->fields_cbegin(); prevFieldDeclI != symbol->fields_cend(); prevFieldDeclI++ ) {
            if ( ( *prevFieldDeclI ) == fieldDeclaration )
                continue;
            // we only check against the previous fields that have already been resolved at this point:
            if ( auto prevFieldQType = ( *prevFieldDeclI )->get_definer()->attempt_qtype() ) {
                if ( prevFieldQType->get_type_class() == TXTC_FUNCTION ) {
                    if ( equal_signature( fieldQType.type(), prevFieldQType.type() ) ) {
                        CERR_THROWRES( fieldDeclaration->get_definer(), "Can't overload two functions of identical types:\n\t"
                                       << symbol->get_full_name() << ": " << fieldQType );
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
        CERR_THROWRES( fieldDeclaration->get_definer(), "Can't overload symbol with non-function " << fieldDeclaration << " of type " << fieldQType );
        return nullptr;
    }

    return new TxField( fieldDeclaration, fieldQType );
}

llvm::Value* TxField::code_gen_field_decl( LlvmGenerationContext& context ) const {
    return static_cast<TxFieldDefiningNode*>( this->get_declaration()->get_definer() )->code_gen_field_decl( context );
}
