#include "ast_fielddef_node.hpp"
#include "ast_wrappers.hpp"
#include "type/ast_types.hpp"
#include "ast_declpass.hpp"

#include "symbol/entity_type.hpp"

static const TxType* get_mutable_specialization( TxNode* parent, const TxType* origType ) {
    const TxLocation& loc = parent->parseLocation;
    auto newbindings = new std::vector<TxTypeArgumentNode*>();
    auto actType = origType->type();
    for ( auto bdecl : actType->get_bindings() ) {
        if ( auto btypedecl = dynamic_cast<const TxTypeDeclaration*>( bdecl ) ) {
            auto btype = btypedecl->get_definer()->resolve_type();
            if ( btype->is_modifiable() ) {
                auto newbind = new TxTypeDeclWrapperNode( loc, btypedecl );
                newbindings->push_back( new TxTypeTypeArgumentNode( newbind ) );
            }
            else {
                auto newbind = new TxModifiableTypeNode( loc, new TxTypeDeclWrapperNode( loc, btypedecl ) );
                newbindings->push_back( new TxTypeTypeArgumentNode( newbind ) );
            }
        }
        else {
            auto bvaluedecl = dynamic_cast<const TxFieldDeclaration*>( bdecl );
            auto previnitexpr = bvaluedecl->get_definer()->get_init_expression();
            if ( auto convinitexpr = dynamic_cast<TxMaybeConversionNode*>( previnitexpr ) )
                previnitexpr = convinitexpr->originalExpr;
            auto newbind = new TxExprWrapperNode( previnitexpr );
            newbindings->push_back( new TxValueTypeArgumentNode( newbind ) );
        }
    }
    auto genBaseTypeNode = new TxTypeDeclWrapperNode( loc, actType->get_semantic_base_type()->get_declaration() );
    auto mutTypeDef = new TxGenSpecTypeNode( loc, genBaseTypeNode, newbindings );
    run_declaration_pass( mutTypeDef, parent, "mut-type" );
    const TxType* type = mutTypeDef->resolve_type();
    LOG_DEBUG( parent->LOGGER(), "Created mutable specialization for " << parent );
    return type;
}

const TxType* TxFieldDefNode::define_type() {
    LOG_TRACE( this->LOGGER(), "defining  type  of " << this );
    const TxType* type;
    if ( this->typeExpression ) {
        type = this->typeExpression->resolve_type();
        // also resolve initExpression from here, which guards against recursive field value initialization:
        if ( this->initExpression ) {
            auto nonModType = ( type->is_modifiable() ? type->get_base_type() : type );  // rvalue doesn't need to be modifiable
            this->initExpression->insert_conversion( nonModType );
            try {
                this->initExpression->resolve_type();
            }
            catch ( const resolution_error& err ) {
                // by catching here, we allow the field to be created without initializer, reducing follow-on errors
                LOG_DEBUG(this->LOGGER(), "Caught field initializer expression resolution error in " << this << ": " << err);
            }
        }
    }
    else {
        type = this->initExpression->resolve_type();
        if ( this->modifiable ) {
            if ( !type->is_modifiable() ) {
                if ( !type->is_mutable() ) {
                    if ( type->is_generic_specialization() && type->get_semantic_base_type()->is_mutable() ) {
                        // copying an immutable type to a modifiable field is ok if we can obtain the mutable specialization
                        // corresponding to the source's immutable specialization
                        type = get_mutable_specialization( this, type );
                    }
                    else
                        CERR_THROWRES( this, "Can't use immutable type as modifiable: " << type );
                }
                type = this->registry().get_modifiable_type( nullptr, type );
            }
        }
        else if ( type->is_modifiable() )
            // if initialization expression is modifiable type, and modifiable not explicitly specified,
            // lose modifiable attribute (modifiability must be explicit)
            type = type->get_base_type();
    }
    return type;
}

const TxField* TxFieldDefNode::define_field() {
    LOG_TRACE( this->LOGGER(), "defining  field of " << this );
    // FUTURE: consider if EXPERR decls shouldn't get their field created
    return TxField::make_field( this->declaration, this->attempt_get_type() );
}

void TxFieldDefNode::symbol_resolution_pass() {
    auto field = this->resolve_field();
    if ( this->initExpression ) {
        if ( this->typeExpression ) {
            this->typeExpression->symbol_resolution_pass();
        }
        this->initExpression->symbol_resolution_pass();
    }
    else {  // if initExpression is null then typeExpression is set
        this->typeExpression->symbol_resolution_pass();
    }

    if ( !field->get_type()->is_concrete() ) {
        if ( !this->context().is_generic() )
            CERROR( this, "Field type is not concrete: "
                    << this->get_identifier() << " : " << field->get_type() );
        else
            LOG_DEBUG( this->LOGGER(), "(Not error since generic context) Field type is not concrete: "
                       << this->get_identifier() << " : " << field->get_type() );
    }
    if ( this->get_declaration()->get_decl_flags() & TXD_CONSTRUCTOR ) {
        // TODO: check that constructor function type has void return value
    }
}
