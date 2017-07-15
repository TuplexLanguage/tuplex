#include "ast_fielddef_node.hpp"

#include "../symbol/qual_type.hpp"
#include "ast_wrappers.hpp"
#include "type/ast_types.hpp"
#include "ast_declpass.hpp"


static const TxQualType* make_mutable_specialization( TxNode* node, const TxType* origType, const TxType* newSemBase=nullptr ) {
    const TxLocation& loc = node->ploc;
    auto newbindings = new std::vector<TxTypeArgumentNode*>();
    auto actType = origType->acttype();
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
    auto genBaseTypeDecl = ( newSemBase ? newSemBase->acttype()->get_declaration() : actType->get_semantic_base_type()->get_declaration() );
    auto genBaseTypeNode = new TxTypeDeclWrapperNode( loc, genBaseTypeDecl );
    auto mutTypeDef = new TxGenSpecTypeNode( loc, genBaseTypeNode, newbindings );
    run_declaration_pass( mutTypeDef, node, "mut-type" );
    const TxQualType* qtype = mutTypeDef->resolve_type();
    LOG_DEBUG( node->LOGGER(), "Created mutable specialization for " << node << ": " << qtype );
    return qtype;
}

static const TxQualType* get_mutable_specialization( TxNode* node, const TxType* type ) {
    if ( !type->is_generic_specialization() )
        CERR_THROWRES( node, "Can't specialize mutable type from base type: " << type );;
    if ( type->get_semantic_base_type()->is_mutable() )
        return make_mutable_specialization( node, type );
    else {
        auto newSemBase = get_mutable_specialization( node, type->get_semantic_base_type() );
        return make_mutable_specialization( node, type, newSemBase->type() );
    }
}

const TxQualType* TxFieldDefNode::define_type() {
    LOG_TRACE( this->LOGGER(), "defining  type  of " << this );
    const TxQualType* qtype;
    if ( this->typeExpression ) {
        qtype = this->typeExpression->resolve_type();
        // also resolve initExpression from here, which guards against recursive field value initialization:
        if ( this->initExpression ) {
            this->initExpression->insert_conversion( qtype->type() );
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
        qtype = this->initExpression->resolve_type();
        if ( this->modifiable ) {
            if ( !qtype->is_modifiable() ) {
                auto typeEnt = qtype->type();
                if ( !typeEnt->is_mutable() ) {
                    if ( typeEnt->is_generic_specialization() && typeEnt->get_source_base_type()->is_mutable() ) {
                        // copying an immutable type to a modifiable field is ok if we can obtain the mutable specialization
                        // corresponding to the source's immutable specialization
                        qtype = new TxQualType( get_mutable_specialization( this, typeEnt )->type(), true );
                        //std::cerr << "Made mutable specialization from " << typeEnt << "  to " << qtype->type()->acttype() << std::endl;
                    }
                    else
                        CERR_THROWRES( this, "Can't use immutable type as modifiable: " << qtype );
                }
                else
                    qtype = new TxQualType( typeEnt, true );
            }
        }
        else if ( qtype->is_modifiable() )
            // if initialization expression is modifiable type, and modifiable not explicitly specified,
            // lose modifiable attribute (modifiability must be explicit)
            qtype = new TxQualType( qtype->type(), false );
    }
    return qtype;
}

const TxField* TxFieldDefNode::define_field() {
    LOG_TRACE( this->LOGGER(), "defining  field of " << this );
    // FUTURE: consider if EXPERR decls shouldn't get their field created
    return TxField::make_field( this->declaration, this->attempt_qualtype() );
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

    if ( is_not_properly_concrete( this, field->qualtype()->type() ) ) {
        CERROR( this, "Field type is not concrete: " << this->get_descriptor() << " : " << field->qualtype() );
    }
    if ( this->get_declaration()->get_decl_flags() & TXD_CONSTRUCTOR ) {
        // TODO: check that constructor function type has void return value
    }
}
