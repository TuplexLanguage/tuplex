#include "ast_fielddef_node.hpp"

#include "ast_wrappers.hpp"
#include "ast_declpass.hpp"
#include "type/ast_types.hpp"


const TxField* TxFieldDefiningNode::resolve_field() {
    ASSERT( this->is_context_set(), "Declaration pass has not been run (lexctx not set) before resolving " << this );
    if ( !this->_field ) {
        if ( this->hasResolved ) {
            throw resolution_error( this, "Previous field resolution failed in " + this->str() );
        }
        LOG_TRACE( this->LOGGER(), "resolving field of " << this );

        if ( this->startedRslv ) {
            CERR_THROWRES( this, "Recursive definition of field '" << this->get_descriptor() << "'" );
        }
        this->startedRslv = true;
        try {
            this->_type = this->define_type( TXP_RESOLUTION );
            this->_field = this->define_field();
        }
        catch ( const resolution_error& err ) {
            this->hasResolved = true;
            //LOG(this->LOGGER(), DEBUG, "Caught and re-threw resolution error in " << this << ": " << err);
            throw;
        }
        ASSERT( this->_type && this->_field, "NULL-resolved type/field but no exception thrown in " << this );
        this->hasResolved = true;
    }
    return this->_field;
}

bool TxFieldDefiningNode::is_statically_constant() const {
    return this->get_init_expression() && this->get_init_expression()->is_statically_constant();
}


static const TxActualType* make_mutable_specialization( TxPassInfo passInfo, TxNode* origin, const TxActualType* origType,
                                                        const TxActualType* newSemBase=nullptr ) {
    const TxLocation& loc = origin->ploc;
    auto newbindings = new std::vector<TxTypeArgumentNode*>();
    auto actType = origType;
    for ( auto bdecl : actType->get_bindings() ) {
        if ( auto btypedecl = dynamic_cast<const TxTypeDeclaration*>( bdecl ) ) {
//            auto btype = btypedecl->get_definer()->resolve_type( passInfo );
//            if ( btype.is_modifiable() ) {
//                auto newbind = new TxTypeDeclWrapperNode( loc, btypedecl );
//                newbindings->push_back( new TxTypeTypeArgumentNode( newbind ) );
//            }
//            else
            {
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
    auto genBaseTypeDecl = ( newSemBase ? newSemBase->get_declaration() : actType->get_semantic_base_type()->get_declaration() );
    auto genBaseTypeNode = new TxTypeDeclWrapperNode( loc, genBaseTypeDecl );
    auto mutTypeDef = new TxGenSpecTypeNode( loc, genBaseTypeNode, newbindings );
    mutTypeDef->set_requires_mutable( true );
    run_declaration_pass( mutTypeDef, origin, "mut-type" );
    auto type = mutTypeDef->resolve_type( passInfo ).type();
    LOG_DEBUG( origin->LOGGER(), "Created mutable specialization for " << origin << ": " << type );
    return type;
}

static const TxActualType* get_mutable_specialization( TxPassInfo passInfo, TxNode* origin, const TxActualType* type ) {
    if ( !type->is_generic_specialization() )
        CERR_THROWRES( origin, "Can't specialize mutable type from base type: " << type );;
    if ( type->get_semantic_base_type()->is_mutable() )
        return make_mutable_specialization( passInfo, origin, type );
    else {
        auto newSemBase = get_mutable_specialization( passInfo, origin, type->get_semantic_base_type() );
        return make_mutable_specialization( passInfo, origin, type, newSemBase );
    }
}

TxQualType TxFieldDefiningNode::define_type( TxPassInfo passInfo ) {
    LOG_TRACE( this->LOGGER(), "defining  type  of " << this );
    TxQualType qtype;
    if ( this->typeExpression ) {
        qtype = this->typeExpression->resolve_type( passInfo );
        // also resolve initExpression from here, which guards against recursive field value initialization:
        if ( this->initExpression ) {
            this->initExpression->insert_conversion( passInfo, qtype.type(), this->_explicit );
        }
    }
    else {
        qtype = this->initExpression->resolve_type( passInfo );
        if ( this->modifiable ) {
            if ( !qtype.is_modifiable() ) {
                auto typeEnt = qtype.type();
                if ( !typeEnt->is_mutable() ) {
                    if ( typeEnt->is_generic_specialization() && typeEnt->get_source_base_type()->is_mutable() ) {
                        // copying an immutable type to a modifiable field is ok if we can obtain the mutable specialization
                        // corresponding to the source's immutable specialization
                        qtype = TxQualType( get_mutable_specialization( passInfo, this->initExpression, typeEnt ), true );
                        //std::cerr << "Made mutable specialization from " << typeEnt << "  to " << qtype->type() << std::endl;
                    }
                    else
                        CERR_THROWRES( this, "Can't use immutable type as modifiable: " << qtype );
                }
                else
                    qtype = TxQualType( typeEnt, true );
            }
        }
        else if ( qtype.is_modifiable() )
            // if initialization expression is modifiable type, and modifiable not explicitly specified,
            // lose modifiable attribute (modifiability must be explicit)
            qtype = TxQualType( qtype.type(), false );
    }
    return qtype;
}

const TxField* TxFieldDefiningNode::define_field() {
    LOG_TRACE( this->LOGGER(), "defining  field of " << this );
    // FUTURE: consider if EXPERR decls shouldn't get their field created
    return TxField::make_field( this->declaration, this->attempt_qtype() );
}

void TxFieldDefiningNode::verification_pass() const {
    auto type = this->attempt_qtype();
    if ( !type ) {
        LOG_TRACE( this->LOGGER(), "Skipping verification for " << this << " due to earlier resolution failure" );
        return;
    }
    if ( is_not_properly_concrete( this, type ) ) {
        CERROR( this, "Field type is not concrete: " << this->get_descriptor() << " : " << this->field()->qtype() );
    }
    if ( this->get_declaration()->get_decl_flags() & TXD_CONSTRUCTOR ) {
        // TODO: check that constructor function type has void return value
    }
}
