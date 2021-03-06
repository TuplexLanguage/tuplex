#include "ast_fielddef_node.hpp"

#include "ast_wrappers.hpp"
#include "ast_declpass.hpp"
#include "expr/ast_constexpr.hpp"
#include "expr/ast_lambda_node.hpp"
#include "type/ast_types.hpp"

#include "symbol/package.hpp"


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
            this->_type = this->define_type( TXR_FULL_RESOLUTION );
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


//static const TxActualType* make_mutable_specialization( TxPassInfo typeResLevel, TxNode* origin, const TxActualType* origType,
//                                                        const TxActualType* newSemBase=nullptr ) {
//    const TxLocation& loc = origin->ploc;
//    auto newbindings = new std::vector<TxTypeArgumentNode*>();
//    auto actType = origType;
//    for ( auto bdecl : actType->get_bindings() ) {
//        if ( auto btypedecl = dynamic_cast<const TxTypeDeclaration*>( bdecl ) ) {
////            auto btype = btypedecl->get_definer()->resolve_type( typeResLevel );
////            if ( btype.is_modifiable() ) {
////                auto newbind = new TxTypeDeclWrapperNode( loc, btypedecl );
////                newbindings->push_back( new TxTypeArgumentNode( newbind ) );
////            }
////            else
//            {
//                auto newbind = new TxModifiableTypeNode( loc, new TxTypeDeclWrapperNode( loc, btypedecl ) );
//                newbindings->push_back( new TxTypeArgumentNode( newbind ) );
//            }
//        }
//        else {
//            auto bvaluedecl = dynamic_cast<const TxFieldDeclaration*>( bdecl );
//            auto previnitexpr = bvaluedecl->get_definer()->get_init_expression();
//            if ( auto convinitexpr = dynamic_cast<TxMaybeConversionNode*>( previnitexpr ) )
//                previnitexpr = convinitexpr->originalExpr;
//            auto newbind = new TxExprWrapperNode( previnitexpr );
//            newbindings->push_back( new TxTypeArgumentNode( newbind ) );
//        }
//    }
//    auto genBaseTypeDecl = ( newSemBase ? newSemBase->get_declaration() : actType->get_semantic_base_type()->get_declaration() );
//    auto genBaseTypeNode = new TxTypeDeclWrapperNode( loc, genBaseTypeDecl );
//    auto mutTypeDef = new TxGenSpecTypeNode( loc, genBaseTypeNode, newbindings );
//    mutTypeDef->set_requires_mutable( true );
//    run_declaration_pass( mutTypeDef, origin, "mut-type" );
//    auto type = mutTypeDef->resolve_type( typeResLevel ).type();
//    LOG_DEBUG( origin->LOGGER(), "Created mutable specialization for " << origin << ": " << type );
//    return type;
//}
//
//static const TxActualType* get_mutable_specialization( TxPassInfo typeResLevel, TxNode* origin, const TxActualType* type ) {
//    if ( !type->is_generic_specialization() )
//        CERR_THROWRES( origin, "Can't specialize mutable type from base type: " << type );;
//    if ( type->get_semantic_base_type()->is_mutable() )
//        return make_mutable_specialization( typeResLevel, origin, type );
//    else {
//        auto newSemBase = get_mutable_specialization( typeResLevel, origin, type->get_semantic_base_type() );
//        return make_mutable_specialization( typeResLevel, origin, type, newSemBase );
//    }
//}

TxQualType TxFieldDefiningNode::define_type( TxTypeResLevel typeResLevel ) {
    LOG_TRACE( this->LOGGER(), "defining  type  of " << this );
    TxQualType qtype;
    if ( this->typeExpression ) {
        qtype = this->typeExpression->resolve_type( typeResLevel );
        // also resolve initExpression from here, which guards against recursive field value initialization:
        if ( this->initExpression ) {
            if ( qtype->get_type_class() == TXTC_ARRAY ) {
                // This implementation relaxes the auto-conversion check to allow assignment of arrays with unknown C.
                // We only handle auto-dereferencing of the rvalue:
                auto rtype = this->initExpression->originalExpr->resolve_type( typeResLevel );
                if ( rtype->get_type_class() == TXTC_REFERENCE )
                    this->initExpression->insert_qual_conversion( typeResLevel, rtype->target_type() );
                else
                    this->initExpression->resolve_type( typeResLevel );
            }
            else {
                if ( is_not_properly_concrete( this, qtype ) ) {
                    // duplicated this check here from verification pass in order to provide better error messaging
                    CERR_THROWRES( this, "Field type is not concrete: " << this->get_descriptor() << " : " << qtype );
                }
                this->initExpression->insert_conversion( typeResLevel, qtype.type(), this->_explicit );
            }
        }
    }
    else {
        qtype = this->initExpression->resolve_type( typeResLevel );
        if ( !dynamic_cast<TxModifiableValueNode*>( this->initExpression->originalExpr )
             && !dynamic_cast<TxStackConstructionNode*>( this->initExpression->originalExpr ) ) {
            if ( qtype.is_modifiable() )
                // if initialization expression is modifiable type, and modifiable not explicitly specified,
                // lose modifiable attribute (modifiability must be explicit)
                qtype = TxQualType( qtype.type(), false );
        }
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

    if ( type->get_type_class() == TXTC_ARRAY ) {
        // special check since we allow assignment between arrays of different capacities (i.e. sizes)
        verify_array_assignment( this, type.type(), ( this->initExpression ? this->initExpression->qtype().type() : nullptr ) );
    }
    else if ( is_not_properly_concrete( this, type ) ) {
        CERROR( this, "Field type is not concrete: " << this->get_descriptor() << " : " << this->field()->qtype() );
    }

    if ( this->get_declaration()->get_decl_flags() & TXD_CONSTRUCTOR ) {
        // TODO: check that constructor function type has void return value
    }

    if ( this->typeExpression && this->initExpression ) {
        if ( !type.is_modifiable() && dynamic_cast<TxModifiableValueNode*>( this->initExpression->originalExpr ) ) {
            CWARNING( this->initExpression->originalExpr, "Field type is non-modifiable, explicit value modifiabily modifier is ignored" );
        }
    }
}



void verify_array_assignment( const TxNode* origin, const TxActualType* ltype, const TxActualType* rtype ) {
    if ( ltype->is_type_generic() ) {
        if ( !origin->context().is_type_generic() && !ltype->is_generic_param() )
            CERROR( origin, "Assignee is not a specific array type: " << ltype );
        else
            LOG_DEBUG( origin->LOGGER(), origin << " " << origin->context().scope()
                       << " (Not error since type-generic context) Assignee is not a specific array type: " << ltype );
    }
    else if ( rtype ) {
        if ( rtype->get_type_class() != TXTC_ARRAY
             || !rtype->element_type()->is_assignable_to( *ltype->element_type() ) ) {
            CERROR( origin, "Can't auto-convert value\n\tFrom: " << rtype << "\n\tTo:   " << ltype  );
        }

        // Note: In theory, if this expression is statically constant we could perform the bounds checking in resolution pass.
        // However accessing the cogegen'd value of Array.L isn't guaranteed before the type preparation has been run.
        if ( auto lCapExpr = ltype->capacity() ) {
            if ( lCapExpr->is_statically_constant() ) {
                auto lArrayCap = eval_unsigned_int_constant( lCapExpr );  // capacity is statically known

                if ( auto rCapExpr = rtype->capacity() ) {
                    if ( rCapExpr->is_statically_constant() ) {
                        auto rArrayCap = eval_unsigned_int_constant( rCapExpr );  // capacity is statically known

                        if ( lArrayCap < rArrayCap )
                            CWARNING( origin, "Array assignee has lower capacity than assigned value: " << lArrayCap << " < " << rArrayCap );
                    }
                }
            }
        }
    }
}



void TxLocalFieldDefNode::declare_field( const TxNode* declarer, TxScopeSymbol* scope, TxDeclarationFlags declFlags, TxFieldStorage storage ) {
    this->declaration = scope->declare_field( this->fieldName->ident(), declarer, this, declFlags, storage );
}


void TxNonLocalFieldDefNode::declare_field( const TxNode* declarer, TxScopeSymbol* scope, TxDeclarationFlags declFlags, TxFieldStorage storage ) {
    std::string declName = this->fieldName->ident();
    if ( declName == "self" ) {
        // handle constructor declaration
        if ( storage != TXS_INSTANCEMETHOD )
            CERROR( this, "Illegal declaration name for non-constructor member: " << declName );
        declName = CONSTR_IDENT;
        declFlags = declFlags | TXD_CONSTRUCTOR;
    }
    else if ( declName == CONSTR_IDENT ) {  // built-in
        ASSERT( declFlags & ( TXD_BUILTIN | TXD_IMPLICIT ), "Built-in or Implicit flag not set: " << declFlags << " at " << this );
        if ( declFlags & TXD_INITIALIZER ) {
            ASSERT( storage == TXS_STATIC,
                    "Initializer not a static field: " << storage << " at " << this );
        }
        else {
            ASSERT( declFlags & TXD_CONSTRUCTOR, "Constructor flag not set: " << declFlags << " at " << this );
            ASSERT( storage == TXS_INSTANCEMETHOD,
                    "Constructor not an instance method: " << storage << " at " << this );
        }
    }

    // Note: Field is processed in the 'outer' scope and not in the 'inner' scope of its declaration.
    this->declaration = scope->declare_field( declName, declarer, this, declFlags, storage );

    // handle main() function declarations:
    if ( declName == "main" ) {
        if ( dynamic_cast<TxLambdaExprNode*>( this->initExpression->originalExpr ) ) {
            scope->get_root_scope()->register_main_func( this->declaration );
        }
        // non-function symbols declared with the name 'main' are allowed
    }
}
