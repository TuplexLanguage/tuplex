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
            if ( qtype->get_type_class() == TXTC_ARRAY ) {
                // This implementation relaxes the auto-conversion check to allow assignment of arrays with unknown C.
                // We only handle auto-dereferencing of the rvalue:
                auto rtype = this->initExpression->originalExpr->resolve_type( TXP_RESOLUTION );
                if ( rtype->get_type_class() == TXTC_REFERENCE )
                    this->initExpression->insert_conversion( TXP_RESOLUTION, rtype->target_type() );
                else
                    this->initExpression->resolve_type( TXP_RESOLUTION );
            }
            else {
                this->initExpression->insert_conversion( passInfo, qtype.type(), this->_explicit );
            }
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



void TxLocalFieldDefNode::declare_field( TxScopeSymbol* scope, TxDeclarationFlags declFlags, TxFieldStorage storage ) {
    this->declaration = scope->declare_field( this->fieldName->ident(), this, declFlags, storage, TxIdentifier() );
}


void TxNonLocalFieldDefNode::declare_field( TxScopeSymbol* scope, TxDeclarationFlags declFlags, TxFieldStorage storage ) {
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
    this->declaration = scope->declare_field( declName, this, declFlags, storage, TxIdentifier() );
}

bool TxNonLocalFieldDefNode::is_main_signature_valid( const TxActualType* funcType ) const {
    auto retType = funcType->return_type();
    bool retOk = bool( retType->get_type_class() == TXTC_VOID
                       || retType->is_a( *this->context().package()->registry().get_builtin_type( TXBT_INTEGER ) ) );
    if ( !retOk )
        CERROR( this, "main() method has non-integer return type (must be void or integer): " << retType );

    auto & argTypes = funcType->argument_types();
    if ( argTypes.size() == 0 )
        return retOk;
    else if ( argTypes.size() == 1 ) {
        const TxActualType* argsType = argTypes.at( 0 );
        if ( argsType->get_type_class() == TXTC_REFERENCE ) {
            auto targetType = argsType->target_type();
            if ( targetType->get_type_class() == TXTC_ARRAY ) {
                auto elemType = targetType->element_type();
                if ( elemType->get_type_class() == TXTC_REFERENCE ) {
                    auto elemTargetType = elemType->target_type();
                    if ( elemTargetType->get_type_class() == TXTC_ARRAY ) {
                        auto elemTargetElemType = elemTargetType->element_type();
                        if ( elemTargetElemType->is_builtin( TXBT_UBYTE ) )
                            return retOk;
                    }
                }
            }
        }
        CERROR( this, "main() method has invalid argument [required signature is  main()  or  main( &[]&[]UByte )] : " << argsType );
    }
    else
        CERROR( this, "main() method has too many arguments [required signature is  main()  or  main( &[]&[]UByte )]" );
    return false;
}

void TxNonLocalFieldDefNode::resolution_pass() {
    TxFieldDefiningNode::resolution_pass();

    // handle main() function declarations:
    if ( this->fieldName->ident() == "main" ) {
        auto funcField = this->field();
        if ( funcField->qtype()->get_type_class() == TXTC_FUNCTION ) {
            // verify main program function candidate
            if ( !( funcField->get_storage() == TXS_GLOBAL || funcField->get_storage() == TXS_STATIC ) )
                CERROR( this, "main() method must have global or static storage: " << funcField->get_storage() );
            if ( is_main_signature_valid( funcField->qtype().type() ) ) {
                // register main program function candidate
                bool first = this->context().package()->registerMainFunc( this->declaration );
                if ( first ) {
                    ASSERT( dynamic_cast<TxLambdaExprNode*>( this->initExpression->originalExpr ),
                            "Not a lambda expr: " << this->initExpression->originalExpr );
                    static_cast<TxLambdaExprNode*>( this->initExpression->originalExpr )->add_function_attribute(
                            llvm::Attribute::AttrKind::NoInline );
                }
            }
        }
        // non-function symbols declared with the name 'main' are allowed
    }
}
