#include "ast_field.hpp"

#include "ast_exprs.hpp"
#include "ast_constexpr.hpp"
#include "ast_ref.hpp"
#include "ast_conv.hpp"

#include "ast/ast_util.hpp"

#include "symbol/qual_type.hpp"

int get_reinterpretation_degree( TxExpressionNode* originalExpr, const TxType *requiredType ) {
    const TxType* originalType = originalExpr->resolve_type()->type();

    if ( *originalType == *requiredType ) {
        //std::cerr << "Types equal: " << originalType << "   ==   " << requiredType << std::endl;
        return 0;
    }

    // TODO: check if provided type is narrower than the expected type

    if ( auto_converts_to( originalExpr, requiredType ) )
        return 2;

    if ( requiredType->get_type_class() == TXTC_REFERENCE ) {
        if ( auto expRefTargetType = requiredType->target_type() ) {
            if ( originalType->is_a( *expRefTargetType->type() ) ) {
                if ( !expRefTargetType->is_modifiable() )
                    return 3;  // expression will be auto-wrapped with a reference-to node
            }
        }
    }

    if ( originalType->get_type_class() == TXTC_REFERENCE ) {
        if ( auto provRefTargetType = originalType->target_type() ) {
            if ( provRefTargetType->type()->auto_converts_to( *requiredType ) ) {
                return 3;  // expression will be wrapped with a dereference node
            }
        }
    }

    return -1;  // does not match
}

const TxFieldDeclaration* resolve_field( const TxExpressionNode* origin, TxEntitySymbol* entitySymbol,
                                         const std::vector<TxExpressionNode*>* arguments ) {
    if ( !arguments ) {
        if ( entitySymbol->field_count() == 1 )
            return entitySymbol->get_first_field_decl();
        if ( entitySymbol->field_count() > 1 )
            LOG_DEBUG( origin->LOGGER(), entitySymbol << " must be resolved using type parameters but none provided from " << origin );
        return nullptr;
    }

    if ( entitySymbol->field_count() == 0 )
        return nullptr;

    const TxFieldDeclaration* closestDecl = nullptr;
    uint64_t closestReint = UINT64_MAX;

    for ( auto fieldCandidateI = entitySymbol->fields_cbegin();
            fieldCandidateI != entitySymbol->fields_cend(); fieldCandidateI++ ) {
        const TxFieldDeclaration* fieldDecl = ( *fieldCandidateI );
        if ( !( fieldDecl->get_decl_flags() & TXD_EXPERRBLOCK ) ) {
            auto field = fieldDecl->get_definer()->resolve_field();

            // first screen the fields that are of function type and take the correct number of arguments:
            if ( field->get_type()->get_type_class() == TXTC_FUNCTION ) {
                const TxType* fieldType = field->get_type()->type();
                auto candArgTypes = fieldType->argument_types();
                const TxType* arrayArgElemType = fieldType->vararg_elem_type();
                const TxType* fixedArrayArgType = nullptr;

                if ( arrayArgElemType ) {
                    // var-arg tail parameter accepts zero or more arguments
                    if ( arguments->size() < candArgTypes.size() - 1 )
                        continue;  // mismatching number of function args
                }
                else if ( ( fixedArrayArgType = fieldType->fixed_array_arg_type() ) ) {
                    // fixed array parameter accepts matching number of arguments
                    auto lenExpr = static_cast<const TxArrayType*>( fixedArrayArgType->acttype() )->capacity();
                    auto len = eval_unsigned_int_constant( lenExpr );
                    if ( !( arguments->size() == 1 || arguments->size() == len ) )
                        continue;  // mismatching number of function args
                    arrayArgElemType = fixedArrayArgType->element_type()->type();
                }
                else if ( arguments->size() != candArgTypes.size() ) {
                    continue;  // mismatching number of function args
                }

                {
                    //LOG_INFO(entitySymbol->LOGGER(), "Candidate function: " << field->get_type());

                    // next check that the argument types match, and how close they match:
                    uint16_t reint[4] = { 0, 0, 0, 0 };
                    for ( unsigned i = 0; i < arguments->size(); i++ ) {
                        TxExpressionNode* argNode = arguments->at( i );
                        const TxType* argDef = ( arrayArgElemType && i >= candArgTypes.size() - 1 ? arrayArgElemType
                                                                                                  : candArgTypes.at( i ) );
                        int degree = get_reinterpretation_degree( argNode, argDef );
                        if ( degree < 0 ) {
                            if ( arrayArgElemType && i == candArgTypes.size() - 1 && candArgTypes.size() == arguments->size() ) {
                                // if last provided arg is an array of the correct type, match it against the var-arg tail if present
                                //std::cerr << " cand-arg: " << candArgTypes.at( i ) << "   prov-arg: " << argType << std::endl;
                                degree = get_reinterpretation_degree( argNode, candArgTypes.at( i ) );
                                if ( degree < 0 )
                                    goto NEXT_CANDIDATE;
                            }
                            else {
                                //entitySymbol->LOGGER()->info("Argument mismatch, can't convert\n\tFrom: %80s\n\tTo:   %80s",
                                //                             argType->str(true).c_str(), argDef->str(true).c_str());
                                goto NEXT_CANDIDATE;
                            }
                        }
                        reint[degree]++;
                    }

                    //origin->LOGGER()->trace( "Arguments match for %s: %-32s: %d, %d, %d, %d", field->str().c_str(), field->get_type()->str().c_str(),
                    //                         reint[0], reint[1], reint[2], reint[3] );
                    uint64_t candReint = ( ( (uint64_t) reint[3] ) << 48 | ( (uint64_t) reint[2] ) << 32 | ( (uint64_t) reint[1] ) << 16 | reint[0] );
                    if ( candReint <= closestReint ) {
                        if ( candReint == closestReint ) {
                            // Note, multiple functions with the exact same signature is checked elsewhere.
                            // If arguments for multiple "equal" top candidates are matched via reinterpretation, we just pick the first one found.
                            // TODO: Pick the narrowest match, not the first found match
                            //CWARNING(origin, "Ambiguous function call to " << entitySymbol->get_full_name() << ": "
                            //         << field->get_type() << ", multiple signatures match equally well "
                            //         << "[ " << reint[0] << ", " << reint[1] << ", " << reint[2] << ", " << reint[3] << " ]");
                        }
                        else {
                            closestDecl = *fieldCandidateI;
                            closestReint = candReint;
                        }
                    }
                }
            }
        }

        NEXT_CANDIDATE:
        ;
    }

    if ( closestDecl ) {
        return closestDecl;
    }

    LOG_DEBUG( origin->LOGGER(), "Arguments do not match any overloaded candidate of " << entitySymbol );
    return nullptr;
}

TxScopeSymbol* TxFieldValueNode::resolve_symbol() {
    if (this->symbol)
        return this->symbol;
    TxScopeSymbol* vantageScope = this->context().scope();
    if ( this->baseExpr ) {
        // baseExpr may or may not refer to a type (e.g. modules don't)
        const TxType* baseType = this->baseExpr->resolve_type()->type();

        if ( baseType->get_type_class() == TXTC_REFERENCE ) {
            // implicit dereferencing ('^') operation:
            if ( auto baseRefTargetType = baseType->target_type()->type() ) {
                //std::cerr << "Adding implicit '^' to: " << this->baseExpr << "  six=" << six << std::endl;
                auto derefNode = new TxReferenceDerefNode( this->baseExpr->ploc, this->baseExpr );
                derefNode->node_declaration_pass( this );
                derefNode->symbol_resolution_pass();
                this->baseExpr = derefNode;
                baseType = baseRefTargetType;
            }
        }

        if ( auto baseSymbolNode = dynamic_cast<TxFieldValueNode*>( this->baseExpr ) ) {
            if ( baseType->get_type_class() == TXTC_VOID ) {
                // base is a non-entity symbol
                if ( auto baseSymbol = baseSymbolNode->resolve_symbol() ) {
                    this->symbol = lookup_member( vantageScope, baseSymbol, *this->symbolName );
                }
            }
            else {
                // base is a type or value expression  FIXME: if type, don't include instance members in lookup
                this->symbol = lookup_inherited_member( vantageScope, baseType->acttype(), this->symbolName->str() );
            }
        }
        else {
            // base is a value expression
            this->symbol = lookup_inherited_member( vantageScope, baseType->acttype(), this->symbolName->str() );
        }
    }
    else {
        this->symbol = search_symbol( vantageScope, *this->symbolName );
    }
    return this->symbol;
}

const TxEntityDeclaration* TxFieldValueNode::resolve_decl() {
    if ( this->declaration )
        return this->declaration;
//    if ( get_node_id() == 1584 )
//        std::cerr << "Here " << this << std::endl;
    if ( auto symbol = this->resolve_symbol() ) {
        if ( auto entitySymbol = dynamic_cast<TxEntitySymbol*>( symbol ) ) {
            // if symbol can be resolved to actual field, then do so
            if ( entitySymbol->field_count() ) {
                if ( auto fieldDecl = resolve_field( this, entitySymbol, this->appliedFuncArgs ) ) {
                    this->declaration = fieldDecl;
                    return this->declaration;
                }
            }
            // if symbol is a type, and arguments are applied, and they match a constructor, the resolve to that constructor
            if ( auto typeDecl = entitySymbol->get_type_decl() ) {
                if ( this->appliedFuncArgs ) {
                    auto allocType = typeDecl->get_definer()->resolve_type();
                    // constructors aren't inherited, except for empty/modifiable derivations:
                    auto instanceBaseType = allocType->type()->get_instance_base_type();
                    auto constrMember = lookup_member( this->context().scope(), instanceBaseType->get_declaration()->get_symbol(), CONSTR_IDENT );
                    if ( auto constructorSymbol = dynamic_cast<TxEntitySymbol*> ( constrMember ) ) {
                        if ( auto constructorDecl = resolve_field( this, constructorSymbol, this->appliedFuncArgs ) ) {
                            ASSERT( constructorDecl->get_decl_flags() & ( TXD_CONSTRUCTOR | TXD_INITIALIZER ),
                                    "field named " CONSTR_IDENT " is not flagged as TXD_CONSTRUCTOR or TXD_INITIALIZER: " << constructorDecl->str() );
                            this->constructedType = allocType;
                            this->declaration = constructorDecl;
                            return this->declaration;
                        }
                    }
                    CERR_THROWRES( this,"No matching constructor in type " << allocType
                                   << " for args (" << join( resolve_typevec( this->appliedFuncArgs ), ", ") << ")" );
                }
                else {
                    // resolve this symbol to its type
                    this->declaration = typeDecl;
                    return this->declaration;
                }
            }
            else
                CERR_THROWRES( this, "Symbol " << entitySymbol << " could not be resolved to a distinct field or type: "
                               << this->get_full_identifier() );
        }
        else {
            //not an error, symbol is not an entity but valid
            //CERROR(this, "Symbol is not a field or type: " << this->get_full_identifier());
            return nullptr;
        }
    }
    else {
        if ( this->baseExpr )
            CERR_THROWRES( this, "Unknown symbol '" << this->get_full_identifier()
                           << "' (base expression type is " << this->baseExpr->qualtype() << ")" );
        else
            CERR_THROWRES( this, "Unknown symbol '" << this->get_full_identifier() << "'" );
    }
    // function returns or throws resolution exception before this
    ASSERT( false, "unexpected execution point in " << this );
    return nullptr;
}

const TxQualType* TxFieldValueNode::define_type() {
    if ( auto decl = this->resolve_decl() ) {
        if ( auto fieldDecl = dynamic_cast<const TxFieldDeclaration*>( decl ) ) {
            this->field = fieldDecl->get_definer()->resolve_field();

            if ( fieldDecl->get_storage() == TXS_INSTANCE || fieldDecl->get_storage() == TXS_INSTANCEMETHOD ) {
                if ( !( fieldDecl->get_decl_flags() & ( TXD_CONSTRUCTOR | TXD_INITIALIZER | TXD_GENPARAM | TXD_GENBINDING ) ) ) {
                    if ( this->baseExpr ) {
                        if ( auto baseSymbolNode = dynamic_cast<TxFieldValueNode*>( this->baseExpr ) ) {
                            if ( !baseSymbolNode->get_field() ) {
                                CERR_THROWRES( this, "Instance member field referenced without instance base: " << this->get_full_identifier() );
                            }
                        }
                    }
                    else {
                        CERR_THROWRES( this, "Instance member field referenced without instance base: " << this->get_full_identifier() );
                    }
                }
            }
            return this->field->get_type();
        }
        else
            return static_cast<const TxTypeDeclaration*>( decl )->get_definer()->resolve_type();
    }
    // Symbol is not a field or type, return Void as placeholder type
    return new TxQualType( this->registry().get_builtin_type( TXBT_VOID ) );
}

void TxFieldValueNode::field_base_resolution_pass() {
    TxExpressionNode::symbol_resolution_pass();
    if ( this->baseExpr ) {
        if ( auto baseField = dynamic_cast<TxFieldValueNode*>( this->baseExpr ) )
            baseField->field_base_resolution_pass();
        else
            this->baseExpr->symbol_resolution_pass();
    }
}

void TxFieldValueNode::symbol_resolution_pass() {
    this->field_base_resolution_pass();

    if ( auto typeDecl = dynamic_cast<const TxTypeDeclaration*>( this->declaration ) )
        CERROR( this, "'" << get_full_identifier() << "' resolved to a type, not a field: " << typeDecl );
}

const TxExpressionNode* TxFieldValueNode::get_data_graph_origin_expr() const {
    if ( this->baseExpr ) {
        if ( auto fieldBase = dynamic_cast<TxFieldValueNode*>( this->baseExpr ) ) {
            if ( !fieldBase->get_field() )
                return nullptr;  // baseExpr identifies a namespace
        }
    }
    return this->baseExpr;
}

TxFieldStorage TxFieldValueNode::get_storage() const {
    if ( this->field->get_storage() == TXS_VIRTUAL && !this->baseExpr )
        return TXS_STATIC;
    return this->field->get_storage();
}

bool TxFieldValueNode::is_statically_constant() const {
    if ( this->field ) {
        // A field is statically constant if it is unmodifiable, isn't virtual, and has a statically constant initializer or base expression
        auto storage = this->get_storage();
        if ( auto initExpr = this->field->get_declaration()->get_definer()->get_init_expression() ) {
            if ( storage == TXS_VIRTUAL || storage == TXS_INSTANCEMETHOD )
                return false;
            return ( !this->field->get_type()->is_modifiable() && initExpr->is_statically_constant() );
        }
        else if ( storage == TXS_INSTANCE ) {
            return ( this->baseExpr && this->baseExpr->is_statically_constant() );
        }
        // FUTURE: allow a virtual field lookup, with a constant base expression, to behave as a static field lookup (i.e. non-polymorphic)
        // FUTURE: support getting instance method lambda object of statically constant objects
        return false;
    }
    else if ( this->symbol ) {
        return true;
    }
    else
        return false;
}
