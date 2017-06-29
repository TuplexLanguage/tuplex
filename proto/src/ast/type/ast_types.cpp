#include "ast_types.hpp"

#include "ast/ast_wrappers.hpp"
#include "ast/expr/ast_lambda_node.hpp"

TxScopeSymbol* TxIdentifiedSymbolNode::resolve_symbol() {
    if (this->symbol)
        return this->symbol;
    if ( this->baseSymbol ) {
        // baseSymbol may or may not refer to a type (e.g. modules don't)
        auto baseType = this->baseSymbol->resolve_type();

        TxScopeSymbol* vantageScope = this->context().scope();
        if ( auto baseSymbolNode = dynamic_cast<TxIdentifiedSymbolNode*>( this->baseSymbol ) ) {
            if ( baseType->get_type_class() == TXTC_VOID ) {
                // base is a non-entity symbol
                if ( auto baseSymbol = baseSymbolNode->resolve_symbol() ) {
                    this->symbol = lookup_member( vantageScope, baseSymbol, *this->symbolName );
                }
            }
            else {
                // base is a type expression
                this->symbol = baseType->lookup_inherited_instance_member( vantageScope, this->symbolName->str() );
            }
        }
        else {
            // base is a type expression
            this->symbol = baseType->lookup_inherited_instance_member( vantageScope, this->symbolName->str() );
        }
    }
    else {
        // capable of looking up both simple names and fully qualified names
        this->symbol = lookup_symbol( this->context().scope(), *this->symbolName );
    }
    return this->symbol;
}

const TxType* TxIdentifiedSymbolNode::define_type() {
    if ( auto symbol = this->resolve_symbol() ) {
        if ( auto entitySym = dynamic_cast<const TxEntitySymbol*>( symbol) ) {
            if ( auto typeDecl = entitySym->get_type_decl() )
                return typeDecl->get_definer()->resolve_type();
            if ( entitySym->field_count() == 1 )
                return entitySym->get_first_field_decl()->get_definer()->resolve_type();
            CERR_THROWRES( this, "Can't resolve type of overloaded symbol '" << this->get_full_identifier() << "'" );
        }
        // Symbol is not an entity (field or type), return Void as placeholder type
        return this->registry().get_builtin_type( TXBT_VOID );
    }
    CERR_THROWRES( this, "Unknown symbol: '" << this->symbolName << "'" );
}

const TxType* TxNamedTypeNode::define_type() {
    if ( auto symbol = this->symbolNode->resolve_symbol() ) {
        if ( auto entitySym = dynamic_cast<const TxEntitySymbol*>( symbol) ) {
            if ( auto typeDecl = entitySym->get_type_decl() ) {
                auto type = typeDecl->get_definer()->resolve_type();
                if ( auto decl = this->get_declaration() ) {
                    // create empty specialization (uniquely named but identical type)
                    return this->registry().make_empty_derivation( decl, type, this->requires_mutable_type() );
                }
                return type;
            }
            CERR_THROWRES( this, "Can't resolve type of symbol '" << this->symbolNode->get_full_identifier() << "'" );
// for now we don't allow an identified field to imply its type
//            if ( entitySym->field_count() == 1 )
//                return entitySym->get_first_field_decl()->get_definer()->resolve_type();
//            CERR_THROWRES( this, "Can't resolve type of overloaded symbol " << this->get_full_identifier() );
        }
        // Symbol is not a field or type
        CERR_THROWRES( this, "Not a type: '" << this->symbolNode->get_full_identifier() << "'" );
    }
    CERR_THROWRES( this, "Unknown symbol: '" << this->symbolNode->get_full_identifier() << "'" );
}

const TxType* TxMemberTypeNode::define_type() {
    auto baseType = this->baseTypeExpr->resolve_type();
    if (auto memberSymbol = baseType->lookup_inherited_instance_member( this->context().scope(), this->memberName )) {
        if ( auto entitySym = dynamic_cast<const TxEntitySymbol*>( memberSymbol) ) {
            if ( auto typeDecl = entitySym->get_type_decl() ) {
                auto type = typeDecl->get_definer()->resolve_type();
                if ( auto decl = this->get_declaration() ) {
                    // create empty specialization (uniquely named but identical type)
                    return this->registry().make_empty_derivation( decl, type, this->requires_mutable_type() );
                }
                return type;
            }
            CERR_THROWRES( this, "Can't resolve type of symbol '" << this->memberName << "'" );
// for now we don't allow an identified field to imply its type
//            if ( entitySym->field_count() == 1 )
//                return entitySym->get_first_field_decl()->get_definer()->resolve_type();
//            CERR_THROWRES( this, "Can't resolve type of overloaded symbol " << this->get_full_identifier() );
        }
        // Symbol is not a field or type
        CERR_THROWRES( this, "Not a type: '" << this->memberName << "'" );
    }
    CERR_THROWRES( this, "Unknown symbol: '" << this->memberName << "'" );
/* previous identified-type implementation
    if ( auto identifiedTypeDecl = lookup_type( this->context().scope(), *this->symbolName ) ) {
        auto identifiedType = identifiedTypeDecl->get_definer()->resolve_type();
        if ( auto declEnt = this->get_declaration() ) {
            // create empty specialization (uniquely named but identical type)
            return this->registry().make_empty_derivation( declEnt, identifiedType );
        }
        return identifiedType;
    }
    else
        CERR_THROWRES( this, "Unknown type: " << this->symbolName << " (from " << this->context().scope() << ")" );
*/
}

const TxType* TxArrayTypeNode::define_type() {
    if ( this->requires_mutable_type() ) {
        if ( auto elemTypeArg = dynamic_cast<TxMaybeModTypeNode*>( this->elementTypeNode->typeExprNode ) )
            elemTypeArg->set_modifiable( true );
    }
    if ( this->capacityNode ) {
        static_cast<TxMaybeConversionNode*>( this->capacityNode->valueExprNode )->insert_conversion(
                this->registry().get_builtin_type( ARRAY_SUBSCRIPT_TYPE_ID ) );
        return this->registry().get_array_type( this, this->elementTypeNode, this->capacityNode, this->requires_mutable_type() );
    }
    else
        return this->registry().get_array_type( this, this->elementTypeNode, this->requires_mutable_type() );
}

const TxType* TxGenSpecTypeNode::define_type() {
    auto genType = this->genTypeExpr->resolve_type();
    // copy vector because of const conversion:
    auto tmp = std::vector<const TxTypeArgumentNode*>( this->typeArgs->size() );
    std::copy( this->typeArgs->cbegin(), this->typeArgs->cend(), tmp.begin() );
    return this->registry().get_type_specialization( this, genType, tmp, this->requires_mutable_type() );
}

void TxDerivedTypeNode::init_implicit_types() {
    if ( !this->baseType ) {
        if ( this->get_decl_interface_kw() )
            this->baseType = new TxNamedTypeNode( this->ploc, "tx.Interface" );
        else
            this->baseType = new TxNamedTypeNode( this->ploc, "tx.Tuple" );
    }

    // implicit type member 'Super' for types with a body:
    // (Note, 'Self' is created in the symbol table for all types, as an alias directly to the type.)
    TxTypeExpressionNode* superTypeExprN = new TxTypeExprWrapperNode( this->baseType );
    auto superRefTypeExprN = new TxReferenceTypeNode( this->ploc, nullptr, superTypeExprN );
    const std::string superTypeName = "Super";
    this->superRefTypeNode = new TxTypeDeclNode( this->ploc, TXD_IMPLICIT, superTypeName, nullptr, superRefTypeExprN );
}

const TxType* TxDerivedTypeNode::define_type() {
    ASSERT( this->get_declaration(), "No declaration for derived type " << *this );

    if ( this->builtinTypeDefiner ) {
        return this->builtinTypeDefiner->resolve_type();
    }

    const TxType* baseObjType = this->baseType->resolve_type();
    std::vector<const TxType*> interfaceTypes;
    interfaceTypes.reserve( this->interfaces->size() );
    for ( auto interface : *this->interfaces ) {
        interfaceTypes.emplace_back( interface->resolve_type() );
    }

    return this->registry().make_type_derivation( this, baseObjType, interfaceTypes, this->requires_mutable_type() );
}

void TxDerivedTypeNode::symbol_resolution_pass() {
    TxTypeExpressionNode::symbol_resolution_pass();
    this->baseType->symbol_resolution_pass();
    for ( auto interface : *this->interfaces ) {
        interface->symbol_resolution_pass();
    }

    this->superRefTypeNode->symbol_resolution_pass();

    for ( auto member : *this->members ) {
        member->symbol_resolution_pass();
        // TODO: can't put immutable instance member in non-immutable type (except via reference)
        //       (OR: disable whole-object-assignment)
    }
}

void TxDerivedTypeNode::visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) {
    this->baseType->visit_ast( visitor, thisCursor, "basetype", context );
    for ( auto interface : *this->interfaces )
        interface->visit_ast( visitor, thisCursor, "interface", context );

    this->superRefTypeNode->visit_ast( visitor, thisCursor, "superreftype", context );

    for ( auto member : *this->members )
        member->visit_ast( visitor, thisCursor, "member", context );
}

void TxFunctionTypeNode::typeexpr_declaration_pass() {
    // overrides in order to create implicit declaration for the function type
    if ( !this->get_declaration() ) {
        std::string funcTypeName = lexContext.scope()->make_unique_name( "$Ftype", true );
        TxDeclarationFlags flags = TXD_IMPLICIT;  // TXD_PUBLIC, TXD_EXPERRBLOCK ?
        auto declaration = lexContext.scope()->declare_type( funcTypeName, this, flags );
        if ( !declaration ) {
            CERROR( this, "Failed to declare type " << funcTypeName );
            return;
        }
        this->set_declaration( declaration );
        LOG_TRACE( this->LOGGER(), this << ": Declared type " << declaration );
    }
}

const TxType* TxFunctionTypeNode::define_type() {
    std::vector<const TxType*> argumentTypes;
    for ( auto argDefNode : *this->arguments ) {
        argumentTypes.push_back( argDefNode->resolve_type() );
    }
    if ( this->context().enclosing_lambda() && this->context().enclosing_lambda()->get_constructed() )
        return this->registry().get_constructor_type( this->get_declaration(), argumentTypes, this->context().enclosing_lambda()->get_constructed() );
    else if ( this->returnField )
        return this->registry().get_function_type( this->get_declaration(), argumentTypes, this->returnField->resolve_type(), modifying );
    else
        return this->registry().get_function_type( this->get_declaration(), argumentTypes, modifying );
}

void TxModifiableTypeNode::typeexpr_declaration_pass() {
    // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
    if ( auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>( this->baseType ) ) {
        if ( auto maybeModElem = dynamic_cast<TxMaybeModTypeNode*>( arrayBaseType->elementTypeNode->typeExprNode ) ) {
            // (can this spuriously add Modifiable node to predeclared modifiable type, generating error?)
            this->LOGGER()->debug( "Implicitly declaring Array Element modifiable at %s", this->str().c_str() );
            maybeModElem->set_modifiable( true );
        }
    }
}

void TxMaybeModTypeNode::typeexpr_declaration_pass() {
    // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
    if ( !this->isModifiable ) {
        if ( auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>( this->baseType ) )
            if ( typeid(*arrayBaseType->elementTypeNode->typeExprNode) == typeid(TxModifiableTypeNode) ) {
                this->LOGGER()->debug( "Implicitly declaring Array modifiable at %s", this->str().c_str() );
                this->isModifiable = true;
            }
    }

    if ( this->isModifiable )
        TxModifiableTypeNode::typeexpr_declaration_pass();
    else {
        // "pass through" entity declaration to the underlying type
        this->baseType->set_declaration( this->get_declaration() );
    }
}
