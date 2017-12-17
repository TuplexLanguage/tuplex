#include "ast_types.hpp"

#include "ast_qualtypes.hpp"
#include "ast/ast_wrappers.hpp"
#include "ast/expr/ast_lambda_node.hpp"
#include "ast/expr/ast_field.hpp"
#include "symbol/symbol_lookup.hpp"

#include "tx_error.hpp"


//TxIdentifiedSymbolNode* TxIdentifiedSymbolNode::make_ident_sym_node( const TxLocation& ploc, const std::string& compoundName ) {
//    TxIdentifier ci( compoundName );
//    TxIdentifiedSymbolNode* symNode = nullptr;
//    for ( auto it = ci.segments_cbegin(); it != ci.segments_cend(); it++ ) {
//        symNode = new TxIdentifiedSymbolNode( ploc, symNode, new TxIdentifierNode( ploc, *it ) );
//    }
//    return symNode;
//}
//
//TxScopeSymbol* TxIdentifiedSymbolNode::resolve_symbol() {
//    if ( !this->symbol ) {
//        TxScopeSymbol* vantageScope = this->context().scope();
//        if ( this->baseSymbolNode ) {
//            if ( auto baseSymbol = this->baseSymbolNode->resolve_symbol() ) {
//                // baseSymbol may refer to a namespace, type, or field
//                this->symbol = lookup_inherited_member( vantageScope, baseSymbol, this->symbolName->ident() );
//            }
//        }
//        else {
//            this->symbol = search_name( vantageScope, this->symbolName->ident() );
//        }
//    }
//    return this->symbol;
//}


TxNamedTypeNode::TxNamedTypeNode( const TxLocation& ploc, const std::string& compoundName )
        : TxTypeExpressionNode( ploc ), exprNode( make_compound_symbol_expression( ploc, compoundName ) ) {
}

TxQualType TxNamedTypeNode::define_type( TxPassInfo passInfo ) {
    return this->exprNode->resolve_type( passInfo );
//    if ( auto symbol = this->symbolNode->resolve_symbol() ) {
//        if ( auto entitySym = dynamic_cast<const TxEntitySymbol*>( symbol) ) {
//            if ( auto typeDecl = entitySym->get_type_decl() ) {
//                return typeDecl->get_definer()->resolve_type( passInfo );
//            }
//        }
//        // Symbol is not a field or type
//        CERR_THROWRES( this, "Not a type: '" << this->symbolNode->get_full_identifier() << "'" );
//    }
//    CERR_THROWRES( this, "Unknown symbol: '" << this->symbolNode->get_full_identifier() << "'" );
}

TxQualType TxMemberTypeNode::define_type( TxPassInfo passInfo ) {
    auto baseType = this->baseTypeExpr->resolve_type( passInfo );
    if ( auto memberSymbol = lookup_inherited_member( this->context().scope(), baseType.type(), this->memberName->ident() ) ) {
        if ( auto entitySym = dynamic_cast<const TxEntitySymbol*>( memberSymbol ) ) {
            if ( auto typeDecl = entitySym->get_type_decl() ) {
                return typeDecl->get_definer()->resolve_type( passInfo );
            }
        }
        // Symbol is not a field or type
        CERR_THROWRES( this, "Not a type: '" << this->memberName << "'" );
    }
    CERR_THROWRES( this, "Unknown symbol: '" << this->memberName << "'" );
}


TxActualType* TxGenSpecTypeNode::create_type( TxPassInfo passInfo ) {
    // copy vector because of const conversion:
    std::vector<const TxTypeArgumentNode*> bindings( this->typeArgs->cbegin(), this->typeArgs->cend() );
    return this->registry().instantiate_type( this, this->genTypeExpr, bindings, this->requires_mutable_type() );
}


TxActualType* TxReferenceTypeNode::create_type( TxPassInfo passInfo ) {
    return this->registry().get_reference_type( this, this->targetTypeNode, this->dataspace );
}

TxActualType* TxArrayTypeNode::create_type( TxPassInfo passInfo ) {
    if ( this->elementTypeNode->is_value() )
        CERR_THROWRES( this->elementTypeNode, "Array element type is not a type expression" );
    if ( this->requires_mutable_type() ) {
        if ( auto elemTypeArg = dynamic_cast<TxMaybeModTypeNode*>( this->elementTypeNode->type_expr_node() ) )
            elemTypeArg->set_modifiable( true );
    }
    if ( this->capacityNode ) {
        if ( !this->capacityNode->is_value() )
            CERR_THROWRES( this->capacityNode, "Array capacity is not a value expression" );
        auto ven = static_cast<TxMaybeConversionNode*>( this->capacityNode->value_expr_node() );
        ven->insert_conversion( passInfo, this->registry().get_builtin_type( ARRAY_SUBSCRIPT_TYPE_ID ) );
        return this->registry().get_array_type( this, this->elementTypeNode, this->capacityNode, this->requires_mutable_type() );
    }
    else
        return this->registry().get_array_type( this, this->elementTypeNode, this->requires_mutable_type() );
}



void TxDerivedTypeNode::init_implicit_types() {
    if ( !this->baseTypeNode ) {
        if ( this->get_decl_interface_kw() )
            this->baseTypeNode = new TxNamedTypeNode( this->ploc, "tx.Interface" );
        else
            this->baseTypeNode = new TxNamedTypeNode( this->ploc, "tx.Tuple" );
        this->baseTypeNode->set_requires_mutable( this->requires_mutable_type() );
    }

    if (! this->builtinTypeDefiner ) {
        // implicit type member 'Super' for types with a body:
        // (Note, 'Self' is created in the symbol table for all types, as an alias directly to the type.)
        auto superTypeExprN = new TxAliasTypeNode( this->ploc, new TxTypeExprWrapperNode( this->baseTypeNode ) );
        auto superTypeName = new TxIdentifierNode( this->ploc, "Super" );
        this->superRefTypeNode = new TxTypeDeclNode( this->ploc, TXD_IMPLICIT, superTypeName, nullptr, superTypeExprN );
    }
}

TxActualType* TxDerivedTypeNode::create_type( TxPassInfo passInfo ) {
    ASSERT( this->get_declaration(), "No declaration for derived type " << *this );

    if ( this->builtinTypeDefiner ) {
        return const_cast<TxActualType*>( this->builtinTypeDefiner->resolve_type( passInfo ).type() );
    }

    // copy vector because of const conversion:
    std::vector<const TxTypeExpressionNode*> ifNodes( this->interfaces->cbegin(), this->interfaces->cend() );
    return this->registry().instantiate_type( this->get_declaration(), this->baseTypeNode, ifNodes, this->requires_mutable_type() );
}

void TxDerivedTypeNode::set_requires_mutable( bool mut ) {
    TxTypeExpressionNode::set_requires_mutable( mut );
    if ( this->baseTypeNode )
        this->baseTypeNode->set_requires_mutable( mut );
}

void TxDerivedTypeNode::visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) {
    if ( !this->builtinTypeDefiner ) {
        this->baseTypeNode->visit_ast( visitor, thisCursor, "basetype", context );
    }

    for ( auto interface : *this->interfaces )
        interface->visit_ast( visitor, thisCursor, "interface", context );

    if ( this->superRefTypeNode )
        this->superRefTypeNode->visit_ast( visitor, thisCursor, "superreftype", context );

    for ( auto member : *this->members )
        member->visit_ast( visitor, thisCursor, "member", context );

    if ( auto qtype = this->attempt_qtype() ) {
        for ( auto implConstr : qtype->get_implicit_constructors() )
            implConstr->visit_ast( visitor, thisCursor, "initializer", context );
    }
}

void TxFunctionTypeNode::typeexpr_declaration_pass() {
    // overrides in order to create implicit declaration for the function type
    if ( !dynamic_cast<const TxTypeDeclNode*>( this->parent() ) ) {
        TxDeclarationFlags fieldFlags = TXD_NONE;
        if ( auto enclFieldDefNode = this->enclosing_node<TxFieldDefiningNode>() ) {
            fieldFlags = enclFieldDefNode->get_declaration()->get_decl_flags();
        }
        TxDeclarationFlags inheritedFlagsFilter = TXD_EXTERNC | TXD_PUBLIC | TXD_PROTECTED | TXD_BUILTIN | TXD_IMPLICIT | TXD_EXPERROR;
        TxDeclarationFlags flags = ( fieldFlags & inheritedFlagsFilter ) | TXD_IMPLICIT;
        std::string funcTypeName = lexContext.scope()->make_unique_name( "$Ftype", true );
        auto declaration = lexContext.scope()->declare_type( funcTypeName, this, this, flags );
        if ( !declaration ) {
            CERROR( this, "Failed to declare type " << funcTypeName );
            return;
        }
        this->set_declaration( declaration );
        LOG_TRACE( this->LOGGER(), this << ": Declared type " << declaration );
    }
}

TxActualType* TxFunctionTypeNode::create_type( TxPassInfo passInfo ) {
    // FUTURE: Be able to define a function type that has an argument of the same function type (requires arg types to be resolved afterwards)
    std::vector<const TxActualType*> argumentTypes;
    for ( auto argDefNode : *this->arguments ) {
        argumentTypes.push_back( argDefNode->resolve_type( passInfo ).type() );
    }
    TxActualType* type;
    if ( this->context().enclosing_lambda() && this->context().enclosing_lambda()->get_constructed() )
        type = this->registry().get_constructor_type( this->get_declaration(), argumentTypes, this->context().enclosing_lambda()->get_constructed() );
    else if ( this->get_declaration()->get_decl_flags() & TXD_EXTERNC ) {
        type = this->registry().get_externc_function_type( this->get_declaration(), argumentTypes,
                                                           ( this->returnField ? this->returnField->resolve_type( passInfo ).type() : nullptr ) );
    }
    else if ( this->returnField )
        type = this->registry().get_function_type( this->get_declaration(), argumentTypes, this->returnField->resolve_type( passInfo ).type(), modifying );
    else
        type = this->registry().get_function_type( this->get_declaration(), argumentTypes, modifying );
    return type;
}
