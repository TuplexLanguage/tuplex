#include "ast.hpp"
#include "ast_util.hpp"

#include "builtin/builtin_types.hpp"

static bool commonNameValidityChecks( TxNode* node, TxDeclarationFlags declFlags, const std::string& name ) {
    if ( name.empty() ) {
        CERROR( node, "Name string is empty." );
        return false;
    }
    return true;
}

bool validateTypeName( TxNode* node, TxDeclarationFlags declFlags, const std::string& name ) {
// TODO
//    if (! isupper( name.at(0) ))
//        CWARNING(node, "The first letter of type names should be uppercase: " << name);
    return commonNameValidityChecks( node, declFlags, name );
}

bool validateFieldName( TxNode* node, const std::string& name ) {
// TODO
//    if (! islower( name.at(0) ))
//        CWARNING(node, "The first letter of field names should be lowercase: " << name);
    return commonNameValidityChecks( node, TXD_NONE, name );
}

template<typename Node>
std::vector<const TxType*> attempt_typevec( const std::vector<Node*>* nodevec ) {
    std::vector<const TxType*> types = std::vector<const TxType*>( nodevec->size() );
    std::transform( nodevec->cbegin(), nodevec->cend(), types.begin(), []( Node* node ) -> const TxType* {return node->attempt_get_type();} );
    return types;
}

template<typename Node>
std::vector<const TxType*> resolve_typevec( const std::vector<Node*>* nodevec ) {
    std::vector<const TxType*> types = std::vector<const TxType*>( nodevec->size() );
    std::transform( nodevec->cbegin(), nodevec->cend(), types.begin(), []( Node* node ) -> const TxType* {
        node->resolve_type()->type(); return node->attempt_get_type(); } );
    return types;
}



AstVisitor declPassVisitor = []( TxNode* node, const AstCursor& parent, const std::string& role, void* parserCtx ) {
    node->node_declaration_pass( parent.node );
};

void run_declaration_pass( TxNode* node, const TxNode* parentNode, const std::string& role) {
    ASSERT(parentNode, "NULL parentNode");
    const AstCursor parent( parentNode );
    node->visit_ast( declPassVisitor, parent, role, nullptr );
}

void run_declaration_pass( TxNode* node, const LexicalContext& lexContext ) {
    ASSERT( lexContext.scope(), "uninitialized lex-context" );
    auto internalRoot = new TxInternalRootNode( node->parseLocation, node, lexContext );
    run_declaration_pass( node, internalRoot, "node" );
}

Logger& TxNode::_LOG = Logger::get( "AST" );

unsigned TxNode::nextNodeId = 0;

std::string TxNode::str() const {
    auto ident = this->get_identifier();
    const size_t bsize = 128;
    char buf[bsize];
    snprintf( buf, bsize, "%-11s %4u %-24s %s", this->parse_loc_string().c_str(), this->get_node_id(), typeid(*this).name(), ident.c_str() );
    if ( this->lexContext.reinterpretationDefiner )
        return std::string( buf ) + " <: " + this->lexContext.reinterpretationDefiner->str();
    else
        return std::string( buf );
}

std::string TxNode::parse_loc_string() const {
    const size_t bsize = 32;
    char buf[bsize];
    if ( parseLocation.begin.line == parseLocation.end.line ) {
        int lcol = ( parseLocation.end.column > parseLocation.begin.column ) ? parseLocation.end.column : parseLocation.end.column;
        snprintf( buf, bsize, "%3d.%2d-%d", parseLocation.begin.line, parseLocation.begin.column, lcol );
    }
    else
        snprintf( buf, bsize, "%3d.%2d-%d.%d", parseLocation.begin.line, parseLocation.begin.column, parseLocation.end.line, parseLocation.end.column );
    return std::string( buf );
}

void TxNode::visit_ast( AstVisitor visitor, const AstCursor& parent, const std::string& role, void* context ) {
    visitor( this, parent, role, context );
    const AstCursor thisCursor( &parent, this );
    this->visit_descendants( visitor, thisCursor, role, context );
}

void TxNode::visit_ast( AstVisitor visitor, void* context ) {
    const AstCursor parent(nullptr);  // a 'null' parent
    this->visit_ast( visitor, parent, "", context );
}

const TxType* TxTypeDefiningNode::resolve_type() {
    ASSERT( this->is_context_set(), "Declaration pass has not been run (lexctx not set) before resolving " << this );
    if ( !this->type ) {
        if ( this->hasResolved ) {
            throw resolution_error( this, "Previous type resolution failed in " + this->str() );
        }
        LOG_TRACE( this->LOGGER(), "resolving type  of " << this );

        if ( this->startedRslv ) {
            CERR_THROWRES( this, "Recursive definition of type '" << this->get_identifier() << "'" );
        }
        this->startedRslv = true;
        try {
            this->type = this->define_type();
        }
        catch ( const resolution_error& err ) {
            this->hasResolved = true;
            //LOG(this->LOGGER(), DEBUG, "Caught and re-threw resolution error in " << this << ": " << err);
            throw;
        }
        ASSERT( this->type, "NULL-resolved type but no exception thrown in " << this );
        this->hasResolved = true;
    }
    return this->type;
}

const TxField* TxFieldDefiningNode::resolve_field() {
    ASSERT( this->is_context_set(), "Declaration pass has not been run (lexctx not set) before resolving " << this );
    if ( !this->field ) {
        if ( this->hasResolved ) {
            throw resolution_error( this, "Previous field resolution failed in " + this->str() );
        }
        LOG_TRACE( this->LOGGER(), "resolving field of " << this );

        if ( this->startedRslv ) {
            CERR_THROWRES( this, "Recursive definition of field '" << this->get_identifier() << "'" );
        }
        this->startedRslv = true;
        try {
            this->type = this->define_type();
            this->field = this->define_field();
        }
        catch ( const resolution_error& err ) {
            this->hasResolved = true;
            //LOG(this->LOGGER(), DEBUG, "Caught and re-threw resolution error in " << this << ": " << err);
            throw;
        }
        ASSERT( this->type && this->field, "NULL-resolved type/field but no exception thrown in " << this );
        this->hasResolved = true;
    }
    return this->field;
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
            if ( !type->is_modifiable() )
                type = this->registry().get_modifiable_type( nullptr, type );
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


void TxFieldDeclNode::declaration_pass() {
    TxDeclarationFlags flags = this->get_decl_flags();

    if ( field->initExpression ) {
        if ( flags & TXD_ABSTRACT )
            CERROR( this, "'abstract' is invalid modifier for field / method that has an initializer / body: " << field->initExpression );
    }

    TxFieldStorage storage;
    const TxTypeDeclaration* outerTypeDecl = nullptr;
    if ( auto entitySymbol = dynamic_cast<TxEntitySymbol*>( this->context().scope() ) )
        outerTypeDecl = entitySymbol->get_type_decl();

    if ( this->isMethodSyntax && outerTypeDecl ) {
        // Note: instance method storage is handled specially (technically the function pointer is a static field)

        TxLambdaExprNode* lambdaExpr = nullptr;
        if ( auto initExpr = dynamic_cast<TxMaybeConversionNode*>( field->initExpression ) )
            lambdaExpr = dynamic_cast<TxLambdaExprNode*>( initExpr->originalExpr );

        if ( !lambdaExpr && !( flags & TXD_ABSTRACT ) )
            CERROR( this, "Missing modifier 'abstract' for method that has no body" );

        if ( flags & TXD_STATIC ) {
            storage = TXS_STATIC;
        }
        else {
            if ( lambdaExpr )
                lambdaExpr->set_instance_method( true );
            storage = TXS_INSTANCEMETHOD;
        }
    }
    else if ( dynamic_cast<TxModule*>( lexContext.scope() ) ) {  // if in global scope
        if ( flags & TXD_STATIC )
            CERROR( this, "'static' is invalid modifier for module scope field " << this->field->get_identifier() );
        if ( flags & TXD_FINAL )
            CERROR( this, "'final' is invalid modifier for module scope field " << this->field->get_identifier() );
        if ( flags & TXD_OVERRIDE )
            CERROR( this, "'override' is invalid modifier for module scope field " << this->field->get_identifier() );
        if ( flags & TXD_ABSTRACT )
            CERROR( this, "'abstract' is invalid modifier for module scope field " << this->field->get_identifier() );
        storage = TXS_GLOBAL;
    }
    else {
        if ( flags & TXD_ABSTRACT ) {
            if ( !( flags & TXD_STATIC ) )
                CERROR( this, "'abstract' fields must also be declared 'static': " << this->field->get_identifier() );
            if ( !( flags & ( TXD_PROTECTED | TXD_PUBLIC ) ) )
                CERROR( this, "'abstract' fields cannot be private (since private are non-virtual): " << this->field->get_identifier() );
        }
        storage = ( flags & TXD_STATIC ) ? TXS_STATIC : TXS_INSTANCE;
    }

    // TXS_STATIC may be changed to TXS_VIRTUAL depending on context:
    if ( storage == TXS_STATIC
         && ( flags & ( TXD_PUBLIC | TXD_PROTECTED ) )          // private fields are non-virtual
         && !( flags & TXD_INITIALIZER )                        // initializers are static
         && ( ( flags & ( TXD_OVERRIDE | TXD_FINAL ) ) != TXD_FINAL ) ) { // if final but doesn't override, its effectively non-virtual
        storage = TXS_VIRTUAL;
    }

    std::string declName = this->field->fieldName->str();
    if ( declName == "self" ) {
        // handle constructor declaration
        if ( storage != TXS_INSTANCEMETHOD )
            CERROR( this, "Illegal declaration name for non-constructor member: " << declName );
        declName = CONSTR_IDENT;
        flags = flags | TXD_CONSTRUCTOR;
    }

    this->field->declare_field( declName, lexContext.scope(), flags, storage );
    // Note: Field is processed in the 'outer' scope and not in the 'inner' scope of its declaration.
}

void TxFieldDeclNode::symbol_resolution_pass() {
    try {
        this->field->symbol_resolution_pass();
    }
    catch ( const resolution_error& err ) {
        LOG( this->LOGGER(), DEBUG, "Caught resolution error in " << this->field << ": " << err );
        return;
    }

    auto type = this->field->get_type();
    auto storage = this->field->get_declaration()->get_storage();
    if ( type->is_modifiable() ) {
        if ( storage == TXS_GLOBAL )
            CERROR( this, "Global fields may not be modifiable: " << field->get_identifier() );
    }

    switch ( storage ) {
    case TXS_INSTANCE:
        // FUTURE: ensure TXS_INSTANCE fields are initialized either here or in every constructor
        if ( this->field->initExpression ) {
            if ( !( this->field->get_declaration()->get_decl_flags() & TXD_GENBINDING ) )  // hackish... skips tx.Array.L
                CWARNING( this, "Not yet supported: Inline initializer for instance fields (initialize within constructor instead): "
                          << this->field->get_identifier() );
        }
        if ( type->is_modifiable() ) {
            if ( auto entitySymbol = dynamic_cast<TxEntitySymbol*>( this->context().scope() ) ) {
                const TxTypeDeclaration* outerTypeDecl = entitySymbol->get_type_decl();
                if ( !outerTypeDecl->get_definer()->get_type()->is_mutable() ) {
                    // FIXME: evaluate if this should be OK (especially for specializations)
                    if ( this->context().reinterpretation_definer() )
                        CWARNING( this, "Instance field of an immutable type is declared modifiable: " << field->get_identifier() );
                    else
                        CERROR( this, "Instance field of an immutable type is declared modifiable: " << field->get_identifier() );
                }
            }
        }
        break;
    case TXS_INSTANCEMETHOD:
        if ( this->field->initExpression ) {
            auto lambdaExpr = static_cast<TxLambdaExprNode*>( static_cast<TxMaybeConversionNode*>( field->initExpression )->originalExpr );
            if ( lambdaExpr->funcHeaderNode->is_modifying() ) {
                if ( auto entitySymbol = dynamic_cast<TxEntitySymbol*>( this->context().scope() ) ) {
                    const TxTypeDeclaration* outerTypeDecl = entitySymbol->get_type_decl();
                    if ( !outerTypeDecl->get_definer()->get_type()->is_mutable() ) {
                        if ( !this->context().reinterpretation_definer() ) {
                            // (we skip this error for type specializations that have not been declared mutable, this method will be suppressed)
                            CERROR( this, "Instance method of an immutable type may not be declared modifying: " << field->get_identifier() );
                        }
                    }
                }
            }
        }
        // no break
    case TXS_VIRTUAL:
        if ( !this->field->initExpression ) {
            if ( !( this->field->get_declaration()->get_decl_flags() & TXD_ABSTRACT ) )
                if ( this->field->fieldName->str() != "$adTypeId" )
                    CERROR( this, "Non-abstract virtual fields/methods must have an initializer: " << this->field->get_identifier() );
            // FUTURE: When static initializers in types are supported, static/virtual fields' initialization may be deferred.
        }
        else {
            if ( !this->field->initExpression->is_statically_constant() )
                CERROR( this, "Non-constant initializer for virtual field " << this->field->fieldName );
        }
        break;
    case TXS_GLOBAL:
    case TXS_STATIC:
        if ( !this->field->initExpression ) {
            if ( !( this->field->get_declaration()->get_decl_flags() & ( TXD_BUILTIN | TXD_EXTERN ) ) )
                CERROR( this, "Global/static fields must have an initializer: " << this->field->get_identifier() );
            // FUTURE: When static initializers in types are supported, static/virtual fields' initialization may be deferred.
        }
        else {
            // field is expected to have a statically constant initializer
            if ( !this->field->initExpression->is_statically_constant() )
                CERROR( this, "Non-constant initializer for global/static field " << this->field->fieldName );
        }
        break;
    default:
        // Note: TXS_STACK is not declared via this node
        CERROR( this, "Invalid storage type in field declaration: " << this->field );
    }
}

void TxTypeDeclNode::declaration_pass() {
    const TxTypeDeclaration* declaration = nullptr;
    if ( this->get_decl_flags() & TXD_BUILTIN ) {
        if ( auto entSym = dynamic_cast<const TxEntitySymbol*>( lexContext.scope()->get_member_symbol( this->typeName->str() ) ) ) {
            if ( ( declaration = entSym->get_type_decl() ) ) {
                if ( declaration->get_decl_flags() & TXD_BUILTIN ) {
                    //std::cerr << "existing builtin type declaration: " << declaration << "  new type expr: " << this->typeExpression << std::endl;
                    ASSERT( dynamic_cast<TxDerivedTypeNode*>( this->typeExpression ),
                            "Expected definer for builtin-type to be a TxDerivedTypeNode: " << this->typeExpression );
                    merge_builtin_type_definers( static_cast<TxDerivedTypeNode*>( this->typeExpression ), declaration->get_definer() );
                    this->_builtinCode = true;
                }
            }
        }
        if ( !this->_builtinCode && !this->get_parser_context()->is_internal_builtin() )
            CERROR( this, "Declaration qualifier 'builtin' used for a non-builtin type: " << this->typeName );
    }

    if ( !this->_builtinCode ) {
        declaration = lexContext.scope()->declare_type( this->typeName->str(), this->typeExpression, this->get_decl_flags() );
        if ( !declaration ) {
            CERROR( this, "Failed to declare type " << this->typeName );
            return;
        }
        LOG_TRACE( this->LOGGER(), this << ": Declared type " << declaration );
    }

    if ( !lexContext.is_generic() && this->typeParamDecls ) {
        for ( auto paramDeclNode : *this->typeParamDecls ) {
            if (paramDeclNode->get_decl_flags() & TXD_GENPARAM) {
                this->lexContext.generic = true;
                break;
            }
        }
    }
    this->lexContext._scope = declaration->get_symbol();
    this->typeExpression->set_declaration( declaration );
}

bool TxTypeExpressionNode::requires_mutable_type() const {
    const TxNode* p = this->parent();
    if ( auto d = dynamic_cast<const TxTypeDeclNode*>( p ) )
        return d->mutableType;
    if ( auto m = dynamic_cast<const TxModifiableTypeNode*>( p ) ) {
        if ( m->is_modifiable() )
            return true;
        if ( auto m = dynamic_cast<const TxMaybeModTypeNode*>( p ) ) {
            if ( auto d = dynamic_cast<const TxTypeDeclNode*>( m->parent() ) )
                return d->mutableType;
        }
    }
    return false;
}

bool TxTypeExpressionNode::get_decl_interface_kw() const {
    const TxNode* p = this->parent();
    if ( auto d = dynamic_cast<const TxTypeDeclNode*>( p ) )
        return d->interfaceKW;
    if ( auto m = dynamic_cast<const TxModifiableTypeNode*>( p ) ) {
        //if ( m->is_modifiable() )
        //    return false;
        if ( auto d = dynamic_cast<const TxTypeDeclNode*>( m->parent() ) )
            return d->interfaceKW;
    }
    return false;
}

void TxTypeExpressionNode::declaration_pass() {
    // The context of this node represents its outer scope.
    // The type expression's created type entity, if any, represents its inner scope.
    if (this->declaration)
        this->lexContext._scope = this->declaration->get_symbol();
    this->typeexpr_declaration_pass();
}

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
            CERR_THROWRES( this, "Can't resolve type of overloaded symbol " << this->get_full_identifier() );
        }
        // Symbol is not an entity (field or type), return Void as placeholder type
        return this->registry().get_builtin_type( TXBT_VOID );
    }
    CERR_THROWRES( this, "Unknown symbol: " << this->symbolName );
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
            CERR_THROWRES( this, "Can't resolve type of symbol " << this->symbolNode->get_full_identifier() );
// for now we don't allow an identified field to imply its type
//            if ( entitySym->field_count() == 1 )
//                return entitySym->get_first_field_decl()->get_definer()->resolve_type();
//            CERR_THROWRES( this, "Can't resolve type of overloaded symbol " << this->get_full_identifier() );
        }
        // Symbol is not a field or type
        CERR_THROWRES( this, "Not a type: " << this->symbolNode->get_full_identifier() );
    }
    CERR_THROWRES( this, "Unknown symbol: " << this->symbolNode->get_full_identifier() );
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
            CERR_THROWRES( this, "Can't resolve type of symbol " << this->memberName );
// for now we don't allow an identified field to imply its type
//            if ( entitySym->field_count() == 1 )
//                return entitySym->get_first_field_decl()->get_definer()->resolve_type();
//            CERR_THROWRES( this, "Can't resolve type of overloaded symbol " << this->get_full_identifier() );
        }
        // Symbol is not a field or type
        CERR_THROWRES( this, "Not a type: " << this->memberName );
    }
    CERR_THROWRES( this, "Unknown symbol: " << this->memberName );
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
            this->baseType = new TxNamedTypeNode( this->parseLocation, "tx.Interface" );
        else
            this->baseType = new TxNamedTypeNode( this->parseLocation, "tx.Tuple" );
    }

    // implicit type member '$Super' for types with a body:
    // (Note, '$Self' is created in the symbol table for all types, as an alias directly to the type.)
    TxTypeExpressionNode* superTypeExprN = new TxTypeExprWrapperNode( this->baseType );
    auto superRefTypeExprN = new TxReferenceTypeNode( this->parseLocation, nullptr, superTypeExprN );
    const std::string superTypeName = "$Super";
    this->superRefTypeNode = new TxTypeDeclNode( this->parseLocation, TXD_IMPLICIT, superTypeName, nullptr, superRefTypeExprN );
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

void TxLambdaExprNode::declaration_pass() {
    std::string funcName = ( this->fieldDefNode && this->fieldDefNode->get_declaration() )
                                  ? this->fieldDefNode->get_declaration()->get_unique_name()
                                  : "";
    TxScopeSymbol* funcScope = lexContext.scope()->create_code_block_scope( *this, funcName );
    if ( this->is_instance_method() ) {
        auto entitySym = dynamic_cast<TxEntitySymbol*>( lexContext.scope() );
        if ( entitySym && entitySym->get_type_decl() ) {  // if in type scope
            if ( this->fieldDefNode->get_declaration()->get_decl_flags() & TXD_CONSTRUCTOR ) {
                // this is a constructor
                this->constructedObjTypeDecl = entitySym->get_type_decl();
            }
        }
        else
            CERROR( this, "The scope of instance method must be a type scope: " << lexContext.scope() );

        this->selfRefNode->declare_field( funcScope, TXD_NONE, TXS_STACK );
        this->superRefNode->declare_field( funcScope, TXD_NONE, TXS_STACK );
    }
    this->lexContext._scope = funcScope;
    this->lexContext.enclosingLambda = this;
    // FUTURE: define implicit closure object when in code block
}

bool TxLambdaExprNode::is_suppressed_modifying_method() {
    return ( this->funcHeaderNode->is_modifying()
             && this->context().reinterpretation_definer()
             && !this->selfTypeNode->resolve_type()->is_mutable() );
}

void TxLambdaExprNode::symbol_resolution_pass() {
    TxExpressionNode::symbol_resolution_pass();
    if ( this->is_instance_method() ) {
        if ( this->is_suppressed_modifying_method() ) {
            // modifying methods in an immutable specialization of a generic type are effectively 'abstract'
            //this->funcHeaderNode->symbol_resolution_pass();  // function header
            return;
        }
        try {
            this->selfRefNode->symbol_resolution_pass();
            this->superRefNode->symbol_resolution_pass();
        }
        catch ( const resolution_error& err ) {
            LOG( this->LOGGER(), DEBUG, "Caught resolution error in self/super of instance method " << this << ": " << err );
        }
    }
    else {
        if ( this->funcHeaderNode->is_modifying() )
            CERROR( this, "Only instance methods can be declared 'modifying': " << this->funcHeaderNode->get_type() );
        // TODO: review if there are non-method functions with closure that can declare 'modifying'?
    }

    this->funcHeaderNode->symbol_resolution_pass();  // function header
    this->suite->symbol_resolution_pass();  // function body

    if ( this->funcHeaderNode->returnField ) {
        // verify that body always ends with explicit return statement
        if ( !this->suite->ends_with_return_stmt() )
            CERROR( this, "Function has return value, but not all code paths end with a return statement." );
    }
}

void TxSuiteNode::stmt_declaration_pass() {
    if (! dynamic_cast<const TxLambdaExprNode*>(this->parent()))
        this->lexContext._scope = lexContext.scope()->create_code_block_scope( *this );
}

void TxAssignStmtNode::symbol_resolution_pass() {
    this->lvalue->symbol_resolution_pass();
    auto ltype = this->lvalue->resolve_type();

    // note: similar rules to passing function arg
    if ( !ltype->is_concrete() ) {
        if ( !this->context().is_generic() )
            CERROR( this->lvalue, "Assignee is not concrete: " << ltype );
        else
            LOG_DEBUG( this->LOGGER(), "(Not error since generic context) Assignee is not concrete: " << ltype );
    }
    else {
        if ( !( this->context().enclosing_lambda() && this->context().enclosing_lambda()->get_constructed() ) ) {
            // TODO: only members of constructed object should skip error
            if ( !lvalue->is_mutable() ) {
                // error message already generated
                //CERROR( this, "Assignee or assignee's container is not modifiable (nominal type of assignee is " << ltype << ")" );
            }
        }
        // Note: If the object as a whole is modifiable, it can be assigned to.
        // If it has any "non-modifiable" members, those will still get overwritten.
        // We could add custom check to prevent that scenario for Arrays, but then
        // it would in this regard behave differently than other aggregate objects.
    }
    // if assignee is a reference:
    // TODO: check dataspace rules

    auto nonModLType = ( ltype->is_modifiable() ? ltype->get_base_type() : ltype );  // rvalue doesn't need to be modifiable
    this->rvalue->insert_conversion( nonModLType );
    this->rvalue->symbol_resolution_pass();
}

TxAssertStmtNode::TxAssertStmtNode( const TxLocation& parseLocation, TxExpressionNode* expr )
        : TxStatementNode( parseLocation ), expr( expr ) {
    auto invertedCond = new TxUnaryLogicalNotNode( expr->parseLocation, expr );

    // print assert failed message:
    /* example C assert failed message:
     txc: /home/christer/proj/workspace/proto/src/ast.cpp:515: TxAssertStmtNode::TxAssertStmtNode(const TxLocation&, TxExpressionNode*): Assertion `!this->expr' failed.
     */
    auto & pLoc = parseLocation;
    std::stringstream msg;
    //msg << procName << ": ";         // TODO: will need to be determined via system call
    msg << *pLoc.begin.filename << ":" << pLoc.begin.line;
    //msg << ": " << srcFuncHeader;    // TODO: source text needed for this
    msg << ": Assertion failed";
    //msg << ": `" << srcExpr << "`";  // TODO: source text needed for this
    //msg << ": " << customMessage;    // TODO: supported custom assert message
    std::string assertFailedMsg = "c\"" + msg.str() + "\"";
    auto msgExpr = new TxCStringLitNode( pLoc, assertFailedMsg );
    auto convStrExpr = new TxReferenceToNode( pLoc, new TxElemDerefNode( pLoc, msgExpr, new TxIntegerLitNode( pLoc, 0, false ) ) );
    auto putsCallee = new TxFieldValueNode( pLoc, nullptr, "tx.c.puts" );
    auto putsCallExpr = new TxFunctionCallNode( pLoc, putsCallee, new std::vector<TxExpressionNode*>( { convStrExpr } ) );
    TxStatementNode* putsStmt = new TxCallStmtNode( pLoc, putsCallExpr );
    // TODO: emit it to stderr instead of stdout

    // we call c library abort() upon assertion failure
    auto abortCallee = new TxFieldValueNode( pLoc, nullptr, "tx.c.abort" );
    auto abortCallExpr = new TxFunctionCallNode( pLoc, abortCallee, new std::vector<TxExpressionNode*>() );
    TxStatementNode* abortStmt = new TxCallStmtNode( pLoc, abortCallExpr );

    auto failureSuite = new TxSuiteNode( pLoc, new std::vector<TxStatementNode*>( { putsStmt, abortStmt } ) );
    this->ifStmt = new TxIfStmtNode( pLoc, invertedCond, failureSuite );
}

int get_reinterpretation_degree( const TxType *expectedType, const TxType* providedType ) {
    if ( *expectedType == *providedType ) {
        //std::cerr << "Types equal: " << expectedType << "   ==   " << providedType << std::endl;
        return 0;
    }

    // TODO: check if provided type is narrower than the expected type

    if ( providedType->auto_converts_to( *expectedType ) )
        return 2;

    if ( expectedType->get_type_class() == TXTC_REFERENCE ) {
        if ( auto expRefTargetType = expectedType->target_type() ) {
            if ( providedType->is_a( *expRefTargetType ) ) {
                if ( !expRefTargetType->is_modifiable() )
                    return 3;  // expression will be auto-wrapped with a reference-to node
            }
        }
    }

    if ( providedType->get_type_class() == TXTC_REFERENCE ) {
        if ( auto provRefTargetType = providedType->target_type() ) {
            if ( provRefTargetType->auto_converts_to( *expectedType ) ) {
                return 3;  // expression will be wrapped with a dereference node
            }
        }
    }

    return -1;  // does not match
}

/** Attempts to resolve an identified entity symbol, that is potentially overloaded,
 * to a specific field by matching with the provided arguments' types.
 * The closest matching, valid field is picked. If no field matched, NULL is returned.
 * If a field was matched, and implicit conversions were needed for any arguments,
 * those conversions are inserted for those arguments within this call.
 *
 * All included fields that have the matching number of arguments and compatible argument types are candidates.
 * Candidate selection is done by counting the number and degree of argument reinterpretations necessary to match it.
 * (A single 2nd degree reinterpretation is "further away" than many 1st degree reinterpretations.)
 *
 * Degrees of reinterpretation (to be thought of as degrees of "distance"):
 * 0: Argument and receiver have the exact same type
 * 1: Argument and receiver have equivalent types (according to narrowing/widening type rules)
 * 2: Argument can be implicitly converted to the receiver's type (e.g. Int -> Long)
 * 3: Argument can be transformed via implicit operation to the receiver's type (e.g. implicit referencing)
 *
 * Note: This function doesn't generate compiler errors; if no match is found null is returned.
 */
static const TxFieldDeclaration* resolve_field( const TxExpressionNode* origin, TxEntitySymbol* entitySymbol,
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

    // prepare vector of provided arguments' original types:
    std::vector<const TxType*> argTypes;
    for ( auto argNode : *arguments ) {
        auto argType = argNode->resolve_type();
        if ( !argType )
            return nullptr;
        argTypes.push_back( argType );
    }

    const TxFieldDeclaration* closestDecl = nullptr;
    uint64_t closestReint = UINT64_MAX;

    for ( auto fieldCandidateI = entitySymbol->fields_cbegin();
            fieldCandidateI != entitySymbol->fields_cend(); fieldCandidateI++ ) {
        const TxFieldDeclaration* fieldDecl = ( *fieldCandidateI );
        if ( !( fieldDecl->get_decl_flags() & TXD_EXPERRBLOCK ) ) {
            auto field = fieldDecl->get_definer()->resolve_field();

            // first screen the fields that are of function type and take the correct number of arguments:
            if ( field->get_type()->get_type_class() == TXTC_FUNCTION ) {
                auto candArgTypes = field->get_type()->argument_types();
                auto arrayArgElemType = field->get_type()->vararg_elem_type();
                const TxType* fixedArrayArgType = nullptr;

                if ( arrayArgElemType ) {
                    // var-arg tail parameter accepts zero or more arguments
                    if ( arguments->size() < candArgTypes.size() - 1 )
                        continue;  // mismatching number of function args
                }
                else if ( ( fixedArrayArgType = field->get_type()->fixed_array_arg_type() ) ) {
                    // fixed array parameter accepts matching number of arguments
                    auto lenExpr = static_cast<const TxArrayType*>( fixedArrayArgType->type() )->length();
                    auto len = eval_UInt_constant( lenExpr );
                    if ( !( arguments->size() == 1 || arguments->size() == len ) )
                        continue;  // mismatching number of function args
                    arrayArgElemType = fixedArrayArgType->element_type();
                }
                else if ( arguments->size() != candArgTypes.size() ) {
                    continue;  // mismatching number of function args
                }

                {
                    //LOG_INFO(entitySymbol->LOGGER(), "Candidate function: " << field->get_type());

                    // next check that the argument types match, and how close they match:
                    uint16_t reint[4] = { 0, 0, 0, 0 };
                    for ( unsigned i = 0; i < argTypes.size(); i++ ) {
                        const TxType* argType = argTypes.at( i );
                        const TxType* argDef = ( arrayArgElemType && i >= candArgTypes.size() - 1 ? arrayArgElemType
                                                                                                    :
                                                                                                    candArgTypes.at( i ) );
                        int degree = get_reinterpretation_degree( argDef, argType );
                        if ( degree < 0 ) {
                            if ( arrayArgElemType && i == candArgTypes.size() - 1 && candArgTypes.size() == arguments->size() ) {
                                // if last provided arg is an array of the correct type, match it against the var-arg tail if present
                                //std::cerr << " cand-arg: " << candArgTypes.at( i ) << "   prov-arg: " << argType << std::endl;
                                degree = get_reinterpretation_degree( candArgTypes.at( i ), argType );
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
    if ( this->baseExpr ) {
        // baseExpr may or may not refer to a type (e.g. modules don't)
        auto baseType = this->baseExpr->resolve_type();

        if ( baseType->get_type_class() == TXTC_REFERENCE ) {
            // implicit dereferencing ('^') operation:
            if ( auto baseRefTargetType = baseType->target_type() ) {
                //std::cerr << "Adding implicit '^' to: " << this->baseExpr << "  six=" << six << std::endl;
                auto derefNode = new TxReferenceDerefNode( this->baseExpr->parseLocation, this->baseExpr );
                derefNode->node_declaration_pass( this ); //set_context( this->baseExpr->context() );  // in lieu of declaration_pass()
                derefNode->symbol_resolution_pass();
                this->baseExpr = derefNode;
                baseType = baseRefTargetType;
            }
        }

        TxScopeSymbol* vantageScope = this->context().scope();
        if ( auto baseSymbolNode = dynamic_cast<TxFieldValueNode*>( this->baseExpr ) ) {
            if ( baseType->get_type_class() == TXTC_VOID ) {
                // base is a non-entity symbol
                if ( auto baseSymbol = baseSymbolNode->resolve_symbol() ) {
                    this->symbol = lookup_member( vantageScope, baseSymbol, *this->symbolName );
                }
            }
            else {
                // base is a type or value expression  FIXME: if type, don't include instance members in lookup
                this->symbol = baseType->lookup_inherited_instance_member( vantageScope, this->symbolName->str() );
            }
        }
        else {
            // base is a value expression
            this->symbol = baseType->lookup_inherited_instance_member( vantageScope, this->symbolName->str() );
        }
    }
    else {
        this->symbol = lookup_symbol( this->context().scope(), *this->symbolName );
    }
    return this->symbol;
}

const TxEntityDeclaration* TxFieldValueNode::resolve_decl() {
    if ( this->declaration )
        return this->declaration;
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
                    if ( auto constructorSymbol = allocType->get_instance_base_type()->get_instance_member( CONSTR_IDENT ) ) { // (constructors aren't inherited)
                        if ( auto constructorDecl = resolve_field( this, constructorSymbol, this->appliedFuncArgs ) ) {
                            ASSERT( constructorDecl->get_decl_flags() & ( TXD_CONSTRUCTOR | TXD_INITIALIZER ),
                                    "field named " CONSTR_IDENT " is not flagged as TXD_CONSTRUCTOR or TXD_INITIALIZER: " << constructorDecl->str() );
                            //std::cerr << "resolving field to constructor: " << this << ": " << constructorDecl << std::endl;
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
                           << "' (base expression type is " << this->baseExpr->get_type() << ")" );
        else
            CERR_THROWRES( this, "Unknown symbol '" << this->get_full_identifier() << "'" );
    }
    // function returns or throws resolution exception before this
    ASSERT( false, "unexpected execution point in " << this );
    return nullptr;
}

const TxType* TxFieldValueNode::define_type() {
    if ( auto decl = this->resolve_decl() ) {
        if ( auto fieldDecl = dynamic_cast<const TxFieldDeclaration*>( decl ) ) {
            this->field = fieldDecl->get_definer()->resolve_field();

            if ( ( fieldDecl->get_storage() == TXS_INSTANCE || fieldDecl->get_storage() == TXS_INSTANCEMETHOD )
                 && !( fieldDecl->get_decl_flags() & ( TXD_CONSTRUCTOR | TXD_INITIALIZER ) ) ) {
                if ( !this->baseExpr ) {
                    CERR_THROWRES( this, "Instance member field referenced without instance base: " << this->get_full_identifier() );
                    return nullptr;
                }
                else {
                    if ( auto baseSymbolNode = dynamic_cast<TxFieldValueNode*>( this->baseExpr ) ) {
                        if ( !baseSymbolNode->get_field() ) {
                            CERR_THROWRES( this, "Instance member field referenced without instance base: " << this->get_full_identifier() );
                            return nullptr;
                        }
                    }
//                    auto baseExprType = this->baseExpr->get_type();
//                    std::cerr << "base expr type: " << baseExprType << std::endl;
//                    auto fieldType = field->get_type();
//                    if ( fieldDecl->get_storage() == TXS_INSTANCEMETHOD ) {
//
//                    }
//                    else if ( fieldType->is_modifiable() ) {
//
//                    }
                }
            }

            return this->field->get_type();
        }
        else
            return static_cast<const TxTypeDeclaration*>( decl )->get_definer()->resolve_type();
    }
    // Symbol is not a field or type, return Void as placeholder type
    return this->registry().get_builtin_type( TXBT_VOID );
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

bool TxFieldValueNode::is_statically_constant() const {
    if ( this->field ) {
        if ( this->field->get_storage() == TXS_INSTANCEMETHOD )
            return false;
        if ( this->field->get_storage() == TXS_VIRTUAL ) {
            // allow a virtual field lookup, with a constant base expression, to behave as a static field lookup (i.e. non-polymorphic),
            // unless it has a non-constant base expression:
            if ( this->baseExpr && !this->baseExpr->is_statically_constant() )
                return false;
        }
        return this->field->is_statically_constant();
    }
    else if ( this->symbol ) {
        return true;
    }
    else
        return false;
}

const TxType* TxConstructorCalleeExprNode::define_type() {
    ASSERT( this->appliedFuncArgs, "appliedFuncArgTypes of TxConstructorCalleeExprNode not initialized" );
    {
        auto allocType = this->objectExpr->resolve_type();
        // find the constructor
        if ( auto constructorSymbol = allocType->get_instance_base_type()->get_instance_member( CONSTR_IDENT ) ) {  // (constructors aren't inherited)
            if ( auto constructorDecl = resolve_field( this, constructorSymbol, this->appliedFuncArgs ) ) {
                ASSERT( constructorDecl->get_decl_flags() & ( TXD_CONSTRUCTOR | TXD_INITIALIZER ),
                        "field named " CONSTR_IDENT " is not flagged as TXD_CONSTRUCTOR or TXD_INITIALIZER: " << constructorDecl->str() );
                this->declaration = constructorDecl;
                auto constructorField = constructorDecl->get_definer()->resolve_field();
                return constructorField->get_type();
            }
        }
        if ( this->appliedFuncArgs->size() == 0 ) {
            // TODO: support default value constructor
        }
        else if ( this->appliedFuncArgs->size() == 1 ) {
            // TODO: support default assignment constructor
        }
        CERR_THROWRES( this, "No matching constructor in type " << allocType
                       << " for args (" << join( resolve_typevec( this->appliedFuncArgs ), ", ") << ")" );
    }
    return nullptr;
}

TxFunctionCallNode::TxFunctionCallNode( const TxLocation& parseLocation, TxExpressionNode* callee,
                                        const std::vector<TxExpressionNode*>* argsExprList )
        : TxExpressionNode( parseLocation ), callee( callee ), origArgsExprList( argsExprList ), argsExprList( make_args_vec( argsExprList ) ) {
    if ( auto fieldValueNode = dynamic_cast<TxFieldValueNode*>( this->callee ) ) {
        // handle direct constructor invocation - self() and super()
        auto identifier = fieldValueNode->get_full_identifier();
        if ( identifier == "self" || identifier == "super" ) {
            auto objectDeref = new TxReferenceDerefNode( this->parseLocation, this->callee );
            this->callee = new TxConstructorCalleeExprNode( this->parseLocation, objectDeref );
            this->isSelfSuperConstructorInvocation = true;
        }
    }
}

void TxFunctionCallNode::declaration_pass() {
    if ( this->isSelfSuperConstructorInvocation ) {
        if ( !( this->context().enclosing_lambda() && this->context().enclosing_lambda()->get_constructed() ) )
            CERROR( this, "self() / super() constructor may only be invoked from within the type's other constructors" );
        // TODO: shall only be legal as first statement within constructor body
    }
}

const TxType* TxFunctionCallNode::define_type() {
    // The resolution here shall resolve to the function signature that *closest* matches the argument types,
    // but also takes automatic (implicit) type conversions into account (if needed).
    // The automatic type conversions thus considered shall then be applied upon function invocation.
    // Prepare for resolving possible function overloading by registering actual function signature with
    // the callee node, BEFORE the callee node type is resolved:
    ASSERT( !this->callee->get_applied_func_args(), "callee already has applied func arg types: " << this->callee );
    this->callee->set_applied_func_args( this->origArgsExprList );
    this->calleeType = this->callee->resolve_type();
    if ( this->calleeType->get_type_class() != TXTC_FUNCTION ) {
        CERR_THROWRES( this, "Callee of function call expression is not of function type: " << this->calleeType );
    }
    else if ( auto constructorType = dynamic_cast<const TxConstructorType*>( this->calleeType->type() ) ) {
        // constructor functions return void but the constructor invocation expression yields the constructed type:
        auto objectDefiner = constructorType->get_constructed_type_decl()->get_definer();
        return objectDefiner->resolve_type();
    }
    else
        return this->calleeType->return_type();
}

void TxFunctionCallNode::symbol_resolution_pass() {
    TxExpressionNode::symbol_resolution_pass();

    auto actualCalleeType = ( this->calleeType ? this->calleeType->type() : nullptr );
    auto constructorType = dynamic_cast<const TxConstructorType*>( actualCalleeType );
    if ( constructorType ) {
        // Stack construction syntactically looks like a function call, e.g. Int(42)
        // If the callee is a constructor, we substitute this function call with a stack construction expression:
        if ( !dynamic_cast<TxConstructorCalleeExprNode*>( this->callee ) ) {  // (prevents infinite recursion)
            auto typeDeclNode = new TxTypeDeclWrapperNode( this->parseLocation, constructorType->get_constructed_type_decl() );
            // Implementation note: Declaration pass is already run on the args, but we need to run it on the new construction node
            // and its new children, and we need to run resolution pass on the whole sub-tree.
            auto wrappedArgs = make_expr_wrapper_vec( this->origArgsExprList );
            this->inlinedExpression = new TxStackConstructionNode( this->parseLocation, typeDeclNode, wrappedArgs );
            run_declaration_pass( this->inlinedExpression, this, "inlinedexpr" );
            for ( auto argExpr : *this->origArgsExprList )
                argExpr->symbol_resolution_pass();
            this->inlinedExpression->symbol_resolution_pass();
            return;
        }
    }

    // Verify arguments and apply implicit conversions if needed:
    if ( this->calleeType && this->calleeType->get_type_class() == TXTC_FUNCTION ) {
        auto calleeArgTypes = this->calleeType->argument_types();
        auto arrayArgElemType = this->calleeType->vararg_elem_type();
        if ( !arrayArgElemType ) {
            if ( auto fixedArrayArgType = this->calleeType->fixed_array_arg_type() ) {
                arrayArgElemType = fixedArrayArgType->element_type();
            }
        }

// this check has already been done in callee resolution
//        if (calleeArgTypes.size() != this->argsExprList->size()
//                && !( varArgElemType && this->argsExprList->size() >= calleeArgTypes.size() - 1 )) {
//            CERROR(this, "Callee of function call expression has mismatching argument count: " << this->calleeType);
//        }
//        else
        {
            if ( arrayArgElemType
                 && !( calleeArgTypes.size() == this->origArgsExprList->size()
                       && get_reinterpretation_degree( calleeArgTypes.back(), this->origArgsExprList->back()->get_type() ) >= 0 ) ) {
                // Calling a var-args function, and last provided arg does not directly match the var-arg tail arg.
                // transform the passed var-args into an array which is passed as the last argument
                unsigned lastCalleeArgIx = calleeArgTypes.size() - 1;
                auto arrayArgs = new std::vector<TxMaybeConversionNode*>();
                for ( unsigned i = lastCalleeArgIx; i < this->argsExprList->size(); i++ ) {
                    arrayArgs->push_back( this->argsExprList->at( i ) );
                }
                this->argsExprList->resize( lastCalleeArgIx );
                const TxLocation& varArgLoc = ( arrayArgs->empty() ? this->parseLocation : arrayArgs->front()->parseLocation );
                auto elemTypeExpr = new TxTypeExprWrapperNode( arrayArgElemType->get_definer() );
                auto arrayArgNode = new TxMaybeConversionNode( new TxArrayLitNode( varArgLoc, elemTypeExpr, arrayArgs ) );
                run_declaration_pass( arrayArgNode, this, "arg" );
                this->argsExprList->push_back( arrayArgNode );
            }
            ASSERT( calleeArgTypes.size() == this->argsExprList->size(), "Mismatching argument count for callee " << this->calleeType );

            for ( unsigned i = 0; i < this->argsExprList->size(); i++ ) {
                auto argExpr = this->argsExprList->at( i );
                auto argDefType = calleeArgTypes.at( i );
                // note: similar rules to assignment
                // TODO: check dataspace rules if function arg is a reference
                argExpr->insert_conversion( argDefType );  // generates compilation error upon mismatch
            }
        }

        auto funcType = static_cast<const TxFunctionType*>( actualCalleeType );
        if ( !constructorType && funcType->modifiable_closure() ) {
            ASSERT( this->callee->get_data_graph_origin_expr(), "Callee with modifiable closere didn't have origin expression: " << this->callee );
            if ( !this->callee->get_data_graph_origin_expr()->check_chain_mutable() ) {
                // error message already generated
                //CERROR( this, "Can't invoke modifying method on immutable closure: " << funcType );
            }
        }
    }

    if ( auto inlineCalleeType = dynamic_cast<const TxInlineFunctionType*>( actualCalleeType ) ) {
        this->inlinedExpression = inlineCalleeType->make_inline_expr( this->callee, this->argsExprList );
    }

    if ( this->inlinedExpression ) {
        this->inlinedExpression->symbol_resolution_pass();
        return;
    }

    callee->symbol_resolution_pass();
    for ( auto argExpr : *this->argsExprList )
        argExpr->symbol_resolution_pass();
}
