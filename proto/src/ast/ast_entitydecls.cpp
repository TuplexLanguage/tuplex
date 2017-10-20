#include "ast_entitydecls.hpp"
#include "ast/expr/ast_lambda_node.hpp"
#include "builtin/builtin_types.hpp"

#include "parsercontext.hpp"

void TxFieldDeclNode::declaration_pass() {
    TxDeclarationFlags flags = this->get_decl_flags();

    if ( this->fieldDef->initExpression ) {
        if ( flags & TXD_EXTERNC )
            CERROR( this, "'externc' is invalid modifier for field / method that has an initializer / body: " << this->fieldDef->initExpression );
        if ( flags & TXD_ABSTRACT )
            CERROR( this, "'abstract' is invalid modifier for field / method that has an initializer / body: " << this->fieldDef->initExpression );
    }
//    else if ( flags & TXD_EXTERNC ) {
//        // create the FFI wrapper
//        initExpression = new TxLambdaExprNode(...);
//        initExpression->set_field_def_node( this );
//        this->field->initExpression = new TxMaybeConversionNode( initExpression );
//    }

    TxFieldStorage storage;
    const TxTypeDeclaration* outerTypeDecl = nullptr;
    if ( auto entitySymbol = dynamic_cast<TxEntitySymbol*>( this->context().scope() ) )
        outerTypeDecl = entitySymbol->get_type_decl();

    if ( this->isMethodSyntax && outerTypeDecl ) {
        // Note: instance method storage is handled specially (technically the function pointer is a virtual/static field)

        if ( flags & TXD_EXTERNC )
            CERROR( this, "'externc' is not a valid modifier for a method: " << this->fieldDef->get_descriptor() );

        TxLambdaExprNode* lambdaExpr = nullptr;
        if ( auto initExpr = dynamic_cast<TxMaybeConversionNode*>( this->fieldDef->initExpression ) )
            lambdaExpr = dynamic_cast<TxLambdaExprNode*>( initExpr->originalExpr );

        if ( !lambdaExpr && !( flags & TXD_ABSTRACT ) )
            CERROR( this, "Missing modifier 'abstract' for method that has no body" );

        if ( flags & TXD_VIRTUAL ) {
            storage = TXS_VIRTUAL;
        }
        else {
            if ( lambdaExpr )
                lambdaExpr->set_instance_method( true );
            storage = TXS_INSTANCEMETHOD;
        }
    }
    else if ( dynamic_cast<TxModule*>( lexContext.scope() ) ) {  // if in global scope
        if ( flags & TXD_VIRTUAL )
            CERROR( this, "'virtual' is invalid modifier for module scope field " << this->fieldDef->get_descriptor() );
        if ( flags & TXD_FINAL )
            CERROR( this, "'final' is invalid modifier for module scope field " << this->fieldDef->get_descriptor() );
        if ( flags & TXD_OVERRIDE )
            CERROR( this, "'override' is invalid modifier for module scope field " << this->fieldDef->get_descriptor() );
        if ( flags & TXD_ABSTRACT )
            CERROR( this, "'abstract' is invalid modifier for module scope field " << this->fieldDef->get_descriptor() );
        storage = TXS_GLOBAL;
    }
    else {
        if ( flags & TXD_EXTERNC )
            CERROR( this, "'externc' is not a valid modifier for a non-global field: " << this->fieldDef->get_descriptor() );
        if ( flags & TXD_ABSTRACT ) {
            if ( !( flags & TXD_VIRTUAL ) )
                CERROR( this, "'abstract' fields must also be declared 'virtual': " << this->fieldDef->get_descriptor() );
            if ( !( flags & ( TXD_PROTECTED | TXD_PUBLIC ) ) )
                CERROR( this, "'abstract' fields cannot be private (since private are non-virtual): " << this->fieldDef->get_descriptor() );
        }
        storage = ( flags & TXD_VIRTUAL ) ? TXS_VIRTUAL : TXS_INSTANCE;
    }

    // TXS_VIRTUAL may be changed to TXS_STATIC depending on context:
    if ( storage == TXS_VIRTUAL
         && ( !( flags & ( TXD_PUBLIC | TXD_PROTECTED ) )          // private fields are static
              || ( flags & TXD_INITIALIZER )                       // initializers are static
              || ( ( flags & ( TXD_OVERRIDE | TXD_FINAL ) ) == TXD_FINAL ) // if final but doesn't override, its effectively static
         ) ) {
        storage = TXS_STATIC;
        // Note: If declared virtual, the virtual declaration flag is still set on this declaration
    }

    std::string declName = this->fieldDef->fieldName->str();
    if ( declName == "self" ) {
        // handle constructor declaration
        if ( storage != TXS_INSTANCEMETHOD )
            CERROR( this, "Illegal declaration name for non-constructor member: " << declName );
        declName = CONSTR_IDENT;
        flags = flags | TXD_CONSTRUCTOR;
    }
    else if ( declName == CONSTR_IDENT ) {  // built-in
        ASSERT( flags & TXD_BUILTIN, "Built-in flag not set: " << flags << " at " << this << " in " << this->context().scope() );
        if ( flags & TXD_INITIALIZER ) {
            ASSERT( storage == TXS_STATIC,
                    "Initializer not a static field: " << storage << " at " << this << " in " << this->context().scope() );
        }
        else {
            ASSERT( flags & TXD_CONSTRUCTOR, "Constructor flag not set: " << flags << " at " << this << " in " << this->context().scope() );
            ASSERT( storage == TXS_INSTANCEMETHOD,
                    "Constructor not an instance method: " << storage << " at " << this << " in " << this->context().scope() );
        }
    }

    this->fieldDef->declare_field( declName, lexContext.scope(), flags, storage );
    // Note: Field is processed in the 'outer' scope and not in the 'inner' scope of its declaration.
}

void TxFieldDeclNode::verification_pass() const {
    auto type = this->fieldDef->attempt_qtype();
    if ( !type ) {
        LOG_TRACE( this->LOGGER(), "Skipping verification for " << this << " due to earlier resolution failure" );
        return;
    }
    auto storage = this->fieldDef->get_declaration()->get_storage();
    switch ( storage ) {
    case TXS_INSTANCE:
        // FUTURE: ensure TXS_INSTANCE fields are initialized either here or in every constructor
        if ( this->fieldDef->initExpression ) {
            if ( !( this->fieldDef->get_declaration()->get_decl_flags() & TXD_GENBINDING ) )  // hackish... skips tx.Array.C
                CWARNING( this, "Not yet supported: Inline initializer for instance fields (initialize within constructor instead): "
                          << this->fieldDef->get_descriptor() );
        }
        if ( type.is_modifiable() ) {
            if ( auto entitySymbol = dynamic_cast<TxEntitySymbol*>( this->context().scope() ) ) {
                const TxTypeDeclaration* outerTypeDecl = entitySymbol->get_type_decl();
                if ( !outerTypeDecl->get_definer()->qtype()->is_mutable() ) {
                    if ( !this->context().reinterpretation_definer() ) {
                        // (suppressed if this is a specialization)
                        CERROR( this, "Instance field of an immutable type is declared modifiable: " << fieldDef->get_descriptor() );
                    }
                }
            }
        }
        break;
    case TXS_INSTANCEMETHOD:
        if ( this->fieldDef->initExpression ) {
            auto lambdaExpr = static_cast<TxLambdaExprNode*>( static_cast<TxMaybeConversionNode*>( fieldDef->initExpression )->originalExpr );
            if ( lambdaExpr->funcHeaderNode->is_modifying() ) {
                if ( auto entitySymbol = dynamic_cast<TxEntitySymbol*>( this->context().scope() ) ) {
                    const TxTypeDeclaration* outerTypeDecl = entitySymbol->get_type_decl();
                    if ( !outerTypeDecl->get_definer()->qtype()->is_mutable() ) {
                        if ( !this->context().reinterpretation_definer() ) {
                            // (we skip this error for type specializations that have not been declared mutable, this method will be suppressed)
                            CERROR( this, "Instance method of an immutable type may not be declared modifying: " << fieldDef->get_descriptor() );
                        }
                    }
                }
            }
        }
        // no break
    case TXS_VIRTUAL:
        if ( !this->fieldDef->initExpression ) {
            if ( !( this->fieldDef->get_declaration()->get_decl_flags() & TXD_ABSTRACT ) )
                if ( this->fieldDef->fieldName->str() != "$adTypeId" )
                    CERROR( this, "Non-abstract virtual fields/methods must have an initializer: " << this->fieldDef->get_descriptor() );
            // FUTURE: When static initializers in types are supported, static/virtual fields' initialization may be deferred.
        }
        else {
            if ( !this->fieldDef->initExpression->is_statically_constant() )
                CERROR( this, "Non-constant initializer for virtual field " << this->fieldDef->fieldName );
        }
        break;
    case TXS_GLOBAL:
        if ( type.is_modifiable() ) {
            CERROR( this, "Global fields may not be modifiable: " << fieldDef->get_descriptor() );
        }
        // no break
    case TXS_STATIC:
        if ( !this->fieldDef->initExpression ) {
            if ( !( this->fieldDef->get_declaration()->get_decl_flags() & ( TXD_BUILTIN | TXD_EXTERNC ) ) )
                CERROR( this, "Global/static fields must have an initializer: " << this->fieldDef->get_descriptor() );
            // FUTURE: When static initializers in types are supported, static/virtual fields' initialization may be deferred.
        }
        else {
            // field is expected to have a statically constant initializer
            if ( !this->fieldDef->initExpression->is_statically_constant() )
                CERROR( this, "Non-constant initializer for global/static field " << this->fieldDef->fieldName );
        }
        break;
    default:
        // Note: TXS_STACK is not declared via this node
        CERROR( this, "Invalid storage type in field declaration: " << this->fieldDef );
    }
}

TxTypeDeclNode::TxTypeDeclNode( const TxLocation& ploc, const TxDeclarationFlags declFlags, const std::string& typeName,
                const std::vector<TxDeclarationNode*>* typeParamDecls, TxTypeCreatingNode* typeCreatingNode,
                bool interfaceKW, bool mutableType )
        : TxDeclarationNode( ploc, declFlags ), typeName( new TxIdentifier( typeName ) ),
          interfaceKW( interfaceKW ), mutableType( mutableType ), typeParamDecls( typeParamDecls ), typeCreatingNode( typeCreatingNode ) {
    typeCreatingNode->set_interface( interfaceKW );
    if ( mutableType || interfaceKW ) {  // (interfaces are implicitly mutable)
        typeCreatingNode->set_requires_mutable( true );
        if ( this->typeParamDecls ) {
            for ( auto pdecl : *this->typeParamDecls ) {
                if ( auto tpdecl = dynamic_cast<TxTypeDeclNode*>( pdecl ) ) {
                    if ( tpdecl->get_decl_flags() & TXD_GENPARAM )
                        tpdecl->typeCreatingNode->set_requires_mutable( true );
                    // Note: It should be legal to specify an immutable type as constraint type for a type parameter of a mutable type.
                    //       Will the above cause spurious errors in such a case?
                }
            }
        }
    }
}

void TxTypeDeclNode::declaration_pass() {
    const TxTypeDeclaration* declaration = nullptr;
    if ( this->get_decl_flags() & TXD_BUILTIN ) {
        if ( auto entSym = dynamic_cast<const TxEntitySymbol*>( lexContext.scope()->get_member_symbol( this->typeName->str() ) ) ) {
            declaration = entSym->get_type_decl();
            if ( declaration && ( declaration->get_decl_flags() & TXD_BUILTIN ) ) {
                //std::cerr << "existing builtin type declaration: " << declaration << "  new type expr: " << this->typeExpression << std::endl;
                ASSERT( dynamic_cast<TxDerivedTypeNode*>( this->typeCreatingNode ),
                        "Expected definer for builtin-type to be a TxDerivedTypeNode: " << this->typeCreatingNode );
                merge_builtin_type_definers( static_cast<TxDerivedTypeNode*>( this->typeCreatingNode ), declaration->get_definer() );
                this->_builtinCode = true;
            }
        }
        if ( !this->_builtinCode && !this->get_parser_context()->is_internal_builtin() )
            CERROR( this, "Declaration qualifier 'builtin' used for a non-builtin type: " << this->typeName );
    }

    if ( !this->_builtinCode ) {
        declaration = lexContext.scope()->declare_type( this->typeName->str(), this->typeCreatingNode, this->get_decl_flags() );
        if ( !declaration ) {
            CERROR( this, "Failed to declare type " << this->typeName );
            return;
        }
        this->_declaration = declaration;
        LOG_TRACE( this->LOGGER(), this << ": Declared type " << declaration );
    }

    if ( !lexContext.is_generic() && this->typeParamDecls ) {
        for ( auto paramDeclNode : *this->typeParamDecls ) {
            if ( paramDeclNode->get_decl_flags() & TXD_GENPARAM ) {
                // Note: This identities a generic type declaration, but not specializations whose bindings are generic-dependent
                if ( dynamic_cast<TxTypeDeclNode*>( paramDeclNode ) ) {
                    this->lexContext.generic = true;
                    break;
                }
            }
        }
    }
    this->lexContext._scope = declaration->get_symbol();
    this->typeCreatingNode->set_declaration( declaration );
}

const std::vector<const TxEntityDeclaration*>& TxTypeDeclNode::get_type_params() const {
    if ( !this->typeParams.empty() || this->typeParamDecls->empty() )
        return this->typeParams;  // no type parameters declared
    for ( auto paramDeclNode : *this->typeParamDecls ) {
        if ( paramDeclNode->get_decl_flags() & TXD_GENPARAM )
            this->typeParams.push_back( paramDeclNode->get_declaration() );
    }
    return this->typeParams;
}

void TxTypeDeclNode::verification_pass() const {
    if ( auto qtype = this->typeCreatingNode->attempt_qtype() ) {
        if ( qtype->is_initialized() ) {
            if ( this->interfaceKW ) {
                if ( this->typeCreatingNode->qtype()->get_type_class() != TXTC_INTERFACE )
                    CERROR( this, "Interface type cannot derive from non-interface type: " << this->typeCreatingNode->qtype() );
            }
            else {
                if ( this->typeCreatingNode->qtype()->get_type_class() == TXTC_INTERFACE )
                    if ( !( this->get_decl_flags() & ( TXD_GENPARAM | TXD_GENBINDING | TXD_IMPLICIT ) ) )
                        //&& !this->typeExpression->get_type()->is_modifiable() )
                        CWARNING( this, "Interface type not declared with 'interface' keyword: " << this->typeCreatingNode->qtype() );
            }
        }
    }
}

void TxExpErrDeclNode::declaration_pass() {
    this->lexContext.expErrCtx = this->expError;
    if ( this->body ) {
        if ( !this->context().is_reinterpretation() ) {
            this->get_parse_location().parserCtx->register_exp_err_node( this );
        }
    }
}
