#include "ast/expr/ast_lambda_node.hpp"
#include "ast_entitydecls.hpp"
#include "builtin/builtin_types.hpp"

#include "parsercontext.hpp"

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
        // Note: instance method storage is handled specially (technically the function pointer is a virtual/static field)

        TxLambdaExprNode* lambdaExpr = nullptr;
        if ( auto initExpr = dynamic_cast<TxMaybeConversionNode*>( field->initExpression ) )
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
            CERROR( this, "'virtual' is invalid modifier for module scope field " << this->field->get_descriptor() );
        if ( flags & TXD_FINAL )
            CERROR( this, "'final' is invalid modifier for module scope field " << this->field->get_descriptor() );
        if ( flags & TXD_OVERRIDE )
            CERROR( this, "'override' is invalid modifier for module scope field " << this->field->get_descriptor() );
        if ( flags & TXD_ABSTRACT )
            CERROR( this, "'abstract' is invalid modifier for module scope field " << this->field->get_descriptor() );
        storage = TXS_GLOBAL;
    }
    else {
        if ( flags & TXD_ABSTRACT ) {
            if ( !( flags & TXD_VIRTUAL ) )
                CERROR( this, "'abstract' fields must also be declared 'virtual': " << this->field->get_descriptor() );
            if ( !( flags & ( TXD_PROTECTED | TXD_PUBLIC ) ) )
                CERROR( this, "'abstract' fields cannot be private (since private are non-virtual): " << this->field->get_descriptor() );
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

    auto type = this->field->qualtype();
    auto storage = this->field->get_declaration()->get_storage();
    if ( type->is_modifiable() ) {
        if ( storage == TXS_GLOBAL )
            CERROR( this, "Global fields may not be modifiable: " << field->get_descriptor() );
    }

    switch ( storage ) {
    case TXS_INSTANCE:
        // FUTURE: ensure TXS_INSTANCE fields are initialized either here or in every constructor
        if ( this->field->initExpression ) {
            if ( !( this->field->get_declaration()->get_decl_flags() & TXD_GENBINDING ) )  // hackish... skips tx.Array.C
                CWARNING( this, "Not yet supported: Inline initializer for instance fields (initialize within constructor instead): "
                          << this->field->get_descriptor() );
        }
        if ( type->is_modifiable() ) {
            if ( auto entitySymbol = dynamic_cast<TxEntitySymbol*>( this->context().scope() ) ) {
                const TxTypeDeclaration* outerTypeDecl = entitySymbol->get_type_decl();
                if ( !outerTypeDecl->get_definer()->qualtype()->type()->is_mutable() ) {
                    if ( !this->context().reinterpretation_definer() ) {
                        // (suppressed if this is a specialization)
                        CERROR( this, "Instance field of an immutable type is declared modifiable: " << field->get_descriptor() );
                    }
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
                    if ( !outerTypeDecl->get_definer()->qualtype()->type()->is_mutable() ) {
                        if ( !this->context().reinterpretation_definer() ) {
                            // (we skip this error for type specializations that have not been declared mutable, this method will be suppressed)
                            CERROR( this, "Instance method of an immutable type may not be declared modifying: " << field->get_descriptor() );
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
                    CERROR( this, "Non-abstract virtual fields/methods must have an initializer: " << this->field->get_descriptor() );
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
                CERROR( this, "Global/static fields must have an initializer: " << this->field->get_descriptor() );
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

void TxTypeDeclNode::symbol_resolution_pass() {
    if ( this->_builtinCode ) {
        // the definer has been merged with the built-in type
        return;
    }
    if ( this->typeParamDecls ) {
        for ( auto paramDeclNode : *this->typeParamDecls )
            paramDeclNode->symbol_resolution_pass();
    }
    try {
        this->typeExpression->symbol_resolution_pass();
    }
    catch ( const resolution_error& err ) {
        LOG( this->LOGGER(), DEBUG, "Caught resolution error in " << this->typeExpression << ": " << err );
        return;
    }
    if (this->interfaceKW) {
        if (this->typeExpression->qualtype()->get_type_class() != TXTC_INTERFACE)
            CERROR(this, "Interface type cannot derive from non-interface type: " << this->typeExpression->qualtype());
    }
    else {
        if (this->typeExpression->qualtype()->get_type_class() == TXTC_INTERFACE)
            if ( !( this->get_decl_flags() & ( TXD_GENPARAM | TXD_GENBINDING | TXD_IMPLICIT ) ) )
                 //&& !this->typeExpression->get_type()->is_modifiable() )
                CWARNING(this, "Interface type not declared with 'interface' keyword: " << this->typeExpression->qualtype());
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
