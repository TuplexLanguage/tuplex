#include "ast_entitydecls.hpp"

#include "expr/ast_lambda_node.hpp"

#include "llvm_generator.hpp"

using namespace llvm;

/** Convenience function that returns true if type is a pointer to a non-single value type. */
inline bool is_complex_pointer( const Type* type ) {
    bool ret = ( type->isPointerTy() && !type->getPointerElementType()->isSingleValueType() );
    //std::cout << "is_complex_pointer(): " << ret << ": type: " << type << std::endl;
    return ret;
}

void TxTypeDeclNode::code_gen( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    if ( this->context().exp_error() ) {
        LOG_DEBUG( this->LOGGER(), "Skipping codegen for AST of type with ExpErr context: " << this->typeExpression->qualtype() );
        return;
    }
    if ( this->typeExpression->qualtype()->type()->acttype()->is_generic_dependent() ) {  // this->context().is_generic()
        //if ( this->typeExpression->qualtype()->type()->acttype()->has_vtable_id() )
        //    std::cerr << "Skipping code-gen for type with vtable id: " << this->typeExpression->qualtype() << std::endl;
        LOG_DEBUG( context.LOGGER(), "Skipping codegen for AST of generic-dependent type: "
                   << this->typeExpression << " : " << this->typeExpression->qualtype() );
        // Note that this skips codegen for the entire AST of all generic-dependent types,
        // which means none of their members are generated, including any statically declared inner/local types.
        // FUTURE: Evaluate capability for generic types to have global static members (e.g. inner types independent of the outer type parameters).
        return;
    }
    if ( !this->typeExpression->qualtype()->type()->acttype()->has_vtable_id() ) {
        LOG_DEBUG( context.LOGGER(), "Skipping codegen for AST of type without vtable id: "
                    << this->typeExpression << " : " << this->typeExpression->qualtype() );
        return;
    }
    this->typeExpression->code_gen_type( context );
}

static Value* make_constant_nonlocal_field( LlvmGenerationContext& context, const std::string& uniqueName, Constant* constantInitializer, Type* llvmType ) {
    if ( is_complex_pointer( constantInitializer->getType() ) ) {
        context.LOGGER()->note( "Global field %s with complex ptr constant initializer", uniqueName.c_str() );
        // TODO: review and perhaps remove/refactor
        ASSERT( !context.llvmModule().getNamedGlobal( uniqueName ),
                "Can't declare llvm alias since global variable with same name already declared: " << uniqueName );
        return GlobalAlias::create( llvmType, 0, GlobalValue::InternalLinkage, uniqueName,
                                    constantInitializer, &context.llvmModule() );
    }

    // Also handles the case when there has been a "forward-declaration" of this field:
    // Note: If the global exists but has the wrong type: return the function with a constantexpr cast to the right type.
    Constant* maybe = context.llvmModule().getOrInsertGlobal( uniqueName, llvmType );
    //std::cout << "maybe type: " << maybe->getType() << "  value: " << maybe << "  llvmType: " << llvmType << std::endl;
    auto globalV = cast<GlobalVariable>( maybe );  // bails if bitcast has been inserted, which means wrong type has been chosen
    globalV->setConstant( true );
    globalV->setLinkage( GlobalValue::InternalLinkage );
    ASSERT( !globalV->hasInitializer(), "global already has initializer: " << globalV );
    globalV->setInitializer( constantInitializer );
    return globalV;
}

void TxFieldDeclNode::code_gen( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    if ( this->field->typeExpression )
        this->field->typeExpression->code_gen_type( context );

    auto fieldDecl = this->field->get_declaration();
    std::string uniqueName = fieldDecl->get_unique_full_name();
    auto txType = this->field->qualtype()->type();

    Value* fieldVal = nullptr;
    switch ( fieldDecl->get_storage() ) {
    case TXS_INSTANCEMETHOD:
        if ( !( fieldDecl->get_decl_flags() & TXD_ABSTRACT )
             // constructors in generic types are suppressed (they are not abstract per se, but aren't code generated):
             && !( ( fieldDecl->get_decl_flags() & ( TXD_CONSTRUCTOR | TXD_INITIALIZER ) )
                   && static_cast<TxEntitySymbol*>( fieldDecl->get_symbol()->get_outer() )->get_type_decl()->get_definer()->qualtype()->type()->is_generic() ) ) {
            if ( static_cast<TxLambdaExprNode*>( this->field->initExpression->originalExpr )->is_suppressed_modifying_method() ) {
                // modifying instance methods in immutable specializations of generic types are suppressed (as if abstract)
                auto closureType = context.get_llvm_type( txType );
                Type* fieldType = closureType->getStructElementType( 0 );
                fieldVal = Constant::getNullValue( fieldType );
                uniqueName += "$func";
            }
            else {
                ASSERT( this->field->initExpression, "instance method does not have an initializer/definition: " << uniqueName );
                auto initLambdaV = this->field->initExpression->code_gen_const_value( context );
                auto funcPtrV = initLambdaV->getAggregateElement( (unsigned) 0 );
                fieldVal = funcPtrV;  // the naked $func is stored (as opposed to a full lambda object)
                uniqueName = fieldVal->getName();
            }
            context.register_llvm_value( uniqueName, fieldVal );
        }
        return;

    case TXS_GLOBAL:
        if ( fieldDecl->get_decl_flags() & TXD_EXTERNC ) {
            // FIXME: Also support externc field declarations such as stdout
            // create the external C function declaration
            std::string externalName( fieldDecl->get_unique_name() );
            if ( txType->get_type_class() == TXTC_FUNCTION ) {
                LOG_DEBUG( context.LOGGER(), "Codegen for extern-C function declaration '" << externalName << "': " << txType );
                StructType* lambdaT = cast<StructType>( context.get_llvm_type( txType ) );
                FunctionType* externFuncType = cast<FunctionType>( lambdaT->getElementType( 0 )->getPointerElementType() );
                //std::cerr << "Extern-C function type: " << externFuncType << std::endl;
                Function* extern_c_func = Function::Create( externFuncType, GlobalValue::ExternalLinkage, externalName, &context.llvmModule() );
                extern_c_func->setCallingConv( CallingConv::C );

                // construct the lambda object in the Tuplex name space:
                auto nullClosureRefV = Constant::getNullValue( lambdaT->getStructElementType( 1 ) );
                auto lambdaC = ConstantStruct::get( lambdaT, extern_c_func, nullClosureRefV, NULL );
                fieldVal = make_constant_nonlocal_field( context, uniqueName, lambdaC, lambdaT );
            }
            else {
                Type *externFieldT = txType->acttype()->make_llvm_externc_type( context );
                auto externDeclC = cast<GlobalVariable>( context.llvmModule().getOrInsertGlobal( externalName, externFieldT ) );

                // construct the alias in the Tuplex name space:
                if ( auto aliasV = context.llvmModule().getNamedAlias( uniqueName ) ) {
                    // has been forward-declared
                    //std::cerr << "forward-declared externc: " << aliasV << std::endl;
                    aliasV->setAliasee( externDeclC );
                    fieldVal = aliasV;
                }
                else {
                    fieldVal = GlobalAlias::create( GlobalValue::InternalLinkage, uniqueName, externDeclC );
                    //std::cerr << "newly declared externc: " << fieldVal << std::endl;
                }
                //fieldVal = make_constant_nonlocal_field( context, uniqueName, externDeclC, fieldT );
            }

            context.register_llvm_value( uniqueName, fieldVal );
            return;
        }
        // no break
    case TXS_STATIC:
    case TXS_VIRTUAL:
        if ( !( fieldDecl->get_decl_flags() & ( TXD_ABSTRACT | TXD_INITIALIZER ) ) ) {
            if ( this->field->initExpression ) {
                if ( this->field->initExpression->is_statically_constant() ) {
                    Constant* constantInitializer = this->field->code_gen_constant_init_expr( context );
                    Type* llvmType = context.get_llvm_type( txType );
                    fieldVal = make_constant_nonlocal_field( context, uniqueName, constantInitializer, llvmType );
                    context.register_llvm_value( uniqueName, fieldVal );
                    return;
                }
                // TODO: support non-constant initializers for static and virtual fields
            }
            LOG( context.LOGGER(), WARN, "Skipping codegen for global/static/virtual field without constant initializer: "
                 << uniqueName );
        }
        return;

    case TXS_INSTANCE:
        // just a type definition; field storage isn't created until parent object is allocated
        return;

    case TXS_NOSTORAGE:
    case TXS_STACK:
        THROW_LOGIC( "TxFieldDeclNode can not apply to fields with storage " << fieldDecl->get_storage() << ": " << uniqueName );
    }
}


#include "ast_fielddef_node.hpp"

Constant* TxFieldDefiningNode::code_gen_constant_init_expr( LlvmGenerationContext& context ) const {
    if (! this->cachedConstantInitializer) {
        ASSERT( this->get_init_expression() && this->get_init_expression()->is_statically_constant(), "Expected constant initializer in " << this );
        this->cachedConstantInitializer = this->get_init_expression()->code_gen_const_value( context );
    }
    return this->cachedConstantInitializer;
}
