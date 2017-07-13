#include "ast_fielddef_node.hpp"
#include "expr/ast_lambda_node.hpp"
#include "symbol/qual_type.hpp"

#include "llvm_generator.hpp"

using namespace llvm;


Constant* TxFieldDefNode::code_gen_const_init_value( LlvmGenerationContext& context, bool genBody ) const {
    if (! this->cachedConstantInitializer) {
        ASSERT( this->initExpression && this->initExpression->is_statically_constant(), "Expected constant initializer in " << this );
        if ( !genBody ) {
            if ( auto lambdaExpr = dynamic_cast<TxLambdaExprNode*>( this->initExpression->originalExpr ) ) {
                this->cachedConstantInitializer = lambdaExpr->code_gen_const_decl( context );
                return this->cachedConstantInitializer;
            }
        }
        this->cachedConstantInitializer = this->initExpression->code_gen_const_value( context );

    }
    else if ( genBody ) {
        if ( dynamic_cast<TxLambdaExprNode*>( this->initExpression->originalExpr ) )
            this->initExpression->code_gen_const_value( context );
    }
    return this->cachedConstantInitializer;
}


Value* TxLocalFieldDefNode::code_gen_field_decl( LlvmGenerationContext& context ) const {
    return this->get_field()->get_llvm_value();
}

void TxLocalFieldDefNode::code_gen_field( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    if ( this->typeExpression )
        this->typeExpression->code_gen_type( context );

    ASSERT( this->declaration, "NULL declaration in " << this );
    ASSERT( this->declaration->get_storage() == TXS_STACK, "Local field gen can only apply to TX_STACK storage fields: " << this );

    // If init expression does a stack allocation of this field's type (instance-equivalent type),
    // this field shall bind to that allocation.

    auto acttype = this->qualtype()->type()->acttype();
    Value* fieldPtrV;
    if ( this->initExpression ) {
        if ( this->initExpression->is_stack_allocation_expression() ) {
            fieldPtrV = this->initExpression->code_gen_addr( context, scope );
        }
        else {
            fieldPtrV = acttype->gen_alloca( context, scope, this->declaration->get_symbol()->get_name() );
            // create implicit assignment statement
            if ( this->cachedConstantInitializer )
                scope->builder->CreateStore( this->cachedConstantInitializer, fieldPtrV );
            else {
                Value* initializer = this->initExpression->code_gen_expr( context, scope );
                if ( auto constInit = dyn_cast<Constant>( initializer ) )
                    this->cachedConstantInitializer = constInit;
                scope->builder->CreateStore( initializer, fieldPtrV );
            }
        }
    }
    else {
        fieldPtrV = acttype->gen_alloca( context, scope, this->declaration->get_symbol()->get_name() );
        // We don't automatically invoke default constructor (in future, a code flow validator should check that initialized before first use)
    }
    this->get_field()->set_llvm_value( fieldPtrV );
}


Value* TxNonLocalFieldDefNode::code_gen_field_decl( LlvmGenerationContext& context ) const {
    if ( get_node_id()==16913)
        std::cerr<<"here " << this << std::endl;
    if ( !this->get_field()->has_llvm_value() ) {
        this->inner_code_gen_field( context, false );
    }
    return this->get_field()->get_llvm_value();
}

void TxNonLocalFieldDefNode::code_gen_field( LlvmGenerationContext& context ) const {
    if ( !this->get_field()->has_llvm_value() ) {
        this->inner_code_gen_field( context, true );
    }
    else if (this->initExpression ) {
        if ( dynamic_cast<TxLambdaExprNode*>( this->initExpression->originalExpr ) )
            this->initExpression->code_gen_const_value( context );
    }
}


static Value* make_constant_nonlocal_field( LlvmGenerationContext& context, Type* llvmType, Constant* constantInitializer,
                                            const std::string& name ) {
    // Also handles the case when there has been a "forward-declaration" of this field:
    // Note: If the global exists but has the wrong type: return the function with a constantexpr cast to the right type.
    Constant* maybe = context.llvmModule().getOrInsertGlobal( name, llvmType );
    //std::cout << "maybe type: " << maybe->getType() << "  value: " << maybe << "  llvmType: " << llvmType << std::endl;
    auto globalV = cast<GlobalVariable>( maybe );  // bails if bitcast has been inserted, which means wrong type has been chosen
    globalV->setConstant( true );
    globalV->setLinkage( GlobalValue::InternalLinkage );
    ASSERT( !globalV->hasInitializer(), "global already has initializer: " << globalV );
    globalV->setInitializer( constantInitializer );
    return globalV;
}

void TxNonLocalFieldDefNode::inner_code_gen_field( LlvmGenerationContext& context, bool genBody ) const {
    TRACE_CODEGEN( this, context );
    if ( this->typeExpression )
        this->typeExpression->code_gen_type( context );

    auto fieldDecl = this->get_declaration();
    auto uniqueName = fieldDecl->get_unique_full_name();
    auto txType = this->qualtype()->type();

    switch ( fieldDecl->get_storage() ) {
    case TXS_INSTANCEMETHOD:
        if ( !( fieldDecl->get_decl_flags() & TXD_ABSTRACT ) ) {
            // constructors in generic types (that are not pure VALUE specializations) are suppressed as if abstract
            // (they are not abstract per se, but aren't code generated):
            auto enclosingType = static_cast<TxEntitySymbol*>( fieldDecl->get_symbol()->get_outer() )
                                   ->get_type_decl()->get_definer()->qualtype()->type();
            if ( !( ( fieldDecl->get_decl_flags() & ( TXD_CONSTRUCTOR | TXD_INITIALIZER ) )
                    && enclosingType->is_type_generic() ) ) {
            Value* fieldVal = nullptr;
            if ( static_cast<TxLambdaExprNode*>( this->initExpression->originalExpr )->is_suppressed_modifying_method() ) {
                // modifying instance methods in immutable specializations of generic types are suppressed (as if abstract)
                auto closureType = context.get_llvm_type( txType );
                Type* fieldType = closureType->getStructElementType( 0 );
                fieldVal = Constant::getNullValue( fieldType );
            }
            else {
                ASSERT( this->initExpression, "instance method does not have an initializer/definition: " << fieldDecl->get_unique_full_name() );
                auto initLambdaV = this->code_gen_const_init_value( context, genBody );
                auto funcPtrV = initLambdaV->getAggregateElement( (unsigned) 0 );
                fieldVal = funcPtrV;  // the naked $func is stored (as opposed to a full lambda object)
            }
            this->get_field()->set_llvm_value( fieldVal );
            }
        }
        return;

    case TXS_GLOBAL:
        if ( fieldDecl->get_decl_flags() & TXD_EXTERNC ) {
            // Note: External declaration, no initialization expression.
            std::string externalName( fieldDecl->get_unique_name() );
            if ( txType->get_type_class() == TXTC_FUNCTION ) {
                // create the external C function declaration
                LOG_DEBUG( context.LOGGER(), "Codegen for extern-C function declaration '" << externalName << "': " << txType );
                StructType* lambdaT = cast<StructType>( context.get_llvm_type( txType ) );
                FunctionType* externFuncType = cast<FunctionType>( lambdaT->getElementType( 0 )->getPointerElementType() );
                Function* extern_c_func = Function::Create( externFuncType, GlobalValue::ExternalLinkage, externalName, &context.llvmModule() );
                extern_c_func->setCallingConv( CallingConv::C );

                // construct the lambda object (a Tuplex object in Tuplex name space):
                auto nullClosureRefV = Constant::getNullValue( lambdaT->getStructElementType( 1 ) );
                auto lambdaC = ConstantStruct::get( lambdaT, extern_c_func, nullClosureRefV, NULL );
                this->get_field()->set_llvm_value( make_constant_nonlocal_field( context, lambdaT, lambdaC, uniqueName ) );
            }
            else {
                // create the external C field declaration
                Type *externFieldT = txType->acttype()->make_llvm_externc_type( context );
                auto externDeclC = cast<GlobalVariable>( context.llvmModule().getOrInsertGlobal( externalName, externFieldT ) );
                this->get_field()->set_llvm_value( externDeclC );
            }

            return;
        }
        // no break
    case TXS_STATIC:
    case TXS_VIRTUAL:
        if ( !( fieldDecl->get_decl_flags() & ( TXD_ABSTRACT | TXD_INITIALIZER ) ) ) {
            if ( this->initExpression ) {
                if ( this->initExpression->is_statically_constant() ) {
                    Constant* constantInitializer = this->code_gen_const_init_value( context, genBody );
                    this->get_field()->set_llvm_value( make_constant_nonlocal_field( context, context.get_llvm_type( txType ),
                                                                                     constantInitializer, uniqueName ) );
                    return;
                }
                // TODO: support non-constant initializers for static and virtual fields
            }
            LOG( context.LOGGER(), WARN, "Skipping codegen for global/static/virtual field without constant initializer: " << uniqueName );
        }
        return;

    case TXS_INSTANCE:
        // just a type definition; field storage isn't created until parent object is allocated
        return;

    case TXS_NOSTORAGE:
    case TXS_STACK:
        THROW_LOGIC( "TxFieldDeclNode can not apply to fields with storage " << fieldDecl->get_storage() << ": " << fieldDecl );
    }
}
