#include "ast_fielddef_node.hpp"
#include "expr/ast_lambda_node.hpp"
#include "symbol/qual_type.hpp"

#include "llvm_generator.hpp"
#include "parsercontext.hpp"

using namespace llvm;


Constant* TxFieldDefiningNode::code_gen_const_init_value( LlvmGenerationContext& context, bool genBody ) const {
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
    return this->field()->get_llvm_value();
}

void TxLocalFieldDefNode::code_gen_field( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    if ( this->typeExpression )
        this->typeExpression->code_gen_type( context );

    ASSERT( this->declaration, "NULL declaration in " << this );
    ASSERT( this->declaration->get_storage() == TXS_STACK, "Local field gen can only apply to TX_STACK storage fields: " << this );

    // If init expression performs a stack allocation (unbound) of this field's type (instance-equivalent type),
    // this field shall bind to that allocation.

    auto type = this->qtype().type();
    Value* fieldPtrV;
    if ( this->initExpression ) {
        // (since we allow initialization of a longer array with a shorter one, we force explicit allocation if can't guarantee types to be exactly equal)
        bool maybeMismatchingArrayLen = type->get_type_class() == TXTC_ARRAY && bool( this->typeExpression );
        if ( this->initExpression->get_storage() == TXS_UNBOUND_STACK && !maybeMismatchingArrayLen ) {
            fieldPtrV = this->initExpression->code_gen_addr( context, scope );
        }
        else {
            // (if storage is TXS_STACK, we allocate new stack space for making copy of already bound stack value)
            fieldPtrV = type->gen_alloca( context, scope, this->declaration->get_symbol()->get_name() );
            // create implicit assignment statement
            if ( type->get_type_class() == TXTC_ARRAY ) {
                ASSERT( !this->cachedConstantInitializer, "Constant initializer value already set when initializing array field " << this );  // how handle?
                Value* initializer = this->initExpression->code_gen_addr( context, scope );
                auto lvalTypeIdC = ConstantInt::get( IntegerType::getInt32Ty( context.llvmContext ), type->get_runtime_type_id() );
                TxAssignStmtNode::code_gen_array_copy( this, context, scope, type, lvalTypeIdC, fieldPtrV, initializer );
            }
            else {
                Value* initializer = this->cachedConstantInitializer;
                if ( !initializer ) {
                    initializer = this->initExpression->code_gen_expr( context, scope );
                    if ( auto constInit = dyn_cast<Constant>( initializer ) )
                        this->cachedConstantInitializer = constInit;
                }
                scope->builder->CreateStore( initializer, fieldPtrV );
            }
        }
    }
    else {
        fieldPtrV = type->gen_alloca( context, scope, this->declaration->get_symbol()->get_name() );
        // We don't automatically invoke default constructor (in future, a code flow validator should check that initialized before first use)
    }
    this->field()->set_llvm_value( fieldPtrV );

    // Create a debug descriptor for the variable:
    auto pos = this->get_declaration()->get_definer()->ploc.begin;
    DILocalVariable *argVarD = context.debug_builder()->createAutoVariable(
            scope->debug_scope(), this->field()->get_unique_name(), this->get_parser_context()->debug_file(),
            pos.line, context.get_debug_type( this->qtype() ) );
    context.debug_builder()->insertDeclare( fieldPtrV, argVarD, context.debug_builder()->createExpression(),
                                            DebugLoc::get( pos.line, pos.column, scope->debug_scope() ),
                                            scope->builder->GetInsertBlock() );
}


Value* TxNonLocalFieldDefNode::code_gen_field_decl( LlvmGenerationContext& context ) const {
    if ( !this->field()->has_llvm_value() ) {
        this->inner_code_gen_field( context, false );
    }
    return this->field()->get_llvm_value();
}

void TxNonLocalFieldDefNode::code_gen_field( LlvmGenerationContext& context ) const {
    if ( !this->field()->has_llvm_value() ) {
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
    auto txType = this->qtype();

    switch ( fieldDecl->get_storage() ) {
    case TXS_INSTANCEMETHOD:
        if ( !( fieldDecl->get_decl_flags() & TXD_ABSTRACT ) ) {
            // constructors in generic types (that are not pure VALUE specializations) are suppressed as if abstract
            // (they are not abstract per se, but aren't code generated):
            auto enclosingType = static_cast<TxEntitySymbol*>( fieldDecl->get_symbol()->get_outer() )
                                   ->get_type_decl()->get_definer()->qtype();
            if ( !( ( fieldDecl->get_decl_flags() & ( TXD_CONSTRUCTOR | TXD_INITIALIZER ) ) && enclosingType->is_type_generic() ) ) {
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
                    //fieldVal = initLambdaV;
                    auto funcPtrV = initLambdaV->getAggregateElement( (unsigned) 0 );
                    fieldVal = funcPtrV;  // the naked $func is stored (as opposed to a full lambda object)
                }
                this->field()->set_llvm_value( fieldVal );
            }
        }
        return;

    case TXS_INSTANCE:
        if ( !( fieldDecl->get_decl_flags() & ( TXD_CONSTRUCTOR | TXD_INITIALIZER | TXD_GENPARAM | TXD_GENBINDING ) ) ) {
            // just a type definition; field storage isn't created until parent object is allocated
            return;
        }
        // no break

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
                auto lambdaC = ConstantStruct::get( lambdaT, extern_c_func, nullClosureRefV );
                this->field()->set_llvm_value( make_constant_nonlocal_field( context, lambdaT, lambdaC, uniqueName ) );
            }
            else {
                // create the external C field declaration
                Type *externFieldT = txType->make_llvm_externc_type( context );
                auto externDeclC = cast<GlobalVariable>( context.llvmModule().getOrInsertGlobal( externalName, externFieldT ) );
                this->field()->set_llvm_value( externDeclC );
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
                    this->field()->set_llvm_value( make_constant_nonlocal_field( context, context.get_llvm_type( txType ),
                                                                                     constantInitializer, uniqueName ) );
                    return;
                }
                // FUTURE: support non-constant initializers for static and virtual fields
            }
            LOG( context.LOGGER(), WARN, "Skipping codegen for global/static/virtual field without constant initializer: " << fieldDecl );
        }
        return;

    default:
        THROW_LOGIC( "TxFieldDeclNode can not apply to fields with storage " << fieldDecl->get_storage() << ": " << fieldDecl );
    }
}
