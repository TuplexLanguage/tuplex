#include "ast_base.hpp"
#include "llvm_generator.hpp"

#include <llvm/Analysis/ConstantFolding.h>

using namespace llvm;

/** Convenience function that returns true if type is a pointer to a non-single value type. */
inline bool is_complex_pointer( const Type* type ) {
    bool ret = ( type->isPointerTy() && !type->getPointerElementType()->isSingleValueType() );
    //std::cout << "is_complex_pointer(): " << ret << ": type: " << type << std::endl;
    return ret;
}

void TxParsingUnitNode::code_gen( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    this->module->code_gen( context );
}

void TxModuleNode::code_gen( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    if ( this->members ) {
        for ( auto mem : *this->members )
            mem->code_gen( context );
    }
    if ( this->subModules ) {
        for ( auto mod : *this->subModules )
            mod->code_gen( context );
    }
}

void TxTypeDeclNode::code_gen( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    if ( this->context().exp_error() ) {
        LOG_DEBUG( this->LOGGER(), "Skipping codegen for AST of type with ExpErr context: " << this->typeExpression->get_type() );
        return;
    }
    if ( this->context().is_generic() ) {
        LOG_DEBUG( context.LOGGER(), "Skipping codegen for AST of generic-dependent type: "
                   << this->typeExpression << " : " << this->typeExpression->get_type() );
        // Note that this skips codegen for the entire AST of all generic-dependent types,
        // which means none of their members are generated, including any statically declared inner/local types.
        // FUTURE: Evaluate capability for generic types to have global static members (e.g. inner types independent of the outer type parameters).
        return;
    }
    this->typeExpression->code_gen_type( context );
}

static Value* make_constant_nonlocal_field( LlvmGenerationContext& context, const std::string& uniqueName, Constant* constantInitializer, Type* llvmType ) {
    if ( is_complex_pointer( constantInitializer->getType() ) ) {
        context.LOGGER()->note( "Global field %s with complex ptr constant initializer", uniqueName.c_str() );
        // TODO: review and perhaps remove/refactor
        ASSERT( !context.llvmModule.getNamedGlobal( uniqueName ),
                "Can't declare llvm alias since global variable with same name already declared: " << uniqueName );
        return GlobalAlias::create( llvmType, 0, GlobalValue::InternalLinkage, uniqueName,
                                    constantInitializer, &context.llvmModule );
    }

    // handle case when there has been a "forward-declaration" of this field:
    Constant* maybe = context.llvmModule.getOrInsertGlobal( uniqueName, llvmType );
    //std::cout << "maybe type: " << *maybe->getType() << "  value: " << *maybe << std::endl;
    auto globalV = cast<GlobalVariable>( maybe );  // bails if bitcast has been inserted, which means wrong type has been chosen
    globalV->setConstant( true );
    globalV->setLinkage( GlobalValue::InternalLinkage );
    ASSERT( !globalV->hasInitializer(), "global already has initializer: " << globalV );
    globalV->setInitializer( constantInitializer );
    return globalV;
    //context.LOG.alert("Global field %s", fullName.to_string().c_str());
    //return new GlobalVariable(context.llvmModule, llvmType, true, GlobalValue::InternalLinkage,
    //                                constantInitializer, fullName.to_string());
}

void TxFieldDeclNode::code_gen( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    if ( this->field->typeExpression )
        this->field->typeExpression->code_gen_type( context );

    auto fieldDecl = this->field->get_declaration();
    std::string uniqueName = fieldDecl->get_unique_full_name();
    auto txType = this->field->get_type()->type();

    Value* fieldVal = nullptr;
    switch ( fieldDecl->get_storage() ) {
    case TXS_INSTANCEMETHOD:
        if ( !( fieldDecl->get_decl_flags() & TXD_ABSTRACT )
             // constructors in generic types are suppressed (they are not abstract per se, but aren't code generated):
             && !( ( fieldDecl->get_decl_flags() & ( TXD_CONSTRUCTOR | TXD_INITIALIZER ) )
                   && static_cast<TxEntitySymbol*>( fieldDecl->get_symbol()->get_outer() )->get_type_decl()->get_definer()->get_type()->is_generic() ) ) {
            if ( static_cast<TxLambdaExprNode*>( this->field->initExpression->originalExpr )->is_suppressed_modifying_method() ) {
                // modifying instance methods in immutable specializations of generic types are suppressed (as if abstract)
                auto closureType = context.get_llvm_type( txType );
                Type* fieldType = closureType->getStructElementType( 0 );
                fieldVal = Constant::getNullValue( fieldType );
                uniqueName += "$func";
            }
            else {
                ASSERT( this->field->initExpression, "instance method does not have an initializer/definition: " << uniqueName );
                auto initLambdaV = this->field->initExpression->code_gen_constant( context );
                auto funcPtrV = initLambdaV->getAggregateElement( (unsigned) 0 );
                fieldVal = funcPtrV;  // the naked $func is stored (as opposed to a full lambda object)
                uniqueName = fieldVal->getName();
            }
            break;
        }
        return;

    case TXS_GLOBAL:
    case TXS_STATIC:
    case TXS_VIRTUAL:
        if ( !( fieldDecl->get_decl_flags() & ( TXD_ABSTRACT | TXD_INITIALIZER ) ) ) {
            if ( this->field->initExpression ) {
                if ( this->field->initExpression->is_statically_constant() ) {
                    auto uniqueName = field->get_declaration()->get_unique_full_name();
                    Constant* constantInitializer = field->code_gen_constant_init_expr( context );
                    Type* llvmType = context.get_llvm_type( txType );
                    fieldVal = make_constant_nonlocal_field( context, uniqueName, constantInitializer, llvmType );
                    break;
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

    context.register_llvm_value( uniqueName, fieldVal );
}

Constant* TxFieldDefiningNode::code_gen_constant_init_expr( LlvmGenerationContext& context ) const {
    if (! this->cachedConstantInitializer) {
        ASSERT( this->get_init_expression() && this->get_init_expression()->is_statically_constant(), "Expected constant initializer in " << this );
        this->cachedConstantInitializer = this->get_init_expression()->code_gen_constant( context );
    }
    return this->cachedConstantInitializer;
}

Value* TxExpressionNode::code_gen_expr( LlvmGenerationContext& context, GenScope* scope ) const {
    if (this->is_statically_constant())
        return this->code_gen_constant( context );
    else
        return this->code_gen_value( context, scope );
}

Value* TxExpressionNode::code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->get_type()->type()->gen_typeid( context, scope );
}
