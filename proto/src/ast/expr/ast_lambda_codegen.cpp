#include "ast_lambda_node.hpp"
#include "ast_ref.hpp"

#include "llvm_generator.hpp"

using namespace llvm;


Value* gen_lambda( LlvmGenerationContext& context, GenScope* scope, Type* lambdaT, Value* funcV, Value* closureRefV ) {
    Value* lambdaV = UndefValue::get( lambdaT );
    auto castFuncV = scope->builder->CreatePointerCast( funcV, lambdaT->getStructElementType( 0 ) );
    lambdaV = scope->builder->CreateInsertValue( lambdaV, castFuncV, 0 );
    lambdaV = scope->builder->CreateInsertValue( lambdaV, closureRefV, 1 );
    return lambdaV;
}

Constant* gen_lambda( LlvmGenerationContext& context, Type* lambdaT, Constant* funcC, Constant* closureRefC ) {
    auto lambdaC = ConstantStruct::get( cast<StructType>( lambdaT ), funcC, closureRefC, NULL );
    return lambdaC;
}


static Value* gen_local_field( LlvmGenerationContext& context, GenScope* scope, const TxField* field, Value* fieldV ) {
    fieldV->setName( field->get_unique_name() );
    const TxType* txType = field->get_type()->type();
    auto fieldA = txType->acttype()->gen_alloca( context, scope, field->get_unique_name() + "_" );
    scope->builder->CreateStore( fieldV, fieldA );
    //context.register_llvm_value( field->get_declaration()->get_unique_full_name(), fieldA );
    field->set_llvm_value( fieldA );
    return fieldA;
}

Function* TxLambdaExprNode::code_gen_forward_decl( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, " forward declaration" );
    std::string funcName;
    if ( this->fieldDefNode ) {
        auto declaration = this->fieldDefNode->get_declaration();
        if ( declaration->get_decl_flags() & TXD_CONSTRUCTOR )
            funcName = declaration->get_unique_full_name();
        else
            funcName = declaration->get_unique_full_name() + "$func";
    }
    else
        funcName = "$func";  // anonymous function

    //FunctionType *ftype = cast<FunctionType>(context.get_llvm_type(this->funcTypeNode->get_type()));
    StructType *lambdaT = cast<StructType>( context.get_llvm_type( this->funcHeaderNode->qualtype() ) );
    FunctionType *funcT = cast<FunctionType>( cast<PointerType>( lambdaT->getElementType( 0 ) )->getPointerElementType() );
    ASSERT( funcT, "Couldn't get LLVM type for function type " << this->funcHeaderNode->qualtype() );

    Function* function = cast<Function>( context.llvmModule().getOrInsertFunction( funcName, funcT ) );
    // function->setLinkage(GlobalValue::InternalLinkage);  TODO (can cause LLVM to rename function)
    //Function *function = Function::Create(ftype, GlobalValue::InternalLinkage, funcName.c_str(), &context.llvmModule);
    // note: function is of LLVM function pointer type (since it is an LLVM global value)
    return function;
}

llvm::Constant* TxLambdaExprNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, " function body" );
    Function* function = this->code_gen_forward_decl( context );
    ASSERT( function, "NULL function pointer in " << this );

    // FUTURE: if this is a lambda within a code-block, define the implicit closure object here

    StructType *lambdaT = cast<StructType>( context.get_llvm_type( this->funcHeaderNode->qualtype() ) );

    // generate the function body:
    BasicBlock *entryBlock = BasicBlock::Create( context.llvmContext, "entry", function );
    IRBuilder<> builder( entryBlock );
    GenScope fscope( &builder );

    // name the concrete args (and self, if present) and allocate them on the stack:
    Function::arg_iterator fArgI = function->arg_begin();
    if ( this->is_instance_method() ) {
        // (both self and super refer to the same object, but with different ref types)
        {
            this->selfRefNode->typeExpression->code_gen_type( context );
            auto selfT = context.get_llvm_type( this->selfRefNode->qualtype() );
            auto convSelfV = gen_ref_conversion( context, &fscope, &(*fArgI), selfT );
            gen_local_field( context, &fscope, this->selfRefNode->get_field(), convSelfV );
        }
        {
            this->superRefNode->typeExpression->code_gen_type( context );
            auto superT = context.get_llvm_type( this->superRefNode->qualtype() );
            auto convSuperV = gen_ref_conversion( context, &fscope, &(*fArgI), superT );
            gen_local_field( context, &fscope, this->superRefNode->get_field(), convSuperV );
        }
    }
    fArgI++;
    for ( auto argDefI = this->funcHeaderNode->arguments->cbegin();
            argDefI != this->funcHeaderNode->arguments->cend();
            fArgI++, argDefI++ )
            {
        ( *argDefI )->typeExpression->code_gen_type( context );
        gen_local_field( context, &fscope, ( *argDefI )->get_field(), &(*fArgI) );
    }

    this->suite->code_gen( context, &fscope );

    if ( !this->funcHeaderNode->returnField && !fscope.builder->GetInsertBlock()->getTerminator() ) {
        LOG_DEBUG( context.LOGGER(), "inserting default void return instruction for last block of function " << function->getName().str() );
        fscope.builder->CreateRetVoid();
    }
    ASSERT( entryBlock->getTerminator(), "Function entry block has no terminator" );

    // construct the lambda object:
    auto nullClosureRefC = Constant::getNullValue( lambdaT->getElementType( 1 ) );
    auto lambdaC = gen_lambda(context, lambdaT, function, nullClosureRefC);
    return lambdaC;
}

Value* TxLambdaExprNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->code_gen_const_value( context );
}
