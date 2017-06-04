#include "llvm_generator.hpp"

#include "ast_lambda_node.hpp"

using namespace llvm;

// defined in ast_stmts_codegen.cpp  TODO: review
extern void do_store( LlvmGenerationContext& context, GenScope* scope, Value* lval, Value* rval );

static Value* gen_local_field( LlvmGenerationContext& context, GenScope* scope, const TxField* field, Value* fieldV ) {
    fieldV->setName( field->get_unique_name() );
    const TxType* txType = field->get_type();
    auto fieldA = txType->type()->gen_alloca( context, scope, field->get_unique_name() + "_" );
    do_store( context, scope, fieldA, fieldV );
    context.register_llvm_value( field->get_declaration()->get_unique_full_name(), fieldA );
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
    StructType *lambdaT = cast<StructType>( context.get_llvm_type( this->funcHeaderNode->get_type() ) );
    FunctionType *funcT = cast<FunctionType>( cast<PointerType>( lambdaT->getElementType( 0 ) )->getPointerElementType() );
    ASSERT( funcT, "Couldn't get LLVM type for function type " << this->funcHeaderNode->get_type() );

    Function* function = cast<Function>( context.llvmModule().getOrInsertFunction( funcName, funcT ) );
    // function->setLinkage(GlobalValue::InternalLinkage);  TODO (can cause LLVM to rename function)
    //Function *function = Function::Create(ftype, GlobalValue::InternalLinkage, funcName.c_str(), &context.llvmModule);
    // note: function is of LLVM function pointer type (since it is an LLVM global value)
    return function;
}

llvm::Constant* TxLambdaExprNode::code_gen_constant( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, " function body" );
    Function* function = this->code_gen_forward_decl( context );
    ASSERT( function, "NULL function pointer in " << this );

    // FUTURE: if this is a lambda within a code-block, define the implicit closure object here

    StructType *lambdaT = cast<StructType>( context.get_llvm_type( this->funcHeaderNode->get_type() ) );

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
            auto selfT = context.get_llvm_type( this->selfRefNode->get_type() );
            auto convSelfV = TxReferenceType::gen_ref_conversion( context, &fscope, &(*fArgI), selfT );
            gen_local_field( context, &fscope, this->selfRefNode->get_field(), convSelfV );
        }
        {
            this->superRefNode->typeExpression->code_gen_type( context );
            auto superT = context.get_llvm_type( this->superRefNode->get_type() );
            auto convSuperV = TxReferenceType::gen_ref_conversion( context, &fscope, &(*fArgI), superT );
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
    auto nullClosureRefV = Constant::getNullValue( lambdaT->getElementType( 1 ) );
    auto lambdaV = ConstantStruct::get( lambdaT, function, nullClosureRefV, NULL );
    //auto lambdaV = gen_lambda(context, scope, lambdaT, function, nullClosureRefV);
    return lambdaV;
}

Value* TxLambdaExprNode::code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const {
    ASSERT( false, "code_gen_value() in TxLambdaExprNode called; code_gen_constant() should be called instead: " << this );
    return this->code_gen_constant( context );
}
