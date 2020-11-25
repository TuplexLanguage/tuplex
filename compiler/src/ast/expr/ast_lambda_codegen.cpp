#include "ast_lambda_node.hpp"
#include "ast_ref.hpp"

#include "llvm_generator.hpp"
#include "parsercontext.hpp"

using namespace llvm;


Value* gen_lambda( LlvmGenerationContext& context, GenScope* scope, Type* lambdaT, Value* funcV, Value* closureRefV ) {
    Value* lambdaV = UndefValue::get( lambdaT );
    auto castFuncV = scope->builder->CreatePointerCast( funcV, lambdaT->getStructElementType( 0 ) );
    lambdaV = scope->builder->CreateInsertValue( lambdaV, castFuncV, 0 );
    lambdaV = scope->builder->CreateInsertValue( lambdaV, closureRefV, 1 );
    return lambdaV;
}

Constant* gen_lambda( LlvmGenerationContext& context, Type* lambdaT, Constant* funcC, Constant* closureRefC ) {
    auto lambdaC = ConstantStruct::get( cast<StructType>( lambdaT ), funcC, closureRefC );
    return lambdaC;
}


static Value* gen_local_field( LlvmGenerationContext& context, GenScope* scope, const TxField* field, Value* fieldV ) {
    fieldV->setName( field->get_unique_name() );
    auto fieldA = field->qtype()->gen_alloca( context, scope, field->get_unique_name() + "_" );
    scope->builder->CreateStore( fieldV, fieldA );
    field->set_llvm_value( fieldA );
    return fieldA;
}

void TxLambdaExprNode::add_function_attribute( Attribute::AttrKind llvmAttrKind ) {
    _funcAttrBuilder.addAttribute( Attribute::AttrKind::NoInline );
}

Function* TxLambdaExprNode::code_gen_function_decl( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context, " function declaration" );
    std::string funcName;
    if ( this->fieldDefNode ) {
        auto declaration = this->fieldDefNode->get_declaration();
        if ( declaration->get_decl_flags() & TXD_CONSTRUCTOR )
            funcName = declaration->get_unique_full_name();
        else
            funcName = declaration->get_unique_full_name() + "$func";
    }
    else {
        //std::cerr << "Anonymous function in " << this << std::endl;
        funcName = this->context().scope()->get_full_name().str() + "$func";  // anonymous function
    }

    StructType *lambdaT = cast<StructType>( context.get_llvm_type( this->funcHeaderNode->qtype() ) );
    FunctionType *funcT = cast<FunctionType>( cast<PointerType>( lambdaT->getElementType( 0 ) )->getPointerElementType() );
    ASSERT( funcT, "Couldn't get LLVM type for function type " << this->funcHeaderNode->qtype() );

    AttributeList attribs = AttributeList::get( context.llvmContext, AttributeList::FunctionIndex, _funcAttrBuilder );
    Function* function = cast<Function>( context.llvmModule().getOrInsertFunction( funcName, funcT, attribs ).getCallee() );
    function->setLinkage( GlobalValue::InternalLinkage );
    // Note: function is of LLVM function pointer type (since it is an LLVM global value)
    return function;
}

void TxLambdaExprNode::code_gen_function_body( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    if ( this->is_suppressed_modifying_method() ) {
        //std::cerr << "skipping suppressed method " << this << std::endl;
        return;
    }
    ASSERT( this->functionPtr, "NULL functionPtr in " << this );

    // FUTURE: if this is a lambda within a code-block, define the implicit closure object here

    // generate the function debug info:
    DIFile* fileScope = this->get_parser_context()->debug_file();
    {
        // TODO: this puts all functions' debug info as if in file scope, do we need to reflect that methods and lambdas are local?
        DIScope* outerDebugScope = this->get_parser_context()->debug_unit();
        auto linkageName = StringRef();
        DISubroutineType* subRoutineType = cast<DISubroutineType>( context.get_debug_type( this->qtype() ) );
        unsigned scopeLine = this->body->ploc.begin.line;
//        bool isLocalToUnit = true;  // internal linkage
//        DITemplateParameterArray funcTemplParams = nullptr;  // can we use this for generic functions in future?
        DISubprogram *subProg = context.debug_builder()->createFunction(
            outerDebugScope, this->functionPtr->getName(), linkageName,
            fileScope, this->ploc.begin.line, subRoutineType, scopeLine,
            DINode::FlagPrototyped,
            DISubprogram::SPFlagLocalToUnit | DISubprogram::SPFlagDefinition
//            DINode::FlagPrototyped  // FlagVirtual might be relevant for virtual methods?
//            isLocalToUnit,
//            true /* isDefinition */,
//            false /* isOptimized */,
//            funcTemplParams
            );
        this->functionPtr->setSubprogram( subProg );

//        // Push the current scope.
//        KSDbgInfo.LexicalBlocks.push_back( subProg );
    }

    // generated the function entry block and prologue:

    BasicBlock *entryBlock = BasicBlock::Create( context.llvmContext, "entry", this->functionPtr );
    IRBuilder<> builder( entryBlock );
    GenScope fscope( &builder, this->functionPtr->getSubprogram() );

    // Kaleidoscope recommended unsetting the location for the entry block, but that caused problems when optimizing the code.
    //    // Unset the location for the prologue emission (leading instructions with no
    //    // location in a function are considered part of the prologue and the debugger
    //    // will run past them when breaking on a function)
    //    builder.SetCurrentDebugLocation( DebugLoc() );
    builder.SetCurrentDebugLocation( DebugLoc::get( this->ploc.begin.line, this->ploc.begin.column, fscope.debug_scope() ) );

    if ( this->selfSuperStmt )
        this->selfSuperStmt->code_gen( context, &fscope );
    // name the concrete args and allocate them on the stack:
    Function::arg_iterator fArgI = this->functionPtr->arg_begin();
//    if ( this->selfRefNode ) {
//        // (both self and super refer to the same object, but with different ref types)
//        // from the closure reference argument, create the local self and super fields:
//        Value* closureRefV = &(*fArgI);
//        Value* tidV = gen_get_ref_typeid( context, &fscope, closureRefV );
//        Value* origPtrV = gen_get_ref_pointer( context, &fscope, closureRefV );
//
//        this->selfRefNode->typeExpression->code_gen_type( context );
//        auto selfT = context.get_llvm_type( this->selfRefNode->qtype() );
//        auto convSelfV =  gen_ref( context, &fscope, selfT, origPtrV, tidV );
//        gen_local_field( context, &fscope, this->selfRefNode->field(), convSelfV );
//
//        if ( this->superRefNode ) {
//            this->superRefNode->typeExpression->code_gen_type( context );
//            auto superT = context.get_llvm_type( this->superRefNode->qtype() );
//            auto convSuperV =  gen_ref( context, &fscope, superT, origPtrV, tidV );
//            gen_local_field( context, &fscope, this->superRefNode->field(), convSuperV );
//        }
//    }

    fArgI++;
    unsigned argIx = 0;
    for ( auto argDefI = this->funcHeaderNode->arguments->cbegin();
            argDefI != this->funcHeaderNode->arguments->cend();
            fArgI++, argDefI++ )
            {
        TxLocalFieldDefNode* argDef = *argDefI;
        argDef->typeExpression->code_gen_type( context );
        auto argA = gen_local_field( context, &fscope, argDef->field(), &(*fArgI) );

        // Create a debug descriptor for the argument variable:
        auto pos = argDef->get_declaration()->get_definer()->ploc.begin;
        DILocalVariable *argVarD = context.debug_builder()->createParameterVariable(
                fscope.debug_scope(), argDef->field()->get_unique_name(), ++argIx, this->get_parser_context()->debug_file(),
                pos.line, context.get_debug_type( argDef->qtype() ), true /* alwaysPreserve */ );
        context.debug_builder()->insertDeclare( argA, argVarD, context.debug_builder()->createExpression(),
                                                DebugLoc::get( pos.line, pos.column, fscope.debug_scope() ),
                                                entryBlock);
    }

    // generate the function body:
    BasicBlock *bodyBlock = BasicBlock::Create( context.llvmContext, "fbody", this->functionPtr );
    builder.CreateBr( bodyBlock );
    builder.SetInsertPoint( bodyBlock );
    builder.SetCurrentDebugLocation( DebugLoc::get( this->ploc.begin.line, this->ploc.begin.column, fscope.debug_scope() ) );
    this->body->code_gen( context, &fscope );

    if ( !this->funcHeaderNode->returnField && !fscope.builder->GetInsertBlock()->getTerminator() ) {
        LOG_DEBUG( context.LOGGER(), "inserting default void return instruction for last block of function " << this->functionPtr->getName().str() );
        fscope.builder->CreateRetVoid();
    }
}

Constant* TxLambdaExprNode::code_gen_const_decl( LlvmGenerationContext& context ) const {
    if ( !this->functionPtr )
        this->functionPtr = this->code_gen_function_decl( context );
    // construct the lambda object:
    StructType *lambdaT = cast<StructType>( context.get_llvm_type( this->funcHeaderNode->qtype() ) );
    auto nullClosureRefC = Constant::getNullValue( lambdaT->getElementType( 1 ) );
    return gen_lambda(context, lambdaT, this->functionPtr, nullClosureRefC);
}

Constant* TxLambdaExprNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    auto lambdaC = this->code_gen_const_decl( context );
    this->code_gen_function_body( context );
    return lambdaC;
}

Value* TxLambdaExprNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->code_gen_const_value( context );
}
