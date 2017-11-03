#include "ast_exprs.hpp"
#include "ast_ref.hpp"
#include "ast_lambda_node.hpp"

#include "llvm_generator.hpp"

using namespace llvm;


Value* gen_lambda_call( LlvmGenerationContext& context, GenScope* scope, Value* lambdaV,
                        std::vector<Value*>& passedArgs, const std::string& exprLabel, bool doesNotReturn ) {
    auto functionPtrV = gen_get_struct_member( context, scope, lambdaV, 0 );
    auto closureRefV = gen_get_struct_member( context, scope, lambdaV, 1 );

    std::vector<Value*> args;
    args.push_back( closureRefV );
    for ( auto arg : passedArgs ) {
        args.push_back( arg );
    }

    auto call = scope->builder->CreateCall( functionPtrV, args, exprLabel );
    if ( doesNotReturn )
        call->setDoesNotReturn();
    return call;
}

static Value* gen_externc_call( const TxFunctionCallNode* node, LlvmGenerationContext& context, GenScope* scope, Value* lambdaV,
                                const std::string& exprLabel, bool doesNotReturn ) {
    //std::cout << "gen_externc_call() to " << functionPtrV << std::endl;
    std::vector<Value*> args;
    for ( auto argDef : *node->argsExprList ) {
        // convert type if necessary and allowed:

        // Note: Passing arrays by value is not supported, only by reference/pointer.
        if ( argDef->qtype()->get_type_class() == TXTC_REFERENCE ) {
            auto argV = argDef->code_gen_expr( context, scope );
            argV = gen_get_ref_pointer( context, scope, argV );

            auto targetType = argDef->qtype()->target_type();
            if ( targetType->get_type_class() == TXTC_ARRAY ) {
                auto elemType = targetType->element_type();
                if ( elemType->get_type_class() == TXTC_ELEMENTARY ) {
                    if ( auto structPtrT = dyn_cast<PointerType>( argV->getType() ) ) {  // address of struct
                        ASSERT( structPtrT->getPointerElementType()->isStructTy(), "expected pointer element to be a struct: " << argV );
                        argV = scope->builder->CreateStructGEP( structPtrT->getPointerElementType(), argV, 2 );
                        auto elemT = cast<ArrayType>( argV->getType()->getPointerElementType() )->getArrayElementType();
                        argV = scope->builder->CreatePointerCast( argV, elemT->getPointerTo() );
                    }
                    else
                        THROW_LOGIC( "Expected pointer type: " << argV );
                }
                else {
                    CERR_CODECHECK( node, "Not supported to convert array of type " << elemType << " in external C function call" );
                }
            }
            else if ( targetType->get_type_class() != TXTC_ELEMENTARY ) {
                CERR_CODECHECK( node, "Not supported to convert references to type " << targetType << " in external C function call" );
            }
            args.push_back( argV );
        }
        else if ( argDef->qtype()->get_type_class() == TXTC_ELEMENTARY ) {
            args.push_back( argDef->code_gen_expr( context, scope ) );
        }
        else {
            CERR_CODECHECK( node, "Not supported to convert type " << argDef->qtype() << " in external C function call" );
        }
    }

    auto functionPtrV = gen_get_struct_member( context, scope, lambdaV, 0 );
    auto call = scope->builder->CreateCall( functionPtrV, args, exprLabel );
    if ( doesNotReturn )
        call->setDoesNotReturn();
    return call;
}

static Value* gen_call( const TxFunctionCallNode* node, LlvmGenerationContext& context, GenScope* scope, const std::string& exprLabel,
                        bool doesNotReturn) {
    auto lambdaV = node->callee->code_gen_dyn_value( context, scope );
    if ( dynamic_cast<const TxExternCFunctionType*>( node->callee->qtype().type() ) ) {
        // this is a call to an external C function
        return gen_externc_call( node, context, scope, lambdaV, exprLabel, doesNotReturn );
    }
    else {
        std::vector<Value*> args;
        for ( auto argDef : *node->argsExprList ) {
            args.push_back( argDef->code_gen_expr( context, scope ) );
        }
        return gen_lambda_call( context, scope, lambdaV, args, exprLabel, doesNotReturn );
    }
}

Constant* TxFunctionCallNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    ASSERT( this->inlinedExpression, "invoked code_gen_constant() on function call that has no inlined expression: " << this );
    return this->inlinedExpression->code_gen_const_value( context );
}

Value* TxFunctionCallNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    if ( this->inlinedExpression )
        return this->inlinedExpression->code_gen_dyn_value( context, scope );
    else {
        // pick field's plain name, if available, for the expression value:
        const std::string fieldName = ( this->fieldDefNode ? this->fieldDefNode->get_descriptor() : "" );
        return gen_call( this, context, scope, fieldName, this->doesNotReturn );
    }
}

Value* TxFunctionCallNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    auto storage = this->get_storage();
    if ( storage == TXS_STACK || storage == TXS_UNBOUND_STACK ) {  // only true if there is an inlined expression
        return this->inlinedExpression->code_gen_dyn_address( context, scope );
    }
    else {
        // automatically allocates stack space for the return value
        auto valueV = this->code_gen_expr( context, scope );

        scope->use_alloca_insertion_point();
        Value* valuePtrV = scope->builder->CreateAlloca( valueV->getType(), nullptr, "returnval" );
        scope->use_current_insertion_point();

        scope->builder->CreateStore( valueV, valuePtrV );
        return valuePtrV;
    }
}

Value* TxConstructorCalleeExprNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    Value* funcPtrV = this->gen_func_ptr( context, scope );
    auto allocType = this->objectExpr->qtype();
    Constant* instanceTypeIdV = allocType->gen_typeid( context );
    // construct the lambda object:
    auto closureRefT = context.get_closureRefT();
    auto closureRefV = gen_ref( context, scope, closureRefT, this->gen_obj_ptr( context, scope ), instanceTypeIdV );
    auto lambdaT = cast<StructType>( context.get_llvm_type( this->qtype() ) );
    return gen_lambda( context, scope, lambdaT, funcPtrV, closureRefV );
}

Value* TxConstructorCalleeExprNode::gen_obj_ptr( LlvmGenerationContext& context, GenScope* scope ) const {
    if ( !this->objectPtrV ) {
        this->objectPtrV = this->objectExpr->code_gen_dyn_address( context, scope );
        ASSERT( this->objectPtrV->getType()->isPointerTy(), "Expected baseValue to be of pointer type but was: " << this->objectPtrV->getType() );
    }
    return this->objectPtrV;
}

Value* TxConstructorCalleeExprNode::gen_func_ptr( LlvmGenerationContext& context, GenScope* scope ) const {
    // constructors are similar to instance methods, but they are not virtual (and not in vtable)

    // find the constructor
    // (constructors aren't inherited, but we bypass equivalent specializations to find the code-generated constructor)
    auto uniqueName = this->declaration->get_unique_name();
    const TxActualType* allocType = this->objectExpr->qtype().type();
    while ( allocType->is_equivalent_derivation() )  // as we don't generate code for equivalent specializations
        allocType = allocType->get_semantic_base_type();
    auto uniqueFullName = allocType->get_declaration()->get_unique_full_name() + "." + uniqueName;
    //std::cerr << "Code-generated constructor name: " << uniqueFullName << " (from: " << this->get_spec(0).declaration->get_unique_full_name() << ")" << std::endl;

    auto constrField = this->declaration->get_definer()->field();
    Value* funcPtrV = constrField->code_gen_field_decl( context );
    return funcPtrV;
}

Value* TxHeapAllocNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    this->objTypeExpr->code_gen_type( context );
    return this->qtype()->gen_malloc( context, scope );
}

Value* TxStackAllocNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    this->objTypeExpr->code_gen_type( context );
    return this->qtype()->gen_alloca( context, scope );
}

Value* TxNewConstructionNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    // new constructor returns the constructed object by reference
    TRACE_CODEGEN( this, context );

    Value* objAllocV = static_cast<TxConstructorCalleeExprNode*>( this->constructorCall->callee )->gen_obj_ptr( context, scope );
    // initialize the object
    if ( this->initializationExpression ) {
        auto initValue = this->initializationExpression->code_gen_dyn_value( context, scope );
        ASSERT( scope, "new expression not supported in global/static scope: " << this->parse_loc_string() );
        scope->builder->CreateStore( initValue, objAllocV );
    }
    else {
        this->constructorCall->code_gen_dyn_value( context, scope );
    }

    Constant* objTypeIdV = this->get_object_type()->gen_typeid( context );
    Type* objRefT = context.get_llvm_type( this->qtype() );
    auto objRefV = gen_ref( context, scope, objRefT, objAllocV, objTypeIdV );
    return objRefV;
}

Value* TxStackConstructionNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    // stack constructor returns the constructed object by value, not by reference
    TRACE_CODEGEN( this, context );
    if ( this->initializationExpression ) {
        // if inlined, the stack constructor doesn't need to actually allocate storage on stack
        // (the receiver of this expression value might do this, if it needs to)
        return this->initializationExpression->code_gen_dyn_value( context, scope );
    }
    else {
        Value* objAllocV = static_cast<TxConstructorCalleeExprNode*>( this->constructorCall->callee )->gen_obj_ptr( context, scope );
        this->constructorCall->code_gen_dyn_value( context, scope );
        return scope->builder->CreateLoad( objAllocV );
    }
}

Value* TxStackConstructionNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    // stack constructor returns the constructed object by direct pointer, not by reference
    TRACE_CODEGEN( this, context );
    ASSERT( !this->initializationExpression, "Can't get *address* of stack construction which has inlined expression in " << this
            << "   parent: " << this->parent() << std::endl );
    Value* objAllocV = static_cast<TxConstructorCalleeExprNode*>( this->constructorCall->callee )->gen_obj_ptr( context, scope );
    this->constructorCall->code_gen_dyn_value( context, scope );
    return objAllocV;
}

Value* TxAssigneeNode::code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const {
    // default implementation is the statically known type's id; overridden by assignee forms that are dynamically dependent
    return this->qtype()->gen_typeid( context );
}
