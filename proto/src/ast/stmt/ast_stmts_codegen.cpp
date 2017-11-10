#include "ast_stmts.hpp"
#include "ast_assertstmt_node.hpp"
#include "ast_panicstmt_node.hpp"
#include "ast/expr/ast_ref.hpp"
#include "symbol/package.hpp"
#include "driver.hpp"
#include "parsercontext.hpp"

#include "llvm_generator.hpp"
#include "llvm/IR/IntrinsicInst.h"

using namespace llvm;


void TxFieldStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    this->fieldDef->code_gen_field( context, scope );
}

void TxTypeStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    this->typeDecl->code_gen( context );
}

void TxAssertStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    if ( !context.tuplexPackage.driver().get_options().suppress_asserts )
        this->ifStmt->code_gen( context, scope );
}

void TxPanicStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    this->suite->code_gen( context, scope );
}

void TxSuiteNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    auto suiteDScope = context.debug_builder()->createLexicalBlock( scope->debug_scope(), get_parser_context()->debug_file(),
                                                                    ploc.begin.line, ploc.begin.column );
    scope->push_debug_scope( suiteDScope );
    for ( auto stmt : *this->suite ) {
        stmt->code_gen( context, scope );
    }
    scope->pop_debug_scope();
}

void TxExprStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    this->expr->code_gen_expr( context, scope );
}


static Value* gen_local_field( LlvmGenerationContext& context, GenScope* scope, const TxField* field, Value* fieldV ) {
    fieldV->setName( field->get_unique_name() );
    auto fieldA = field->qtype()->gen_alloca( context, scope, field->get_unique_name() + "_" );
    scope->builder->CreateStore( fieldV, fieldA );
    field->set_llvm_value( fieldA );

    // Create debug descriptor for the field:
    auto pos = field->get_parse_location().begin;
    DILocalVariable *selfVarD = context.debug_builder()->createAutoVariable(
            scope->debug_scope(), field->get_unique_name(), field->get_parser_context()->debug_file(),
            pos.line, context.get_debug_type( field->qtype() ), true /* alwaysPreserve */ );
    context.debug_builder()->insertDeclare( fieldA, selfVarD, context.debug_builder()->createExpression(),
                                            DebugLoc::get( pos.line, pos.column, scope->debug_scope() ),
                                            scope->builder->GetInsertBlock() );
    return fieldA;
}

void TxSelfSuperFieldsStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );

    // (both self and super refer to the same object, but with different ref types)
    // from the closure reference argument, create the local self and super fields:
    auto parentFunc = scope->builder->GetInsertBlock()->getParent();
    Value* closureRefV = &( *parentFunc->arg_begin() );
    Value* tidV = gen_get_ref_typeid( context, scope, closureRefV );
    Value* origPtrV = gen_get_ref_pointer( context, scope, closureRefV );

    this->selfRefNode->typeExpression->code_gen_type( context );
    auto selfT = context.get_llvm_type( this->selfRefNode->qtype() );
    auto convSelfV = gen_ref( context, scope, selfT, origPtrV, tidV );
    gen_local_field( context, scope, this->selfRefNode->field(), convSelfV );

    this->superRefNode->typeExpression->code_gen_type( context );
    auto superT = context.get_llvm_type( this->superRefNode->qtype() );
    auto convSuperV = gen_ref( context, scope, superT, origPtrV, tidV );
    gen_local_field( context, scope, this->superRefNode->field(), convSuperV );
}

void TxMemberInitNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    if (this->constructorCallExpr->is_inlined()) {
        // this is an "initializer" and we need to perform an assignment of its result value:
        auto fieldPtrV = static_cast<TxConstructorCalleeExprNode*>( this->constructorCallExpr->callee )->gen_obj_ptr( context, scope );
        auto initV = this->constructorCallExpr->code_gen_expr( context, scope );
        scope->builder->CreateStore( initV, fieldPtrV );
    }
    else
        this->constructorCallExpr->code_gen_expr( context, scope );
}

void TxInitStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    this->selfSuperStmt->code_gen( context, scope );
    for ( auto initClause : *this->initClauseList )
        initClause->code_gen( context, scope );
}

void TxReturnStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    if ( this->expr ) {
        auto exprV = this->expr->code_gen_expr( context, scope );
        scope->builder->CreateRet( exprV );
    }
    else
        scope->builder->CreateRetVoid();
}

void TxBreakStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    scope->builder->CreateBr( scope->compStmtStack.top()->breakBlock );
}

void TxContinueStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    scope->builder->CreateBr( scope->compStmtStack.top()->continueBlock );
}


void TxAssignStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    auto lval = this->lvalue->code_gen_address( context, scope );
    ASSERT ( lval->getType()->isPointerTy(), "At " << this->parse_loc_string() << ": L-value is not of pointer type:\n" << lval );
    if ( this->lvalue->qtype()->get_type_class() == TXTC_ARRAY ) {
        auto lvalTypeIdV = this->lvalue->code_gen_typeid( context, scope );
        auto rval = this->rvalue->code_gen_addr( context, scope );
        code_gen_array_copy( context, scope, this->lvalue->qtype().type(), lvalTypeIdV, lval, rval );
    }
    else {
        auto rval = this->rvalue->code_gen_expr( context, scope );
        scope->builder->CreateStore( rval, lval );
    }
}


void TxAssignStmtNode::code_gen_array_copy( LlvmGenerationContext& context, GenScope* scope, const TxActualType* lvalType,
                                            Value* lvalTypeIdV, Value* lvalArrayPtrV, Value* rvalArrayPtrV ) {
//    std::cerr << "lval type: " << lvalType << std::endl;
//    std::cerr << "lval: " << lvalArrayPtrV << std::endl;
//    std::cerr << "rval: " << rvalArrayPtrV << std::endl;
    Value* lenIxs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                        ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 1 ) };
    auto srcLenPtrV = scope->builder->CreateInBoundsGEP( rvalArrayPtrV, lenIxs );
    auto srcLenV = scope->builder->CreateLoad( srcLenPtrV );

    // perform bounds check:
    {
        auto parentFunc = scope->builder->GetInsertBlock()->getParent();
        BasicBlock* trueBlock = BasicBlock::Create( context.llvmContext, "abc_if_out", parentFunc );
        BasicBlock* nextBlock = BasicBlock::Create( context.llvmContext, "abc_if_post", parentFunc );

        Value* capIxs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                            ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ) };
        auto dstCapPtrV = scope->builder->CreateInBoundsGEP( lvalArrayPtrV, capIxs );
        auto dstCapV = scope->builder->CreateLoad( dstCapPtrV );

        auto condV = scope->builder->CreateICmpULT( dstCapV, srcLenV );
        scope->builder->CreateCondBr( condV, trueBlock, nextBlock );

        { // if assignee has insufficient capacity
            scope->builder->SetInsertPoint( trueBlock );
            auto srcLen64V = scope->builder->CreateZExt( srcLenV, Type::getInt64Ty( context.llvmContext ) );
            context.gen_panic_call( scope, "Assigned array is longer than assignee's capacity, length = %d\n", srcLen64V );
            scope->builder->CreateBr( nextBlock );  // terminate block, though won't be executed
        }

        scope->builder->SetInsertPoint( nextBlock );
    }

    // set assignee's length equal to source's length:
    auto dstLenPtrV = scope->builder->CreateInBoundsGEP( lvalArrayPtrV, lenIxs );
    scope->builder->CreateStore( srcLenV, dstLenPtrV );

    // copy array contents:
    {
        // Current implementation is to memcpy the data. (This will include uninitialized padding bytes.)
        // FUTURE: For non-elementary element types, copy each element according to type's copy constructor, if allowed.
        // compute array data size:
        auto elemSizeV = context.gen_get_element_size( scope, lvalType, lvalTypeIdV );
        auto elemSize64V = scope->builder->CreateZExtOrBitCast( elemSizeV, Type::getInt64Ty( context.llvmContext ) );
        auto srcLen64V = scope->builder->CreateZExtOrBitCast( srcLenV, Type::getInt64Ty( context.llvmContext ) );
        auto dataSizeV = scope->builder->CreateMul( elemSize64V, srcLen64V, "datasize" );

        // mem-copy array contents:
        Value* dataIxs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                             ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 2 ) };
        auto dstDataPtrV = scope->builder->CreateInBoundsGEP( lvalArrayPtrV, dataIxs );
        auto srcDataPtrV = scope->builder->CreateInBoundsGEP( rvalArrayPtrV, dataIxs );
        scope->builder->CreateMemCpy( dstDataPtrV, srcDataPtrV, dataSizeV, 8 );
    }
}
