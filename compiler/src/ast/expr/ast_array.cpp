#include "ast_array.hpp"

#include "ast_lit.hpp"
#include "ast_field.hpp"
#include "ast_op_exprs.hpp"
#include "ast_constexpr.hpp"

#include "ast/ast_wrappers.hpp"
#include "ast/ast_declpass.hpp"

#include "ast/stmt/ast_panicstmt_node.hpp"
#include "ast/stmt/ast_stmts.hpp"

// Note: similar to helper function in function call node
static std::vector<TxMaybeConversionNode*>* make_args_vec( const std::vector<TxExpressionNode*>* argsExprList ) {
    std::vector<TxMaybeConversionNode*>* copyVec = new std::vector<TxMaybeConversionNode*>( argsExprList->size() );
    std::transform( argsExprList->cbegin(), argsExprList->cend(), copyVec->begin(),
                    []( TxExpressionNode* n ) -> TxMaybeConversionNode* {return new TxMaybeConversionNode( n );} );
    return copyVec;
}

//bool TxArrayLitNode::requires_mutable_type() const {
//    auto par = this->parent();
//    if ( auto conv = dynamic_cast<const TxMaybeConversionNode*>( par ) )
//        par = conv->parent();
//    if ( auto fieldDef = dynamic_cast<const TxFieldDefiningNode*>( par ) )
//        return fieldDef->modifiable;
//    return false;
//}

TxFilledArrayLitNode::TxFilledArrayLitNode( const TxLocation& ploc, TxQualTypeExprNode* elementTypeExpr,
                                const std::vector<TxExpressionNode*>* elemExprList,
                                TxExpressionNode* capacityExpr )
        : TxArrayLitNode( ploc ), origElemExprList( elemExprList ),
          elementTypeNode( elementTypeExpr ? new TxTypeArgumentNode( elementTypeExpr ) : nullptr ),
          capacityExpr( capacityExpr ? new TxMaybeConversionNode( capacityExpr ) : nullptr ), elemExprList( make_args_vec( elemExprList ) )
{
}

TxFilledArrayLitNode::TxFilledArrayLitNode( const TxLocation& ploc, const std::vector<TxExpressionNode*>* elemExprList )
        : TxFilledArrayLitNode( ploc, nullptr, elemExprList )
{
}

TxFilledArrayLitNode::TxFilledArrayLitNode( const TxLocation& ploc, TxQualTypeExprNode* elementTypeExpr,
                                            const std::vector<TxMaybeConversionNode*>* elemExprList )
        : TxArrayLitNode( ploc ), origElemExprList( nullptr ),
          elementTypeNode( elementTypeExpr ? new TxTypeArgumentNode( elementTypeExpr ) : nullptr ),
          capacityExpr( nullptr ), elemExprList( elemExprList ) {
}

TxQualType TxFilledArrayLitNode::define_type( TxTypeResLevel typeResLevel ) {
    TxQualType expectedArgQType;
    const TxActualType* arrayType = nullptr;

    auto elemTypeNode = this->elementTypeNode;
    if ( !elemTypeNode ) {
        elemTypeNode = new TxTypeArgumentNode( new TxQualTypeExprNode( new TxTypeExprWrapperNode( elemExprList->front()->originalExpr ) ) );
        run_declaration_pass( elemTypeNode, this, "elem-type" );
    }
    TxQualType elemQType = elemTypeNode->type_expr_node()->resolve_type( typeResLevel );

    if ( this->capacityExpr ) {
        this->capacityExpr->insert_conversion( typeResLevel, this->registry().get_builtin_type( ARRAY_SUBSCRIPT_TYPE_ID ) );
        auto capacityNode = new TxTypeArgumentNode( this->capacityExpr );
        capacityNode->node_declaration_pass( this );
        arrayType = this->registry().get_array_type( this, elemTypeNode, capacityNode );
        const_cast<TxActualType*>(arrayType)->integrate();
        if ( this->elemExprList->size() == 1
             && get_reinterpretation_degree( this->elemExprList->front()->originalExpr, arrayType ) >= 0 ) {
            // treat as array to array assignment
            this->_directArrayArg = true;
            expectedArgQType = arrayType;
        }
        else {
            expectedArgQType = elemQType;
        }
    }
    else {
        if ( this->elemExprList->size() == 1 ) {
            auto singleArgType = this->elemExprList->front()->originalExpr->resolve_type( typeResLevel ).type();
            if ( singleArgType->get_type_class() == TXTC_REFERENCE ) {
                singleArgType = singleArgType->target_type().type();  // auto-dereferencing
            }
            if ( singleArgType->get_type_class() == TXTC_ARRAY ) {
                auto argElemQType = singleArgType->element_type();
                if ( this->elementTypeNode
                     && argElemQType->is_assignable_to( *this->elementTypeNode->type_expr_node()->resolve_type( typeResLevel ) ) ) {
                    if ( singleArgType->capacity() ) {
                        // concrete array capacity - treat as array to array assignment
                        this->_directArrayArg = true;
                        arrayType = singleArgType;
                        expectedArgQType = arrayType;
                    }
                }
            }
        }

        if ( !arrayType ) {
            auto tmpcapacityExpr = new TxIntegerLitNode( this->ploc, elemExprList->size(), false, ARRAY_SUBSCRIPT_TYPE_ID );
            auto capacityNode = new TxTypeArgumentNode( tmpcapacityExpr );
            run_declaration_pass( capacityNode, this, "capacity" );
            arrayType = this->registry().get_array_type( this, elemTypeNode, capacityNode );
            expectedArgQType = elemQType;
        }
    }

    for ( auto elemExpr : *this->elemExprList )
        // strip qualifiers since this effectively copies value
        elemExpr->insert_conversion( typeResLevel, expectedArgQType.type() );
    return arrayType;
}

void TxFilledArrayLitNode::resolution_pass() {
    TxExpressionNode::resolution_pass();
    this->_constant = true;
    for ( auto arg : *this->elemExprList ) {
        if ( !arg->is_statically_constant() ) {
            this->_constant = false;
            break;
        }
    }
}

void TxFilledArrayLitNode::verification_pass() const {
    if ( this->capacityExpr && this->capacityExpr->attempt_qtype() ) {
        if ( this->capacityExpr->is_statically_constant() ) {
            if ( !this->_directArrayArg && eval_unsigned_int_constant( this->capacityExpr ) != this->elemExprList->size() )
                CERROR( this, "Capacity expression of array literal equals " << eval_unsigned_int_constant( this->capacityExpr )
                        << ", but number of elements is " << this->elemExprList->size() );
        }
        else
            CERROR( this, "Capacity expression of filled array literal is not statically constant" );
    }
}



TxUnfilledArrayLitNode::TxUnfilledArrayLitNode( const TxLocation& ploc, TxTypeExpressionNode* arrayTypeExpr )
        : TxArrayLitNode( ploc ), arrayTypeNode( arrayTypeExpr ) {
}

TxQualType TxUnfilledArrayLitNode::define_type( TxTypeResLevel typeResLevel ) {
    return this->arrayTypeNode->resolve_type( typeResLevel );
}



TxUnfilledArrayCompLitNode::TxUnfilledArrayCompLitNode( const TxLocation& ploc, TxQualTypeExprNode* elementTypeExpr,
                                                        TxExpressionNode* capacityExpr )
        : TxArrayLitNode( ploc ), elementTypeNode( new TxTypeArgumentNode( elementTypeExpr ) ),
          capacityExpr( new TxMaybeConversionNode( capacityExpr ? capacityExpr : new TxIntegerLitNode( ploc, "0", false ) ) ) {
}

TxQualType TxUnfilledArrayCompLitNode::define_type( TxTypeResLevel typeResLevel ) {
    this->capacityExpr->insert_conversion( typeResLevel, this->registry().get_builtin_type( ARRAY_SUBSCRIPT_TYPE_ID ) );
    auto capacityNode = new TxTypeArgumentNode( this->capacityExpr );
    capacityNode->node_declaration_pass( this );
    auto arrayType = this->registry().get_array_type( this, this->elementTypeNode, capacityNode );
    return arrayType;
}



//static TxStatementNode* make_bounds_check_node( TxNode* parent, TxMaybeConversionNode* array, TxMaybeConversionNode* subscript,
//                                                bool isAssignment ) {
//    TxStatementNode* panicNode;
//    {
//        auto & loc = subscript->ploc;
//        auto lengthNode = new TxFieldValueNode( loc, new TxExprWrapperNode( array ), "L" );
//        auto condExpr = new TxBinaryOperatorNode( loc, new TxExprWrapperNode( subscript ),
//                                                  TXOP_GE, lengthNode );
//        auto panicStmt = new TxPanicStmtNode( loc, "Array index out of bounds" );
//        if ( isAssignment ) {
//
//        }
//        else {
//            panicNode = new TxIfStmtNode( loc, condExpr, panicStmt );
//        }
//    }
//
//    run_declaration_pass( panicNode, parent, "boundscheck" );
//    panicNode->resolution_pass();
//    return panicNode;
//}

TxElemDerefNode::TxElemDerefNode( const TxLocation& ploc, TxExpressionNode* operand, TxExpressionNode* subscript )
        : TxExpressionNode( ploc ),
          array( new TxMaybeConversionNode( operand ) ), subscript( new TxMaybeConversionNode( subscript ) ) {
//    if ( !unchecked ) {
//        // Note: In theory, if this expression is statically constant we could perform the bounds checking in resolution pass.
//        // However accessing the cogegen'd value of Array.L isn't guaranteed before the type preparation has been run.
//        this->panicNode = new TxPanicStmtNode( this->subscript->ploc, "Array index out of bounds" );
//    }
}

void TxElemDerefNode::set_elem_assignee_expr() {
    this->_elemAssignment = true;
    if ( auto outerElemDerefExpr = dynamic_cast<TxElemDerefNode*>( this->array->originalExpr ) ) {
        outerElemDerefExpr->set_elem_assignee_expr();
    }
}

TxElemAssigneeNode::TxElemAssigneeNode( const TxLocation& ploc, TxExpressionNode* array, TxExpressionNode* subscript )
        : TxAssigneeNode( ploc ),
          array( new TxMaybeConversionNode( array ) ), subscript( new TxMaybeConversionNode( subscript ) ) {
//    if ( !unchecked ) {
//        // TO DO: When we support accessing fields of constant instances, we can access L of constant arrays in compile time
//        this->panicNode = new TxPanicStmtNode( this->subscript->ploc, "Array index out of bounds" );
//    }
}

TxQualType TxElemAssigneeNode::define_type( TxTypeResLevel typeResLevel ) {
    this->subscript->insert_conversion( typeResLevel, this->registry().get_builtin_type( ARRAY_SUBSCRIPT_TYPE_ID ) );

    auto opType = this->array->originalExpr->resolve_type( typeResLevel );
    if ( opType->get_type_class() == TXTC_REFERENCE ) {
        auto targType = opType->target_type();
        if ( targType->get_type_class() == TXTC_ARRAY ) {
            this->array->insert_qual_conversion( typeResLevel, targType );
        }
        // TODO: May cause code_gen_typeid() to return a too-general type, override code_gen_typeid()
    }
    opType = this->array->resolve_type( typeResLevel );
    if ( opType->get_type_class() != TXTC_ARRAY )
        CERR_THROWRES( this, "Can't subscript non-array assignee expression: " << opType );

    if ( auto outerElemDerefExpr = dynamic_cast<TxElemDerefNode*>( this->array->originalExpr ) ) {
        outerElemDerefExpr->set_elem_assignee_expr();
    }
    return opType->element_type();
}
