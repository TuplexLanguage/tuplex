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
//    if ( auto fieldDef = dynamic_cast<const TxFieldDefNode*>( par ) )
//        return fieldDef->modifiable;
//    return false;
//}

TxFilledArrayLitNode::TxFilledArrayLitNode( const TxLocation& ploc, TxTypeExpressionNode* elementTypeExpr,
                                const std::vector<TxExpressionNode*>* elemExprList,
                                TxExpressionNode* capacityExpr )
        : TxArrayLitNode( ploc ), origElemExprList( elemExprList ),
          elementTypeNode( elementTypeExpr ? new TxTypeTypeArgumentNode( elementTypeExpr ) : nullptr ),
          capacityExpr( capacityExpr ? new TxMaybeConversionNode( capacityExpr ) : nullptr ), elemExprList( make_args_vec( elemExprList ) )
{
}

TxFilledArrayLitNode::TxFilledArrayLitNode( const TxLocation& ploc, const std::vector<TxExpressionNode*>* elemExprList )
        : TxFilledArrayLitNode( ploc, nullptr, elemExprList )
{
}

TxFilledArrayLitNode::TxFilledArrayLitNode( const TxLocation& ploc, const std::vector<TxMaybeConversionNode*>* elemExprList )
        : TxFilledArrayLitNode( ploc, nullptr, elemExprList )
{
}

TxFilledArrayLitNode::TxFilledArrayLitNode( const TxLocation& ploc, TxTypeExpressionNode* elementTypeExpr,
                                const std::vector<TxMaybeConversionNode*>* elemExprList )
        : TxArrayLitNode( ploc ), origElemExprList( nullptr ),
          elementTypeNode( elementTypeExpr ? new TxTypeTypeArgumentNode( elementTypeExpr ) : nullptr ),
          capacityExpr( nullptr ), elemExprList( elemExprList ) {
}

const TxType* TxFilledArrayLitNode::define_type() {
    const TxType* expectedArgType;
    const TxType* arrayType = nullptr;
    if ( this->capacityExpr ) {
        auto elemTypeNode = this->elementTypeNode;
        if ( !elemTypeNode ) {
            elemTypeNode = new TxTypeTypeArgumentNode( new TxTypeExprWrapperNode( elemExprList->front()->originalExpr ) );
            run_declaration_pass( elemTypeNode, this, "elem-type" );
        }
        this->capacityExpr->insert_conversion( this->registry().get_builtin_type( ARRAY_SUBSCRIPT_TYPE_ID ) );
        auto capacityNode = new TxValueTypeArgumentNode( this->capacityExpr );
        capacityNode->node_declaration_pass( this );
        arrayType = this->registry().get_array_type( this, elemTypeNode, capacityNode );
        if ( this->elemExprList->size() == 1
             && get_reinterpretation_degree( arrayType, this->elemExprList->front()->originalExpr->resolve_type() ) >= 0 ) {
            // treat as array to array assignment
            this->_directArrayArg = true;
            expectedArgType = arrayType;
        }
        else
            expectedArgType = arrayType->element_type();
    }
    else {
        if ( this->elemExprList->size() == 1 ) {
            auto singleArgType = this->elemExprList->front()->originalExpr->resolve_type();
            if ( singleArgType->get_type_class() == TXTC_REFERENCE ) {
                singleArgType = singleArgType->target_type();  // auto-dereferencing
            }
            if ( singleArgType->get_type_class() == TXTC_ARRAY ) {
                auto argElemType = singleArgType->element_type();
                if ( this->elementTypeNode && argElemType->is_assignable_to( *this->elementTypeNode->typeExprNode->resolve_type() ) ) {
                    if ( static_cast<const TxArrayType*>( singleArgType->type() )->capacity() ) {
                        // concrete array capacity - treat as array to array assignment
                        this->_directArrayArg = true;
                        arrayType = singleArgType;
                        expectedArgType = arrayType;
                    }
                }
            }
        }

        if ( !arrayType ) {
            auto elemTypeNode = this->elementTypeNode;
            if ( !elemTypeNode ) {
                elemTypeNode = new TxTypeTypeArgumentNode( new TxTypeExprWrapperNode( elemExprList->front()->originalExpr ) );
                run_declaration_pass( elemTypeNode, this, "elem-type" );
            }
            auto tmpcapacityExpr = new TxIntegerLitNode( this->ploc, elemExprList->size(), false, TXBT_UINT );
            auto capacityNode = new TxValueTypeArgumentNode( tmpcapacityExpr );
            run_declaration_pass( capacityNode, this, "capacity" );
            arrayType = this->registry().get_array_type( this, elemTypeNode, capacityNode );
            expectedArgType = arrayType->element_type();
        }
    }

    for ( auto elemExpr : *this->elemExprList )
        elemExpr->insert_conversion( expectedArgType );
    return arrayType;
}

void TxFilledArrayLitNode::symbol_resolution_pass() {
    TxExpressionNode::symbol_resolution_pass();
    if ( this->elementTypeNode )
        this->elementTypeNode->symbol_resolution_pass();
    for ( auto elemExpr : *this->elemExprList )
        elemExpr->symbol_resolution_pass();

    if ( this->capacityExpr ) {
        this->capacityExpr->symbol_resolution_pass();

        if ( this->capacityExpr->is_statically_constant() ) {
            if ( this->elemExprList->size() == 1 && this->elemExprList->front()->get_type()->is_assignable_to( *this->get_type() ) ) {
                // array to array assignment
            }
            else if ( eval_unsigned_int_constant( this->capacityExpr ) != this->elemExprList->size() )
                CERROR( this, "Capacity expression of array literal equals " << eval_unsigned_int_constant( this->capacityExpr )
                        << ", but number of elements is " << this->elemExprList->size() );
        }
        else
            CERROR( this, "Capacity expression of filled array literal is not statically constant" );
    }

    this->_constant = true;
    for ( auto arg : *this->elemExprList ) {
        if ( !arg->is_statically_constant() ) {
            this->_constant = false;
            break;
        }
    }
}



TxUnfilledArrayLitNode::TxUnfilledArrayLitNode( const TxLocation& ploc, TxTypeExpressionNode* arrayTypeExpr )
        : TxArrayLitNode( ploc ), arrayTypeNode( arrayTypeExpr ) {
}

const TxType* TxUnfilledArrayLitNode::define_type() {
    return this->arrayTypeNode->resolve_type();
}

void TxUnfilledArrayLitNode::symbol_resolution_pass() {
    TxExpressionNode::symbol_resolution_pass();
    this->arrayTypeNode->symbol_resolution_pass();
}



TxUnfilledArrayCompLitNode::TxUnfilledArrayCompLitNode( const TxLocation& ploc, TxTypeExpressionNode* elementTypeExpr,
                                                        TxExpressionNode* capacityExpr )
        : TxArrayLitNode( ploc ), elementTypeNode( new TxTypeTypeArgumentNode( elementTypeExpr ) ),
          capacityExpr( new TxMaybeConversionNode( capacityExpr ? capacityExpr : new TxIntegerLitNode( ploc, "0", false ) ) ) {
}

const TxType* TxUnfilledArrayCompLitNode::define_type() {
    this->capacityExpr->insert_conversion( this->registry().get_builtin_type( ARRAY_SUBSCRIPT_TYPE_ID ) );
    auto capacityNode = new TxValueTypeArgumentNode( this->capacityExpr );
    capacityNode->node_declaration_pass( this );
    auto arrayType = this->registry().get_array_type( this, this->elementTypeNode, capacityNode );
    return arrayType;
}

void TxUnfilledArrayCompLitNode::symbol_resolution_pass() {
    TxExpressionNode::symbol_resolution_pass();
    this->elementTypeNode->symbol_resolution_pass();
    this->capacityExpr->symbol_resolution_pass();
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
//    panicNode->symbol_resolution_pass();
//    return panicNode;
//}

void TxElemDerefNode::symbol_resolution_pass() {
    TxExpressionNode::symbol_resolution_pass();
    this->array->symbol_resolution_pass();
    this->subscript->symbol_resolution_pass();

    // TODO: Support negative array indexing.

    if ( !this->unchecked ) {
        // Note: In theory, if this expression is statically constant we could perform the bounds checking here.
        // However accessing the cogegen'd value of Array.L isn't guaranteed before the type preparation has been run.
        this->panicNode = new TxPanicStmtNode( this->subscript->ploc, "Array index out of bounds" );
        run_declaration_pass( panicNode, this, "panic" );
        panicNode->symbol_resolution_pass();
    }
}

void TxElemAssigneeNode::symbol_resolution_pass() {
    TxAssigneeNode::symbol_resolution_pass();
    array->symbol_resolution_pass();
    subscript->symbol_resolution_pass();

    // TODO: Support negative array indexing.

    if ( !this->unchecked ) {
        // TODO: When we support accessing fields of constant instances, we can access L of constant arrays in compile time
        this->panicNode = new TxPanicStmtNode( this->subscript->ploc, "Array index out of bounds" );
        run_declaration_pass( panicNode, this, "panic" );
        panicNode->symbol_resolution_pass();
    }
}
