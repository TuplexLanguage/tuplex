#pragma once

#include "ast_expr_node.hpp"
#include "ast_maybe_conv_node.hpp"

#include "symbol/entity_type.hpp"
#include "symbol/type_registry.hpp"

#include "tx_operations.hpp"


class TxOperatorValueNode : public TxExpressionNode {
public:
    TxOperatorValueNode( const TxLocation& parseLocation )
            : TxExpressionNode( parseLocation ) {
    }
};

class TxBinaryOperatorNode : public TxOperatorValueNode {
protected:
    virtual const TxType* define_type() override {
        auto ltype = lhs->originalExpr->resolve_type();
        auto rtype = rhs->originalExpr->resolve_type();

        const TxType* arithResultType = nullptr;
        if ( ltype->is_scalar() ) {
            if ( rtype->is_scalar() ) {
                if ( ltype != rtype ) {
                    if ( rtype->auto_converts_to( *ltype ) ) {
                        // wrap rhs with conversion node
                        this->rhs->insert_conversion( ltype );
                        arithResultType = this->rhs->resolve_type();
                    }
                    else if ( ltype->auto_converts_to( *rtype ) ) {
                        // wrap lhs with conversion node
                        this->lhs->insert_conversion( rtype );
                        arithResultType = this->lhs->resolve_type();
                    }
                }
                else
                    // same type, no additional action necessary
                    arithResultType = ltype;
            }
            if ( arithResultType ) {
                if ( op_class == TXOC_BOOLEAN )
                    CERROR( this, "Can't perform boolean operation on operands of scalar type: " << ltype );
            }
            else
                CERR_THROWRES( this, "Mismatching scalar operand types for binary operator " << this->op << ": " << ltype << ", " << rtype );
        }
        else if ( ltype->is_builtin( TXBT_BOOL ) ) {
            if ( rtype->is_builtin( TXBT_BOOL ) ) {
                if ( op_class == TXOC_ARITHMETIC )
                    CERROR( this, "Can't perform arithmetic operation on operands of boolean type: " << this->op );
            }
            else
                CERROR( this, "Mismatching operand types for binary operator " << this->op << ": " << ltype << ", " << rtype );
        }
        else if ( ltype->get_type_class() == TXTC_REFERENCE ) {
            if ( rtype->get_type_class() == TXTC_REFERENCE ) {
                if ( op_class != TXOC_EQUALITY )
                    CERROR( this, "Invalid operator for reference operands: " << this->op );
            }
            else
                CERROR( this, "Mismatching operand types for binary operator " << this->op << ": " << ltype << ", " << rtype );
        }
        else
            CERR_THROWRES( this, "Unsupported operand types for binary operator " << this->op << ": " << ltype << ", " << rtype );

        if ( this->op_class == TXOC_ARITHMETIC ) {
            // Note: After analyzing conversions, the lhs will hold the proper resulting type.
            if ( !arithResultType )
                throw resolution_error( this, "Mismatching arithmetic binary operand types" );
            return arithResultType;
        }
        else {  // TXOC_EQUALITY, TXOC_COMPARISON, TXOC_BOOLEAN
            return this->registry().get_builtin_type( TXBT_BOOL );
        }
    }

public:
    const TxOperation op;
    TxMaybeConversionNode* lhs;
    TxMaybeConversionNode* rhs;
    const int op_class;

    TxBinaryOperatorNode( const TxLocation& parseLocation, TxExpressionNode* lhs, const TxOperation op, TxExpressionNode* rhs )
            : TxOperatorValueNode( parseLocation ), op( op ),
              lhs( new TxMaybeConversionNode( lhs ) ), rhs( new TxMaybeConversionNode( rhs ) ), op_class( get_op_class( op ) ) {
        ASSERT( is_valid( op ), "Invalid operator value: " << (int)op );
    }

    virtual TxBinaryOperatorNode* make_ast_copy() const override {
        return new TxBinaryOperatorNode( this->parseLocation, this->lhs->originalExpr->make_ast_copy(), this->op,
                                         this->rhs->originalExpr->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        lhs->symbol_resolution_pass();
        rhs->symbol_resolution_pass();
    }

    virtual bool is_statically_constant() const override {
        return this->lhs->is_statically_constant() && this->rhs->is_statically_constant();
    }

    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->lhs->visit_ast( visitor, thisCursor, "lhs", context );
        this->rhs->visit_ast( visitor, thisCursor, "rhs", context );
    }
};

class TxUnaryMinusNode : public TxOperatorValueNode {
protected:
    virtual const TxType* define_type() override {
        auto type = this->operand->originalExpr->resolve_type();
        if ( !type->is_scalar() )
            CERR_THROWRES( this, "Invalid operand type for unary '-', not of scalar type: " << type );
        else if ( auto intType = dynamic_cast<const TxIntegerType*>( type->type() ) ) {
            if ( !intType->is_signed() ) {
                // promote unsigned integers upon negation
                // TODO: if operand is an integer literal (or statically constant) and small enough, convert to signed of same width
                bool mod = intType->is_modifiable();
                switch ( intType->get_type_id() ) {
                case TXBT_UBYTE:
                    type = this->registry().get_builtin_type( TXBT_SHORT, mod );
                    break;
                case TXBT_USHORT:
                    type = this->registry().get_builtin_type( TXBT_INT, mod );
                    break;
                case TXBT_UINT:
                    type = this->registry().get_builtin_type( TXBT_LONG, mod );
                    break;
                case TXBT_ULONG:
                    CERR_THROWRES( this, "Invalid operand type for unary '-': " << type );
                    break;
                default:
                    ASSERT( false, "Unknown unsigned integer type id=" << intType->get_type_id() << ": " << intType );
                }
                this->operand->insert_conversion( type );
                return this->operand->resolve_type();
            }
        }
        return type;
    }

public:
    TxMaybeConversionNode* operand;
    TxUnaryMinusNode( const TxLocation& parseLocation, TxExpressionNode* operand )
            : TxOperatorValueNode( parseLocation ), operand( new TxMaybeConversionNode( operand ) ) {
    }

    virtual TxUnaryMinusNode* make_ast_copy() const override {
        return new TxUnaryMinusNode( this->parseLocation, this->operand->originalExpr->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        operand->symbol_resolution_pass();
    }

    virtual bool is_statically_constant() const override {
        return this->operand->is_statically_constant();
    }

    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->operand->visit_ast( visitor, thisCursor, "operand", context );
    }
};

class TxUnaryLogicalNotNode : public TxOperatorValueNode {
protected:
    virtual const TxType* define_type() override {
        return this->registry().get_builtin_type( TXBT_BOOL );
    }

public:
    TxExpressionNode* operand;
    TxUnaryLogicalNotNode( const TxLocation& parseLocation, TxExpressionNode* operand )
            : TxOperatorValueNode( parseLocation ), operand( operand ) {
    }

    virtual TxUnaryLogicalNotNode* make_ast_copy() const override {
        return new TxUnaryLogicalNotNode( this->parseLocation, this->operand->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        operand->symbol_resolution_pass();
        auto type = operand->get_type();
        // assume arithmetic, scalar negation:
        if ( !type->is_builtin( TXBT_BOOL ) )
            // should we support any auto-conversion to Bool?
            CERROR( this, "Operand of unary '!' is not of Bool type: " << (type ? type->str().c_str() : "NULL") );
    }

    virtual bool is_statically_constant() const override {
        return this->operand->is_statically_constant();
    }

    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->operand->visit_ast( visitor, thisCursor, "operand", context );
    }
};
