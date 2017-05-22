#pragma once

#include "ast_declbase.hpp"
#include "ast_wrappers.hpp"
#include "ast_fields.hpp"
#include "ast_types.hpp"
#include "ast_conv.hpp"

extern llvm::Value* gen_get_struct_member( LlvmGenerationContext& context, GenScope* scope, llvm::Value* structV, unsigned ix );

extern llvm::Value* gen_get_ref_pointer( LlvmGenerationContext& context, GenScope* scope, llvm::Value* refV );
extern llvm::Value* gen_get_ref_typeid( LlvmGenerationContext& context, GenScope* scope, llvm::Value* refV );
extern llvm::Value* gen_ref( LlvmGenerationContext& context, GenScope* scope, llvm::Type* refT, llvm::Value* ptrV, llvm::Value* tidV );

extern llvm::Value* gen_lambda( LlvmGenerationContext& context, GenScope* scope, llvm::Type* lambdaT, llvm::Value* funcV, llvm::Value* closureRefV );

class TxReferenceDerefNode : public TxExpressionNode {
    /** internal "cache" to prevent multiple code generations */
    mutable llvm::Value* refExprValue = nullptr;

protected:
    virtual const TxType* define_type() override {
        auto refType = this->reference->resolve_type();
        if ( refType->get_type_class() != TXTC_REFERENCE )
            CERR_THROWRES( this, "Can't de-reference non-reference expression: " << refType );
//        if (refType->target_type()->get_type_class() == TXTC_ARRAY) {
//            std::cerr << "   refType:     " << refType << std::endl;
//            std::cerr << "   target type: " << refType->target_type() << std::endl;
//        }
        return refType->target_type();
    }

public:
    TxExpressionNode* reference;
    TxReferenceDerefNode( const TxLocation& parseLocation, TxExpressionNode* operand )
            : TxExpressionNode( parseLocation ), reference( operand ) {
    }

    virtual TxReferenceDerefNode* make_ast_copy() const override {
        return new TxReferenceDerefNode( this->parseLocation, this->reference->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->reference->symbol_resolution_pass();
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->reference;
    }

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->reference->visit_ast( visitor, thisCursor, "ref", context );
    }
};

class TxElemDerefNode : public TxExpressionNode {
protected:
    virtual const TxType* define_type() override {
        this->subscript->insert_conversion( this->registry().get_builtin_type( ARRAY_SUBSCRIPT_TYPE_ID ) );

        auto opType = this->array->originalExpr->resolve_type();
        if ( opType->get_type_class() == TXTC_REFERENCE ) {
            auto targType = opType->target_type();
            if ( targType->get_type_class() == TXTC_ARRAY ) {
                this->array->insert_conversion( targType );
            }
        }
        opType = this->array->resolve_type();
        if ( opType->get_type_class() != TXTC_ARRAY )
            CERR_THROWRES( this, "Can't subscript non-array expression: " << opType );
        return opType->element_type();
    }

public:
    TxMaybeConversionNode* array;
    TxMaybeConversionNode* subscript;

    TxElemDerefNode( const TxLocation& parseLocation, TxExpressionNode* operand, TxExpressionNode* subscript )
            : TxExpressionNode( parseLocation ), array( new TxMaybeConversionNode( operand ) ),
              subscript( new TxMaybeConversionNode( subscript ) ) {
    }

    virtual TxElemDerefNode* make_ast_copy() const override {
        return new TxElemDerefNode( this->parseLocation, this->array->originalExpr->make_ast_copy(),
                                    this->subscript->originalExpr->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->array->symbol_resolution_pass();
        this->subscript->symbol_resolution_pass();
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->array;
    }

    virtual bool is_statically_constant() const override {
        return ( this->array->is_statically_constant() && this->subscript->is_statically_constant() );
    }

    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->array->visit_ast( visitor, thisCursor, "array", context );
        this->subscript->visit_ast( visitor, thisCursor, "subscript", context );
    }
};

class TxReferenceToNode : public TxExpressionNode {
    TxTypeTypeArgumentNode* targetTypeNode;
    TxExpressionNode* target;

protected:
    virtual void declaration_pass() override {
        // Special case handling: When this is is applied as an implicit conversion node,
        // target may have already run declaration pass, so we run it for targetTypeNode:
        if ( this->target->is_context_set() ) {
            run_declaration_pass( this->targetTypeNode, this, "type" );
        }
    }

    virtual const TxType* define_type() override {
        return this->registry().get_reference_type( this, this->targetTypeNode, nullptr );
    }

public:
    TxReferenceToNode( const TxLocation& parseLocation, TxExpressionNode* target )
            : TxExpressionNode( parseLocation ), target( target ) {
        TxTypeExprWrapperNode* targetTypeExpr = new TxTypeExprWrapperNode( this->target );
        this->targetTypeNode = new TxTypeTypeArgumentNode( targetTypeExpr );
    }

    virtual TxReferenceToNode* make_ast_copy() const override {
        return new TxReferenceToNode( this->parseLocation, this->target->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->target->symbol_resolution_pass();

        if ( dynamic_cast<TxFieldValueNode*>( this->target ) ) {
        }
        else if ( dynamic_cast<TxElemDerefNode*>( this->target ) ) {
        }
        else if ( this->target->is_statically_constant() ) {
        }
        else
            CERROR( this, "Can't construct reference to non-addressable expression / rvalue." );
    }

    virtual const std::vector<TxExpressionNode*>* get_applied_func_args() const override {
        return this->target->get_applied_func_args();
    }
    virtual void set_applied_func_args( const std::vector<TxExpressionNode*>* appliedTypeParameters ) override {
        this->target->set_applied_func_args( appliedTypeParameters );
    }

    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->targetTypeNode->visit_ast( visitor, thisCursor, "type", context );
        this->target->visit_ast( visitor, thisCursor, "target", context );
    }
};

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
                        arithResultType = ltype;
                    }
                    else if ( ltype->auto_converts_to( *rtype ) ) {
                        // wrap lhs with conversion node
                        this->lhs->insert_conversion( rtype );
                        arithResultType = rtype;
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
        else if ( auto intType = dynamic_cast<const TxIntegerType*>( type->type() ) )
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
                    CERROR( this, "Invalid operand type for unary '-': " << type );
                    break;
                default:
                    ASSERT( false, "Unknown unsigned integer type id=" << intType->get_type_id() << ": " << intType );
                }
                this->operand->insert_conversion( type );
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

class TxFunctionCallNode : public TxExpressionNode {
    const TxType* calleeType = nullptr;
    bool isSelfSuperConstructorInvocation = false;
    TxExpressionNode* inlinedExpression = nullptr;  // substitutes the function/constructor call if non-null

    static std::vector<TxMaybeConversionNode*>* make_args_vec( const std::vector<TxExpressionNode*>* argsExprList ) {
        std::vector<TxMaybeConversionNode*>* copyVec = new std::vector<TxMaybeConversionNode*>( argsExprList->size() );
        std::transform( argsExprList->cbegin(), argsExprList->cend(), copyVec->begin(),
                        []( TxExpressionNode* n ) -> TxMaybeConversionNode* {return new TxMaybeConversionNode( n );} );
        return copyVec;
    }

protected:
    virtual void declaration_pass() override;

    virtual const TxType* define_type() override;

public:
    TxExpressionNode* callee;
    std::vector<TxExpressionNode*> const * const origArgsExprList;
    std::vector<TxMaybeConversionNode*>* argsExprList;

    TxFunctionCallNode( const TxLocation& parseLocation, TxExpressionNode* callee, const std::vector<TxExpressionNode*>* argsExprList );

    virtual TxFunctionCallNode* make_ast_copy() const override {
        return new TxFunctionCallNode( this->parseLocation, this->callee->make_ast_copy(), make_node_vec_copy( this->origArgsExprList ) );
    }

    virtual void symbol_resolution_pass() override;

    virtual bool is_stack_allocation_expression() const override {
        if ( this->inlinedExpression )
            return this->inlinedExpression->is_stack_allocation_expression();
        return false;
    }

    virtual bool is_statically_constant() const override {
        if ( this->inlinedExpression )
            return this->inlinedExpression->is_statically_constant();
        return false;
    }

    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( this->inlinedExpression )
            this->inlinedExpression->visit_ast( visitor, thisCursor, "inlinedexpr", context );
        else {
            this->callee->visit_ast( visitor, thisCursor, "callee", context );
            for ( auto arg : *this->argsExprList )
                arg->visit_ast( visitor, thisCursor, "arg", context );
        }
    }
};

/** Special callee expression node for calling constructors. */
class TxConstructorCalleeExprNode : public TxExpressionNode {
    const TxFieldDeclaration* declaration = nullptr;
    mutable llvm::Value* objectPtrV = nullptr;

    /** @return a function pointer (not a lambda value) */
    virtual llvm::Value* gen_func_ptr( LlvmGenerationContext& context, GenScope* scope ) const;

protected:
    /** Produces the object - either an allocation, or a self/super reference */
    TxExpressionNode* objectExpr;

    virtual const TxType* define_type() override;

public:
    TxConstructorCalleeExprNode( const TxLocation& parseLocation, TxExpressionNode* objectExpr )
            : TxExpressionNode( parseLocation ), objectExpr( objectExpr ) {
    }

    virtual TxConstructorCalleeExprNode* make_ast_copy() const override {
        return new TxConstructorCalleeExprNode( this->parseLocation, this->objectExpr->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        this->objectExpr->symbol_resolution_pass();
    }

    /** @return a lambda value */
    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    /** @return an object pointer (not a lambda value) */
    virtual llvm::Value* gen_obj_ptr( LlvmGenerationContext& context, GenScope* scope ) const;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->objectExpr->visit_ast( visitor, thisCursor, "objectexpr", context );
    }
};

/** Abstract superclass for memory allocation expressions, for heap and stack allocators. */
class TxMemAllocNode : public TxExpressionNode {
protected:
    TxTypeExpressionNode* objTypeExpr;

    virtual const TxType* define_type() override {
        return this->objTypeExpr->resolve_type();
    }

    TxMemAllocNode( const TxLocation& parseLocation, TxTypeExpressionNode* objTypeExpr )
            : TxExpressionNode( parseLocation ), objTypeExpr( objTypeExpr ) {
    }

public:
    virtual void symbol_resolution_pass() override {
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->objTypeExpr->visit_ast( visitor, thisCursor, "type", context );
    }

    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override {
        THROW_LOGIC( "Unsupported: code_gen() for node type " << this );
    }

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override = 0;
};

class TxHeapAllocNode : public TxMemAllocNode {
public:
    TxHeapAllocNode( const TxLocation& parseLocation, TxTypeExpressionNode* objTypeExpr )
            : TxMemAllocNode( parseLocation, objTypeExpr ) {
    }

    virtual TxHeapAllocNode* make_ast_copy() const override {
        return new TxHeapAllocNode( this->parseLocation, this->objTypeExpr->make_ast_copy() );
    }

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;
};

class TxStackAllocNode : public TxMemAllocNode {
public:
    TxStackAllocNode( const TxLocation& parseLocation, TxTypeExpressionNode* objTypeExpr )
            : TxMemAllocNode( parseLocation, objTypeExpr ) {
    }

    virtual TxStackAllocNode* make_ast_copy() const override {
        return new TxStackAllocNode( this->parseLocation, this->objTypeExpr->make_ast_copy() );
    }

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;
};

/** Abstract common superclass for new expression and local init expression */
class TxMakeObjectNode : public TxExpressionNode {

protected:
    /** the type of the object to make/allocate */
    TxTypeExpressionNode* typeExpr;
    TxFunctionCallNode* constructorCall;
    TxExpressionNode* initializationExpression = nullptr;  // substitutes the function/constructor call if non-null

    /** Gets the type of the allocated object. Should not be called before resolution. */
    virtual const TxType* get_object_type() const = 0;

    TxMakeObjectNode( const TxLocation& parseLocation, TxTypeExpressionNode* typeExpr, TxFunctionCallNode* constructorCall )
            : TxExpressionNode( parseLocation ), typeExpr( typeExpr ), constructorCall( constructorCall ) {
    }

public:
    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->typeExpr->symbol_resolution_pass();
        if ( !this->typeExpr->get_type()->is_concrete() ) {
            if ( !this->context().is_generic() )
                CERROR( this->typeExpr, "Object to allocate is not concrete: " << this->typeExpr->get_type() );
            else
                LOG_DEBUG( this->LOGGER(), "(Not error since generic context) Object to allocate is not concrete: "
                           << this->typeExpr->get_type() );
        }

        this->constructorCall->symbol_resolution_pass();

        if ( auto calleeType = this->constructorCall->callee->get_type() ) {
            if ( auto inlineCalleeType = dynamic_cast<const TxInlineFunctionType*>( calleeType->type() ) ) {
                // This constructor is an inlineable function that returns the initializer value
                // (as opposed to a constructor whose code assigns value to the object's members).
                // We replace the constructor call with the initialization expression:
                this->initializationExpression = inlineCalleeType->make_inline_expr( this->constructorCall->callee,
                                                                                     this->constructorCall->argsExprList );
            }
        }
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->typeExpr->visit_ast( visitor, thisCursor, "type", context );
        if ( this->initializationExpression )
            this->initializationExpression->visit_ast( visitor, thisCursor, "initexpr", context );
        else
            this->constructorCall->visit_ast( visitor, thisCursor, "call", context );
    }
};

/** Makes a new object in newly allocated heap memory and returns it by reference. */
class TxNewConstructionNode : public TxMakeObjectNode {
    TxTypeTypeArgumentNode* targetTypeNode;

protected:
    virtual void declaration_pass() override {
        targetTypeNode->node_declaration_pass( this );  // special case instead of wrapping typeExpr and overriding visit_descendants()
    }

    virtual const TxType* get_object_type() const override {
        return this->typeExpr->get_type();
    }

    virtual const TxType* define_type() override {
        // new constructor returns the constructed object by reference
        return this->registry().get_reference_type( this, this->targetTypeNode, nullptr );
    }

public:
    TxNewConstructionNode( const TxLocation& parseLocation, TxTypeExpressionNode* typeExpr, std::vector<TxExpressionNode*>* argsExprList )
            : TxMakeObjectNode( parseLocation, typeExpr,
                                new TxFunctionCallNode(
                                        parseLocation,
                                        new TxConstructorCalleeExprNode(
                                                parseLocation, new TxHeapAllocNode( parseLocation, new TxTypeExprWrapperNode( typeExpr ) ) ),
                                        argsExprList ) ) {
        targetTypeNode = new TxTypeTypeArgumentNode( this->typeExpr );
    }

    virtual TxNewConstructionNode* make_ast_copy() const override {
        return new TxNewConstructionNode( this->parseLocation, this->typeExpr->make_ast_copy(),
                                          make_node_vec_copy( this->constructorCall->origArgsExprList ) );
    }

    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;
};

/** Makes a new object in newly allocated stack memory and returns it by value. */
class TxStackConstructionNode : public TxMakeObjectNode {
protected:
    virtual const TxType* get_object_type() const override {
        return this->get_type();
    }

    virtual const TxType* define_type() override {
        // stack constructor returns the constructed object by value, not by reference
        return this->typeExpr->resolve_type();
    }

public:
    /** produced by the expression syntax: <...type-expr...>(...constructor-args...) */
    TxStackConstructionNode( const TxLocation& parseLocation, TxTypeExpressionNode* typeExpr,
                             const std::vector<TxExpressionNode*>* argsExprList )
            : TxMakeObjectNode( parseLocation, typeExpr,
                                new TxFunctionCallNode(
                                        parseLocation,
                                        new TxConstructorCalleeExprNode(
                                                parseLocation, new TxStackAllocNode( parseLocation, new TxTypeExprWrapperNode( typeExpr ) ) ),
                                        argsExprList ) ) {
    }

    virtual TxStackConstructionNode* make_ast_copy() const override {
        return new TxStackConstructionNode( this->parseLocation, this->typeExpr->make_ast_copy(),
                                            make_node_vec_copy( this->constructorCall->origArgsExprList ) );
    }

    virtual bool is_stack_allocation_expression() const override {
        return !this->initializationExpression;  // performs stack allocation unless this is an inlined value expression
    }

    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;
};

/*=== assignee expressions ===*/

class TxFieldAssigneeNode : public TxAssigneeNode {
protected:
    virtual const TxType* define_type() override {
        return this->field->resolve_type();
    }

public:
    TxFieldValueNode* field;
    TxFieldAssigneeNode( const TxLocation& parseLocation, TxFieldValueNode* field )
            : TxAssigneeNode( parseLocation ), field( field ) {
    }

    virtual TxFieldAssigneeNode* make_ast_copy() const override {
        return new TxFieldAssigneeNode( this->parseLocation, this->field->make_ast_copy() );
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->field->get_data_graph_origin_expr();
    }

    virtual void symbol_resolution_pass() override {
        TxAssigneeNode::symbol_resolution_pass();
        field->symbol_resolution_pass();

        auto fieldDecl = field->get_field_declaration();
        if ( fieldDecl && fieldDecl->get_storage() == TXS_NOSTORAGE )
            CERROR( this, "Assignee '" << field->symbolName << "' is not an L-value / has no storage." );
    }

    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->field->visit_ast( visitor, thisCursor, "field", context );
    }
};

class TxDerefAssigneeNode : public TxAssigneeNode {
protected:
    virtual const TxType* define_type() override {
        auto refType = this->operand->resolve_type();
        if ( refType->get_type_class() != TXTC_REFERENCE )
            CERR_THROWRES( this, "Can't de-reference non-reference expression: " << refType );
        return refType->target_type();
    }

public:
    TxExpressionNode* operand;

    TxDerefAssigneeNode( const TxLocation& parseLocation, TxExpressionNode* operand )
            : TxAssigneeNode( parseLocation ), operand( operand ) {
    }

    virtual TxDerefAssigneeNode* make_ast_copy() const override {
        return new TxDerefAssigneeNode( this->parseLocation, this->operand->make_ast_copy() );
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->operand;
    }

    virtual void symbol_resolution_pass() override {
        TxAssigneeNode::symbol_resolution_pass();
        operand->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->operand->visit_ast( visitor, thisCursor, "operand", context );
    }
};

class TxElemAssigneeNode : public TxAssigneeNode {
protected:
    virtual const TxType* define_type() override {
        this->subscript->insert_conversion( this->registry().get_builtin_type( ARRAY_SUBSCRIPT_TYPE_ID ) );

        auto opType = this->array->originalExpr->resolve_type();
        if ( opType->get_type_class() == TXTC_REFERENCE ) {
            auto targType = opType->target_type();
            if ( targType->get_type_class() == TXTC_ARRAY ) {
                this->array->insert_conversion( targType );
            }
        }
        opType = this->array->resolve_type();
        if ( opType->get_type_class() != TXTC_ARRAY )
            CERR_THROWRES( this, "Can't subscript non-array assignee expression: " << opType );
        return opType->element_type();
    }

public:
    TxMaybeConversionNode* array;
    TxMaybeConversionNode* subscript;

    TxElemAssigneeNode( const TxLocation& parseLocation, TxExpressionNode* array, TxExpressionNode* subscript )
            : TxAssigneeNode( parseLocation ), array( new TxMaybeConversionNode( array ) ), subscript( new TxMaybeConversionNode( subscript ) ) {
    }

    virtual TxElemAssigneeNode* make_ast_copy() const override {
        return new TxElemAssigneeNode( this->parseLocation, this->array->originalExpr->make_ast_copy(),
                                       this->subscript->originalExpr->make_ast_copy() );
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->array;
    }

    virtual void symbol_resolution_pass() override {
        TxAssigneeNode::symbol_resolution_pass();
        array->symbol_resolution_pass();
        subscript->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->array->visit_ast( visitor, thisCursor, "array", context );
        this->subscript->visit_ast( visitor, thisCursor, "subscript", context );
    }
};
