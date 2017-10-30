#pragma once

#include "../ast_entitydecls.hpp"
#include "../ast_identifier.hpp"
#include "ast_expr_node.hpp"
#include "ast_assignee_node.hpp"

/** Returns the "degree of reinterpretation" required to implicitly transform a provided value
 * to a value of an expected type.
 * Returns 0 if the types are effectively equal (no conversion or casting required);
 * >0 if the provided type can be cast or implicitly converted to the expected type
 * (the higher the value, the greater the "degree of conversion" that is required
 * ("distance" between the types);
 * or <0 if it can't be cast or implicitly converted to the expected type.
 */
int get_reinterpretation_degree( TxExpressionNode* originalExpr, const TxActualType*requiredType );

/** Attempts to resolve an identified entity symbol, that is potentially overloaded,
 * to a specific field by matching with the provided arguments' types.
 * The closest matching, valid field is picked. If no field matched, NULL is returned.
 * If a field was matched, and implicit conversions were needed for any arguments,
 * those conversions are inserted for those arguments within this call.
 *
 * All included fields that have the matching number of arguments and compatible argument types are candidates.
 * Candidate selection is done by counting the number and degree of argument reinterpretations necessary to match it.
 * (A single 2nd degree reinterpretation is "further away" than many 1st degree reinterpretations.)
 *
 * Degrees of reinterpretation (to be thought of as degrees of "distance"):
 * 0: Argument and receiver have the exact same type
 * 1: Argument and receiver have equivalent types (according to narrowing/widening type rules)
 * 2: Argument can be implicitly converted to the receiver's type (e.g. Int -> Long)
 * 3: Argument can be transformed via implicit operation to the receiver's type (e.g. implicit referencing)
 *
 * Note: This function doesn't generate compiler errors; if no match is found null is returned.
 */
const TxFieldDeclaration* resolve_field( const TxExpressionNode* origin, TxEntitySymbol* entitySymbol,
                                         const std::vector<TxExpressionNode*>* arguments, bool printCandidates=false );

/** Attempts to resolve a constructor, that is potentially overloaded, for the specified type.
 */
const TxFieldDeclaration* resolve_constructor( TxExpressionNode* origin, const TxActualType* allocType,
                                               const std::vector<TxExpressionNode*>* arguments );


/** Returns an instance method lambda object value. */
llvm::Value* instance_method_value_code_gen( LlvmGenerationContext& context, GenScope* scope,
                                       const TxActualType* staticBaseType, llvm::Value* runtimeBaseTypeIdV, llvm::Value* basePtrV,
                                       const TxActualType* fieldType, const std::string& fieldName,
                                       bool nonvirtualLookup );


class TxFieldValueNode : public TxExpressionNode {
    const TxField* _field = nullptr;
    const TxEntityDeclaration* declaration = nullptr;
    TxScopeSymbol* symbol = nullptr;

    /** If this field value resolves to a constructor, this is the constructed type */
    const TxActualType* constructedType = nullptr;

    TxScopeSymbol* resolve_symbol();
    const TxEntityDeclaration* resolve_decl();

protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override;

    virtual void verification_pass() const override;

public:
    TxExpressionNode* baseExpr;
    TxIdentifierNode* symbolName;

    /** Creates a new TxFieldValueNode.
     * @param base is the base expression (preceding expression adjoined with the '.' operator), or NULL if none
     * @param member is the specified literal field name
     */
    TxFieldValueNode( const TxLocation& ploc, TxExpressionNode* base, TxIdentifierNode* memberName )
            : TxExpressionNode( ploc ), baseExpr( base ), symbolName( memberName ) {
    }

    TxFieldValueNode( const TxLocation& ploc, const std::string& compoundName )
            : TxExpressionNode( ploc ) {
        TxIdentifier ci( compoundName );
        TxFieldValueNode* base = nullptr;
        for ( auto it = ci.segments_cbegin(); it != std::prev( ci.segments_cend() ); it++ ) {
            base = new TxFieldValueNode( ploc, base, new TxIdentifierNode( ploc, *it ) );
        }
        this->baseExpr = base;
        this->symbolName = new TxIdentifierNode( ploc, ci.name() );
    }

    virtual TxFieldValueNode* make_ast_copy() const override {
        return new TxFieldValueNode( this->ploc, ( this->baseExpr ? this->baseExpr->make_ast_copy() : nullptr ),
                                     this->symbolName->make_ast_copy() );
    }

    /** Returns the full identifier (dot-separated full name) as specified in the program text, up to and including this name. */
    std::string get_full_identifier() const {
        if ( auto baseSymbolNode = dynamic_cast<TxFieldValueNode*>( this->baseExpr ) )
            return baseSymbolNode->get_full_identifier() + '.' + this->symbolName->ident();
        else
            return this->symbolName->ident();
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override;

    virtual TxFieldStorage get_storage() const override;

    virtual bool is_statically_constant() const override;

    /** Returns null if this wasn't resolved to a field. (should not be called before symbol is resolved) */
    inline const TxField* field() const {
        //ASSERT( this->_field, "field not resolved: " << this );
        return this->_field;
    }

    inline const TxFieldDeclaration* get_field_declaration() const {
        return dynamic_cast<const TxFieldDeclaration*>( this->declaration );
    }

    virtual const TxActualType* get_constructed_type( TxPassInfo passInfo ) const override {
        return constructedType;
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Constant* code_gen_const_address( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( this->baseExpr )
            this->baseExpr->visit_ast( visitor, thisCursor, "base", context );
    }

    virtual const std::string& get_descriptor() const override {
        return this->symbolName->get_descriptor();
    }
};


class TxFieldAssigneeNode : public TxAssigneeNode {
protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override {
        return this->fieldNode->resolve_type( passInfo );
    }

    virtual void verification_pass() const override {
        if ( auto fieldDecl = fieldNode->get_field_declaration() ) {
            if ( fieldDecl->get_storage() == TXS_NOSTORAGE )
                CERROR( this, "Assignee '" << fieldNode->symbolName << "' is not an L-value / has no storage." );
        }
    }

public:
    TxFieldValueNode* fieldNode;

    TxFieldAssigneeNode( const TxLocation& ploc, TxFieldValueNode* fieldNode )
            : TxAssigneeNode( ploc ), fieldNode( fieldNode ) {
    }

    virtual TxFieldAssigneeNode* make_ast_copy() const override {
        return new TxFieldAssigneeNode( this->ploc, this->fieldNode->make_ast_copy() );
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->fieldNode->get_data_graph_origin_expr();
    }

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->fieldNode->visit_ast( visitor, thisCursor, "field", context );
    }
};
