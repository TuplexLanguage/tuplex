#pragma once

#include "ast/ast_decls.hpp"
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
extern int get_reinterpretation_degree( const TxType *expectedType, const TxType* providedType );

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
extern const TxFieldDeclaration* resolve_field( const TxExpressionNode* origin, TxEntitySymbol* entitySymbol,
                                                const std::vector<TxExpressionNode*>* arguments );

extern llvm::Value* gen_lambda( LlvmGenerationContext& context, GenScope* scope, llvm::Type* lambdaT, llvm::Value* funcV, llvm::Value* closureRefV );


class TxFieldValueNode : public TxExpressionNode {
    const TxField* field = nullptr;
    const TxEntityDeclaration* declaration = nullptr;
    TxScopeSymbol* symbol = nullptr;

    /** If this field value resolves to a constructor, this is the constructed type */
    const TxType* constructedType = nullptr;

    TxScopeSymbol* resolve_symbol();
    const TxEntityDeclaration* resolve_decl();

protected:
    virtual const TxType* define_type() override;

public:
    TxExpressionNode* baseExpr;
    const TxIdentifier* symbolName;

    /** Creates a new TxFieldValueNode.
     * @param base is the base expression (preceding expression adjoined with the '.' operator), or NULL if none
     * @param member is the specified literal field name
     */
    TxFieldValueNode( const TxLocation& parseLocation, TxExpressionNode* base, const std::string& memberName )
            : TxExpressionNode( parseLocation ), baseExpr( base ), symbolName( new TxIdentifier( memberName ) ) {
    }

    virtual TxFieldValueNode* make_ast_copy() const override {
        return new TxFieldValueNode( this->parseLocation, ( this->baseExpr ? this->baseExpr->make_ast_copy() : nullptr ), this->symbolName->str() );
    }

    /** Returns the full identifier (dot-separated full name) as specified in the program text, up to and including this name. */
    TxIdentifier get_full_identifier() const {
        if ( auto baseSymbolNode = dynamic_cast<TxFieldValueNode*>( this->baseExpr ) )
            return TxIdentifier( baseSymbolNode->get_full_identifier(), this->symbolName->str() );
        else
            return *this->symbolName;
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        // not invoking baseExpr->symbol_resolution_pass() since that is only done via define_type()
        if ( auto typeDecl = dynamic_cast<const TxTypeDeclaration*>( this->declaration ) )
            CERROR( this, "'" << get_full_identifier() << "' resolved to a type, not a field: " << typeDecl );
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override;

    virtual bool is_statically_constant() const override;

    // should not be called before symbol is resolved:
    inline const TxField* get_field() const {
        return this->field;
    }
    inline const TxFieldDeclaration* get_field_declaration() const {
        return dynamic_cast<const TxFieldDeclaration*>( this->declaration );
    }

    /** If this field value resolves to a constructor, returns the constructed type, otherwise null. */
    inline const TxType* get_constructed_type() const {
        return constructedType;
    }

    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( this->baseExpr )
            this->baseExpr->visit_ast( visitor, thisCursor, "base", context );
    }

    virtual std::string get_identifier() const override {
        return this->symbolName->str();
    }
};


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

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->field->visit_ast( visitor, thisCursor, "field", context );
    }
};
