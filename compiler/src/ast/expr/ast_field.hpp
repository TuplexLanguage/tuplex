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

/** Resolve a constructor, that is potentially overloaded, for the specified type.
 * Generates error and throws resolution exception if not successful.
 */
const TxFieldDeclaration* resolve_constructor( TxExpressionNode* origin, const TxActualType* allocType,
                                               const std::vector<TxExpressionNode*>* arguments );


/** Returns an instance method lambda object value. */
llvm::Value* instance_method_value_code_gen( LlvmGenerationContext& context, GenScope* scope,
                                       const TxActualType* staticBaseType, llvm::Value* runtimeBaseTypeIdV, llvm::Value* basePtrV,
                                       const TxActualType* fieldType, const std::string& fieldName,
                                       bool nonvirtualLookup );

class TxFieldValueNode;
TxFieldValueNode* make_compound_symbol_expression( const TxLocation& ploc, const std::string& compoundName );

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

public:
    TxTypeResolvingNode* baseExpr;
    TxIdentifierNode* symbolName;

    /** Creates a new TxFieldValueNode.
     * @param base is the base expression (preceding expression adjoined with the '.' operator), or NULL if none
     * @param member is the specified literal field name
     */
    TxFieldValueNode( const TxLocation& ploc, TxTypeResolvingNode* base, TxIdentifierNode* memberName )
            : TxExpressionNode( ploc ), baseExpr( base ), symbolName( memberName ) {
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

    virtual bool is_value() const override;

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override;

    virtual TxFieldStorage get_storage() const override;

    virtual bool is_statically_constant() const override;

    /** Returns null if this wasn't resolved to a field. (should not be called before symbol is resolved) */
    inline const TxField* field() const {
        //ASSERT( this->_field, "field not resolved: " << this );
        return this->_field;
    }

    inline const TxEntityDeclaration* get_declaration() const {
        return this->declaration;
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


/** Identifies a field via name. */
class TxNamedFieldNode : public TxExpressionNode {
protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override {
        return this->exprNode->resolve_type( passInfo );
    }

    virtual void verification_pass() const override;

public:
    TxFieldValueNode* exprNode;

    TxNamedFieldNode( const TxLocation& ploc, TxFieldValueNode* exprNode )
            : TxExpressionNode( ploc ), exprNode( exprNode ) {
    }

    TxNamedFieldNode( const TxLocation& ploc, const std::string& compoundName )
            : TxExpressionNode( ploc ), exprNode( make_compound_symbol_expression( ploc, compoundName ) ) {
    }

    virtual TxNamedFieldNode* make_ast_copy() const override {
        return new TxNamedFieldNode( this->ploc, exprNode->make_ast_copy() );
    }

    virtual const std::vector<TxExpressionNode*>* get_applied_func_args() const override {
        return this->exprNode->get_applied_func_args();
    }
    virtual void set_applied_func_args( const std::vector<TxExpressionNode*>* appliedTypeParameters ) override {
        this->exprNode->set_applied_func_args( appliedTypeParameters );
    }

    virtual const TxActualType* get_constructed_type( TxPassInfo passInfo ) const override {
        return this->exprNode->get_constructed_type( passInfo );
    }


    virtual bool is_value() const override {
        return this->exprNode->is_value();
    }
    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->exprNode->get_data_graph_origin_expr();
    }
    virtual TxFieldStorage get_storage() const override {
        return this->exprNode->get_storage();
    }
    virtual bool is_statically_constant() const override {
        return this->exprNode->is_statically_constant();
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override {
        return this->exprNode->code_gen_const_value( context );
    }
    virtual llvm::Constant* code_gen_const_address( LlvmGenerationContext& context ) const override {
        return this->exprNode->code_gen_const_address( context );
    }
    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override {
        return this->exprNode->code_gen_dyn_address( context, scope );
    }
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override {
        return this->exprNode->code_gen_dyn_value( context, scope );
    }

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->exprNode->visit_ast( visitor, thisCursor, "expr", context );
    }

    virtual const std::string& get_descriptor() const override {
        return this->exprNode->get_descriptor();
    }
};


class TxFieldAssigneeNode : public TxAssigneeNode {
protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override {
        return this->fieldNode->resolve_type( passInfo );
    }

    virtual void verification_pass() const override;

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
