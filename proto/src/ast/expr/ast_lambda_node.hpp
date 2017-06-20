#pragma once

#include "../stmt/ast_stmts.hpp"
#include "ast/type/ast_typeexpr_node.hpp"
#include "ast/type/ast_funcheader_node.hpp"
#include "ast/ast_fielddef_node.hpp"

// (requires types, expressions, and statements to be included before this)

class TxLambdaExprNode : public TxExpressionNode {
    bool instanceMethod = false;
    TxTypeExpressionNode* selfTypeNode = nullptr;
    TxFieldDefNode* selfRefNode = nullptr;
    TxFieldDefNode* superRefNode = nullptr;
    const TxTypeDeclaration* constructedObjTypeDecl = nullptr;

protected:
    virtual void declaration_pass() override;

    virtual const TxType* define_type() override {
        return this->funcHeaderNode->resolve_type();
    }

public:
    TxFunctionHeaderNode* funcHeaderNode;
    TxSuiteNode* suite;
    const bool isMethodSyntax;

    TxLambdaExprNode( const TxLocation& ploc, TxFunctionTypeNode* funcTypeNode, TxSuiteNode* suite, bool isMethodSyntax = false )
            : TxLambdaExprNode( ploc, new TxFunctionHeaderNode( funcTypeNode ), suite, isMethodSyntax ) {
    }

    TxLambdaExprNode( const TxLocation& ploc, TxFunctionHeaderNode* funcHeaderNode, TxSuiteNode* suite, bool isMethodSyntax = false )
            : TxExpressionNode( ploc ), funcHeaderNode( funcHeaderNode ), suite( suite ), isMethodSyntax( isMethodSyntax ) {
        if ( isMethodSyntax ) {
            // 'self' reference:
            this->selfTypeNode = new TxNamedTypeNode( this->ploc, "Self" );
            TxTypeExpressionNode* selfRefTargetTypeNode;
            if ( this->funcHeaderNode->is_modifying() )
                selfRefTargetTypeNode = new TxModifiableTypeNode( this->ploc, this->selfTypeNode );
            else
                selfRefTargetTypeNode = this->selfTypeNode;
            auto selfRefTypeExprN = new TxReferenceTypeNode( this->ploc, nullptr, selfRefTargetTypeNode );
            this->selfRefNode = new TxFieldDefNode( this->ploc, "self", selfRefTypeExprN, nullptr );

            // 'super' reference
            auto superRefTypeExprN = new TxNamedTypeNode( this->ploc, "Super" );
            this->superRefNode = new TxFieldDefNode( this->ploc, "super", superRefTypeExprN, nullptr );
            // FUTURE: if type is modifiable, the super target type should in some cases perhaps be modifiable as well?
        }
    }

    virtual TxLambdaExprNode* make_ast_copy() const override {
        return new TxLambdaExprNode( this->ploc, this->funcHeaderNode->make_ast_copy(), this->suite->make_ast_copy(), this->isMethodSyntax );
    }

    void set_instance_method( bool flag ) {
        if ( flag && !this->isMethodSyntax ) {
            CERROR( this, "Function definition was expected to have instance method syntax" );
            this->instanceMethod = false;
        }
        else
            this->instanceMethod = flag;
    }

    /** Returns true if this lambda expression is an instance method (with a runtime-provided 'self' argument). */
    inline bool is_instance_method() const {
        return this->instanceMethod;
    }

    /** If non-null, this is a constructor and the declaration for the constructed object type is returned. */
    inline const TxTypeDeclaration* get_constructed() const {
        return this->constructedObjTypeDecl;
    }

    /** Returns true if this method is suppressed (as if it were abstract) due to being a modifying instance method
     * in an immutable specialization of a mutable generic type. */
    bool is_suppressed_modifying_method();

    virtual void symbol_resolution_pass() override;

    virtual bool is_statically_constant() const override {
        return true;
    }

    llvm::Function* code_gen_forward_decl( LlvmGenerationContext& context ) const;
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->funcHeaderNode->visit_ast( visitor, thisCursor, "functype", context );
        if ( this->is_instance_method() ) {
            this->selfRefNode->visit_ast( visitor, thisCursor, "selfref", context );
            this->superRefNode->visit_ast( visitor, thisCursor, "superref", context );
        }
        this->suite->visit_ast( visitor, thisCursor, "suite", context );
    }
};
