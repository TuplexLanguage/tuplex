#pragma once

#include "../stmt/ast_stmts.hpp"
#include "ast/type/ast_typeexpr_node.hpp"
#include "ast/type/ast_funcheader_node.hpp"
#include "ast/ast_fielddef_node.hpp"


llvm::Value* gen_lambda( LlvmGenerationContext& context, GenScope* scope, llvm::Type* lambdaT, llvm::Value* funcV, llvm::Value* closureRefV );

llvm::Constant* gen_lambda( LlvmGenerationContext& context, llvm::Type* lambdaT, llvm::Constant* funcC, llvm::Constant* closureRefC );


class TxLambdaExprNode : public TxExpressionNode {
    bool instanceMethod = false;
    TxTypeExpressionNode* selfTypeNode = nullptr;
    TxLocalFieldDefNode* selfRefNode = nullptr;
    TxLocalFieldDefNode* superRefNode = nullptr;
    const TxTypeDeclaration* constructedObjTypeDecl = nullptr;

    /** Generates the function declaration. */
    llvm::Function* code_gen_function_decl( LlvmGenerationContext& context ) const;

    /** Generates the function body. code_gen_function_decl() must have been called prior to this call. */
    void code_gen_function_body( LlvmGenerationContext& context ) const;

    mutable llvm::Function* functionPtr = nullptr;

protected:
    virtual void declaration_pass() override;

    virtual const TxQualType* define_type() override {
        return this->funcHeaderNode->resolve_type();
    }

public:
    TxFunctionHeaderNode* funcHeaderNode;
    TxSuiteNode* suite;
    const bool isMethodSyntax;

    TxLambdaExprNode( const TxLocation& ploc, TxFunctionTypeNode* funcTypeNode, TxSuiteNode* suite, bool isMethodSyntax = false,
                      bool suppressSuper = false )
            : TxLambdaExprNode( ploc, new TxFunctionHeaderNode( funcTypeNode ), suite, isMethodSyntax, suppressSuper ) {
    }

    TxLambdaExprNode( const TxLocation& ploc, TxFunctionHeaderNode* funcHeaderNode, TxSuiteNode* suite, bool isMethodSyntax = false,
                      bool suppressSuper = false )
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
            this->selfRefNode = new TxLocalFieldDefNode( this->ploc, "self", selfRefTypeExprN, nullptr );

            // 'super' reference
            if ( !suppressSuper ) {
                auto superRefTypeExprN = new TxReferenceTypeNode( this->ploc, nullptr, new TxNamedTypeNode( this->ploc, "Super" ) );
                this->superRefNode = new TxLocalFieldDefNode( this->ploc, "super", superRefTypeExprN, nullptr );
                // FUTURE: if type is modifiable, the super target type should in some cases perhaps be modifiable as well?
            }
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
    bool is_suppressed_modifying_method() const;

    virtual void symbol_resolution_pass() override;

    virtual bool is_statically_constant() const override {
        return true;
    }

    /** Only generates the declaration of this expression, not its body.
     * May be invoked multiple times. */
    virtual llvm::Constant* code_gen_const_decl( LlvmGenerationContext& context ) const;

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->funcHeaderNode->visit_ast( visitor, thisCursor, "functype", context );
        if ( this->is_instance_method() ) {
            this->selfRefNode->visit_ast( visitor, thisCursor, "selfref", context );
            if ( this->superRefNode )
                this->superRefNode->visit_ast( visitor, thisCursor, "superref", context );
        }
        this->suite->visit_ast( visitor, thisCursor, "suite", context );
    }
};
