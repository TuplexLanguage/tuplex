#pragma once

#include "ast_stmt_node.hpp"

#include "ast/ast_fielddef_node.hpp"
#include "ast/expr/ast_assignee_node.hpp"
#include "ast/expr/ast_exprs.hpp"


/** Local field declaration */
class TxFieldStmtNode : public TxStatementNode {
    TxDeclarationFlags declFlags = TXD_NONE;

protected:
    virtual void set_exp_error_stmt() override {
        this->declFlags = TXD_EXPERROR;
    }

    virtual void stmt_declaration_pass() override;

    virtual void verification_pass() const override;

public:
    TxLocalFieldDefNode* fieldDef;

    TxFieldStmtNode( const TxLocation& ploc, TxLocalFieldDefNode* fieldDef )
            : TxStatementNode( ploc ), fieldDef( fieldDef ) {
    }

    virtual TxFieldStmtNode* make_ast_copy() const override {
        return new TxFieldStmtNode( this->ploc, this->fieldDef->make_ast_copy() );
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->fieldDef->visit_ast( visitor, thisCursor, "fielddef", context );
    }
};

/** Local type declaration */
class TxTypeStmtNode : public TxStatementNode {
protected:
    virtual void set_exp_error_stmt() override {
        this->typeDecl->declFlags |= TXD_EXPERROR;
    }

public:
    TxTypeDeclNode* const typeDecl;

    TxTypeStmtNode( const TxLocation& ploc, TxTypeDeclNode* typeDecl )
            : TxStatementNode( ploc ), typeDecl( typeDecl ) {
    }

    TxTypeStmtNode( const TxLocation& ploc, TxIdentifierNode* typeName,
                    const std::vector<TxDeclarationNode*>* typeParamDecls,
                    TxTypeCreatingNode* typeCreatingNode, bool interfaceKW = false, bool mutableType = false )
            : TxTypeStmtNode( ploc, new TxTypeDeclNode( ploc, TXD_NONE, typeName, typeParamDecls, typeCreatingNode,
                                                        interfaceKW, mutableType ) )
    {
    }

    virtual TxTypeStmtNode* make_ast_copy() const override {
        return new TxTypeStmtNode( this->ploc, this->typeDecl->make_ast_copy() );
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->typeDecl->visit_ast( visitor, thisCursor, "typedecl", context );
    }
};


/** A statement that executes an expression. */
class TxExprStmtNode : public TxStatementNode {
public:
    TxExpressionNode* expr;

    TxExprStmtNode( const TxLocation& ploc, TxExpressionNode* expr ) : TxStatementNode( ploc ), expr( expr )  { }

    TxExprStmtNode( TxExpressionNode* expr ) : TxExprStmtNode( expr->ploc, expr )  { }

    virtual TxExprStmtNode* make_ast_copy() const override {
        return new TxExprStmtNode( this->ploc, this->expr->make_ast_copy() );
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->expr->visit_ast( visitor, thisCursor, "expr", context );
    }
};

/* Executes a function call without assigning its return value, if any. */
class TxCallStmtNode : public TxExprStmtNode {
public:
    TxCallStmtNode( const TxLocation& ploc, TxFunctionCallNode* call ) : TxExprStmtNode( ploc, call )  { }

    virtual TxCallStmtNode* make_ast_copy() const override {
        return new TxCallStmtNode( this->ploc, static_cast<TxFunctionCallNode*>( this->expr )->make_ast_copy() );
    }
};


class TxSelfSuperFieldsStmtNode : public TxStatementNode {
    TxLocalFieldDefNode* selfRefNode;
    TxLocalFieldDefNode* superRefNode;

protected:
    virtual void stmt_declaration_pass() override;

public:
    TxSelfSuperFieldsStmtNode( const TxLocation& ploc );

    virtual TxSelfSuperFieldsStmtNode* make_ast_copy() const override {
        return new TxSelfSuperFieldsStmtNode( this->ploc );
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->selfRefNode->visit_ast( visitor, thisCursor, "selfref", context );
        this->superRefNode->visit_ast( visitor, thisCursor, "superref", context );
    }
};

class TxMemberInitNode : public TxStatementNode {
    TxFunctionCallNode* constructorCallExpr;

    TxMemberInitNode( const TxLocation& ploc, TxFunctionCallNode* constructorCallExpr );

public:
    TxMemberInitNode( const TxLocation& ploc, TxIdentifierNode* identifier, const std::vector<TxExpressionNode*>* argsExprList );

    virtual TxMemberInitNode* make_ast_copy() const override {
        return new TxMemberInitNode( this->ploc, this->constructorCallExpr );
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->constructorCallExpr->visit_ast( visitor, thisCursor, "initializer", context );
    }

    virtual const std::string& get_descriptor() const override {
        return this->constructorCallExpr->get_descriptor();
    }
};

/* Represents the #init ... ; / self() special initialization statement within a constructor. */
class TxInitStmtNode : public TxStatementNode {
    TxSelfSuperFieldsStmtNode* selfSuperStmt;
    std::vector<TxMemberInitNode*>* initClauseList;

protected:
    virtual void stmt_declaration_pass() override;

    virtual void verification_pass() const override;

public:
    TxInitStmtNode( const TxLocation& ploc, std::vector<TxMemberInitNode*>* initClauseList );

    /** The #self( ... ); short-hand */
    TxInitStmtNode( const TxLocation& ploc, std::vector<TxExpressionNode*>* argsExprList );

    /** The #init; short-hand */
    TxInitStmtNode( const TxLocation& ploc );

    virtual TxInitStmtNode* make_ast_copy() const override {
        return new TxInitStmtNode( this->ploc, make_node_vec_copy( this->initClauseList ) );
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->selfSuperStmt->visit_ast( visitor, thisCursor, "self", context );
        for ( auto initClause : *this->initClauseList )
            initClause->visit_ast( visitor, thisCursor, "memberinit", context );
    }
};


class TxTerminalStmtNode : public TxStatementNode {
protected:
    TxTerminalStmtNode( const TxLocation& ploc )
            : TxStatementNode( ploc ) {
    }

    virtual bool ends_with_terminal_stmt() const override final {
        return true;
    }
};

class TxReturnStmtNode : public TxTerminalStmtNode {
protected:
    virtual void resolution_pass() override;

public:
    TxMaybeConversionNode* expr;

    TxReturnStmtNode( const TxLocation& ploc )
            : TxTerminalStmtNode( ploc ), expr() {
    }
    TxReturnStmtNode( const TxLocation& ploc, TxExpressionNode* expr )
            : TxTerminalStmtNode( ploc ), expr( new TxMaybeConversionNode( expr ) ) {
    }

    virtual TxReturnStmtNode* make_ast_copy() const override {
        return new TxReturnStmtNode( this->ploc, this->expr->originalExpr->make_ast_copy() );
    }

    virtual bool ends_with_return_stmt() const override {
        return true;
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if (this->expr)
            this->expr->visit_ast( visitor, thisCursor, "value", context );
    }
};

class TxBreakStmtNode : public TxTerminalStmtNode {
public:
    TxBreakStmtNode( const TxLocation& ploc )
            : TxTerminalStmtNode( ploc ) {
    }

    virtual TxBreakStmtNode* make_ast_copy() const override {
        return new TxBreakStmtNode( this->ploc );
    }

    virtual bool may_end_with_non_return_stmt() const override {
        return true;
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
    }
};

class TxContinueStmtNode : public TxTerminalStmtNode {
public:
    TxContinueStmtNode( const TxLocation& ploc )
            : TxTerminalStmtNode( ploc ) {
    }

    virtual TxContinueStmtNode* make_ast_copy() const override {
        return new TxContinueStmtNode( this->ploc );
    }

    virtual bool may_end_with_non_return_stmt() const override {
        return true;
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
    }
};

class TxSuiteNode : public TxStatementNode {
protected:
    virtual void stmt_declaration_pass() override;

    virtual void verification_pass() const override {
        TxStatementNode* prev_stmt = nullptr;
        for ( auto stmt : *this->suite ) {
            if ( prev_stmt && prev_stmt->ends_with_terminal_stmt() )
                CERROR( stmt, "This statement is unreachable." );
            prev_stmt = stmt;
        }
    }

public:
    std::vector<TxStatementNode*>* suite;

    TxSuiteNode( const TxLocation& ploc, std::vector<TxStatementNode*>* suite )
            : TxStatementNode( ploc ), suite( suite ) {
        if (suite->size() > 1) {
            // inject predecessor links
            TxStatementNode* pred = suite->front();
            for ( auto stmtI = std::next( suite->begin() ); stmtI != suite->end(); stmtI++ ) {
                (*stmtI)->predecessor = pred;
                pred = (*stmtI);
            }
        }
    }
    TxSuiteNode( const TxLocation& ploc )
            : TxSuiteNode( ploc, new std::vector<TxStatementNode*>() ) {
    }

    virtual TxSuiteNode* make_ast_copy() const override {
        return new TxSuiteNode( this->ploc, make_node_vec_copy( this->suite ) );
    }

    virtual bool may_end_with_non_return_stmt() const override {
        for ( auto stmt : *this->suite )
            if ( stmt->may_end_with_non_return_stmt() )
                return true;
        return false;
    }
    virtual bool ends_with_terminal_stmt() const override {
        return ( !this->suite->empty() && this->suite->back()->ends_with_terminal_stmt() );
    }
    virtual bool ends_with_return_stmt() const override {
        if ( this->suite->empty() )
            return false;
        for ( auto stmt : *this->suite ) {
            if ( stmt->may_end_with_non_return_stmt() )
                return false;
        }
        return this->suite->back()->ends_with_return_stmt();
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        for ( auto stmt : *this->suite )
            stmt->visit_ast( visitor, thisCursor, "stmt", context );
    }
};

class TxAssignStmtNode : public TxStatementNode {
protected:
    virtual void resolution_pass() override;

    virtual void verification_pass() const override;

public:
    TxAssigneeNode* lvalue;
    TxMaybeConversionNode* rvalue;

    TxAssignStmtNode( const TxLocation& ploc, TxAssigneeNode* lvalue, TxExpressionNode* rvalue )
            : TxStatementNode( ploc ), lvalue( lvalue ), rvalue( new TxMaybeConversionNode( rvalue ) ) {
    }

    virtual TxAssignStmtNode* make_ast_copy() const override {
        return new TxAssignStmtNode( this->ploc, this->lvalue->make_ast_copy(), this->rvalue->originalExpr->make_ast_copy() );
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->lvalue->visit_ast( visitor, thisCursor, "lvalue", context );
        this->rvalue->visit_ast( visitor, thisCursor, "rvalue", context );
    }


    static void code_gen_array_copy( LlvmGenerationContext& context, GenScope* scope, const TxActualType* lvalType,
                                     llvm::Value* lvalTypeIdV, llvm::Value* lvalArrayPtrV, llvm::Value* rvalArrayPtrV );
};


class TxExpErrStmtNode : public TxStatementNode {
    ExpectedErrorClause* expError;

protected:
    virtual void stmt_declaration_pass() override;

public:
    TxStatementNode* body;

    TxExpErrStmtNode( const TxLocation& ploc, ExpectedErrorClause* expError, TxStatementNode* body )
            : TxStatementNode( ploc ), expError( expError ), body( body ) {
        if ( dynamic_cast<const TxExpErrStmtNode*>( body ) )
            CERROR( this, "Can't nest Expected Error constructs in a statement" );
        body->set_exp_error_stmt();
    }

    virtual TxExpErrStmtNode* make_ast_copy() const override {
        return new TxExpErrStmtNode( this->ploc, new ExpectedErrorClause(), this->body->make_ast_copy() );
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override { }

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->body->visit_ast( visitor, thisCursor, "stmt", context );
    }
};

class TxNoOpStmtNode : public TxStatementNode {
public:
    TxNoOpStmtNode( const TxLocation& ploc )
            : TxStatementNode( ploc ) {
    }

    virtual TxNoOpStmtNode* make_ast_copy() const override {
        return new TxNoOpStmtNode( this->ploc );
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override { }

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
    }
};
