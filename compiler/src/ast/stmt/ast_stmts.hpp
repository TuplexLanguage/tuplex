#pragma once

#include "ast_stmt_node.hpp"

#include "ast/ast_fielddef_node.hpp"
#include "ast/expr/ast_assignee_node.hpp"
#include "ast/expr/ast_exprs.hpp"


/** Local field declaration */
class TxFieldStmtNode : public TxStatementNode {
    TxDeclarationFlags declFlags = TXD_NONE;

protected:
    void set_exp_error_stmt() override {
        this->declFlags = TXD_EXPERROR;
    }

    void stmt_declaration_pass() override;

    void verification_pass() const override;

public:
    TxLocalFieldDefNode* fieldDef;

    TxFieldStmtNode( const TxLocation& ploc, TxLocalFieldDefNode* fieldDef )
            : TxStatementNode( ploc ), fieldDef( fieldDef ) {
    }

    TxFieldStmtNode* make_ast_copy() const override {
        return new TxFieldStmtNode( this->ploc, this->fieldDef->make_ast_copy() );
    }

    void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->fieldDef->visit_ast( visitor, cursor, "fielddef", aux );
    }
};

/** Local type declaration */
class TxTypeStmtNode : public TxStatementNode {
protected:
    void set_exp_error_stmt() override {
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

    TxTypeStmtNode* make_ast_copy() const override {
        return new TxTypeStmtNode( this->ploc, this->typeDecl->make_ast_copy() );
    }

    void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->typeDecl->visit_ast( visitor, cursor, "typedecl", aux );
    }
};


/** A statement that executes an expression. */
class TxExprStmtNode : public TxStatementNode {
public:
    TxExpressionNode* expr;

    TxExprStmtNode( const TxLocation& ploc, TxExpressionNode* expr ) : TxStatementNode( ploc ), expr( expr )  { }

    explicit TxExprStmtNode( TxExpressionNode* expr ) : TxExprStmtNode( expr->ploc, expr )  { }

    TxExprStmtNode* make_ast_copy() const override {
        return new TxExprStmtNode( this->ploc, this->expr->make_ast_copy() );
    }

    void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->expr->visit_ast( visitor, cursor, "expr", aux );
    }
};

/* Executes a function call without assigning its return value, if any. */
class TxCallStmtNode : public TxExprStmtNode {
public:
    TxCallStmtNode( const TxLocation& ploc, TxFunctionCallNode* call ) : TxExprStmtNode( ploc, call )  { }

    TxCallStmtNode* make_ast_copy() const override {
        return new TxCallStmtNode( this->ploc, static_cast<TxFunctionCallNode*>( this->expr )->make_ast_copy() );
    }
};


class TxSelfSuperFieldsStmtNode : public TxStatementNode {
    TxLocalFieldDefNode* selfRefNode;
    TxLocalFieldDefNode* superRefNode;

protected:
    void stmt_declaration_pass() override;

public:
    explicit TxSelfSuperFieldsStmtNode( const TxLocation& ploc );

    TxSelfSuperFieldsStmtNode* make_ast_copy() const override {
        return new TxSelfSuperFieldsStmtNode( this->ploc );
    }

    /** Returns the current type (Self) */
    TxQualType resolve_type( TxTypeResLevel typeResLevel ) {
        return selfRefNode->resolve_type( typeResLevel )->target_type();
    }

    /** Returns the current type (Self) */
    TxQualType qtype() const {
        return selfRefNode->qtype()->target_type();
    }

    void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->selfRefNode->visit_ast( visitor, cursor, "selfref", aux );
        this->superRefNode->visit_ast( visitor, cursor, "superref", aux );
    }
};

class TxMemberInitNode : public TxStatementNode {
    TxIdentifierNode* identifier;
    const std::vector<TxExpressionNode*>* argsExprList;
    TxFunctionCallNode* constructorCallExpr;

public:
    TxMemberInitNode( const TxLocation& ploc, TxIdentifierNode* identifier, const std::vector<TxExpressionNode*>* argsExprList );

    TxMemberInitNode* make_ast_copy() const override {
        return new TxMemberInitNode( this->ploc, this->identifier->make_ast_copy(), make_node_vec_copy( this->argsExprList ) );
    }

    void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override;

    const TxIdentifierNode* get_identifier() const {
        return this->identifier;
    }

    [[maybe_unused]] const std::vector<TxExpressionNode*>* get_args_expr_list() const {
        return this->argsExprList;
    }

    const std::string& get_descriptor() const override {
        return this->identifier->get_descriptor();
    }
};

/* Represents the #init ... ; / self() special initialization statement within a constructor. */
class TxInitStmtNode : public TxStatementNode {
    TxSelfSuperFieldsStmtNode* selfSuperStmt;
    std::vector<TxMemberInitNode*>* initClauseList;

protected:
    void stmt_declaration_pass() override;

    void resolution_pass() override;

    void verification_pass() const override;

public:
    TxInitStmtNode( const TxLocation& ploc, std::vector<TxMemberInitNode*>* initClauseList );

    /** The #self( ... ); short-hand */
    TxInitStmtNode( const TxLocation& ploc, std::vector<TxExpressionNode*>* argsExprList );

    /** The #init; short-hand */
    explicit TxInitStmtNode( const TxLocation& ploc );

    TxInitStmtNode* make_ast_copy() const override {
        return new TxInitStmtNode( this->ploc, make_node_vec_copy( this->initClauseList ) );
    }

    void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->selfSuperStmt->visit_ast( visitor, cursor, "self", aux );
        for ( auto initClause : *this->initClauseList )
            initClause->visit_ast( visitor, cursor, "init-clause", aux );
    }
};


class TxTerminalStmtNode : public TxStatementNode {
protected:
    explicit TxTerminalStmtNode( const TxLocation& ploc )
            : TxStatementNode( ploc ) {
    }

    bool ends_with_terminal_stmt() const final {
        return true;
    }
};

class TxReturnStmtNode : public TxTerminalStmtNode {
protected:
    void resolution_pass() override;

public:
    TxMaybeConversionNode* expr;

    explicit TxReturnStmtNode( const TxLocation& ploc )
            : TxTerminalStmtNode( ploc ), expr() {
    }
    TxReturnStmtNode( const TxLocation& ploc, TxExpressionNode* expr )
            : TxTerminalStmtNode( ploc ), expr( new TxMaybeConversionNode( expr ) ) {
    }

    TxReturnStmtNode* make_ast_copy() const override {
        return new TxReturnStmtNode( this->ploc, this->expr->originalExpr->make_ast_copy() );
    }

    bool ends_with_return_stmt() const override {
        return true;
    }

    void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        if (this->expr)
            this->expr->visit_ast( visitor, cursor, "value", aux );
    }
};

class TxBreakStmtNode : public TxTerminalStmtNode {
public:
    explicit TxBreakStmtNode( const TxLocation& ploc )
            : TxTerminalStmtNode( ploc ) {
    }

    TxBreakStmtNode* make_ast_copy() const override {
        return new TxBreakStmtNode( this->ploc );
    }

    bool may_end_with_non_return_stmt() const override {
        return true;
    }

    void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
    }
};

class TxContinueStmtNode : public TxTerminalStmtNode {
public:
    explicit TxContinueStmtNode( const TxLocation& ploc )
            : TxTerminalStmtNode( ploc ) {
    }

    TxContinueStmtNode* make_ast_copy() const override {
        return new TxContinueStmtNode( this->ploc );
    }

    bool may_end_with_non_return_stmt() const override {
        return true;
    }

    void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
    }
};

class TxSuiteNode : public TxStatementNode {
protected:
    void stmt_declaration_pass() override;

    void verification_pass() const override {
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
    explicit TxSuiteNode( const TxLocation& ploc )
            : TxSuiteNode( ploc, new std::vector<TxStatementNode*>() ) {
    }

    TxSuiteNode* make_ast_copy() const override {
        return new TxSuiteNode( this->ploc, make_node_vec_copy( this->suite ) );
    }

    bool may_end_with_non_return_stmt() const override {
        for ( auto stmt : *this->suite )
            if ( stmt->may_end_with_non_return_stmt() )
                return true;
        return false;
    }
    bool ends_with_terminal_stmt() const override {
        return ( !this->suite->empty() && this->suite->back()->ends_with_terminal_stmt() );
    }
    bool ends_with_return_stmt() const override {
        if ( this->suite->empty() )
            return false;
        for ( auto stmt : *this->suite ) {
            if ( stmt->may_end_with_non_return_stmt() )
                return false;
        }
        return this->suite->back()->ends_with_return_stmt();
    }

    void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        for ( auto stmt : *this->suite )
            stmt->visit_ast( visitor, cursor, "stmt", aux );
    }
};

class TxAssignStmtNode : public TxStatementNode {
protected:
    void resolution_pass() override;

    void verification_pass() const override;

public:
    TxAssigneeNode* lvalue;
    TxMaybeConversionNode* rvalue;

    TxAssignStmtNode( const TxLocation& ploc, TxAssigneeNode* lvalue, TxExpressionNode* rvalue )
            : TxStatementNode( ploc ), lvalue( lvalue ), rvalue( new TxMaybeConversionNode( rvalue ) ) {
    }

    TxAssignStmtNode* make_ast_copy() const override {
        return new TxAssignStmtNode( this->ploc, this->lvalue->make_ast_copy(), this->rvalue->originalExpr->make_ast_copy() );
    }

    void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->lvalue->visit_ast( visitor, cursor, "lvalue", aux );
        this->rvalue->visit_ast( visitor, cursor, "rvalue", aux );
    }


    static void code_gen_array_copy( const TxNode* origin, LlvmGenerationContext& context, GenScope* scope, const TxActualType* lvalType,
                                     llvm::Value* lvalTypeIdV, llvm::Value* lvalArrayPtrV, llvm::Value* rvalArrayPtrV );
};


class TxExpErrStmtNode : public TxStatementNode {
    ExpectedErrorClause* expError;

protected:
    void stmt_declaration_pass() override;

public:
    TxStatementNode* body;

    TxExpErrStmtNode( const TxLocation& ploc, ExpectedErrorClause* expError, TxStatementNode* body )
            : TxStatementNode( ploc ), expError( expError ), body( body ) {
        if ( dynamic_cast<const TxExpErrStmtNode*>( body ) )
            CERROR( this, "Can't nest Expected Error constructs in a statement" );
        body->set_exp_error_stmt();
    }

    TxExpErrStmtNode* make_ast_copy() const override {
        return new TxExpErrStmtNode( this->ploc, new ExpectedErrorClause(), this->body->make_ast_copy() );
    }

    void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override { }

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->body->visit_ast( visitor, cursor, "stmt", aux );
    }
};

class TxNoOpStmtNode : public TxStatementNode {
public:
    explicit TxNoOpStmtNode( const TxLocation& ploc )
            : TxStatementNode( ploc ) {
    }

    TxNoOpStmtNode* make_ast_copy() const override {
        return new TxNoOpStmtNode( this->ploc );
    }

    void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override { }

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
    }
};
