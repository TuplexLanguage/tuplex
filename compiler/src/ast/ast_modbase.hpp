#pragma once

#include "ast_node.hpp"

#include "symbol/package.hpp"
#include "symbol/module.hpp"

class TxDeclarationNode;

class TxImportNode : public TxNode {
protected:
    virtual void declaration_pass() override {
        static_cast<TxModule*>( this->context().scope() )->register_import( *this, *this->ident );
    }

public:
    const TxIdentifier* ident;

    TxImportNode( const TxLocation& ploc, const TxIdentifier* identifier );

    virtual TxImportNode* make_ast_copy() const override {
        return new TxImportNode( this->ploc, this->ident );
    }

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
    }

    virtual const std::string& get_descriptor() const override {
        return this->ident->str();
    }
};

class TxModuleNode : public TxNode {
    const TxIdentifier* ident;
    std::vector<TxImportNode*>* imports;
    std::vector<TxDeclarationNode*>* members;
    std::vector<TxModuleNode*>* subModules;
    bool builtin;

protected:
    virtual void declaration_pass() override {
        this->lexContext._scope = static_cast<TxModule*>( this->lexContext._scope )->declare_module( *this, *this->ident, this->builtin );
    }

public:
    TxModuleNode( const TxLocation& ploc, const TxIdentifier* identifier,
                  std::vector<TxImportNode*>* imports,
                  std::vector<TxDeclarationNode*>* members,
                  std::vector<TxModuleNode*>* subModules,
                  bool builtin = false )
            : TxNode( ploc ), ident( identifier ), imports( imports ), members( members ), subModules( subModules ), builtin( builtin ) {
        ASSERT( identifier, "NULL identifier" );  // (sanity check on parser)
    }

    virtual TxModuleNode* make_ast_copy() const override;

    void code_gen( LlvmGenerationContext& context ) const;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override;

    virtual const std::string& get_descriptor() const override {
        return this->ident->str();
    }
};

/** Represents a parsing unit, i.e. a given source text input (e.g. source file). */
class TxParsingUnitNode : public TxNode {
public:
    TxModuleNode* module;

    TxParsingUnitNode( const TxLocation& ploc, TxModuleNode* module )
            : TxNode( ploc ), module( module ) {
    }

    virtual TxParsingUnitNode* make_ast_copy() const override {
        ASSERT( false, "Can't make AST copy of TxParsingUnitNode " << this );
        return nullptr;
    }

    inline void set_context( TxPackage* package ) {
        ASSERT( !this->is_context_set(), "lexicalContext already initialized in " << this->str() );
        this->lexContext = LexicalContext( package );
    }

    void code_gen( LlvmGenerationContext& context ) const;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->module->visit_ast( visitor, thisCursor, "module", context );
    }
};
