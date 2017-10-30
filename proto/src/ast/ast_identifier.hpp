#pragma once

#include "ast/ast_node.hpp"


/** Represents a simple/plain identifier (not a '.'-separated sequence of identifiers). */
class TxIdentifierNode : public TxNode {
    std::string _identifier;

public:
    TxIdentifierNode( const TxLocation& ploc, const std::string& identifier )
            : TxNode( ploc ), _identifier( identifier ) {
        ASSERT( _identifier.find( '.' ) == std::string::npos, "Invalid identifier: \"" << _identifier << "\"" );
    }

    TxIdentifierNode( const TxLocation& ploc, std::string&& identifier )
            : TxNode( ploc ), _identifier( identifier ) {
    }

    virtual TxIdentifierNode* make_ast_copy() const override {
        return new TxIdentifierNode( this->ploc, this->_identifier );
    }

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
    }

    const std::string& ident() {
        return this->_identifier;
    }

    virtual const std::string& get_descriptor() const override {
        return this->_identifier;
    }
};
