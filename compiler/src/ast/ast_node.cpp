#include "util/files_env.hpp"

#include "ast_node.hpp"
#include "ast_entitydefs.hpp"
#include "symbol/package.hpp"
#include "parsercontext.hpp"

const std::string TxNode::EMPTY_STRING;  // default-initialized to ""

Logger& TxNode::ASTLOGGER = Logger::get( "AST" );

unsigned TxNode::nextNodeId = 0;

std::string TxNode::str() const {
    auto ident = this->get_descriptor();
    const size_t bsize = 512;
    char buf[bsize];
    snprintf( buf, bsize, "%-13s %4u %-24s %s", this->parse_loc_string().c_str(),
              this->get_node_id(), typeid(*this).name(), ident.c_str() );
    if ( this->lexContext.reinterpretationDefiner )
        return std::string( buf ) + " <: " + this->lexContext.reinterpretationDefiner->str();
    else
        return std::string( buf );
}

std::string TxNode::parse_loc_string() const {
    return format_location( this->ploc );
}

void TxNode::visit_ast( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) {
    if ( this->compilationErrors )
        return;
    try {
        if ( visitor.preFunc )
            visitor.preFunc( this, cursor, role, aux );

        const AstCursor childCursor( &cursor, this );
        this->visit_descendants( visitor, childCursor, role, aux );

        if ( visitor.postFunc )
            visitor.postFunc( this, cursor, role, aux );
    }
    catch ( const compilation_error& err ) {
        LOG_TRACE( this->LOGGER(), "Caught compilation error in node_declaration_pass() in " << this << ": " << err );
        this->compilationErrors = 1; //TXP_DECLARATION;  // TODO: pass pass-info in the visitors
    }
}

TypeRegistry& TxNode::registry() const {
    return this->context().package()->registry();
}