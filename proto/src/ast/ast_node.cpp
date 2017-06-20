#include "util/files_env.hpp"

#include "ast_node.hpp"
#include "ast_entitydefs.hpp"
#include "symbol/package.hpp"


Logger& TxNode::_LOG = Logger::get( "AST" );

unsigned TxNode::nextNodeId = 0;

std::string TxNode::str() const {
    auto ident = this->get_identifier();
    const size_t bsize = 128;
    char buf[bsize];
    std::string filename = ploc.begin.filename ? get_file_name( *ploc.begin.filename ) : "";
    snprintf( buf, bsize, "%s %-11s %4u %-24s %s", filename.c_str(), this->parse_loc_string().c_str(),
              this->get_node_id(), typeid(*this).name(), ident.c_str() );
    if ( this->lexContext.reinterpretationDefiner )
        return std::string( buf ) + " <: " + this->lexContext.reinterpretationDefiner->str();
    else
        return std::string( buf );
}

std::string TxNode::parse_loc_string() const {
    const size_t bsize = 32;
    char buf[bsize];
    if ( ploc.begin.line == ploc.end.line ) {
        int lcol = ( ploc.end.column > ploc.begin.column ) ? ploc.end.column : ploc.end.column;
        snprintf( buf, bsize, "%3d.%2d-%d", ploc.begin.line, ploc.begin.column, lcol );
    }
    else
        snprintf( buf, bsize, "%3d.%2d-%d.%d", ploc.begin.line, ploc.begin.column, ploc.end.line, ploc.end.column );
    return std::string( buf );
}

void TxNode::visit_ast( AstVisitor visitor, const AstCursor& parent, const std::string& role, void* context ) {
    visitor( this, parent, role, context );
    const AstCursor thisCursor( &parent, this );
    this->visit_descendants( visitor, thisCursor, role, context );
}

void TxNode::visit_ast( AstVisitor visitor, void* context ) {
    const AstCursor parent(nullptr);  // a 'null' parent
    this->visit_ast( visitor, parent, "", context );
}

TypeRegistry& TxNode::registry() const {
    return this->context().package()->registry();
}
