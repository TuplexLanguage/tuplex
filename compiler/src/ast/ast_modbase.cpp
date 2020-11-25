#include "ast_modbase.hpp"

#include "ast_entitydecls.hpp"
#include "ast_util.hpp"

#include "parsercontext.hpp"


TxImportNode::TxImportNode( const TxLocation& ploc, const TxIdentifier* identifier )
        : TxNode( ploc ), ident( identifier ) {
    // imports need to be added to the parser context upon AST creation, so that they will be parsed before the declaration pass:
    if ( !this->ident->is_qualified() )
        CERROR( this, "can't import unqualified identifier '" << this->ident << "'" );
    else {
        if ( !this->ploc.parserCtx->add_import( this->ident->parent() ) )
            CERROR( this, "Failed to import module (source not found): " << this->ident->parent() );
    }
}


TxModuleNode* TxModuleNode::make_ast_copy() const {
    return new TxModuleNode( this->ploc, this->ident,
                             make_node_vec_copy( imports ),
                             make_node_vec_copy( members ),
                             make_node_vec_copy( subModules ),
                             builtin );
}

void TxModuleNode::visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) {
    if ( this->imports ) {
        for ( auto imp : *this->imports )
            imp->visit_ast( visitor, thisCursor, "import", context );
    }
    if ( this->members ) {
        for ( auto mem : *this->members )
            mem->visit_ast( visitor, thisCursor, "member", context );
    }
    if ( this->subModules ) {
        for ( auto mod : *this->subModules )
            mod->visit_ast( visitor, thisCursor, "module", context );
    }
}
