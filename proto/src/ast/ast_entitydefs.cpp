#include "ast_entitydefs.hpp"
#include "expr/ast_expr_node.hpp"
#include "symbol/type_base.hpp"


TxQualType TxTypeResolvingNode::resolve_type( TxPassInfo passInfo ) {
    ASSERT( this->is_context_set(), "Declaration pass has not been run (lexctx not set) before resolving " << this );
    if ( !this->_type ) {
        if ( this->hasResolved ) {
            throw resolution_error( this, "Previous type resolution failed in " + this->str() );
        }
        LOG_TRACE( this->LOGGER(), "resolving type  of " << this );

        if ( this->startedRslv ) {
            CERR_THROWRES( this, "Recursive definition of type '" << this->get_descriptor() << "'" );
        }
        this->startedRslv = true;
        try {
            this->_type = this->define_type( passInfo );
        }
        catch ( const resolution_error& err ) {
            this->hasResolved = true;
            //LOG(this->LOGGER(), DEBUG, "Caught and re-threw resolution error in " << this << ": " << err);
            throw;
        }
        ASSERT( this->_type, "NULL-resolved type but no exception thrown in " << this );
        this->hasResolved = true;
    }
    if ( is_full_resolution( passInfo ) && !this->_type->is_integrated() ) {
        const_cast<TxActualType*>(this->_type.type())->integrate();
    }
    return this->_type;
}
