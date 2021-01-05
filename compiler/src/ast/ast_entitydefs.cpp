#include "ast_entitydefs.hpp"
#include "symbol/type_base.hpp"

//static unsigned indent = 0;

TxQualType TxTypeResolvingNode::resolve_type( TxTypeResLevel typeResLevel ) {
    ASSERT( this->is_context_set(), "Declaration pass has not been run (lexctx not set) before resolving " << this );
    if ( !this->_type ) {
        //std::cerr << std::string( indent, ' ' ) << "starting resolve of " << this << ", pass " << typeResLevel << std::endl; indent += 2;
        if ( this->hasResolved ) {
            throw resolution_error( this, "Previous type resolution failed in " + this->str() );
        }
        LOG_TRACE( this->LOGGER(), "resolving type  of " << this );

        if ( this->startedRslv ) {
            CERR_THROWRES( this, "Recursive definition of type '" << this->get_descriptor() << "'" );
        }
        this->startedRslv = true;
        try {
            this->_type = this->define_type( typeResLevel );
            //indent -= 2; std::cerr << std::string( indent, ' ' ) << "completed resolve, type: " << this->_type << std::endl;
        }
        catch ( const resolution_error& err ) {
            this->hasResolved = true;
            //LOG(this->LOGGER(), DEBUG, "Caught and re-threw resolution error in " << this << ": " << err);
            throw;
        }
        ASSERT( this->_type, "NULL-resolved type but no exception thrown in " << this );
        this->hasResolved = true;
    }
    if ( typeResLevel == TXP_FULL_RESOLUTION && !this->_type->is_integrated() ) {
        const_cast<TxActualType*>(this->_type.type())->integrate();
    }
    return this->_type;
}
