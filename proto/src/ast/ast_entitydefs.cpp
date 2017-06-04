#include "ast_entitydefs.hpp"


const TxType* TxTypeDefiningNode::resolve_type() {
    ASSERT( this->is_context_set(), "Declaration pass has not been run (lexctx not set) before resolving " << this );
    if ( !this->type ) {
        if ( this->hasResolved ) {
            throw resolution_error( this, "Previous type resolution failed in " + this->str() );
        }
        LOG_TRACE( this->LOGGER(), "resolving type  of " << this );

        if ( this->startedRslv ) {
            CERR_THROWRES( this, "Recursive definition of type '" << this->get_identifier() << "'" );
        }
        this->startedRslv = true;
        try {
            this->type = this->define_type();
        }
        catch ( const resolution_error& err ) {
            this->hasResolved = true;
            //LOG(this->LOGGER(), DEBUG, "Caught and re-threw resolution error in " << this << ": " << err);
            throw;
        }
        ASSERT( this->type, "NULL-resolved type but no exception thrown in " << this );
        this->hasResolved = true;
    }
    return this->type;
}

const TxField* TxFieldDefiningNode::resolve_field() {
    ASSERT( this->is_context_set(), "Declaration pass has not been run (lexctx not set) before resolving " << this );
    if ( !this->field ) {
        if ( this->hasResolved ) {
            throw resolution_error( this, "Previous field resolution failed in " + this->str() );
        }
        LOG_TRACE( this->LOGGER(), "resolving field of " << this );

        if ( this->startedRslv ) {
            CERR_THROWRES( this, "Recursive definition of field '" << this->get_identifier() << "'" );
        }
        this->startedRslv = true;
        try {
            this->type = this->define_type();
            this->field = this->define_field();
        }
        catch ( const resolution_error& err ) {
            this->hasResolved = true;
            //LOG(this->LOGGER(), DEBUG, "Caught and re-threw resolution error in " << this << ": " << err);
            throw;
        }
        ASSERT( this->type && this->field, "NULL-resolved type/field but no exception thrown in " << this );
        this->hasResolved = true;
    }
    return this->field;
}
