#include "entity_type.hpp"
#include "package.hpp"
#include "type_registry.hpp"
#include "ast/type/ast_typeexpr_node.hpp"
#include "qual_type.hpp"

const TxType* get_type_entity( const TxActualType* actType ) {
    return new TxType( actType->get_declaration()->get_definer()->qtype().type() );
}

inline static const TxTypeDeclaration* get_definer_declaration( const TxTypeResolvingNode* definer ) {
    if ( auto typeExprNode = dynamic_cast<const TxTypeExpressionNode*>( definer ) )
        return typeExprNode->get_declaration();
    return nullptr;
}

TxType::TxType( const TxActualType* actualType )
        : TxEntity( actualType->get_declaration() ), definer( actualType->get_declaration()->get_definer() ), actualTypeProducer(),
          _type( actualType ), startedRslv( true ), hasResolved( true )
{
    actualType->get_declaration()->get_symbol()->get_root_scope()->registry().add_type_usage( this );
}

TxType::TxType( TxTypeResolvingNode* definer, std::function<const TxActualType*( void )> actualTypeProducer )
        : TxEntity( get_definer_declaration( definer ) ), definer( definer ), actualTypeProducer( actualTypeProducer ),
          _type(), startedRslv(), hasResolved()
{
    definer->registry().add_type_usage( this );
}

const TxTypeDeclaration* TxType::get_declaration() const {
    if ( auto decl = TxEntity::get_declaration() )
        return static_cast<const TxTypeDeclaration*>( decl );
    if ( this->_type )
        return this->_type->get_declaration();
    LOG( this->definer->LOGGER(), INFO, "Declaration not known for unresolved TxType, definer is " << this->definer );
    //ASSERT(false, "Declaration not known for unresolved TxType, definer is " << this->definer);
    return nullptr;
}

TxQualType TxType::define_type( TxPassInfo passInfo ) const {
    return this->actualTypeProducer();
}

const TxActualType* TxType::acttype() const {
    if ( !this->_type ) {
        if ( this->hasResolved ) {
            throw resolution_error( this, "Previous ACTUAL type resolution failed in " + this->str() );
        }
        LOG_TRACE( this->LOGGER(), "resolving ACTUAL type of " << this->definer );

        if ( this->startedRslv ) {
            CERR_THROWRES( this->definer, "Recursive resolution of ACTUAL type in " << this->definer );
        }
        this->startedRslv = true;
        try {
            this->_type = this->define_type( TXP_RESOLUTION ).type();
        }
        catch ( const resolution_error& err ) {
            this->hasResolved = true;
            //LOG(this->LOGGER(), DEBUG, "Caught and re-threw resolution error in " << this->definer << ": " << err);
            throw;
        }
        ASSERT( this->_type, "NULL-resolved ACTUAL type but no exception thrown in " << this->definer );
        this->hasResolved = true;
    }
    return this->_type;
}
