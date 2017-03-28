#include "entity_type.hpp"

#include "ast.hpp"


inline static const TxTypeDeclaration* get_definer_declaration( const TxTypeDefiningNode* definer ) {
    if (auto typeExprNode = dynamic_cast<const TxTypeExpressionNode*>(definer))
        return typeExprNode->get_declaration();
    return nullptr;
}


TxType::TxType( const TxActualType* actualType )
    : TxEntity( actualType->get_declaration() ), definer(),  actualTypeProducer(),
      _type( actualType ), startedRslv( true ), hasResolved( true )
{
    actualType->get_nearest_declaration()->get_symbol()->get_root_scope()->types().add_type_usage( this );
}

TxType::TxType( const TxTypeDefiningNode* definer, std::function<const TxActualType*(void)> actualTypeProducer )
    : TxEntity( get_definer_declaration( definer ) ), definer( definer), actualTypeProducer( actualTypeProducer ),
      _type(), startedRslv(), hasResolved()
{
    definer->context().scope()->get_root_scope()->types().add_type_usage( this );
}



const TxActualType* TxType::define_type() const {
    return this->actualTypeProducer();
}

const TxActualType* TxType::type() const {
    if (!this->_type && !this->hasResolved) {
        LOG_TRACE(this->LOGGER(), "resolving type of " << this->definer);
        if (this->startedRslv) {
            CERROR(this->definer, "Recursive resolution of type in " << this->definer);
            return nullptr;
        }
        this->startedRslv = true;
        this->_type = this->define_type();
        if (! this->_type)
            LOG(this->LOGGER(), ERROR, "NULL actual type in " << this->definer);
        this->hasResolved = true;
    }
    return this->_type;
}
