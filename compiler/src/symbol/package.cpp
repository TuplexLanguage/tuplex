#include "package.hpp"

#include "entity.hpp"
#include "type_registry.hpp"
#include "builtin/builtin_types.hpp"

TxPackage::TxPackage( TxDriver& driver, const TxParseOrigin& rootOrigin )
        : TxModule( rootOrigin ), _driver( driver ), typeRegistry(new TypeRegistry( *this )),
          mainFuncCandidates(), mainFunc() {
}


const std::vector<const TxFieldDeclaration*>& TxPackage::get_extlink_declarations() {
    return this->mainFuncCandidates;
}


static bool is_main_signature_valid( const TxField* funcField ) {
    const TxFieldDefiningNode* definer = funcField->get_declaration()->get_definer();
    const TxActualType* funcType = funcField->qtype().type();

    if ( funcType->get_type_class() != TXTC_FUNCTION ) {
        CERROR( definer, "main is not a function: " << funcField );
        return false;
    }

    if ( !( funcField->get_storage() == TXS_GLOBAL || funcField->get_storage() == TXS_STATIC ) )
        CERROR( definer, "main() function must have global or static storage: " << funcField->get_storage() );

    auto retType = funcType->return_type();
    bool retOk = bool( retType->get_type_class() == TXTC_VOID
                       || retType->is_a( *definer->context().package()->registry().get_builtin_type( TXBT_INTEGER ) ) );
    if ( !retOk )
        CERROR( definer, "main() function has non-integer return type (must be void or integer): " << retType );

    auto & argTypes = funcType->argument_types();
    if ( argTypes.empty() )
        return retOk;
    else if ( argTypes.size() == 1 ) {
        const TxActualType* argsType = argTypes.at( 0 );
        if ( argsType->get_type_class() == TXTC_REFERENCE ) {
            auto targetType = argsType->target_type();
            if ( targetType->get_type_class() == TXTC_ARRAY ) {
                auto elemType = targetType->element_type();
                if ( elemType->get_type_class() == TXTC_REFERENCE ) {
                    auto elemTargetType = elemType->target_type();
                    if ( elemTargetType->get_type_class() == TXTC_ARRAY ) {
                        auto elemTargetElemType = elemTargetType->element_type();
                        if ( elemTargetElemType->is_builtin( TXBT_UBYTE ) )
                            return retOk;
                    }
                }
            }
        }
        CERROR( definer, "main() function has invalid argument [required signature is  main()  or  main( &[]&[]UByte )] : " << argsType );
    }
    else
        CERROR( definer, "main() function has too many arguments [required signature is  main()  or  main( &[]&[]UByte )]" );
    return false;
}

void TxPackage::register_main_func( const TxFieldDeclaration* mainFuncCand ) {
    this->mainFuncCandidates.push_back( mainFuncCand );
}

void TxPackage::determine_main_func() {
    // verify main program function candidates
    for ( auto mainCandidate : this->mainFuncCandidates ) {
        if ( is_main_signature_valid( mainCandidate->get_definer()->field()) ) {
            if ( !this->mainFunc ) {
                this->mainFunc = mainCandidate;
                LOG_DEBUG( this->LOGGER(), "Set user main function: " << this->mainFunc );
            }
            else
                CINFO( this->mainFunc->get_definer(), "Multiple main() functions, will use first one encountered: "
                       << this->mainFunc->get_unique_full_name()
                       << " at " << this->mainFunc->get_definer()->parse_loc_string() );
        }
    }
}

const TxFieldDeclaration* TxPackage::get_main_func() const {
    // returns the first valid one encountered
    return this->mainFunc;
}
