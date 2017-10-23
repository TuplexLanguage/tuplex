#include "type_class_handler.hpp"
#include "type_base.hpp"
#include "type_scalar.hpp"
#include "type_function.hpp"
#include "type_composite.hpp"

#include "ast/expr/ast_constexpr.hpp"

const TxBoolTypeClassHandler TxBoolType::boolTypeClassHandler;
const TxExternCFunctionTypeClassHandler TxExternCFunctionType::externcfuncTypeClassHandler;

static const TxTypeClassHandler* TYPE_CLASS_HANDLERS[TXTC_NOF_TYPE_CLASSES] = {
     // TXTC_ANY
     new TxTypeClassHandler( TXTC_ANY ),
     // TXTC_ELEMENTARY - for abstract Elementary types; there are distinct handlers for Bool, Integer, Floatingpoint
     new TxTypeClassHandler( TXTC_ELEMENTARY ),
     // TXTC_REFERENCE
     new TxReferenceTypeClassHandler(),
     // TXTC_ARRAY
     new TxArrayTypeClassHandler(),
     // TXTC_TUPLE
     new TxTupleTypeClassHandler(),
     // TXTC_FUNCTION
     new TxFunctionTypeClassHandler(),
     // TXTC_INTERFACE
     new TxInterfaceTypeClassHandler(),
     // TXTC_INTERFACEADAPTER
     new TxInterfaceAdapterTypeClassHandler(),
     // TXTC_VOID
     new TxTypeClassHandler( TXTC_VOID ),
};

const TxTypeClassHandler* get_type_class_handler( TxTypeClass typeClass ) {
    ASSERT( typeClass < TXTC_NOF_TYPE_CLASSES, "Invalid type class: " << typeClass );
    return TYPE_CLASS_HANDLERS[ typeClass ];
}



bool TxTypeClassHandler::auto_converts_to( const TxActualType* type, const TxActualType* dest ) const {
    return type->is_assignable_to( *dest );
}



/*=== Array and Reference handlers ===*/

//bool TxArrayTypeClassHandler::auto_converts_to( const TxActualType* type, const TxActualType* destination ) const {
//    if ( this != destination->type_class_handler() )
//        return false;
//    if ( this->inner_equals( type, destination ) )
//        return true;
//
//    // if origin has unbound type params that destination does not, origin is more generic and can't be converted to destination
//    if ( auto toElem = destination->element_type() ) {
//        if ( auto fromElem = type->element_type() ) {
//            // note: is-a test insufficient for array elements, since assignable type (same instance data type) required
//            if ( !fromElem->is_assignable_to( *toElem ) )
//                return false;
//        }
//        else
//            return false;  // origin has not bound E
//    }
//    if ( auto lenExpr = destination->capacity() ) {
//        if ( auto fromLenExpr = type->capacity() ) {
//            return ( lenExpr->is_statically_constant() && fromLenExpr->is_statically_constant()
//                     && ( eval_unsigned_int_constant( lenExpr ) == eval_unsigned_int_constant( fromLenExpr ) ) );
//        }
//        else
//            return false;  // origin has not bound C
//    }
//    return true;
//}

bool TxArrayTypeClassHandler::inner_is_assignable_to( const TxActualType* type, const TxActualType* destination ) const {
    ASSERT( destination->get_type_class() == TXTC_ARRAY, "destination not an array: " << destination );
    if ( this->inner_equals( type, destination ) )
        return true;

    // This check is strict and requires statically verifiable assignability.
    // (e.g. ref-to-array conversions need that guarantee)
    // If origin has unbound type params that destination does not, origin is more generic and can't be converted to destination.
    // (For assignments where lengths/capacities are not statically known, code must be generated to check this in runtime.)
    if ( auto toElem = destination->element_type() ) {
        if ( auto fromElem = type->element_type() ) {
            // note: is-a test insufficient for array elements, since assignable type (same instance data type) required
            if ( !fromElem->is_assignable_to( *toElem ) )
                return false;
        }
        else
            return false;  // origin has not bound E
    }
    if ( auto lenExpr = destination->capacity() ) {
        if ( auto fromLenExpr = type->capacity() ) {
            // TODO: ideally we should check source Length instead of source Capacity
            return ( lenExpr->is_statically_constant() && fromLenExpr->is_statically_constant()
                     && ( eval_unsigned_int_constant( lenExpr ) == eval_unsigned_int_constant( fromLenExpr ) ) );
        }
        else
            return false;  // origin has not bound C, dynamic capacities must be checked in runtime
    }
    return true;
}

bool TxReferenceTypeClassHandler::inner_is_assignable_to( const TxActualType* type, const TxActualType* destination ) const {
    ASSERT( destination->get_type_class() == TXTC_REFERENCE, "destination not a reference: " << destination );
    if ( this->inner_equals( type, destination ) )
        return true;

    // if origin has unbound type params that destination does not, origin is more generic and can't be assigned to destination
    if ( auto toTarget = destination->target_type() ) {
        if ( auto fromTarget = type->target_type() ) {
            // is-a test sufficient for reference targets (it isn't for arrays, which require same concrete type)
            //std::cerr << "CHECKING REF ASSIGNABLE\n\tFROM " << fromTarget->str(false) << "\n\tTO   " << toTarget->str(false) << std::endl;
            if ( toTarget.is_modifiable() && !fromTarget.is_modifiable() )
                return false;  // can't lose non-modifiability of target type
            if ( fromTarget->is_a( *toTarget ) )
                return true;
            else
                return false;
        }
        else
            return false;  // origin has not bound T
    }
    else
        return true;
}



bool TxFunctionTypeClassHandler::is_a( const TxActualType* type, const TxActualType* other ) const {
    if ( other->get_type_class() == TXTC_ANY )
        return true;
    else
        return inner_equals( type, other );
}

bool TxFunctionTypeClassHandler::inner_equals( const TxActualType* type, const TxActualType* other ) const {
    if ( other->get_type_class() != TXTC_FUNCTION )
        return false;
    auto & thisArgTypes = type->argument_types();
    auto & otherArgTypes = other->argument_types();
    return ( ( type->return_type() == other->return_type()
               || ( type->return_type()->is_assignable_to( *other->return_type() ) ) )
             && thisArgTypes.size() == otherArgTypes.size()
             && std::equal( thisArgTypes.cbegin(), thisArgTypes.cend(), otherArgTypes.cbegin(),
                            [](const TxActualType* ta, const TxActualType* oa) {return oa->is_assignable_to( *ta );} ) );
    return false;
}
