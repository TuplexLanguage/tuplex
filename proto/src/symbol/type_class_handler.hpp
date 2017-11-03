#pragma once

#include "type_class.hpp"

#include "util/printable.hpp"


/* forward declarations pertaining to LLVM code generation */
class LlvmGenerationContext;
class GenScope;
namespace llvm {
class Type;
class StructType;
class Value;
class Constant;
class DIType;
}

class TxActualType;

/** Contains the type-class specific logic for a TxActualType instance.
 * There is one implementation per type class, except for the Elementary type class
 * which has separate ones for Bool, Integer, and Floatingpoint.
 */
class TxTypeClassHandler : public Printable {
    const TxActualType* common_generic_base_type( const TxActualType* thisType, const TxActualType* thatType ) const;

    bool inner_is_a( const TxActualType* thisType, const TxActualType* otherType ) const;

protected:
    friend class TxActualType;

    TxTypeClass _typeClass;

    /** Returns true if an instance of this type can implicitly convert to an instance of the destination type.
     * Overriding this allows a type class to allow specific automatic (implicit) value conversions.
     * This may be less strict test than is_assignable, since some types that are not directly assignable
     * may be so after an implicit conversion (e.g. Byte -> Int).
     * The default implementation is equal to assignability.
     */
    virtual bool auto_converts_to( const TxActualType* type, const TxActualType* dest ) const;

    /** Overriding this allows a type class to allow structurally equal types to be assignable.
     * The default implementation simply calls inner_equals(). */
    virtual bool inner_is_assignable_to( const TxActualType* type, const TxActualType* dest ) const {
        return this->inner_equals( type, dest );
    }

    /** Overriding this allows a type class to allow structurally equal types to be regarded as equal.
     * The default implementation is simple type instance identity equality. */
    virtual bool inner_equals( const TxActualType* type, const TxActualType* other ) const {
        return type == other;
    }

    /** Returns true if the provided type is the same as this, or a specialization of this.
     * Note that true does not guarantee assignability, for example modifiability is not taken into account.
     */
    virtual bool is_a( const TxActualType* type, const TxActualType* other ) const;

public:
    TxTypeClassHandler( TxTypeClass typeClass ) : _typeClass( typeClass )  { }

    virtual ~TxTypeClassHandler() = default;

    inline TxTypeClass type_class() const { return _typeClass; }

    virtual std::string str() const override final;


    /*--- LLVM code generation methods ---*/

    virtual llvm::StructType* make_vtable_type( const TxActualType* type, LlvmGenerationContext& context ) const;

    /** Makes the llvm::Type for an instance of this type (possibly only an opaque struct declaration). */
    virtual llvm::Type* make_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const;

    /** Makes the llvm::DIType for an instance of this type. */
    virtual llvm::DIType* make_llvm_debug_type( const TxActualType* type, LlvmGenerationContext& context ) const;

    /** Invoked after make_llvm_type() to augment a possibly forward-declared llvm::Type "header" (named, opaque struct).
     * Default implementation returns the "header" type without modifying it;
     * types that actually predefine an opaque header should override and augment the type or return a new, full type. */
    virtual llvm::Type* make_llvm_type_body( const TxActualType* type, LlvmGenerationContext& context, llvm::Type* header ) const {
        return header;
    }

    /** Returns the llvm::Type that an instance of this type is converted to/from when passed to/from an extern-c function. */
    virtual llvm::Type* make_llvm_externc_type( const TxActualType* type, LlvmGenerationContext& context ) const;

    /** Code-generates initialization of bound VALUE parameters for an allocated instance. */
    virtual void initialize_specialized_obj( const TxActualType* type, LlvmGenerationContext& context, GenScope* scope, llvm::Value* objPtrV ) const { }

    /** Generates the formal id of this type. */
    virtual llvm::Constant* gen_typeid( const TxActualType* type, LlvmGenerationContext& context ) const;

    /** Generates the instance/element size of this type. Used by the runtime type info. Only valid for statically concrete types.
     * NOTE: For arrays this returns the instance size of their *element* type. */
    virtual llvm::Constant* gen_static_element_size( const TxActualType* type, LlvmGenerationContext& context ) const;

    /** Generates the size of an instance of this type. Only valid for statically concrete types. */
    virtual llvm::Constant* gen_static_size( const TxActualType* type, LlvmGenerationContext& context ) const;

    virtual llvm::Value* gen_alloca( const TxActualType* type, LlvmGenerationContext& context, GenScope* scope, unsigned alignment, const std::string &varName = "" ) const;
    virtual llvm::Value* gen_alloca( const TxActualType* type, LlvmGenerationContext& context, GenScope* scope, const std::string &varName = "" ) const;
    virtual llvm::Value* gen_malloc( const TxActualType* type, LlvmGenerationContext& context, GenScope* scope, const std::string &varName = "" ) const;
};


const TxTypeClassHandler* get_type_class_handler( TxTypeClass typeClass );
