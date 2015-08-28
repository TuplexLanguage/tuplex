#pragma once

#include "assert.hpp"
#include "printable.hpp"


class TxType;
class TxEntityDeclaration;
class TxTypeDefiner;
class TxExpressionNode;


enum MetaType { TXB_TYPE, TXB_VALUE };


MetaType meta_type_of(const TxEntityDeclaration* declaration);


/** Represents a type parameter of a generic type.
 * Specializations of the generic type provide a binding to this parameter.
 *
 * The parameter may hold constraints on what definitions are permitted to bind to it.
 * The type parameter of a reference base type's target may specify dataspace constraints.
 *
 * A parameter instance is specific to a single specialization, i.e. a specific specialization-index:
 * This matters for the TYPE parameter's constraint type, which can be narrowed in subtypes of a
 * parameterized type.
 *
 * The type parameter object belongs to the type object, which may be named (declared)
 * or anonymous. Type declarations must ensure they have named members in their namespace
 * matching the type parameters.
 */
class TxTypeParam : public Printable {  // FUTURE: add constraints
private:
    MetaType metaType;
//    std::string typeParamName;
//    TxTypeDefiner* constraintTypeDefiner;
    const TxEntityDeclaration* decl;

public:
//    TxTypeParam(MetaType metaType, const std::string& typeParamName, TxTypeDefiner* constraintTypeDefiner)
//            : metaType(metaType), typeParamName(typeParamName), constraintTypeDefiner(constraintTypeDefiner)  {
//        ASSERT(metaType==TXB_TYPE || constraintTypeDefiner, "VALUE type parameter's type is NULL");
//        ASSERT(metaType==TXB_VALUE || constraintTypeDefiner, "TYPE type parameter's constraint type is NULL");
//    }
    TxTypeParam(const TxEntityDeclaration* decl);
//            : metaType(meta_type_of(decl)), decl(decl) {
//        ASSERT(decl->get_decl_flags() & TXD_GENPARAM, "Declaration is not a generic parameter declaration: " << decl);
//    }

    inline MetaType meta_type() const { return metaType; }
    const std::string param_name() const;

    //inline bool has_constraint_type_definer() const { return this->constraintTypeDefiner; }

    /** Gets the type definer that represents the constraint type (if TYPE) or data type (if VALUE) of this parameter. */
    TxTypeDefiner* get_constraint_type_definer() const;

    inline virtual bool operator==(const TxTypeParam& other) const {
        return (this->decl == other.decl);
    }
    inline virtual bool operator!=(const TxTypeParam& other) const {
        return ! this->operator==(other);
    }

    std::string to_string() const {
        switch (this->meta_type()) {
        case TXB_TYPE: return "TYPE " + this->param_name();
        case TXB_VALUE: return "VALUE " + this->param_name();
        default: return std::string();
        }
    }
};


/** Describes the binding of a base type's type parameter to the definition used in a type specialization.
 * A binding instance is specific to a single specialization, i.e. a specific specialization-index.
 *
 * The type parameter of a reference base type's target may specify dataspace constraints.
 * The binding of a reference base type's target can specify the dataspace of the target.
 */
class TxGenericBinding : public Printable {
    const std::string typeParamName;
    const MetaType metaType;
    TxTypeDefiner* typeDefiner;
    TxExpressionNode* valueDefiner;

    TxGenericBinding(const std::string& typeParamName, MetaType metaType,
                     TxTypeDefiner* typeDefiner, TxExpressionNode* valueDefiner)
        : typeParamName(typeParamName), metaType(metaType), typeDefiner(typeDefiner), valueDefiner(valueDefiner)  { }

public:
    static TxGenericBinding make_type_binding(const std::string& typeParamName, TxTypeDefiner* typeDefiner);
    static TxGenericBinding make_value_binding(const std::string& typeParamName, TxExpressionNode* valueDefiner);

    inline const std::string& param_name()    const { return typeParamName; }

    inline MetaType meta_type()    const { return metaType; }

    inline TxTypeDefiner& type_definer()  const {
        ASSERT(metaType==MetaType::TXB_TYPE, "Type parameter binding metatype is VALUE, not TYPE: " << this->to_string());
        return *this->typeDefiner;
    }

    inline TxExpressionNode& value_definer() const {
        ASSERT(metaType==MetaType::TXB_VALUE, "Type parameter binding metatype is TYPE, not VALUE: " << this->to_string());
        return *this->valueDefiner;
    }

    std::string to_string() const;
};

bool operator==(const TxGenericBinding& b1, const TxGenericBinding& b2);
inline bool operator!=(const TxGenericBinding& b1, const TxGenericBinding& b2) { return !(b1 == b2); }
