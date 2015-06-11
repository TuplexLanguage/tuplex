#pragma once

#include "assert.hpp"
#include "printable.hpp"


class TxType;
class TxTypeDefiner;
class TxSpecializableTypeDefiner;
class TxExpressionNode;


/** Represents a type parameter of a generic type.
 * Specializations of the generic type provide a binding to this parameter.
 * The parameter may hold constraints on what definitions are permitted to bind to it.
 * The type parameter of a reference base type's target may specify dataspace constraints.
 *
 * The type parameter object belongs to the type object, which may be named (declared)
 * or anonymous. Type declarations must ensure they have named members in their namespace
 * matching the type parameters.
 */
class TxTypeParam : public Printable {  // FUTURE: add constraints
public:
    enum MetaType { TXB_TYPE, TXB_VALUE };

private:
    MetaType metaType;
    std::string typeParamName;
    TxSpecializableTypeDefiner* constraintTypeDefiner;

public:
    TxTypeParam(MetaType metaType, const std::string& typeParamName, TxSpecializableTypeDefiner* constraintTypeDefiner)
            : metaType(metaType), typeParamName(typeParamName), constraintTypeDefiner(constraintTypeDefiner)  {
        ASSERT(metaType==TXB_TYPE || constraintTypeDefiner, "VALUE type parameter's type is NULL");
        ASSERT(metaType==TXB_VALUE || constraintTypeDefiner, "TYPE type parameter's constraint type is NULL");
    }

    inline MetaType meta_type() const { return metaType; }
    inline const std::string& param_name() const { return typeParamName; }

    inline bool has_constraint_type_definer() const { return this->constraintTypeDefiner; }

    /** Gets the type definer that represents the constraint type (if TYPE) or data type (if VALUE) of this parameter. */
    inline TxSpecializableTypeDefiner* get_constraint_type_definer() const {
        ASSERT(this->has_constraint_type_definer(), "This type parameter '" << this->typeParamName << "' has no constraint type definer set");
        return this->constraintTypeDefiner;
    }

    inline virtual bool operator==(const TxTypeParam& other) const {
        return (this->typeParamName == other.typeParamName && this->metaType == other.metaType);
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
 *
 * The type parameter of a reference base type's target may specify dataspace constraints.
 * The binding of a reference base type's target can specify the dataspace of the target.
 */
class TxGenericBinding : public Printable {
    const std::string typeParamName;
    const TxTypeParam::MetaType metaType;
    TxTypeDefiner* typeDefiner;
    TxExpressionNode* valueExpr;

    TxGenericBinding(const std::string& typeParamName, TxTypeParam::MetaType metaType,
                     TxTypeDefiner* typeDefiner, TxExpressionNode* valueExpr)
        : typeParamName(typeParamName), metaType(metaType), typeDefiner(typeDefiner), valueExpr(valueExpr)  { }

public:
    static TxGenericBinding make_type_binding(const std::string& typeParamName, TxTypeDefiner* typeDefiner);
    static TxGenericBinding make_value_binding(const std::string& typeParamName, TxExpressionNode* valueExpr);

    inline const std::string& param_name()    const { return typeParamName; }

    inline TxTypeParam::MetaType meta_type()    const { return metaType; }

    inline TxTypeDefiner& type_definer()  const {
        ASSERT(metaType==TxTypeParam::MetaType::TXB_TYPE, "Type parameter binding metatype is VALUE, not TYPE: " << this->to_string());
        return *this->typeDefiner;
    }

    inline TxExpressionNode& value_expr() const {
        ASSERT(metaType==TxTypeParam::MetaType::TXB_VALUE, "Type parameter binding metatype is TYPE, not VALUE: " << this->to_string());
        return *this->valueExpr;
    }

    std::string to_string() const;
};

bool operator==(const TxGenericBinding& b1, const TxGenericBinding& b2);
inline bool operator!=(const TxGenericBinding& b1, const TxGenericBinding& b2) { return !(b1 == b2); }
