#pragma once

#include "util/assert.hpp"
#include "util/printable.hpp"


class TxType;
class TxEntityDeclaration;
class TxTypeDefiningNode;
class TxExpressionNode;
class TxLocation;


enum MetaType { TXB_TYPE, TXB_VALUE };


MetaType meta_type_of(const TxEntityDeclaration* declaration);


/** Describes the binding of a base type's type parameter to the definition used in a type specialization.
 * A binding instance is specific to a single specialization, i.e. a specific specialization-index.
 *
 * The type parameter of a reference base type's target may specify dataspace constraints.
 * The binding of a reference base type's target can specify the dataspace of the target.
 */
class TxGenericBinding : public Printable {
    const std::string typeParamName;
    const MetaType metaType;
    TxTypeDefiningNode* typeDefiner;
    TxExpressionNode* valueDefiner;

    TxGenericBinding( const std::string& typeParamName, MetaType metaType,
                      TxTypeDefiningNode* typeDefiner, TxExpressionNode* valueDefiner )
        : typeParamName(typeParamName), metaType(metaType), typeDefiner(typeDefiner), valueDefiner(valueDefiner)  { }

public:
    /** Constructs a copy of a TXB_TYPE binding but with a different type definer. */
    TxGenericBinding( const TxGenericBinding& original, TxTypeDefiningNode* newTypeDefiner )
            : typeParamName(original.typeParamName), metaType(original.metaType), typeDefiner(newTypeDefiner), valueDefiner(nullptr)  {
        ASSERT(metaType==MetaType::TXB_TYPE, "Type parameter binding metatype is VALUE, not TYPE: " << this->str());
    }

    static TxGenericBinding make_type_binding(const std::string& typeParamName, TxTypeDefiningNode* typeDefiner);
    static TxGenericBinding make_value_binding(const std::string& typeParamName, TxExpressionNode* valueDefiner);

    inline const std::string& param_name()    const { return typeParamName; }

    inline MetaType meta_type()    const { return metaType; }

    inline TxTypeDefiningNode& type_definer()  const {
        ASSERT(metaType==MetaType::TXB_TYPE, "Type parameter binding metatype is VALUE, not TYPE: " << this->str());
        return *this->typeDefiner;
    }

    inline TxExpressionNode& value_definer() const {
        ASSERT(metaType==MetaType::TXB_VALUE, "Type parameter binding metatype is TYPE, not VALUE: " << this->str());
        return *this->valueDefiner;
    }

    const TxLocation& get_parse_location() const;

    std::string str() const;
};

//bool operator==(const TxGenericBinding& b1, const TxGenericBinding& b2);
//inline bool operator!=(const TxGenericBinding& b1, const TxGenericBinding& b2) { return !(b1 == b2); }
