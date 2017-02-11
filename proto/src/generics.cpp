#include "util/logging.hpp"

#include "generics.hpp"
#include "ast.hpp"

#include "symbol/type_base.hpp"


MetaType meta_type_of(const TxEntityDeclaration* declaration) {
    return ( dynamic_cast<const TxFieldDeclaration*>(declaration) ? TXB_VALUE : TXB_TYPE );
}


TxGenericBinding TxGenericBinding::make_type_binding(const std::string& paramName, TxTypeDefiningNode* typeDefiner) {
    return TxGenericBinding(paramName, MetaType::TXB_TYPE, typeDefiner, nullptr);
}

TxGenericBinding TxGenericBinding::make_value_binding(const std::string& paramName, TxExpressionNode* valueDefiner) {
    return TxGenericBinding(paramName, MetaType::TXB_VALUE, nullptr, valueDefiner);
}

static inline std::string type_arg_to_string(const TxType* type) {
    if (type) {
        if (type->get_declaration())
            return type->to_string(true);
        else
            return type_arg_to_string(type->get_semantic_base_type());
    }
    else
        return "n/r";
}

const TxLocation& TxGenericBinding::get_parse_location() const {
    return ( metaType == MetaType::TXB_TYPE ? this->typeDefiner->get_parse_location()
                                            : this->valueDefiner->get_parse_location() );
}

std::string TxGenericBinding::to_string() const {
    return this->typeParamName + "=" + ( this->metaType==MetaType::TXB_TYPE
                                                ? type_arg_to_string(this->type_definer().attempt_get_type())
                                                : "expr" );  // this->value_expr().to_string()
}

//bool operator==(const TxGenericBinding& b1, const TxGenericBinding& b2) {
//    if (! (b1.param_name() == b2.param_name() && b1.meta_type() == b2.meta_type()))
//        return false;
//    if (b1.meta_type() == MetaType::TXB_TYPE) {
//        auto t1 = b1.type_definer().resolve_type();
//        auto t2 = b2.type_definer().resolve_type();
//        return ( t1 && t2 && *t1 == *t2 );
//    }
//    else {
//        return ( b1.value_definer().get_static_constant_proxy() &&  b2.value_definer().get_static_constant_proxy()
//             && *b1.value_definer().get_static_constant_proxy() == *b2.value_definer().get_static_constant_proxy() );
//    }
//}
