#include "logging.hpp"

#include "generics.hpp"
#include "type_base.hpp"
#include "ast.hpp"


TxGenericBinding TxGenericBinding::make_type_binding(const std::string& paramName, TxTypeDefiner* typeDefiner) {
    return TxGenericBinding(paramName, TxTypeParam::MetaType::TXB_TYPE, typeDefiner, nullptr);
}

TxGenericBinding TxGenericBinding::make_value_binding(const std::string& paramName, TxExpressionNode* valueDefiner) {
    return TxGenericBinding(paramName, TxTypeParam::MetaType::TXB_VALUE, nullptr, valueDefiner);
}

std::string TxGenericBinding::to_string() const {
    const TxType* type;
    return this->typeParamName + "=" + ( this->metaType==TxTypeParam::MetaType::TXB_TYPE
                                                ? (type = this->type_definer().attempt_get_type(),
                                                   type ? type->to_string(true) : "")
                                                : "expr" );  // this->value_expr().to_string()
}

bool operator==(const TxGenericBinding& b1, const TxGenericBinding& b2) {
    if (! (b1.param_name() == b2.param_name() && b1.meta_type() == b2.meta_type()))
        return false;
    if (b1.meta_type() == TxTypeParam::MetaType::TXB_TYPE) {
        ResolutionContext resCtx;  // resolution currently necessary for Ref's target binding
        return ( *b1.type_definer().resolve_type(resCtx) == *b2.type_definer().resolve_type(resCtx) );
    }
    else {
        return ( b1.value_definer().get_static_constant_proxy() &&  b2.value_definer().get_static_constant_proxy()
             && *b1.value_definer().get_static_constant_proxy() == *b2.value_definer().get_static_constant_proxy() );
    }
}
