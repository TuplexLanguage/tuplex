#include "ast.hpp"


static bool commonNameValidityChecks(TxNode* node, TxDeclarationFlags declFlags, const std::string& name) {
    if (name.empty()) {
        node->cerror("Name string is empty.");
        return false;
    }
    bool valid = true;
// TODO: distinguish between source origin (illegal) and implicitly generated names
//    auto pos = name.find_first_of(".#");
//    if (pos != std::string::npos) {
//        parser_error(node->parseLocation, "Illegal character within a name segment: '%c'", name.at(pos));
//        valid = false;
//    }
    return valid;
}

bool validateTypeName(TxNode* node, TxDeclarationFlags declFlags, const std::string& name) {
    // TODO: warning if first character is not upper case
    return commonNameValidityChecks(node, declFlags, name);
}

bool validateFieldName(TxNode* node, TxDeclarationFlags declFlags, const std::string& name) {
    // TODO: either all chars upper case or first character lower case, else warning
    return commonNameValidityChecks(node, declFlags, name);
}



std::string TxNode::to_string() const {
    char buf[256];
    snprintf(buf, 256, "%-24s : %-11s", typeid(*this).name(), this->parse_loc_string().c_str());
    return std::string(buf);
}

std::string TxNode::parse_loc_string() const {
    char buf[128];
    if (parseLocation.begin.line == parseLocation.end.line) {
        int lcol = (parseLocation.end.column > parseLocation.begin.column) ? parseLocation.end.column-1 : parseLocation.end.column;
        snprintf(buf, 128, "%2d.%2d-%2d", parseLocation.begin.line, parseLocation.begin.column, lcol);
    }
    else
        snprintf(buf, 128, "%2d.%2d-%2d.%2d", parseLocation.begin.line, parseLocation.begin.column, parseLocation.end.line, parseLocation.end.column-1);
    return std::string(buf);
}

void TxNode::cerror(char const *fmt, ...) const {
    va_list ap;
    va_start(ap, fmt);
    char buf[512];
    vsnprintf(buf, 512, fmt, ap);
    va_end(ap);
    if (this->is_context_set())
        this->driver().cerror(this->parseLocation, std::string(buf));
    else {
        TxDriver::emit_comp_error(this->parseLocation, std::string(buf));
    }
}
void TxNode::cwarning(char const *fmt, ...) const {
    va_list ap;
    va_start(ap, fmt);
    char buf[512];
    vsnprintf(buf, 512, fmt, ap);
    va_end(ap);
    if (this->is_context_set())
        this->driver().cwarning(this->parseLocation, std::string(buf));
    else {
        TxDriver::emit_comp_warning(this->parseLocation, std::string(buf));
    }
}



void TxFieldDeclNode::symbol_table_pass(LexicalContext& lexContext) {
    this->set_context(lexContext);
    TxFieldStorage storage;
    if (dynamic_cast<TxModule*>(lexContext.scope())) {  // if in global scope
        if (this->declFlags & TXD_STATIC)
            cerror("'static' is invalid modifier for module scope field %s", this->field->fieldName.c_str());
        if (this->declFlags & TXD_FINAL)
            cerror("'final' is invalid modifier for module scope field %s", this->field->fieldName.c_str());
        if (this->declFlags & TXD_OVERRIDE)
            cerror("'override' is invalid modifier for module scope field %s", this->field->fieldName.c_str());
        storage = TXS_GLOBAL;
    }
    else if (this->isMethod) {
        // static has special meaning for methods, technically a method is always a static function pointer field
        storage = TXS_STATIC;
        if (! (this->declFlags & TXD_STATIC)) {
            // instance method (add implicit self argument)
            static_cast<TxLambdaExprNode*>(field->initExpression)->make_instance_method();
        }
    }
    else {
        storage = (this->declFlags & TXD_STATIC) ? TXS_STATIC : TXS_INSTANCE;
    }
    this->field->symbol_table_pass_decl_field(lexContext, this->declFlags, storage, TxIdentifier(""));
    //this->set_context(this->field);
}


// FUTURE: factor out the 'explicit' code path into separate function
TxExpressionNode* validate_wrap_convert(TxExpressionNode* originalExpr, const TxType* requiredType, bool _explicit) {
    // Note: Symbol table pass and semantic pass are not run on the created wrapper nodes.
    auto originalType = originalExpr->get_type();
    if (! originalType)
        return originalExpr;
    if (originalType == requiredType)
        return originalExpr;
    if (_explicit || requiredType->auto_converts_from(*originalType)) {
        // wrap originalExpr with conversion node
        if (auto scalarType = dynamic_cast<const TxScalarType*>(requiredType))
            return new TxScalarConvNode(originalExpr->parseLocation, originalExpr, scalarType);
        if (auto refType = dynamic_cast<const TxReferenceType*>(requiredType))
            return new TxReferenceConvNode(originalExpr->parseLocation, originalExpr, refType);
        if (auto arrayType = dynamic_cast<const TxArrayType*>(requiredType))
            return new TxObjSpecCastNode(originalExpr->parseLocation, originalExpr, arrayType);
        if (dynamic_cast<const TxFunctionType*>(requiredType))
            return originalExpr;  // or do we actually need to do something here?
    }
    else if (auto refType = dynamic_cast<const TxReferenceType*>(requiredType)) {
        auto refTargetType = refType->target_type();
        if (refTargetType && originalType->is_a(*refTargetType->get_type())) {
            if (refTargetType->get_type()->is_modifiable()) {
                if (!originalType->is_modifiable())
                    originalExpr->cerror("Cannot convert reference with non-mod-target to one with mod target: %s -> %s",
                                 originalType->to_string().c_str(), requiredType->to_string().c_str());
                else
                    originalExpr->cerror("Cannot implicitly convert to reference with modifiable target: %s -> %s",
                                 originalType->to_string().c_str(), requiredType->to_string().c_str());
                return originalExpr;
            }
            else {
                // wrap originalExpr with a reference-to node
                auto refToNode = new TxReferenceToNode(originalExpr->parseLocation, originalExpr);
                refToNode->set_context(originalExpr);
                return new TxReferenceConvNode(originalExpr->parseLocation, refToNode, refType);
            }
        }
    }
    originalExpr->cerror("Can't auto-convert value %s -> %s",
                 originalType->to_string().c_str(), requiredType->to_string().c_str());
    return originalExpr;
}


TxExpressionNode* validate_wrap_assignment(TxExpressionNode* rValueExpr, const TxType* requiredType) {
    if (! requiredType->is_concrete())
        // TODO: dynamic concrete type resolution (recognize actual type in runtime when dereferencing a generic pointer)
        rValueExpr->cerror("Assignee is not a concrete type (size potentially unknown): %s", requiredType->to_string().c_str());
    // if assignee is a reference:
    // TODO: check dataspace rules
    return validate_wrap_convert(rValueExpr, requiredType);
}



TxSuiteNode::TxSuiteNode(const yy::location& parseLocation)
        : TxStatementNode(parseLocation), suite(new std::vector<TxStatementNode*>())  {
}

TxSuiteNode::TxSuiteNode(const yy::location& parseLocation, std::vector<TxStatementNode*>* suite)
        : TxStatementNode(parseLocation), suite(suite)  {
}



const std::vector<TxTypeParam>* TxTypeExpressionNode::makeTypeParams(const std::vector<TxDeclarationNode*>* typeParamDecls) {
    auto paramsVec = new std::vector<TxTypeParam>();
    for (auto decl : *typeParamDecls) {
        if (auto typeDecl = dynamic_cast<TxTypeDeclNode*>(decl))
            paramsVec->push_back(TxTypeParam(TxTypeParam::MetaType::TXB_TYPE, typeDecl->typeName, typeDecl->typeExpression));
        else if (auto valueDecl = dynamic_cast<TxFieldDeclNode*>(decl))
            paramsVec->push_back(TxTypeParam(TxTypeParam::MetaType::TXB_VALUE, valueDecl->field->fieldName, valueDecl->field));
    }
    return paramsVec;
}


void TxTypeDeclNode::symbol_table_pass(LexicalContext& lexContext) {
    this->set_context(lexContext);

    auto declaredEntity = lexContext.scope()->declare_type(this->typeName, this->typeExpression, this->declFlags);
    //this->LOGGER().debug("Defining type %-16s under <lexctx> %-24s <decl scope> %s", this->typeName.c_str(),
    //                     lexContext.scope()->get_full_name().to_string().c_str(), declaredEntity->get_full_name().to_string().c_str());
    if (!declaredEntity)
        cerror("Failed to declare type %s", this->typeName.c_str());
    LexicalContext typeCtx(declaredEntity ? declaredEntity : lexContext.scope());  // (in case declare_type() yields NULL)
    this->typeExpression->symbol_table_pass(typeCtx, this->declFlags, declaredEntity, this->typeParamDecls);

    // declare type parameters, if any, within type definition's scope:
    if (this->typeParamDecls) {
        for (auto paramDecl : *this->typeParamDecls) {
            paramDecl->symbol_table_pass(typeCtx);
        }
    }
}


void TxModifiableTypeNode::symbol_table_pass(LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                             TxTypeEntity* declaredEntity, const std::vector<TxDeclarationNode*>* typeParamDecls) {
    // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
    if (auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>(this->baseType)) {
        if (auto maybeModElem = dynamic_cast<TxMaybeModTypeNode*>(arrayBaseType->elementTypeNode->typeExprNode)) {
            // (can this spuriously add Modifiable node to predeclared modifiable type, generating error?)
            lexContext.scope()->LOGGER().debug("Implicitly declaring Array Element modifiable at %s", this->to_string().c_str());
            maybeModElem->isModifiable = true;
        }
    }

    TxTypeExpressionNode::symbol_table_pass(lexContext, declFlags, declaredEntity, typeParamDecls);
}


bool TxMaybeModTypeNode::directIdentifiedType() const {
    if (this->isModifiable)
        return false;
    if (auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>(this->baseType))
        if (typeid(*arrayBaseType->elementTypeNode->typeExprNode) == typeid(TxModifiableTypeNode))
            return false;
    return this->baseType->directIdentifiedType();
}

void TxMaybeModTypeNode::symbol_table_pass(LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                           TxTypeEntity* declaredEntity, const std::vector<TxDeclarationNode*>* typeParamDecls) {
    // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
    if (! this->isModifiable) {
        if (auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>(this->baseType))
            if (typeid(*arrayBaseType->elementTypeNode->typeExprNode) == typeid(TxModifiableTypeNode)) {
                lexContext.scope()->LOGGER().debug("Implicitly declaring Array modifiable at %s", this->to_string().c_str());
                this->isModifiable = true;
            }
    }

    if (this->isModifiable)
        TxModifiableTypeNode::symbol_table_pass(lexContext, declFlags, declaredEntity, typeParamDecls);
    else {
        // "pass through" entity declaration to the underlying type
        this->set_context(lexContext);
        this->baseType->symbol_table_pass(lexContext, declFlags, declaredEntity, typeParamDecls);
    }
}
