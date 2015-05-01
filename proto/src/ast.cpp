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

std::string make_generic_binding_name(const std::string& originalName) {
    std::string pname = originalName;
    std::replace(pname.begin(), pname.end(), '.', '#');
    return pname;
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



void TxFieldDeclNode::symbol_declaration_pass(LexicalContext& lexContext) {
    this->set_context(lexContext);

    TxFieldStorage storage;
    if (this->isMethodSyntax && dynamic_cast<TxTypeEntity*>(lexContext.scope())) {
        // Note: instance method storage is handled specially (technically a function pointer is always a static field)
        auto lambdaExpr = static_cast<TxLambdaExprNode*>(field->initExpression);
        if (this->declFlags & TXD_STATIC) {
            storage = TXS_STATIC;
        }
        else {
            lambdaExpr->set_instance_method(true);
            storage = TXS_INSTANCEMETHOD;
        }
    }
    else if (dynamic_cast<TxModule*>(lexContext.scope())) {  // if in global scope
        if (this->declFlags & TXD_STATIC)
            cerror("'static' is invalid modifier for module scope field %s", this->field->fieldName.c_str());
        if (this->declFlags & TXD_FINAL)
            cerror("'final' is invalid modifier for module scope field %s", this->field->fieldName.c_str());
        if (this->declFlags & TXD_OVERRIDE)
            cerror("'override' is invalid modifier for module scope field %s", this->field->fieldName.c_str());
        storage = TXS_GLOBAL;
    }
    else {
        storage = (this->declFlags & TXD_STATIC) ? TXS_STATIC : TXS_INSTANCE;
    }

    this->field->symbol_declaration_pass_nonlocal_field(lexContext, this->declFlags, storage, TxIdentifier(""));
}


// FUTURE: factor out the 'explicit' code path into separate function
static TxExpressionNode* inner_validate_wrap_convert(ResolutionContext& resCtx, TxExpressionNode* originalExpr,
                                                     const TxType* requiredType, bool _explicit) {
    // Note: Symbol table pass and semantic pass are not run on the created wrapper nodes.
    auto originalType = originalExpr->resolve_type(resCtx);
    if (! originalType)
        return originalExpr;
    if (originalType == requiredType)
        return originalExpr;
    if (_explicit || requiredType->auto_converts_from(*originalType)) {
        // wrap originalExpr with conversion node
        if (auto boolType = dynamic_cast<const TxBoolType*>(requiredType))
            return new TxBoolConvNode(originalExpr->parseLocation, originalExpr, boolType);
        if (auto scalarType = dynamic_cast<const TxScalarType*>(requiredType))
            return new TxScalarConvNode(originalExpr->parseLocation, originalExpr, scalarType);
        if (auto refType = dynamic_cast<const TxReferenceType*>(requiredType))
            return new TxReferenceConvNode(originalExpr->parseLocation, originalExpr, refType);
        if (auto arrayType = dynamic_cast<const TxArrayType*>(requiredType))
            return new TxObjSpecCastNode(originalExpr->parseLocation, originalExpr, arrayType);
        if (dynamic_cast<const TxFunctionType*>(requiredType))
            return originalExpr;  // or do we actually need to do something here?
        originalExpr->LOGGER().error("Type supposedly auto-converts but no conversion logic available:  %s => %s",
                                     originalType->to_string().c_str(), requiredType->to_string().c_str());
        return originalExpr;
    }
    else if (auto refType = dynamic_cast<const TxReferenceType*>(requiredType)) {
        auto refTargetType = refType->target_type(resCtx);
        if (refTargetType && originalType->is_a(*refTargetType)) {
            if (refTargetType->is_modifiable()) {
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
    originalExpr->cerror("Can't auto-convert value %s => %s",
                         originalType->to_string().c_str(), requiredType->to_string().c_str());
    return originalExpr;
}

TxExpressionNode* validate_wrap_convert(ResolutionContext& resCtx, TxExpressionNode* originalExpr, const TxType* requiredType, bool _explicit) {
    auto exprNode = inner_validate_wrap_convert(resCtx, originalExpr, requiredType, _explicit);
    if (exprNode != originalExpr) {
        originalExpr->LOGGER().trace("Wrapping conversion to type %s around %s", requiredType->to_string(true).c_str(), originalExpr->to_string().c_str());
        exprNode->symbol_declaration_pass(originalExpr->context());
        exprNode->symbol_resolution_pass(resCtx);
    }
    return exprNode;
}

TxExpressionNode* validate_wrap_assignment(ResolutionContext& resCtx, TxExpressionNode* rValueExpr, const TxType* requiredType) {
    if (! requiredType->is_concrete())
        // TODO: dynamic concrete type resolution (recognize actual type in runtime when dereferencing a generic pointer)
        rValueExpr->cerror("Assignee is not a concrete type (size potentially unknown): %s", requiredType->to_string().c_str());
    // if assignee is a reference:
    // TODO: check dataspace rules
    return validate_wrap_convert(resCtx, rValueExpr, requiredType);
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


void TxTypeDeclNode::symbol_declaration_pass(LexicalContext& lexContext) {
    this->set_context(lexContext);

//    auto declaredEntity = lexContext.scope()->declare_type(this->typeName, this->typeExpression, this->declFlags);
//    //this->LOGGER().debug("Defining type %-16s under <lexctx> %-24s <decl scope> %s", this->typeName.c_str(),
//    //                     lexContext.scope()->get_full_name().to_string().c_str(), declaredEntity->get_full_name().to_string().c_str());
//    if (!declaredEntity)
//        cerror("Failed to declare type %s", this->typeName.c_str());
//    LexicalContext typeCtx(declaredEntity ? declaredEntity : lexContext.scope());  // (in case declare_type() yields NULL)
    this->typeExpression->symbol_declaration_pass(lexContext, this->declFlags, this->typeName, this->typeParamDecls);
//
//    // declare type parameters, if any, within type definition's scope:
//    if (this->typeParamDecls) {
//        for (auto paramDecl : *this->typeParamDecls) {
//            paramDecl->symbol_declaration_pass(typeCtx);
//        }
//    }
}


void TxTypeExpressionNode::symbol_declaration_pass(LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                                   const std::string designatedTypeName,
                                                   const std::vector<TxDeclarationNode*>* typeParamDecls) {
    // Each node in a type expression has the option of declaring an entity (i.e. creating a name for)
    // any of its constituent type expressions.
    // Type entities are at minimum needed for:
    //  - explicit type declarations/extensions
    //  - adding members (since members are namespace symbols) - only done in explicit type extensions
    //  - specializations that bind generic type parameters (e.g. Ref<Ref<Int>>)
    //  - using fields in type expressions (resolving to the field's type) (not terribly important)
    // Note: Implicitly declared types should have the same visibility as the type/field they are for.

    // The context of this node represents its outer scope. This node's entity, if any, represents its inner scope.
    this->set_context(lexContext);

    TxTypeEntity* entity = nullptr;
    if (! designatedTypeName.empty()) {
        entity = lexContext.scope()->declare_type(designatedTypeName, this, declFlags);
        //this->LOGGER().debug("Defining type %-16s under <lexctx> %-24s <decl scope> %s", this->typeName.c_str(),
        //                     lexContext.scope()->get_full_name().to_string().c_str(), declaredEntity->get_full_name().to_string().c_str());
        if (! entity)
            cerror("Failed to declare type %s", designatedTypeName.c_str());
        else
            this->set_entity(entity);
    }
    LexicalContext typeCtx(entity ? entity : lexContext.scope());

    // declare type parameters within type declaration's scope, and before rest of type expression is processed:
    if (typeParamDecls) {
        for (auto paramDecl : *typeParamDecls) {
            paramDecl->symbol_declaration_pass(typeCtx);
        }
        this->declTypeParams = this->makeTypeParams(typeParamDecls);
    }

    this->symbol_declaration_pass_descendants(typeCtx, declFlags);
}


void TxPredefinedTypeNode::symbol_declaration_pass(LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                                   const std::string designatedTypeName,
                                                   const std::vector<TxDeclarationNode*>* typeParamDecls) {
    std::string typeName;
    if (designatedTypeName.empty() && !this->typeArgs->empty()) {
        // ensure generic type specializations always have a declared type (handles e.g. Ref<Ref<Int>>)
        typeName = "$type";
        declFlags = declFlags | TXD_IMPLICIT;
    }
    else
        typeName = designatedTypeName;

    if (! typeName.empty()) {
        // If this node declares a new type entity, perform early type name lookup
        // to capture generic type parameter referral, in which case the new entity will be an alias for it.
        // Note: Early lookup does not match the type parameters declared in this node, only prior ones:
        //     type Subtype<A> Type<A>  ## Legal since A is subnode of decl's type expression
        //     type Subtype<A> A        ## Illegal since A is top node of decl's type expression
        ResolutionContext resCtx;
        if (auto identifiedEntity = lexContext.scope()->lookup_type(resCtx, this->identNode->ident)) {
            if (identifiedEntity->get_decl_flags() & TXD_GENPARAM) {
                if (typeParamDecls && !typeParamDecls->empty()) {
                    cerror("Type parameters can't be declared on top of generic type parameter %s", this->identNode->ident.to_string().c_str());
                    /* This might be desirable in future (or maybe not). E.g:
                       type Subtype<A derives Array> {
                          A<Int,5> field
                       }  */
                }

                this->set_context(lexContext);
                auto declaredAlias = lexContext.scope()->declare_alias(typeName, TXD_PUBLIC | TXD_IMPLICIT, identifiedEntity);
                if (declaredAlias) {
                    this->LOGGER().alert("%s: Declared '%s' as alias for GENPARAM %s", this->parse_loc_string().c_str(),
                                         declaredAlias->get_full_name().to_string().c_str(),
                                         identifiedEntity->to_string().c_str());
                    this->set_entity(identifiedEntity);  // Or should this be the alias entity?
                }
                else
                    cerror("Failed to declare alias %s", typeName.c_str());
                LexicalContext typeCtx(declaredAlias ? declaredAlias : lexContext.scope());
                this->symbol_declaration_pass_descendants(typeCtx, declFlags);
                return;
            }
        }
    }

    TxTypeExpressionNode::symbol_declaration_pass(lexContext, declFlags, typeName, typeParamDecls);
}


const TxType* TxPredefinedTypeNode::define_identified_type(ResolutionContext& resCtx, TxSymbolScope* scope) {
    if (auto identifiedEntity = scope->lookup_type(resCtx, this->identNode->ident)) {
        auto identifiedType = identifiedEntity->resolve_symbol_type(resCtx);
        if (!identifiedType)
            return nullptr;
        else if (auto declEnt = this->get_entity()) {
            ASSERT(!declTypeParams || declTypeParams->empty(), "declTypeParams can't be set for 'empty' specialization: " << *this);
            if (identifiedEntity->get_decl_flags() & TXD_GENPARAM) {
                LOGGER().alert("%s: '%s' refers to unbound generic type parameter %s", this->parse_loc_string().c_str(),
                               this->identNode->ident.to_string().c_str(), identifiedEntity->to_string().c_str());
                return identifiedType;
                /*
                if (declEnt->get_name() == make_generic_binding_name(identifiedEntity->get_full_name().to_string())) {
                    // if type-arg resolves to ancestor type's type parameter, it is unspecified in current scope
                    // (need to catch this, lest we get an infinite alias lookup loop or spurious name resolution)
                    LOGGER().warning("%s: skipping alias match for type '%s' to GENPARAM %s", this->parse_loc_string().c_str(),
                                     declEnt->get_full_name().to_string().c_str(), identifiedEntity->to_string().c_str());
                    if (auto outerType = dynamic_cast<TxTypeEntity*>(scope->get_outer())) {
                        // since we declare base types under the subtype's scope,
                        // we may have to lookup via outer (the subtype's) scope
                        return define_identified_type(resCtx, outerType);
                    }
                    else {
                        cerror("'%s' refers to unbound generic type parameter: %s", this->identNode->ident.to_string().c_str(), identifiedEntity->get_full_name().to_string().c_str());
                        return nullptr;
                    }
                }
                else {
                    // let this entity be an alias for the generic type parameter (no unique type is created)
                    declEnt->set_alias(identifiedEntity->get_full_name());
                    LOGGER().warning("%s: Changed TxTypeEntity '%s' into alias for GENPARAM %s", this->parse_loc_string().c_str(),
                                     declEnt->get_full_name().to_string().c_str(), identifiedEntity->to_string().c_str());
                    return identifiedType;
                }
                */
            }
            else {
                // create empty specialization (uniquely named but identical type)
                std::string errorMsg;
                auto type = this->types().get_type_specialization(declEnt, TxTypeSpecialization(identifiedType),
                                                                  false, this->declTypeParams, &errorMsg);
                if (! type)
                    cerror("Failed to create 'empty' type specialization: %s", errorMsg.c_str());
                return type;
            }
        }
        else {
            if (identifiedEntity->get_decl_flags() & TXD_GENPARAM) {
                // Should not happen unless source refers specifically to an unbound type parameter
                cwarning("'%s' refers to unbound generic type parameter %s", this->identNode->ident.to_string().c_str(), identifiedEntity->to_string().c_str());
                // (But if legal use case exists, how let this be an alias entity for the generic type parameter?)
                //LOGGER().error("%s: Can't declare type '%s' as alias for GENPARAM %s since no entity declared for this type node",
                //               this->parse_loc_string().c_str(), this->identNode->ident.to_string().c_str(), identifiedEntity->to_string().c_str());
            }
            return identifiedType;
        }
    }
    return nullptr;
}

const TxType* TxPredefinedTypeNode::define_generic_specialization_type(ResolutionContext& resCtx) {
    auto baseTypeEntity = this->context().scope()->lookup_type(resCtx, this->identNode->ident);
    const TxType* baseType = baseTypeEntity ? baseTypeEntity->resolve_symbol_type(resCtx) : nullptr;
    if (! baseType) {
        cerror("Unknown type: %s (from %s)", this->identNode->ident.to_string().c_str(), this->context().scope()->to_string().c_str());
        return nullptr;
    }
    if (baseType->type_params().size() < this->typeArgs->size()) {
        cerror("Too many generic type arguments specified for type %s", identNode->ident.to_string().c_str());
        return nullptr;
    }
    std::vector<TxGenericBinding> bindings; // e.g. { TxTypeBinding("E", elemType), TxTypeBinding("L", length) }
    for (size_t i = 0; i < this->typeArgs->size(); i++) {
        bindings.push_back(this->typeArgs->at(i)->make_binding(resCtx, baseType, baseType->type_params().at(i)));
    }
    TxTypeSpecialization specialization(baseType, bindings);
    std::string errorMsg;
    auto type = this->types().get_type_specialization(this->get_entity(), specialization, false, this->declTypeParams, &errorMsg);
    if (! type)
        cerror("Failed to specialize type: %s", errorMsg.c_str());
    return type;
}



void TxArrayTypeNode::symbol_declaration_pass_descendants(LexicalContext& lexContext, TxDeclarationFlags declFlags) {
    this->elementTypeNode->symbol_declaration_pass(lexContext);
// If we're to implicitly define a binding the Array.L parameter, we must also fix so that such implicit bindings
// get a corresponding type parameter declaration in the outer type entity.
//    if (! this->lengthNode) {
//        // implicit redeclaration of array length parameter L:
//        TxIdentifierNode* redeclIdent = new TxIdentifierNode(this->parseLocation, new TxIdentifier("L"));
//        TxExpressionNode* lengthExpr = new TxFieldValueNode(this->parseLocation, nullptr, redeclIdent);
//        lengthNode = new TxTypeArgumentNode(lengthExpr);
//    }
    if (this->lengthNode)
        this->lengthNode->symbol_declaration_pass(lexContext);
}


void TxModifiableTypeNode::symbol_declaration_pass(LexicalContext& lexContext, TxDeclarationFlags declFlags,
        const std::string designatedTypeName, const std::vector<TxDeclarationNode*>* typeParamDecls) {
    // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
    if (auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>(this->baseType)) {
        if (auto maybeModElem = dynamic_cast<TxMaybeModTypeNode*>(arrayBaseType->elementTypeNode->typeExprNode)) {
            // (can this spuriously add Modifiable node to predeclared modifiable type, generating error?)
            lexContext.scope()->LOGGER().debug("Implicitly declaring Array Element modifiable at %s", this->to_string().c_str());
            maybeModElem->isModifiable = true;
        }
    }

    TxTypeExpressionNode::symbol_declaration_pass(lexContext, declFlags, designatedTypeName, typeParamDecls);
}


bool TxMaybeModTypeNode::has_predefined_type() const {
    if (this->isModifiable)
        return false;
    if (auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>(this->baseType))
        if (typeid(*arrayBaseType->elementTypeNode->typeExprNode) == typeid(TxModifiableTypeNode))
            return false;
    return this->baseType->has_predefined_type();
}

void TxMaybeModTypeNode::symbol_declaration_pass(LexicalContext& lexContext, TxDeclarationFlags declFlags,
        const std::string designatedTypeName, const std::vector<TxDeclarationNode*>* typeParamDecls) {
    // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
    if (! this->isModifiable) {
        if (auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>(this->baseType))
            if (typeid(*arrayBaseType->elementTypeNode->typeExprNode) == typeid(TxModifiableTypeNode)) {
                lexContext.scope()->LOGGER().debug("Implicitly declaring Array modifiable at %s", this->to_string().c_str());
                this->isModifiable = true;
            }
    }

    if (this->isModifiable)
        TxModifiableTypeNode::symbol_declaration_pass(lexContext, declFlags, designatedTypeName, typeParamDecls);
    else {
        // "pass through" entity declaration to the underlying type
        this->set_context(lexContext);
        this->baseType->symbol_declaration_pass(lexContext, declFlags, designatedTypeName, typeParamDecls);
    }
}
