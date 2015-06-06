#include "ast.hpp"


static bool commonNameValidityChecks(TxNode* node, TxDeclarationFlags declFlags, const std::string& name) {
    if (name.empty()) {
        CERROR(node, "Name string is empty.");
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

//void TxNode::CERROR(this, char const *fmt, ...) const {
//    va_list ap;
//    va_start(ap, fmt);
//    char buf[512];
//    vsnprintf(buf, 512, fmt, ap);
//    va_end(ap);
//    if (this->is_context_set())
//        this->driver().CERROR(this, this->parseLocation, std::string(buf));
//    else {
//        TxDriver::emit_comp_error(this->parseLocation, std::string(buf));
//    }
//}
//void TxNode::cwarning(char const *fmt, ...) const {
//    va_list ap;
//    va_start(ap, fmt);
//    char buf[512];
//    vsnprintf(buf, 512, fmt, ap);
//    va_end(ap);
//    if (this->is_context_set())
//        this->driver().cwarning(this->parseLocation, std::string(buf));
//    else {
//        TxDriver::emit_comp_warning(this->parseLocation, std::string(buf));
//    }
//}


/*
TxDeclarationNode* TxFieldDefNode::declare_stack_constructor(LexicalContext& lexContext) {
    if (auto constructedType = dynamic_cast<TxTypeEntity*>(lexContext.scope())) {
        // FUTURE: stack constructor's access should be combination of type's and constructor's access
        TxDeclarationFlags declFlags = TXD_IMPLICIT | ( ( TXD_STATIC | TXD_PUBLIC | TXD_PROTECTED ) & constructedType->get_decl_flags() );
        auto constrBody = new TxStackConstructorNode(this, constructedType);
        auto constrDef  = new TxFieldDefNode(this->parseLocation, constructedType->get_name(), nullptr, constrBody);
        auto constrDecl = new TxFieldDeclNode(this->parseLocation, declFlags, constrDef);
        LexicalContext outerCtx(constructedType->get_outer());
        constrDecl->symbol_declaration_pass(outerCtx);
        return constrDecl;
    }
    else
        CERROR(this, "Can't declare constructor outside of type declaration");
    return nullptr;
}
*/


void TxFieldDeclNode::symbol_declaration_pass(LexicalContext& lexContext) {
    this->set_context(lexContext);

    TxFieldStorage storage;
    if (this->isMethodSyntax && lexContext.outer_type()) {
        // Note: instance method storage is handled specially (technically the function pointer is a static field)
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
            CERROR(this, "'static' is invalid modifier for module scope field " << this->field->get_field_name());
        if (this->declFlags & TXD_FINAL)
            CERROR(this, "'final' is invalid modifier for module scope field " << this->field->get_field_name());
        if (this->declFlags & TXD_OVERRIDE)
            CERROR(this, "'override' is invalid modifier for module scope field " << this->field->get_field_name());
        storage = TXS_GLOBAL;
    }
    else {
        storage = (this->declFlags & TXD_STATIC) ? TXS_STATIC : TXS_INSTANCE;
    }

    this->field->symbol_declaration_pass_nonlocal_field(lexContext, this->declFlags, storage, TxIdentifier(""));
}


// FUTURE: factor out the 'explicit' code path into separate function
// FUTURE: rework together with overloaded function resolution
static TxExpressionNode* inner_validate_wrap_convert(ResolutionContext& resCtx, TxExpressionNode* originalExpr,
                                                     const TxType* requiredType, bool _explicit) {
    // Note: Symbol table pass and semantic pass are not run on the created wrapper nodes.
    auto originalType = originalExpr->resolve_type(resCtx);
    if (! originalType)
        return originalExpr;
    if (originalType == requiredType) // TODO: test with:  || *originalType == *requiredType)
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
        auto refTargetType = refType->target_type();
        if (refTargetType && originalType->is_a(*refTargetType)) {
            if (refTargetType->is_modifiable()) {
                if (!originalType->is_modifiable())
                    CERROR(originalExpr, "Cannot convert reference with non-mod-target to one with mod target: "
                           << originalType << " -> " << requiredType);
                else
                    CERROR(originalExpr, "Cannot implicitly convert to reference with modifiable target: "
                            << originalType << " -> " << requiredType);
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
    CERROR(originalExpr, "Can't auto-convert value\n\tFrom: " << originalType << "\n\tTo:   " << requiredType);
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
        CERROR(rValueExpr, "Assignee is not a concrete type (size potentially unknown): " << requiredType);
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
            paramsVec->push_back(TxTypeParam(TxTypeParam::MetaType::TXB_VALUE, valueDecl->field->get_field_name(), valueDecl->field));
    }
    return paramsVec;
}


void TxTypeDeclNode::symbol_declaration_pass(LexicalContext& defContext, LexicalContext& lexContext) {
    this->set_context(lexContext);

    this->typeExpression->symbol_declaration_pass(defContext, lexContext, this->declFlags, this->typeName, this->typeParamDecls);
}


void TxTypeExpressionNode::symbol_declaration_pass(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags,
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

    TxTypeDeclaration* declaration = nullptr;
    if (! designatedTypeName.empty()) {
        declaration = lexContext.scope()->declare_type(designatedTypeName, this, declFlags);
        this->LOGGER().debug("%s: Defining type %-16s: %s", this->parse_loc_string().c_str(), designatedTypeName.c_str(),
                             declaration->to_string().c_str());
        if (! declaration)
            CERROR(this, "Failed to declare type " << designatedTypeName);
        else
            this->set_entity(declaration);
    }
    LexicalContext typeCtx(declaration ? declaration->get_symbol() : lexContext.scope());

    // declare type parameters within type declaration's scope, and before rest of type expression is processed:
    if (typeParamDecls) {
        for (auto paramDecl : *typeParamDecls) {
            paramDecl->symbol_declaration_pass(typeCtx);
        }
        this->declTypeParams = this->makeTypeParams(typeParamDecls);

        // if type parameters have been declared, rest of type expression has this lexCtx also as its defCtx:
        this->symbol_declaration_pass_descendants(typeCtx, typeCtx, declFlags);
    }
    else
        this->symbol_declaration_pass_descendants(defContext, typeCtx, declFlags);
}


void TxPredefinedTypeNode::symbol_declaration_pass(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                                   const std::string designatedTypeName,
                                                   const std::vector<TxDeclarationNode*>* typeParamDecls) {
    //std::cout << this->parse_loc_string() << ": defContext: " << defContext.scope()->get_full_name() << "   " << "lexContext: " << lexContext.scope()->get_full_name() << std::endl;
    this->defContext = defContext;

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
        if (auto identifiedTypeDecl = lookup_type(lexContext.scope(), this->identNode->ident)) {
            if (identifiedTypeDecl->get_decl_flags() & TXD_GENPARAM) {
                if (typeParamDecls && !typeParamDecls->empty()) {
                    CERROR(this, "Type parameters can't be declared on top of generic type parameter " << this->identNode->ident);
                    /* This might be desirable in future (or maybe not). E.g:
                       type Subtype<A derives Array> {
                          A<Int,5> field
                       }  */
                }

                this->set_context(lexContext);
                auto declaredAlias = lexContext.scope()->declare_alias(typeName, TXD_PUBLIC | TXD_IMPLICIT, identifiedTypeDecl);
                if (declaredAlias) {
                    this->LOGGER().debug("%s: Declared '%s' as alias for GENPARAM %s", this->parse_loc_string().c_str(),
                                         declaredAlias->get_full_name().to_string().c_str(),
                                         identifiedTypeDecl->to_string().c_str());
                    this->set_entity(identifiedTypeDecl);  // Or should this be the alias entity?
                }
                else
                    CERROR(this, "Failed to declare alias " << typeName.c_str());
                LexicalContext typeCtx(declaredAlias ? declaredAlias : lexContext.scope());
                this->symbol_declaration_pass_descendants(defContext, typeCtx, declFlags);
                return;
            }
        }
    }

    TxTypeExpressionNode::symbol_declaration_pass(defContext, lexContext, declFlags, typeName, typeParamDecls);
}


const TxType* TxPredefinedTypeNode::define_identified_type(ResolutionContext& resCtx) {
    // note: looking up in defContext ("outer" context) to avoid conflation with generic base types' inherited entities
    if (auto identifiedTypeDecl = lookup_type(this->defContext.scope(), this->identNode->ident)) {
        auto identifiedType = identifiedTypeDecl->get_type_definer()->resolve_type(resCtx);
        if (!identifiedType)
            return nullptr;
        else if (auto declEnt = this->get_declaration()) {
            ASSERT(!declTypeParams || declTypeParams->empty(), "declTypeParams can't be set for 'empty' specialization: " << *this);
            if (identifiedTypeDecl->get_decl_flags() & TXD_GENPARAM) {
                // refers to unbound generic type parameter, which is ok within the type declaring the parameter
                LOGGER().debug("%s: '%s' in %s refers to unbound generic type parameter %s", this->parse_loc_string().c_str(),
                               this->identNode->ident.to_string().c_str(), this->defContext.scope()->get_full_name().to_string().c_str(), identifiedTypeDecl->to_string().c_str());
                return identifiedType;
            }
            else {
                // create empty specialization (uniquely named but identical type)
                std::string errorMsg;
                auto type = this->types().get_type_specialization(declEnt, TxTypeSpecialization(identifiedType),
                                                                  false, this->declTypeParams, &errorMsg);
                if (! type)
                    CERROR(this, "Failed to create 'empty' type specialization: " << errorMsg);
                return type;
            }
        }
        else {
            if (identifiedTypeDecl->get_decl_flags() & TXD_GENPARAM) {
                // Should not happen unless source refers specifically to an unbound type parameter
                CWARNING(this, "'" << this->identNode->ident << "' refers to unbound generic type parameter " << identifiedTypeDecl);
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
    auto baseTypeDecl = lookup_type(this->context().scope(), this->identNode->ident);
    const TxType* baseType = baseTypeDecl ? baseTypeDecl->get_type_definer()->resolve_type(resCtx) : nullptr;
    if (! baseType) {
        CERROR(this, "Unknown type: " << this->identNode->ident << " (from " << this->context().scope() << ")");
        return nullptr;
    }
    if (baseType->type_params().size() < this->typeArgs->size()) {
        CERROR(this, "Too many generic type arguments specified for type " << identNode->ident);
        return nullptr;
    }
    std::vector<TxGenericBinding> bindings; // e.g. { TxTypeBinding("E", elemType), TxTypeBinding("L", length) }
    for (size_t i = 0; i < this->typeArgs->size(); i++) {
        bindings.push_back(this->typeArgs->at(i)->make_binding(resCtx, baseType, baseType->type_params().at(i)));
    }
    TxTypeSpecialization specialization(baseType, bindings);
    std::string errorMsg;
    auto type = this->types().get_type_specialization(this->get_declaration(), specialization, false, this->declTypeParams, &errorMsg);
    if (! type)
        CERROR(this, "Failed to specialize type: " << errorMsg);
    return type;
}



void TxArrayTypeNode::symbol_declaration_pass_descendants(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags) {
    this->elementTypeNode->symbol_declaration_pass(defContext, lexContext);
// If we're to implicitly define a binding the Array.L parameter, we must also fix so that such implicit bindings
// get a corresponding type parameter declaration in the outer type entity.
//    if (! this->lengthNode) {
//        // implicit redeclaration of array length parameter L:
//        TxIdentifierNode* redeclIdent = new TxIdentifierNode(this->parseLocation, new TxIdentifier("L"));
//        TxExpressionNode* lengthExpr = new TxFieldValueNode(this->parseLocation, nullptr, redeclIdent);
//        lengthNode = new TxTypeArgumentNode(lengthExpr);
//    }
    if (this->lengthNode)
        this->lengthNode->symbol_declaration_pass(defContext, lexContext);
}


void TxModifiableTypeNode::symbol_declaration_pass(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags,
        const std::string designatedTypeName, const std::vector<TxDeclarationNode*>* typeParamDecls) {
    // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
    if (auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>(this->baseType)) {
        if (auto maybeModElem = dynamic_cast<TxMaybeModTypeNode*>(arrayBaseType->elementTypeNode->typeExprNode)) {
            // (can this spuriously add Modifiable node to predeclared modifiable type, generating error?)
            lexContext.scope()->LOGGER().debug("Implicitly declaring Array Element modifiable at %s", this->to_string().c_str());
            maybeModElem->isModifiable = true;
        }
    }

    TxTypeExpressionNode::symbol_declaration_pass(defContext, lexContext, declFlags, designatedTypeName, typeParamDecls);
}


bool TxMaybeModTypeNode::has_predefined_type() const {
    if (this->isModifiable)
        return false;
    if (auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>(this->baseType))
        if (typeid(*arrayBaseType->elementTypeNode->typeExprNode) == typeid(TxModifiableTypeNode))
            return false;
    return this->baseType->has_predefined_type();
}

void TxMaybeModTypeNode::symbol_declaration_pass(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags,
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
        TxModifiableTypeNode::symbol_declaration_pass(defContext, lexContext, declFlags, designatedTypeName, typeParamDecls);
    else {
        // "pass through" entity declaration to the underlying type
        this->set_context(lexContext);
        this->baseType->symbol_declaration_pass(defContext, lexContext, declFlags, designatedTypeName, typeParamDecls);
    }
}



TxScopeSymbol* TxFieldValueNode::resolve_symbol(ResolutionContext& resCtx) {
    if (! this->cachedSymbol) {
        if (this->hasRunResolve)
            return nullptr;
        this->hasRunResolve = true;
        std::vector<TxScopeSymbol*> tmpPath;
        if (this->baseExpr) {
            // baseExpr may or may not refer to a type (e.g. modules don't)
            auto baseType = this->baseExpr->resolve_type(resCtx);  // must be resolved before lookups via its symbol
            TxScopeSymbol* vantageScope = this->context().scope();
            if (auto baseSymbolNode = dynamic_cast<TxFieldValueNode*>(this->baseExpr)) {
                if (auto baseSymbol = baseSymbolNode->resolve_symbol(resCtx)) {
                    this->cachedSymbol = lookup_member(vantageScope, baseSymbol, this->memberName);
                }
            }
            else if (baseType) {
                // non-name (i.e. computed) value expression
                this->cachedSymbol = baseType->lookup_instance_member(vantageScope, this->memberName);
            }
        }
        else {
            this->cachedSymbol = lookup_symbol(this->context().scope(), this->memberName);
        }
    }

//    if (this->appliedFuncArgTypes) {
//        if (dynamic_cast<const TxOverloadedEntity*>(this->cachedSymbol)) {
//            // if symbol is overloaded, and can be resolved to actual field, then do so
//            if (auto resolvedField = resolve_field_lookup(resCtx, this->cachedSymbol, this->appliedFuncArgTypes))
//                this->cachedSymbol = resolvedField;
//        }
//        else if (dynamic_cast<const TxTypeEntity*>(this->cachedSymbol)) {
//            // if symbol is a type, and the applied arguments match a constructor, the resolve to that constructor
//            std::vector<TxSymbolScope*> tmpPath;
//            if (auto constructorSymbol = this->cachedSymbol->lookup_member(tmpPath, "$init")) {
//                if (auto constructor = resolve_field_lookup(resCtx, constructorSymbol, this->appliedFuncArgTypes))
//                    this->cachedSymbol = constructor;
//            }
//        }
//    }

    return this->cachedSymbol;
}

TxEntityDeclaration* TxFieldValueNode::resolve_decl(ResolutionContext& resCtx) {
    if (auto symbol = this->resolve_symbol(resCtx)) {
        if (auto entitySymbol = dynamic_cast<TxEntitySymbol*>(symbol)) {
            // if symbol can be resolved to actual field, then do so
            if (entitySymbol->field_count()) {
                if (auto fieldDecl = resolve_field_lookup(resCtx, entitySymbol, this->appliedFuncArgTypes))
                    return fieldDecl;
            }
            // if symbol is a type, and arguments are applied, and they match a constructor, the resolve to that constructor
            if (auto typeDecl = entitySymbol->get_type_decl()) {
                if (this->appliedFuncArgTypes) {
                    if (auto allocType = typeDecl->get_type_definer()->resolve_type(resCtx))
                        if (auto constructorSymbol = allocType->lookup_instance_member("$init"))
                            if (auto constructorDecl = resolve_field_lookup(resCtx, constructorSymbol, this->appliedFuncArgTypes)) {
                                ASSERT(constructorDecl->get_decl_flags() & TXD_CONSTRUCTOR, "field named $init is not flagged as TXD_CONSTRUCTOR: " << constructorDecl->to_string());
                                return constructorDecl;
                            }
                }
                // resolve this symbol to its type
                return typeDecl;
            }
            CERROR(this, "Failed to resolve entity symbol to proper field: " << entitySymbol->to_string().c_str());
        }
    }
    else
        CERROR(this, "Unknown symbol: '" << this->get_full_identifier() << "'");
    return nullptr;
}

const TxType* TxFieldValueNode::define_type(ResolutionContext& resCtx) {
    if (auto decl = this->resolve_decl(resCtx)) {
        if (auto fieldDecl = dynamic_cast<TxFieldDeclaration*>(decl)) {
            this->cachedField = fieldDecl->get_field_definer()->resolve_field(resCtx);
            return this->cachedField->get_type();
        }
        else
            return static_cast<TxTypeDeclaration*>(decl)->get_type_definer()->resolve_type(resCtx);
    }
    return nullptr;
}



const TxType* TxConstructorCalleeExprNode::define_type(ResolutionContext& resCtx) {
    ASSERT(this->appliedFuncArgTypes, "appliedFuncArgTypes of TxConstructorCalleeExprNode not initialized");
    if (auto allocType = this->objectExpr->resolve_type(resCtx)) {
        // find the constructor
        if (auto constructorSymbol = allocType->lookup_instance_member("$init")) {
            if (auto constructorDecl = resolve_field_lookup(resCtx, constructorSymbol, this->appliedFuncArgTypes)) {
                ASSERT(constructorDecl->get_decl_flags() & TXD_CONSTRUCTOR, "field named $init is not flagged as TXD_CONSTRUCTOR: " << constructorDecl->to_string());
                this->constructor = constructorDecl->get_field_definer()->resolve_field(resCtx);
                if (this->constructor)
                    return this->constructor->get_type();
            }
            /*
            // Check that constructor is not invalidly inherited:
            if (auto cDefiningTypeSym = dynamic_cast<TxEntitySymbol*>(symbol->get_outer())) {
                if (auto cDefiningTypeDecl = cDefiningTypeSym->get_type_decl()) {
                    if (auto cDefiningType = cDefiningTypeDecl->get_type_definer()->resolve_type(resCtx)) {
                        do {
                            if (*allocType == *cDefiningType) {
                                this->constructorEntity = resolve_field_lookup(resCtx, symbol, this->appliedFuncArgTypes);
                                if (this->constructorEntity)
                                    return this->constructorEntity->resolve_symbol_type(resCtx);
                                else
                                    break;
                            }
                            else if (allocType->is_pure_specialization())
                                allocType = allocType->get_base_type();
                            else
                                break;
                        } while (true);
                    }
                }
            }
            */
        }
        if (this->appliedFuncArgTypes->size() == 0) {
            // TODO: support default value constructor
        }
        else if (this->appliedFuncArgTypes->size() == 1) {
            // TODO: support default assignment constructor
        }
        CERROR(this, "No matching constructor for type " << allocType);
    }
    return nullptr;
}


const TxType* TxFunctionCallNode::define_type(ResolutionContext& resCtx) {
    this->register_callee_signature(resCtx);
    auto calleeType = this->callee->resolve_type(resCtx);
    if (!calleeType)
        return nullptr;
    auto funcType = dynamic_cast<const TxFunctionType*>(calleeType);
    if (! funcType) {
        if (auto fieldValueNode = dynamic_cast<TxFieldValueNode*>(this->callee)) {
            // handle direct constructor invocation - self() and super()
            // TODO: shall only be legal as first statement within constructor body
            auto identifier = fieldValueNode->get_full_identifier();
            if (identifier == "self" || identifier == "super") {
                if (! this->context().is_constructor())
                    CERROR(this, "self() / super() constructor may only be invoked from within the type's other constructors");

                auto objectDeref = new TxReferenceDerefNode(this->callee->parseLocation, this->callee);
                auto constructorCallee = new TxConstructorCalleeExprNode(this->callee->parseLocation, objectDeref);
                //constructorCallee->symbol_declaration_pass(this->context());
                objectDeref->set_context(this);
                constructorCallee->set_context(this);
                constructorCallee->set_applied_func_arg_types(this->callee->get_applied_func_arg_types());

                if (auto constrCalleeType = constructorCallee->resolve_type(resCtx)) {
                    this->callee = constructorCallee;
                    calleeType = constrCalleeType;
                    funcType = dynamic_cast<const TxFunctionType*>(constrCalleeType);
                }
            }
        }

        if (! funcType) {
            CERROR(this, "Callee of function call expression is not of function type: " << calleeType);
            return nullptr;
        }
    }

    if (auto inlineFunc = dynamic_cast<const TxBuiltinConversionFunctionType*>(calleeType)) {
        // "inline" function call by replacing with conversion expression
        this->inlinedExpression = validate_wrap_convert(resCtx, this->argsExprList->front(), inlineFunc->returnType, true);
        return inlineFunc->returnType;
    }

    // verify matching function signature:
    if (funcType->argumentTypes.size() != this->argsExprList->size()) {
        CERROR(this, "Callee of function call expression has mismatching argument count: " << calleeType);
        return nullptr;
    }
    // (regular function call, not inlined expression)
    auto argExprI = this->argsExprList->begin();
    for (auto argDef : funcType->argumentTypes) {
        // note: similar rules to assignment
        *argExprI = validate_wrap_assignment(resCtx, *argExprI, argDef);
        argExprI++;
    }

    if (auto constructorType = dynamic_cast<const TxConstructorType*>(calleeType)) {
        // inline code for stack allocation and constructor invocation
        if (! dynamic_cast<TxConstructorCalleeExprNode*>(this->callee)) {  // (prevents infinite recursion)
            auto objectType = constructorType->get_constructed_type_decl()->get_type_definer()->resolve_type(resCtx);
            this->inlinedExpression = new TxStackConstructorNode(this, objectType);
            this->inlinedExpression->symbol_declaration_pass(this->context());
            this->inlinedExpression->symbol_resolution_pass(resCtx);
            return objectType;
        }
    }

    return funcType->returnType;
}
