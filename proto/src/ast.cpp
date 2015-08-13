#include "ast.hpp"


static bool commonNameValidityChecks(TxSpecializableNode* node, TxDeclarationFlags declFlags, const std::string& name) {
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

bool validateTypeName(TxSpecializableNode* node, TxDeclarationFlags declFlags, const std::string& name) {
    // TODO: warning if first character is not upper case
    return commonNameValidityChecks(node, declFlags, name);
}

bool validateFieldName(TxSpecializableNode* node, TxDeclarationFlags declFlags, const std::string& name) {
    // TODO: either all chars upper case or first character lower case, else warning
    return commonNameValidityChecks(node, declFlags, name);
}



Logger& TxNode::LOG = Logger::get("AST");

unsigned TxNode::nextNodeId = 0;

std::string TxNode::to_string() const {
    char buf[256];
    snprintf(buf, 256, "%-24s %3u : %-11s", typeid(*this).name(), this->get_node_id(), this->parse_loc_string().c_str());
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


void TxFieldDeclNode::symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) {
    this->set_context(six, lexContext);

    if (field->initExpression) {
        if (this->declFlags & TXD_ABSTRACT)
            CERROR(this, "'abstract' is invalid modifier for field / method that has an initializer / body: " << field->initExpression);
    }

    // Note: TXS_STATIC is set here, and may later be changed to TXS_VIRTUAL depending on context.
    TxFieldStorage storage;
    if (this->isMethodSyntax && lexContext.outer_type()) {
        // Note: instance method storage is handled specially (technically the function pointer is a static field)
        auto lambdaExpr = static_cast<TxLambdaExprNode*>(field->initExpression);
        if (!lambdaExpr && !(this->declFlags & TXD_ABSTRACT))
            CERROR(this, "Missing modifier 'abstract' for method that has no body");
        if (this->declFlags & TXD_STATIC) {
            storage = TXS_STATIC;
        }
        else {
            if (lambdaExpr)
                lambdaExpr->set_instance_method(true);
            storage = TXS_INSTANCEMETHOD;
        }
    }
    else if (dynamic_cast<TxModule*>(lexContext.scope())) {  // if in global scope
        if (this->declFlags & TXD_STATIC)
            CERROR(this, "'static' is invalid modifier for module scope field " << this->field->get_source_field_name());
        if (this->declFlags & TXD_FINAL)
            CERROR(this, "'final' is invalid modifier for module scope field " << this->field->get_source_field_name());
        if (this->declFlags & TXD_OVERRIDE)
            CERROR(this, "'override' is invalid modifier for module scope field " << this->field->get_source_field_name());
        if (this->declFlags & TXD_ABSTRACT)
            CERROR(this, "'abstract' is invalid modifier for module scope field " << this->field->get_source_field_name());
        storage = TXS_GLOBAL;
    }
    else {
        if (this->declFlags & TXD_ABSTRACT) {
            if (! (this->declFlags & TXD_STATIC))
                CERROR(this, "'abstract' fields must also be declared 'static': " << this->field->get_source_field_name());
            if (! (this->declFlags & (TXD_PROTECTED | TXD_PUBLIC)))
                CERROR(this, "'abstract' fields cannot be private (since private are non-virtual): " << this->field->get_source_field_name());
        }
        storage = (this->declFlags & TXD_STATIC) ? TXS_STATIC : TXS_INSTANCE;
    }

    this->field->symbol_declaration_pass_nonlocal_field(six, lexContext, this->declFlags, storage, TxIdentifier(""));
}

void TxFieldDeclNode::symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) {
    this->field->symbol_resolution_pass(six, resCtx);

    if (auto type = this->field->get_type(six)) {
        auto storage = this->field->get_declaration(six)->get_storage();
        if (type->is_modifiable()) {
            if (storage == TXS_GLOBAL)
                CERROR(this, "Global fields may not be modifiable: " << field->get_source_field_name().c_str());
        }

        if (this->field->initExpression) {
            if (storage == TXS_INSTANCE) {
                if (this->field->get_source_field_name() != "tx#Array#L")  // hackish...
                    CWARNING(this, "Not yet supported: Inline initializer for instance fields (initialize within constructor instead): " << this->field->get_source_field_name());
            }
        }
        else {
            if (storage == TXS_GLOBAL || storage == TXS_STATIC) {
                if (! (this->field->get_declaration(six)->get_decl_flags() & TXD_GENPARAM))
                    CERROR(this, "Global/static fields must have an initializer: " << this->field->get_source_field_name());
            }
            else if (storage == TXS_VIRTUAL || storage == TXS_INSTANCEMETHOD) {
                if (! (this->field->get_declaration(six)->get_decl_flags() & TXD_ABSTRACT))
                    CERROR(this, "Non-abstract virtual fields/methods must have an initializer: " << this->field->get_source_field_name());
            }
            // Note: TXS_STACK is not declared via this node
            // FUTURE: ensure TXS_INSTANCE fields are initialized either here or in every constructor
        }
    }
}


TxExpressionNode* TxExpressionNode::get_value_definer(TxSpecializationIndex six) {
    return new TxExprWrapperNode(this, six);
}


// FUTURE: factor out the 'explicit' code path into separate function
// FUTURE: rework together with overloaded function resolution
static TxExpressionNode* inner_validate_wrap_convert(TxSpecializationIndex six, ResolutionContext& resCtx,
                                                     TxExpressionNode* originalExpr,
                                                     const TxType* requiredType, bool _explicit) {
    // Note: Symbol table pass and semantic pass are not run on the created wrapper nodes.
    auto originalType = originalExpr->resolve_type(six, resCtx);
    if (! originalType)
        return originalExpr;
    if (originalType == requiredType) // TODO: test with:  || *originalType == *requiredType)
        return originalExpr;
    if (_explicit || originalType->auto_converts_to(*requiredType)) {
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
                refToNode->set_context(six, originalExpr->context(six));
                return new TxReferenceConvNode(originalExpr->parseLocation, refToNode, refType);
            }
        }
    }
    CERROR(originalExpr, "Can't auto-convert value (s-ix " << six << ")\n\tFrom: " << originalType << " \t@" << originalType->get_parse_location()
                         << "\n\tTo:   " << requiredType << " \t@" << requiredType->get_parse_location());
    return originalExpr;
}

TxExpressionNode* validate_wrap_convert(TxSpecializationIndex six, ResolutionContext& resCtx, TxExpressionNode* originalExpr,
                                        const TxType* requiredType, bool _explicit) {
    if (six != 0) {
        // TODO: refactor
        TxExpressionNode* fromExpr = originalExpr;
        if (auto convNode = dynamic_cast<TxConversionNode*>(originalExpr))
            fromExpr = convNode->expr;  // pass-through conversion added in s-ix 0
        if (auto originalType = fromExpr->resolve_type(six, resCtx)) {
            if (! (originalType == requiredType || _explicit || originalType->auto_converts_to(*requiredType)))
                CERROR(fromExpr, "Can't auto-convert value (s-ix " << six << ")\n\tFrom: " << originalType << " \t@" << originalType->get_parse_location()
                                     << "\n\tTo:   " << requiredType << " \t@" << requiredType->get_parse_location());
        }
        return originalExpr;
    }
    auto exprNode = inner_validate_wrap_convert(six, resCtx, originalExpr, requiredType, _explicit);
    if (exprNode != originalExpr) {
        originalExpr->LOGGER().trace("Wrapping conversion to type %s around %s", requiredType->to_string(true).c_str(), originalExpr->to_string().c_str());
        exprNode->symbol_declaration_pass(six, originalExpr->context(six));
        exprNode->symbol_resolution_pass(six, resCtx);
    }
    return exprNode;
}

TxExpressionNode* validate_wrap_assignment(TxSpecializationIndex six, ResolutionContext& resCtx, TxExpressionNode* rValueExpr, const TxType* requiredType) {
    if (! requiredType->is_concrete())
        // TODO: dynamic concrete type resolution (recognize actual type in runtime when dereferencing a generic pointer)
        CERROR(rValueExpr, "Assignee is not a concrete type (size potentially unknown): " << requiredType);
    // if assignee is a reference:
    // TODO: check dataspace rules
    return validate_wrap_convert(six, resCtx, rValueExpr, requiredType);
}



TxSuiteNode::TxSuiteNode(const yy::location& parseLocation)
        : TxStatementNode(parseLocation), suite(new std::vector<TxStatementNode*>())  {
}

TxSuiteNode::TxSuiteNode(const yy::location& parseLocation, std::vector<TxStatementNode*>* suite)
        : TxStatementNode(parseLocation), suite(suite)  {
}



std::vector<TxTypeParam>* TxTypeExpressionNode::makeDeclTypeParams(TxSpecializationIndex six) {
    const std::vector<TxDeclarationNode*>* typeParamDeclNodes = this->getTypeParamDeclNodes(six);
    if (! typeParamDeclNodes)
        return nullptr;
    auto paramsVec = new std::vector<TxTypeParam>();
    for (auto declNode : *typeParamDeclNodes) {
        if (declNode->get_decl_flags() & TXD_GENPARAM) {
            //std::cout << "param decl is a GENPARAM: " << declNode << std::endl;
            if (auto typeDeclNode = dynamic_cast<TxTypeDeclNode*>(declNode))
                paramsVec->push_back(TxTypeParam(TxTypeParam::MetaType::TXB_TYPE, typeDeclNode->typeName, typeDeclNode->typeExpression->get_type_definer(six)));
            else if (auto valueDeclNode = dynamic_cast<TxFieldDeclNode*>(declNode))
                paramsVec->push_back(TxTypeParam(TxTypeParam::MetaType::TXB_VALUE, valueDeclNode->field->get_decl_field_name(), valueDeclNode->field->get_field_definer(six)));
        }
//        else
//            std::cout << "param decl is NOT a GENPARAM: " << declNode << std::endl;
    }
    return paramsVec;
}


void TxTypeDeclNode::symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& defContext, LexicalContext& lexContext) {
    this->set_context(six, lexContext);
    // Note: does not invoke symbol_declaration_pass() on typeParamDecls, that is delegated to typeExpression
    this->typeExpression->symbol_declaration_pass(six, defContext, lexContext, this->declFlags, this->typeName, this->typeParamDecls);
}


void TxTypeExpressionNode::symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& defContext, LexicalContext& lexContext,
                                                   TxDeclarationFlags declFlags, const std::string designatedTypeName,
                                                   const std::vector<TxDeclarationNode*>* typeParamDeclNodes) {
    // Each node in a type expression has the option of declaring an entity (i.e. creating a name for)
    // any of its constituent type expressions.
    // Type entities are at minimum needed for:
    //  - explicit type declarations/extensions
    //  - adding members (since members are namespace symbols) - only done in explicit type extensions
    //  - specializations that bind generic type parameters (e.g. Ref<Ref<Int>>)
    //  - using fields in type expressions (resolving to the field's type) (not terribly important)
    // Note: Implicitly declared types should have the same visibility as the type/field they are for.

    // The context of this node represents its outer scope. This node's entity, if any, represents its inner scope.
    this->set_context(six, lexContext);

    TxTypeDeclaration* declaration = nullptr;
    if (! designatedTypeName.empty()) {
        declaration = lexContext.scope()->declare_type(designatedTypeName, this->get_type_definer(six), declFlags);
        this->LOGGER().debug("%s: Declaring type %-16s: %s", this->parse_loc_string().c_str(), designatedTypeName.c_str(),
                             declaration->to_string().c_str());
        if (! declaration)
            CERROR(this, "Failed to declare type " << designatedTypeName);
        else
            this->get_spec(six).declaration = declaration;
    }
    LexicalContext typeCtx(lexContext, declaration ? declaration->get_symbol() : lexContext.scope());

    // declare type parameters within type declaration's scope, and before rest of type expression is processed:
    if (typeParamDeclNodes) {
        this->setTypeParamDeclNodes(six, typeParamDeclNodes);
        for (auto paramDeclNode : *typeParamDeclNodes) {
            paramDeclNode->symbol_declaration_pass(six, typeCtx);
        }

        // if type parameters have been declared, rest of type expression has this lexCtx also as its defCtx:
        this->symbol_declaration_pass_descendants(six, typeCtx, typeCtx, declFlags);
    }
    else
        this->symbol_declaration_pass_descendants(six, defContext, typeCtx, declFlags);
}


void TxPredefinedTypeNode::symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& defContext,
                                                   LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                                   const std::string designatedTypeName,
                                                   const std::vector<TxDeclarationNode*>* typeParamDeclNodes) {
    //std::cout << this->parse_loc_string() << ": defContext: " << defContext.scope()->get_full_name() << "   " << "lexContext: " << lexContext.scope()->get_full_name() << std::endl;
    this->get_spec(six).defContext = defContext;

    std::string typeName;
    if (designatedTypeName.empty() && !this->typeArgs->empty()) {
        // ensure generic type specializations always have a declared type (handles e.g. Ref<Ref<Int>>)
        typeName = "$type";
        declFlags = declFlags | TXD_IMPLICIT;
    }
    else
        typeName = designatedTypeName;

    if (! typeName.empty()) {
        // If this type node is named (declared), perform early type name lookup
        // to capture generic type parameter referral, in which case the new entity will be an alias for it.
        // Note: Early lookup does not match the type parameters declared in this node, only prior ones:
        //     type Subtype<A> Type<A>  ## Legal since A is subnode of decl's type expression
        //     type Subtype<A> A        ## Illegal since A is top node of decl's type expression
        //std::cerr << "early lookup of " << this->identNode->ident << " under " << lexContext.scope() << std::endl;
        if (auto identifiedTypeDecl = lookup_type(lexContext.scope(), this->identNode->ident)) {
            if (identifiedTypeDecl->get_decl_flags() & TXD_GENPARAM) {
                if (typeParamDeclNodes && !typeParamDeclNodes->empty()) {
                    CERROR(this, "Type parameters can't be declared on top of generic type parameter " << this->identNode->ident);
                    /* This might be desirable in future (or maybe not). E.g:
                       type Subtype<A derives Array> {
                          A<Int,5> field
                       }  */
                }

                this->set_context(six, lexContext);
                auto declaredAlias = lexContext.scope()->declare_alias(typeName, TXD_PUBLIC | TXD_IMPLICIT, identifiedTypeDecl);
                if (declaredAlias) {
                    this->LOGGER().debug("%s: Declared '%s' as alias for GENPARAM %s", this->parse_loc_string().c_str(),
                                         declaredAlias->get_full_name().to_string().c_str(),
                                         identifiedTypeDecl->to_string().c_str());
                    this->get_spec(six).declaration = identifiedTypeDecl;  // Or should this be the alias entity?
                }
                else
                    CERROR(this, "Failed to declare alias " << typeName.c_str());
                LexicalContext typeCtx(lexContext, declaredAlias ? declaredAlias : lexContext.scope());
                this->symbol_declaration_pass_descendants(six, defContext, typeCtx, declFlags);
                return;
            }
        }
    }
//    else
//        std::cerr << "skipped early lookup of " << this->identNode->ident << " under " << lexContext.scope() << std::endl;

    TxTypeExpressionNode::symbol_declaration_pass(six, defContext, lexContext, declFlags, typeName, typeParamDeclNodes);
}


const TxType* TxPredefinedTypeNode::define_identified_type(TxSpecializationIndex six, ResolutionContext& resCtx) {
    // note: looking up in defContext ("outer" context) to avoid conflation with generic base types' inherited entities
    if (auto identifiedTypeDecl = lookup_type(this->get_spec(six).defContext.scope(), this->identNode->ident)) {
        auto identifiedType = identifiedTypeDecl->get_definer()->resolve_type(resCtx);
        if (!identifiedType)
            return nullptr;
        else if (auto declEnt = this->get_declaration(six)) {
            if (identifiedTypeDecl->get_decl_flags() & TXD_GENPARAM) {
                // refers to unbound generic type parameter, which is ok within the type declaring the parameter
                LOGGER().debug("%s: '%s' in %s refers to unbound generic type parameter %s", this->parse_loc_string().c_str(),
                               this->identNode->ident.to_string().c_str(), this->get_spec(six).defContext.scope()->get_full_name().to_string().c_str(), identifiedTypeDecl->to_string().c_str());
                return identifiedType;
            }
            else if (!identifiedType->is_modifiable()){
                // create empty specialization (uniquely named but identical type)
                return this->types(six).get_type_specialization(declEnt, TxTypeSpecialization(identifiedType));
            }
        }
//        else if (identifiedTypeDecl->get_decl_flags() & TXD_GENPARAM) {
//            // Happens if source (e.g. member) refers specifically to an unbound type parameter
//            // (should we let this be an alias for the generic type parameter, and if so, how?)
//            CWARNING(this, "'" << this->identNode->ident << "' refers to unbound generic type parameter " << identifiedTypeDecl);
//        }
        return identifiedType;
    }
    return nullptr;
}

const TxType* TxPredefinedTypeNode::define_generic_specialization_type(TxSpecializationIndex six, ResolutionContext& resCtx) {
    auto baseTypeDecl = lookup_type(this->context(six).scope(), this->identNode->ident);
    const TxType* baseType = baseTypeDecl ? baseTypeDecl->get_definer()->resolve_type(resCtx) : nullptr;
    if (! baseType) {
        CERROR(this, "Unknown type: " << this->identNode->ident << " (from " << this->context(six).scope() << ")");
        return nullptr;
    }
    if (baseType->type_params().size() < this->typeArgs->size()) {
        CERROR(this, "Too many generic type arguments specified for type " << identNode->ident);
        return nullptr;
    }
    auto baseTypeName = baseType->get_declaration()->get_symbol()->get_full_name();
    std::vector<TxGenericBinding> bindings; // e.g. { TxTypeBinding("E", elemType), TxTypeBinding("L", length) }
    for (size_t i = 0; i < this->typeArgs->size(); i++) {
        bindings.push_back(this->typeArgs->at(i)->make_binding(six, baseTypeName, baseType->type_params().at(i)));
    }
    TxTypeSpecialization specialization(baseType, bindings);
    auto declTypeParams = makeDeclTypeParams(six);
    auto noInterf = std::vector<TxTypeSpecialization>();
    return this->types().get_type_specialization(this->get_declaration(six), specialization, noInterf, declTypeParams);
}



void TxArrayTypeNode::symbol_declaration_pass_descendants(TxSpecializationIndex six, LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags) {
    this->elementTypeNode->symbol_declaration_pass(six, defContext, lexContext);
// If we're to implicitly define a binding the Array.L parameter, we must also fix so that such implicit bindings
// get a corresponding type parameter declaration in the outer type entity.
//    if (! this->lengthNode) {
//        // implicit redeclaration of array length parameter L:
//        TxIdentifierNode* redeclIdent = new TxIdentifierNode(this->parseLocation, new TxIdentifier("L"));
//        TxExpressionNode* lengthExpr = new TxFieldValueNode(this->parseLocation, nullptr, redeclIdent);
//        lengthNode = new TxTypeArgumentNode(lengthExpr);
//    }
    if (this->lengthNode)
        this->lengthNode->symbol_declaration_pass(six, defContext, lexContext);
}


void TxDerivedTypeNode::init_implicit_types() {
    // implicit type members '$Self' and '$Super' for types with a body:
    // FUTURE: if type is immutable, the reference target type should perhaps not be modifiable?
    auto selfTypeExprN = new TxTypeExprWrapperNode(this);
    auto selfRefTypeExprN = new TxReferenceTypeNode(this->parseLocation, nullptr,
                                                    new TxModifiableTypeNode(this->parseLocation, selfTypeExprN));
    const std::string selfTypeName = "$Self";
    this->selfRefTypeNode = new TxTypeDeclNode(this->parseLocation, TXD_IMPLICIT, selfTypeName, nullptr, selfRefTypeExprN);

    TxTypeExpressionNode* superTypeExprN = this->baseTypes->empty()
                                       ? new TxPredefinedTypeNode(this->parseLocation, "tx.Tuple")
                                       : static_cast<TxTypeExpressionNode*>(new TxTypeExprWrapperNode(this->baseTypes->at(0)));
    auto superRefTypeExprN = new TxReferenceTypeNode(this->parseLocation, nullptr,
                                                     new TxModifiableTypeNode(this->parseLocation, superTypeExprN));
    const std::string superTypeName = "$Super";
    this->superRefTypeNode = new TxTypeDeclNode(this->parseLocation, TXD_IMPLICIT, superTypeName, nullptr, superRefTypeExprN);
}


void TxModifiableTypeNode::symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                                   const std::string designatedTypeName, const std::vector<TxDeclarationNode*>* typeParamDeclNodes) {
    // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
    if (auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>(this->baseType)) {
        if (auto maybeModElem = dynamic_cast<TxMaybeModTypeNode*>(arrayBaseType->elementTypeNode->typeExprNode)) {
            // (can this spuriously add Modifiable node to predeclared modifiable type, generating error?)
            lexContext.scope()->LOGGER().debug("Implicitly declaring Array Element modifiable at %s", this->to_string().c_str());
            maybeModElem->isModifiable = true;
        }
    }

    TxTypeExpressionNode::symbol_declaration_pass(six, defContext, lexContext, declFlags, designatedTypeName, typeParamDeclNodes);
}


bool TxMaybeModTypeNode::has_predefined_type() const {
    if (this->isModifiable)
        return false;
    if (auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>(this->baseType))
        if (typeid(*arrayBaseType->elementTypeNode->typeExprNode) == typeid(TxModifiableTypeNode))
            return false;
    return this->baseType->has_predefined_type();
}

void TxMaybeModTypeNode::symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                                 const std::string designatedTypeName, const std::vector<TxDeclarationNode*>* typeParamDeclNodes) {
    // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
    if (! this->isModifiable) {
        if (auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>(this->baseType))
            if (typeid(*arrayBaseType->elementTypeNode->typeExprNode) == typeid(TxModifiableTypeNode)) {
                lexContext.scope()->LOGGER().debug("Implicitly declaring Array modifiable at %s", this->to_string().c_str());
                this->isModifiable = true;
            }
    }

    if (this->isModifiable)
        TxModifiableTypeNode::symbol_declaration_pass(six, defContext, lexContext, declFlags, designatedTypeName, typeParamDeclNodes);
    else {
        // "pass through" entity declaration to the underlying type
        this->set_context(six, lexContext);
        this->baseType->symbol_declaration_pass(six, defContext, lexContext, declFlags, designatedTypeName, typeParamDeclNodes);
    }
}


TxAssertStmtNode::TxAssertStmtNode(const yy::location& parseLocation, TxExpressionNode* expr)
        : TxStatementNode(parseLocation), expr(expr)  {
    auto invertedCond = new TxUnaryLogicalNotNode(expr->parseLocation, expr);

    // print assert failed message:
    /* example C assert failed message:
    txc: /home/christer/proj/workspace/proto/src/ast.cpp:515: TxAssertStmtNode::TxAssertStmtNode(const yy::location&, TxExpressionNode*): Assertion `!this->expr' failed.
    */
    auto & pLoc = parseLocation;
    std::stringstream msg;
    //msg << procName << ": ";         // TODO: will need to be determined via system call
    msg << *pLoc.begin.filename << ":" << pLoc.begin.line;
    //msg << ": " << srcFuncHeader;    // TODO: source text needed for this
    msg << ": Assertion failed";
    //msg << ": `" << srcExpr << "`";  // TODO: source text needed for this
    //msg << ": " << customMessage;    // TODO: supported custom assert message
    std::string assertFailedMsg = "c\"" + msg.str() + "\"";
    auto msgExpr = new TxCStringLitNode(pLoc, assertFailedMsg);
    auto convStrExpr = new TxReferenceToNode(pLoc, new TxElemDerefNode(pLoc, msgExpr, new TxIntegerLitNode(pLoc, (uint64_t)0)));
    auto putsCallee = new TxFieldValueNode(pLoc, nullptr, "tx.c.puts");
    auto putsCallExpr = new TxFunctionCallNode( pLoc, putsCallee, new std::vector<TxExpressionNode*>( { convStrExpr } ) );
    TxStatementNode* putsStmt = new TxCallStmtNode(pLoc, putsCallExpr);
    // TODO: emit it to stderr instead of stdout

    // we call c library abort() upon assertion failure
    auto abortCallee = new TxFieldValueNode(pLoc, nullptr, "tx.c.abort");
    auto abortCallExpr = new TxFunctionCallNode(pLoc, abortCallee, new std::vector<TxExpressionNode*>());
    TxStatementNode* abortStmt = new TxCallStmtNode(pLoc, abortCallExpr);

    auto failureSuite = new TxSuiteNode( pLoc, new std::vector<TxStatementNode*>( { putsStmt, abortStmt } ) );
    this->ifStmt = new TxIfStmtNode(pLoc, invertedCond, failureSuite);
}



TxScopeSymbol* TxFieldValueNode::resolve_symbol(TxSpecializationIndex six, ResolutionContext& resCtx) {
    TxScopeSymbol* symbol = nullptr;
    std::vector<TxScopeSymbol*> tmpPath;
    if (this->baseExpr) {
        // baseExpr may or may not refer to a type (e.g. modules don't)
        auto baseType = this->baseExpr->resolve_type(six, resCtx);  // must be resolved before lookups via its symbol
        TxScopeSymbol* vantageScope = this->context(six).scope();
        if (auto baseSymbolNode = dynamic_cast<TxFieldValueNode*>(this->baseExpr)) {
            if (auto baseSymbol = baseSymbolNode->resolve_symbol(six, resCtx)) {
                symbol = lookup_member(vantageScope, baseSymbol, this->symbolName);
            }
        }
        else if (baseType) {
            // non-name (i.e. computed) value expression
            symbol = baseType->lookup_inherited_instance_member(vantageScope, this->symbolName);
        }
    }
    else {
        symbol = lookup_symbol(this->context(six).scope(), this->symbolName);
    }
    return symbol;
}

const TxEntityDeclaration* TxFieldValueNode::resolve_decl(TxSpecializationIndex six, ResolutionContext& resCtx) {
    auto & spec = this->get_spec(six);
    if (spec.declaration)
        return spec.declaration;
    if (auto symbol = this->resolve_symbol(six, resCtx)) {
        if (auto entitySymbol = dynamic_cast<TxEntitySymbol*>(symbol)) {
            // if symbol can be resolved to actual field, then do so
            if (entitySymbol->field_count()) {
                if (auto fieldDecl = resolve_field_lookup(resCtx, entitySymbol, this->get_spec(six).appliedFuncArgTypes)) {
                    if (fieldDecl->get_storage() == TXS_INSTANCE || fieldDecl->get_storage() == TXS_INSTANCEMETHOD) {
                        if (!this->baseExpr) {
                            CERROR(this, "Instance member field referenced without instance base: " << this->get_full_identifier());
                            return nullptr;
                        }
                        else if (auto baseSymbolNode = dynamic_cast<TxFieldValueNode*>(this->baseExpr)) {
                            if (!baseSymbolNode->get_field(six)) {
                                CERROR(this, "Instance member field referenced without instance base: " << this->get_full_identifier());                                return nullptr;
                            }
                        }
                    }
                    spec.declaration = fieldDecl;
                    return spec.declaration;
                }
            }
            // if symbol is a type, and arguments are applied, and they match a constructor, the resolve to that constructor
            if (auto typeDecl = entitySymbol->get_type_decl()) {
                if (this->get_spec(six).appliedFuncArgTypes) {
                    if (auto allocType = typeDecl->get_definer()->resolve_type(resCtx)) {
                        // (constructors aren't inherited, but we bypass empty derivations)
                        while (allocType->is_modifiable() || allocType->is_empty_derivation())
                            allocType = allocType->get_base_type();
                        if (auto constructorSymbol = allocType->get_instance_member("$init"))  // (constructors aren't inherited)
                            if (auto constructorDecl = resolve_field_lookup(resCtx, constructorSymbol, this->get_spec(six).appliedFuncArgTypes)) {
                                ASSERT(constructorDecl->get_decl_flags() & TXD_CONSTRUCTOR, "field named $init is not flagged as TXD_CONSTRUCTOR: " << constructorDecl->to_string());
                                //std::cerr << "resolving field to constructor: " << this << ": " << constructorDecl << std::endl;
                                spec.declaration = constructorDecl;
                                return spec.declaration;
                            }
                    }
                    CERROR(this, "No matching constructor signature for type symbol: " << this->get_full_identifier());
                }
                else {
                    // resolve this symbol to its type
                    spec.declaration = typeDecl;
                    return spec.declaration;
                }
            }
            else
                CERROR(this, "Symbol could not be resolved to a distinct field or type: " << this->get_full_identifier());
        }
        // not an error
        //else
        //    CERROR(this, "Symbol is not a field or type: " << this->get_full_identifier());
    }
    else
        CERROR(this, "Unknown symbol: '" << this->get_full_identifier() << "'");
    return nullptr;
}

const TxType* TxFieldValueNode::define_type(TxSpecializationIndex six, ResolutionContext& resCtx) {
    if (auto decl = this->resolve_decl(six, resCtx)) {
        if (auto fieldDecl = dynamic_cast<const TxFieldDeclaration*>(decl)) {
            if (auto field = fieldDecl->get_definer()->resolve_field(resCtx)) {
                this->get_spec(six).field = field;
                return field->get_type();
            }
        }
        else
            return static_cast<const TxTypeDeclaration*>(decl)->get_definer()->resolve_type(resCtx);
    }
    return nullptr;
}



const TxType* TxConstructorCalleeExprNode::define_type(TxSpecializationIndex six, ResolutionContext& resCtx) {
    auto & spec = this->get_spec(six);
    ASSERT(spec.appliedFuncArgTypes, "appliedFuncArgTypes of TxConstructorCalleeExprNode not initialized");
    if (auto allocType = this->objectExpr->resolve_type(six, resCtx)) {
        // find the constructor
        // (constructors aren't inherited, but we bypass empty derivations)
        while (allocType->is_modifiable() || allocType->is_empty_derivation())
            allocType = allocType->get_base_type();
        if (auto constructorSymbol = allocType->get_instance_member("$init")) {
            if (auto constructorDecl = resolve_field_lookup(resCtx, constructorSymbol, spec.appliedFuncArgTypes)) {
                ASSERT(constructorDecl->get_decl_flags() & TXD_CONSTRUCTOR, "field named $init is not flagged as TXD_CONSTRUCTOR: " << constructorDecl->to_string());
                this->get_spec(six).declaration = constructorDecl;
                if (auto constructorField = constructorDecl->get_definer()->resolve_field(resCtx))
                    return constructorField->get_type();
            }
        }
        if (spec.appliedFuncArgTypes->size() == 0) {
            // TODO: support default value constructor
        }
        else if (spec.appliedFuncArgTypes->size() == 1) {
            // TODO: support default assignment constructor
        }
        CERROR(this, "No matching constructor for type " << allocType);
    }
    return nullptr;
}


const TxType* TxFunctionCallNode::define_type(TxSpecializationIndex six, ResolutionContext& resCtx) {
    this->register_callee_signature(six, resCtx);
    auto calleeType = this->callee->resolve_type(six, resCtx);
    if (!calleeType)
        return nullptr;
    auto funcType = dynamic_cast<const TxFunctionType*>(calleeType);
    if (! funcType) {
        if (auto fieldValueNode = dynamic_cast<TxFieldValueNode*>(this->callee)) {
            // handle direct constructor invocation - self() and super()
            // TODO: shall only be legal as first statement within constructor body
            auto identifier = fieldValueNode->get_full_identifier();
            if (identifier == "self" || identifier == "super") {
                if (! this->context(six).get_constructed())
                    CERROR(this, "self() / super() constructor may only be invoked from within the type's other constructors");

                auto objectDeref = new TxReferenceDerefNode(this->callee->parseLocation, this->callee);
                auto constructorCallee = new TxConstructorCalleeExprNode(this->callee->parseLocation, objectDeref);
                //constructorCallee->symbol_declaration_pass(this->context());
                objectDeref->set_context(six, this->context(six));
                constructorCallee->set_context(six, this->context(six));
                constructorCallee->set_applied_func_arg_types(six, this->callee->get_applied_func_arg_types(six));

                if (auto constrCalleeType = constructorCallee->resolve_type(six, resCtx)) {
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
        this->get_spec(six).inlinedExpression = validate_wrap_convert(six, resCtx, this->argsExprList->front(), inlineFunc->returnType, true);
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
        *argExprI = validate_wrap_assignment(six, resCtx, *argExprI, argDef);
        argExprI++;
    }

    if (auto constructorType = dynamic_cast<const TxConstructorType*>(calleeType)) {
        // inline code for stack allocation and constructor invocation
        if (! dynamic_cast<TxConstructorCalleeExprNode*>(this->callee)) {  // (prevents infinite recursion)
            auto objectDefiner = constructorType->get_constructed_type_decl()->get_definer();
            auto inlinedExpression = new TxStackConstructorNode(this, objectDefiner);
            inlinedExpression->symbol_declaration_pass(six, this->context(six));
            inlinedExpression->symbol_resolution_pass(six, resCtx);
            this->get_spec(six).inlinedExpression = inlinedExpression;
            return objectDefiner->resolve_type(resCtx);
        }
    }

    return funcType->returnType;
}
