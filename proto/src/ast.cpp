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
        int lcol = (parseLocation.end.column > parseLocation.begin.column) ? parseLocation.end.column : parseLocation.end.column;
        snprintf(buf, 128, "%2d.%2d-%2d", parseLocation.begin.line, parseLocation.begin.column, lcol);
    }
    else
        snprintf(buf, 128, "%2d.%2d-%2d.%2d", parseLocation.begin.line, parseLocation.begin.column, parseLocation.end.line, parseLocation.end.column);
    return std::string(buf);
}



std::string TxExpressionNode::get_declared_name() const {
    return ( fieldDefNode ? fieldDefNode->get_declared_name() : "" );
}



void TxFieldDeclNode::symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorDecl ) {
    this->set_context( lexContext);
    TxDeclarationFlags flags = (isExpErrorDecl ? this->declFlags | TXD_EXPERRBLOCK : this->declFlags);

    if (field->initExpression) {
        if (flags & TXD_ABSTRACT)
            CERROR(this, "'abstract' is invalid modifier for field / method that has an initializer / body: " << field->initExpression);
    }

    // Note: TXS_STATIC is set here, and may later be changed to TXS_VIRTUAL depending on context.
    TxFieldStorage storage;
    if (this->isMethodSyntax && lexContext.outer_type()) {
        // Note: instance method storage is handled specially (technically the function pointer is a static field)

        TxLambdaExprNode* lambdaExpr = nullptr;
        if (auto initExpr = dynamic_cast<TxMaybeConversionNode*>(field->initExpression))
            lambdaExpr = dynamic_cast<TxLambdaExprNode*>(initExpr->originalExpr);

        if (!lambdaExpr && !(flags & TXD_ABSTRACT))
            CERROR(this, "Missing modifier 'abstract' for method that has no body");

        if (flags & TXD_STATIC) {
            storage = TXS_STATIC;
        }
        else {
            if (lambdaExpr)
                lambdaExpr->set_instance_method(true);
            storage = TXS_INSTANCEMETHOD;
        }
    }
    else if (dynamic_cast<TxModule*>(lexContext.scope())) {  // if in global scope
        if (flags & TXD_STATIC)
            CERROR(this, "'static' is invalid modifier for module scope field " << this->field->get_source_name());
        if (flags & TXD_FINAL)
            CERROR(this, "'final' is invalid modifier for module scope field " << this->field->get_source_name());
        if (flags & TXD_OVERRIDE)
            CERROR(this, "'override' is invalid modifier for module scope field " << this->field->get_source_name());
        if (flags & TXD_ABSTRACT)
            CERROR(this, "'abstract' is invalid modifier for module scope field " << this->field->get_source_name());
        storage = TXS_GLOBAL;
    }
    else {
        if (flags & TXD_ABSTRACT) {
            if (! (flags & TXD_STATIC))
                CERROR(this, "'abstract' fields must also be declared 'static': " << this->field->get_source_name());
            if (! (flags & (TXD_PROTECTED | TXD_PUBLIC)))
                CERROR(this, "'abstract' fields cannot be private (since private are non-virtual): " << this->field->get_source_name());
        }
        storage = (flags & TXD_STATIC) ? TXS_STATIC : TXS_INSTANCE;
    }

    this->field->symbol_declaration_pass_nonlocal_field( lexContext, this, flags, storage, TxIdentifier("") );
}

void TxFieldDeclNode::symbol_resolution_pass() {
    this->field->symbol_resolution_pass();

    if (auto type = this->field->get_type()) {
        auto storage = this->field->get_declaration()->get_storage();
        if (type->is_modifiable()) {
            if (storage == TXS_GLOBAL)
                CERROR(this, "Global fields may not be modifiable: " << field->get_source_name().c_str());
        }

        if (this->field->initExpression) {
            if (storage == TXS_INSTANCE) {
                if (! (this->field->get_declaration()->get_decl_flags() & TXD_GENBINDING))  // hackish... skips tx.Array.L
                    CWARNING(this, "Not yet supported: Inline initializer for instance fields (initialize within constructor instead): " << this->field->get_source_name());
            }
        }
        else {
            if (storage == TXS_GLOBAL || storage == TXS_STATIC) {
                if (! (this->field->get_declaration()->get_decl_flags() & TXD_GENPARAM))
                    CERROR(this, "Global/static fields must have an initializer: " << this->field->get_source_name());
            }
            else if (storage == TXS_VIRTUAL || storage == TXS_INSTANCEMETHOD) {
                if (! (this->field->get_declaration()->get_decl_flags() & TXD_ABSTRACT))
                    CERROR(this, "Non-abstract virtual fields/methods must have an initializer: " << this->field->get_source_name());
            }
            // Note: TXS_STACK is not declared via this node
            // FUTURE: ensure TXS_INSTANCE fields are initialized either here or in every constructor
        }
    }
}



void TxTypeDeclNode::symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext, bool isExpErrorDecl ) {
    this->set_context( lexContext );
    // Note: does not invoke symbol_declaration_pass() on typeParamDecls, that is delegated to typeExpression
    TxDeclarationFlags flags = (isExpErrorDecl ? this->declFlags | TXD_EXPERRBLOCK : this->declFlags);
    this->declaration = lexContext.scope()->declare_type( this->typeName, this->typeExpression, flags );
    if (! this->declaration) {
        CERROR(this, "Failed to declare type " << this->typeName);
        return;
    }
    this->LOGGER().debug("%s: Declared type %-16s: %s", this->parse_loc_string().c_str(), this->typeName.c_str(),
                         declaration->to_string().c_str());

    // The context of this node represents its outer scope.
    // The type expression's created type entity, if any, represents its inner scope.
    LexicalContext typeCtx(lexContext, this->declaration->get_symbol());

    // declare type parameters within type declaration's scope, and before rest of type expression is processed:
    if (this->typeParamDecls) {
        for (auto paramDeclNode : *this->typeParamDecls) {
            paramDeclNode->symbol_declaration_pass( typeCtx);
        }

        // if type parameters have been declared, rest of type expression has this typeCtx also as its defCtx:
        this->typeExpression->symbol_declaration_pass( lexContext, lexContext, this->declaration );
    }
    else
        this->typeExpression->symbol_declaration_pass( defContext, lexContext, this->declaration );
}


void TxTypeExpressionNode::symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext,
                                                    const TxTypeDeclaration* owningDeclaration ) {
    // The context of this node represents its outer scope.
    // The type expression's created type entity, if any, represents its inner scope.
    this->set_context( lexContext );
    this->declaration = owningDeclaration;
    LexicalContext typeCtx(lexContext, ( owningDeclaration ? owningDeclaration->get_symbol() : lexContext.scope() ) );
    this->symbol_declaration_pass_descendants( defContext, typeCtx );
}


TxGenericBinding TxTypeTypeArgumentNode::make_binding( const std::string& paramName ) {
    return TxGenericBinding::make_type_binding( paramName, this->typeExprNode );
}

TxGenericBinding TxValueTypeArgumentNode::make_binding( const std::string& paramName ) {
    return TxGenericBinding::make_value_binding( paramName, this->valueExprNode );
}


const TxType* TxIdentifiedTypeNode::define_identified_type() {
    if (auto identifiedTypeDecl = lookup_type(this->context().scope(), this->identNode->ident)) {
        if (auto identifiedType = identifiedTypeDecl->get_definer()->resolve_type()) {
            if (auto declEnt = this->get_declaration()) {
                // create empty specialization (uniquely named but identical type)
                return this->types().get_empty_specialization(declEnt, identifiedType);
            }
            return identifiedType;
        }
    }
    return nullptr;
}

const TxType* TxGenSpecializationTypeNode::define_generic_specialization_type() {
    auto baseTypeDecl = lookup_type( this->context().scope(), this->identNode->ident );
    const TxType* baseType = baseTypeDecl ? baseTypeDecl->get_definer()->resolve_type() : nullptr;
    if (! baseType) {
        CERROR(this, "Unknown type: " << this->identNode->ident << " (from " << this->context().scope() << ")");
        return nullptr;
    }
    if (baseType->type_params().size() < this->typeArgs->size()) {
        CERROR(this, "Too many generic type arguments specified for type " << identNode->ident);
        return nullptr;
    }
    auto baseTypeName = baseType->get_declaration()->get_symbol()->get_full_name();
    std::vector<TxGenericBinding> bindings;
    for (size_t i = 0; i < this->typeArgs->size(); i++) {
        bindings.push_back(this->typeArgs->at(i)->make_binding( baseType->type_params().at(i)->get_unique_name() ) );
    }
    return this->types().get_type_specialization( this, baseType, &bindings );
}



void TxArrayTypeNode::symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext ) {
    this->elementTypeNode->symbol_declaration_pass( defContext, lexContext);
// If we're to implicitly define a binding the Array.L parameter, we must also fix so that such implicit bindings
// get a corresponding type parameter declaration in the outer type entity.
//    if (! this->lengthNode) {
//        // implicit redeclaration of array length parameter L:
//        TxIdentifierNode* redeclIdent = new TxIdentifierNode(this->parseLocation, new TxIdentifier("L"));
//        TxExpressionNode* lengthExpr = new TxFieldValueNode(this->parseLocation, nullptr, redeclIdent);
//        lengthNode = new TxTypeArgumentNode(lengthExpr);
//    }
    if (this->lengthNode)
        this->lengthNode->symbol_declaration_pass( defContext, lexContext);
}


void TxDerivedTypeNode::init_implicit_types() {
    // implicit type members '$Self' and '$Super' for types with a body:
    // FUTURE: if type is immutable, the reference target type should perhaps not be modifiable?
    auto selfTypeExprN = new TxTypeExprWrapperNode(this);
    auto selfRefTypeExprN = new TxReferenceTypeNode(this->parseLocation, nullptr,
                                                    new TxModifiableTypeNode(this->parseLocation, selfTypeExprN));
    const std::string selfTypeName = "$Self";
    this->selfRefTypeNode = new TxTypeDeclNode(this->parseLocation, TXD_IMPLICIT, selfTypeName, nullptr, selfRefTypeExprN);

    TxTypeExpressionNode* superTypeExprN = new TxSuperTypeNode(this->parseLocation, new TxTypeExprWrapperNode(this));
    auto superRefTypeExprN = new TxReferenceTypeNode(this->parseLocation, nullptr,
                                                     new TxModifiableTypeNode(this->parseLocation, superTypeExprN));
    const std::string superTypeName = "$Super";
    this->superRefTypeNode = new TxTypeDeclNode(this->parseLocation, TXD_IMPLICIT, superTypeName, nullptr, superRefTypeExprN);
}


void TxModifiableTypeNode::symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext,
                                                    const TxTypeDeclaration* owningDeclaration ) {
    // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
    if (auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>(this->baseType)) {
        if (auto maybeModElem = dynamic_cast<TxMaybeModTypeNode*>(arrayBaseType->elementTypeNode->typeExprNode)) {
            // (can this spuriously add Modifiable node to predeclared modifiable type, generating error?)
            lexContext.scope()->LOGGER().debug("Implicitly declaring Array Element modifiable at %s", this->to_string().c_str());
            maybeModElem->set_modifiable( true );
        }
    }

    TxTypeExpressionNode::symbol_declaration_pass( defContext, lexContext, owningDeclaration );
}


bool TxMaybeModTypeNode::has_predefined_type() const {
    if (this->is_modifiable())
        return false;
    if (auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>(this->baseType))
        if (typeid(*arrayBaseType->elementTypeNode->typeExprNode) == typeid(TxModifiableTypeNode))
            return false;
    return this->baseType->has_predefined_type();
}

void TxMaybeModTypeNode::symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext,
                                                  const TxTypeDeclaration* owningDeclaration) {
    // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
    if (! this->is_modifiable()) {
        if (auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>(this->baseType))
            if (typeid(*arrayBaseType->elementTypeNode->typeExprNode) == typeid(TxModifiableTypeNode)) {
                lexContext.scope()->LOGGER().debug("Implicitly declaring Array modifiable at %s", this->to_string().c_str());
                this->set_modifiable( true );
            }
    }

    if (this->is_modifiable())
        TxModifiableTypeNode::symbol_declaration_pass( defContext, lexContext, owningDeclaration );
    else {
        // "pass through" entity declaration to the underlying type
        this->set_context( lexContext);
        this->baseType->symbol_declaration_pass( defContext, lexContext, owningDeclaration );
    }
}


TxAssertStmtNode::TxAssertStmtNode(const TxLocation& parseLocation, TxExpressionNode* expr)
        : TxStatementNode(parseLocation), expr(expr)  {
    auto invertedCond = new TxUnaryLogicalNotNode(expr->parseLocation, expr);

    // print assert failed message:
    /* example C assert failed message:
    txc: /home/christer/proj/workspace/proto/src/ast.cpp:515: TxAssertStmtNode::TxAssertStmtNode(const TxLocation&, TxExpressionNode*): Assertion `!this->expr' failed.
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
    auto convStrExpr = new TxReferenceToNode(pLoc, new TxElemDerefNode(pLoc, msgExpr, new TxIntegerLitNode(pLoc, 0, false)));
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



TxScopeSymbol* TxFieldValueNode::resolve_symbol() {
    TxScopeSymbol* symbol = nullptr;
    std::vector<TxScopeSymbol*> tmpPath;
    if (this->baseExpr) {
        // baseExpr may or may not refer to a type (e.g. modules don't)
        auto baseType = this->baseExpr->resolve_type();

        if (auto baseRefType = dynamic_cast<const TxReferenceType*>(baseType)) {
            // implicit dereferencing ('^') operation:
            if (auto baseRefTargetType = baseRefType->target_type()) {
                //std::cerr << "Adding implicit '^' to: " << this->baseExpr << "  six=" << six << std::endl;
                auto derefNode = new TxReferenceDerefNode(this->baseExpr->parseLocation, this->baseExpr);
                derefNode->set_context( this->baseExpr->context());  // in lieu of symbol_declaration_pass()
                derefNode->symbol_resolution_pass();
                this->baseExpr = derefNode;
                baseType = baseRefTargetType;
            }
        }

        TxScopeSymbol* vantageScope = this->context().scope();
        if (baseType) {
            // base is a value expression
            symbol = baseType->lookup_inherited_instance_member(vantageScope, this->symbolName);
        }
        else if (auto baseSymbolNode = dynamic_cast<TxFieldValueNode*>(this->baseExpr)) {
            if (auto baseSymbol = baseSymbolNode->resolve_symbol()) {
                symbol = lookup_member(vantageScope, baseSymbol, this->symbolName);
            }
        }
    }
    else {
        symbol = lookup_symbol(this->context().scope(), this->symbolName);
    }
    return symbol;
}

const TxEntityDeclaration* TxFieldValueNode::resolve_decl() {
    if (this->declaration)
        return this->declaration;
    if (auto symbol = this->resolve_symbol()) {
        if (auto entitySymbol = dynamic_cast<TxEntitySymbol*>(symbol)) {
            // if symbol can be resolved to actual field, then do so
            if (entitySymbol->field_count()) {
                if (auto fieldDecl = resolve_field_lookup(entitySymbol, this->appliedFuncArgTypes)) {
                    if (fieldDecl->get_storage() == TXS_INSTANCE || fieldDecl->get_storage() == TXS_INSTANCEMETHOD) {
                        if (!this->baseExpr) {
                            CERROR(this, "Instance member field referenced without instance base: " << this->get_full_identifier());
                            return nullptr;
                        }
                        else if (auto baseSymbolNode = dynamic_cast<TxFieldValueNode*>(this->baseExpr)) {
                            if (!baseSymbolNode->get_field()) {
                                CERROR(this, "Instance member field referenced without instance base: " << this->get_full_identifier());                                return nullptr;
                            }
                        }
                    }
                    this->declaration = fieldDecl;
                    return this->declaration;
                }
            }
            // if symbol is a type, and arguments are applied, and they match a constructor, the resolve to that constructor
            if (auto typeDecl = entitySymbol->get_type_decl()) {
                if (this->appliedFuncArgTypes) {
                    if (auto allocType = typeDecl->get_definer()->resolve_type()) {
                        if (auto constructorSymbol = allocType->get_instance_base_type()->get_instance_member("$init"))  // (constructors aren't inherited)
                            if (auto constructorDecl = resolve_field_lookup(constructorSymbol, this->appliedFuncArgTypes)) {
                                ASSERT(constructorDecl->get_decl_flags() & TXD_CONSTRUCTOR, "field named $init is not flagged as TXD_CONSTRUCTOR: " << constructorDecl->to_string());
                                //std::cerr << "resolving field to constructor: " << this << ": " << constructorDecl << std::endl;
                                this->declaration = constructorDecl;
                                return this->declaration;
                            }
                    }
                    CERROR(this, "No matching constructor signature for type symbol: " << this->get_full_identifier());
                }
                else {
                    // resolve this symbol to its type
                    this->declaration = typeDecl;
                    return this->declaration;
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

const TxType* TxFieldValueNode::define_type() {
    if (auto decl = this->resolve_decl()) {
        if (auto fieldDecl = dynamic_cast<const TxFieldDeclaration*>(decl)) {
            if (auto field = fieldDecl->get_definer()->resolve_field()) {
                this->field = field;
                return field->get_type();
            }
        }
        else
            return static_cast<const TxTypeDeclaration*>(decl)->get_definer()->resolve_type();
    }
    return nullptr;
}



const TxType* TxConstructorCalleeExprNode::define_type() {
    ASSERT(this->appliedFuncArgTypes, "appliedFuncArgTypes of TxConstructorCalleeExprNode not initialized");
    if (auto allocType = this->objectExpr->resolve_type()) {
        // find the constructor
        if (auto constructorSymbol = allocType->get_instance_base_type()->get_instance_member("$init")) {  // (constructors aren't inherited)
            if (auto constructorDecl = resolve_field_lookup(constructorSymbol, this->appliedFuncArgTypes)) {
                ASSERT(constructorDecl->get_decl_flags() & TXD_CONSTRUCTOR, "field named $init is not flagged as TXD_CONSTRUCTOR: " << constructorDecl->to_string());
                this->declaration = constructorDecl;
                if (auto constructorField = constructorDecl->get_definer()->resolve_field())
                    return constructorField->get_type();
            }
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


void TxFunctionCallNode::prepare_self_super_invocations() {
    if (auto fieldValueNode = dynamic_cast<TxFieldValueNode*>(this->callee)) {
        // handle direct constructor invocation - self() and super()
        auto identifier = fieldValueNode->get_full_identifier();
        if (identifier == "self" || identifier == "super") {
            auto objectDeref = new TxReferenceDerefNode(this->parseLocation, this->callee);
            auto constructorCallee = new TxConstructorCalleeExprNode(this->parseLocation, objectDeref);
            this->callee = constructorCallee;
            this->isSelfSuperConstructorInvocation = true;
        }
    }
}

const TxType* TxFunctionCallNode::define_type() {
    this->register_callee_signature();
    auto calleeType = this->callee->resolve_type();
    if (!calleeType)
        return nullptr;
    this->funcType = dynamic_cast<const TxFunctionType*>(calleeType);
    if (! this->funcType) {
        CERROR(this, "Callee of function call expression is not of function type: " << calleeType);
        return nullptr;
    }
    else if (auto constructorType = dynamic_cast<const TxConstructorType*>(funcType)) {
        // constructor functions return void but the constructor invocation expression yields the constructed type:
        auto objectDefiner = constructorType->get_constructed_type_decl()->get_definer();
        return objectDefiner->resolve_type();
    }
    else
        return this->funcType->returnType;
}

void TxFunctionCallNode::symbol_resolution_pass() {
    TxExpressionNode::symbol_resolution_pass();

    if (auto inlineCalleeType = dynamic_cast<const TxBuiltinDefaultConstructorType*>(this->funcType)) {
        // "inline" function call by replacing with conversion expression
        this->inlinedExpression = inlineCalleeType->get_default_init_value_expr();
    }
    else if (/*auto inlineCalleeType =*/ dynamic_cast<const TxBuiltinConversionFunctionType*>(this->funcType)) {
        // "inline" function call by replacing with conversion expression
        this->argsExprList->front()->insert_conversion( this->funcType->returnType, true );
        this->inlinedExpression = this->argsExprList->front();
    }
    else if (auto constructorType = dynamic_cast<const TxConstructorType*>(this->funcType)) {
        // inline code for stack allocation and constructor invocation
        if (! dynamic_cast<TxConstructorCalleeExprNode*>( this->callee )) {  // (prevents infinite recursion)
            this->inlinedExpression = new TxStackConstructorNode( this, constructorType->get_constructed_type_decl() );
            this->inlinedExpression->symbol_declaration_pass( this->context());
        }
        if (this->isSelfSuperConstructorInvocation) {
            if (! this->context().get_constructed())
                CERROR(this, "self() / super() constructor may only be invoked from within the type's other constructors");
            // TODO: shall only be legal as first statement within constructor body
        }
    }

    if (this->inlinedExpression) {
        this->inlinedExpression->symbol_resolution_pass();
        return;
    }

    callee->symbol_resolution_pass();

    if (auto funcType = dynamic_cast<const TxFunctionType*>(this->callee->resolve_type())) {
        // verify matching function signature:
        if (funcType->argumentTypes.size() != this->argsExprList->size()) {
            CERROR(this, "Callee of function call expression has mismatching argument count: " << funcType);
        }
        else {
            auto argExprI = this->argsExprList->begin();
            for (auto argDefType : funcType->argumentTypes) {
                // note: similar rules to assignment
                if (! argDefType->is_concrete())  // move this to lambda expression?
                    // TODO: dynamic concrete type resolution (recognize actual type in runtime when dereferencing a generic pointer)
                    CERROR(*argExprI, "Function argument is not a concrete type (size potentially unknown): " << argDefType);
                // if function arg is a reference:
                // TODO: check dataspace rules

                (*argExprI)->insert_conversion( argDefType );
                argExprI++;
            }
        }
    }

    for (auto argExpr : *this->argsExprList)
        argExpr->symbol_resolution_pass();
}
