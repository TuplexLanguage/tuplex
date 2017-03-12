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



Logger& TxNode::_LOG = Logger::get("AST");

unsigned TxNode::nextNodeId = 0;

std::string TxNode::str() const {
    auto ident = this->get_identifier();
    char buf[256];
    snprintf( buf, 256, "%-11s %4u %-24s %s", this->parse_loc_string().c_str(), this->get_node_id(), typeid(*this).name(),
              ( ident ? ident->c_str() : "" ) );
    return std::string(buf);
}

std::string TxNode::parse_loc_string() const {
    char buf[128];
    if (parseLocation.begin.line == parseLocation.end.line) {
        int lcol = (parseLocation.end.column > parseLocation.begin.column) ? parseLocation.end.column : parseLocation.end.column;
        snprintf(buf, 128, "%3d.%2d-%d", parseLocation.begin.line, parseLocation.begin.column, lcol);
    }
    else
        snprintf(buf, 128, "%3d.%2d-%d.%d", parseLocation.begin.line, parseLocation.begin.column, parseLocation.end.line, parseLocation.end.column);
    return std::string(buf);
}


void TxNode::visit_ast( AstVisitor visitor, const AstParent& parent, const std::string& role, void* context ) const {
    visitor( this, parent, role, context );
    const AstParent thisParent( parent, this );
    this->visit_descendants( visitor, thisParent, role, context );
}

void TxNode::visit_ast( AstVisitor visitor, void* context ) const {
    const AstParent parent( this );
    this->visit_ast( visitor, parent, "", context );
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
    TxTypeDeclaration* declaration = lexContext.scope()->declare_type( this->typeName->str(), this->typeExpression, flags );
    if (! declaration) {
        CERROR(this, "Failed to declare type " << this->typeName);
        return;
    }
    this->LOGGER().debug("%s: Declared type %-16s: %s", this->parse_loc_string().c_str(), this->typeName->str().c_str(),
                         declaration->str().c_str());

    // The context of this node represents its outer scope.
    // The type expression's created type entity, if any, represents its inner scope.
    LexicalContext typeCtx(lexContext, declaration->get_symbol());

    // declare type parameters within type declaration's scope, and before rest of type expression is processed:
    if (this->typeParamDecls) {
        for (auto paramDeclNode : *this->typeParamDecls) {
            paramDeclNode->symbol_declaration_pass( typeCtx);
        }

        // if type parameters have been declared, rest of type expression has this typeCtx also as its defCtx:
        this->typeExpression->symbol_declaration_pass( lexContext, lexContext, declaration );
    }
    else
        this->typeExpression->symbol_declaration_pass( defContext, lexContext, declaration );
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



const TxType* TxIdentifiedTypeNode::define_identified_type() {
    if (auto identifiedTypeDecl = lookup_type(this->context().scope(), *this->ident)) {
//        if (this->get_node_id() == 516) //(*this->ident == "S")
//            std::cerr << this << std::endl;
        if (auto identifiedType = identifiedTypeDecl->get_definer()->resolve_type()) {
            if (auto declEnt = this->get_declaration()) {
                // create empty specialization (uniquely named but identical type)
                return this->types().get_empty_specialization(declEnt, identifiedType);
            }
//            std::cerr << this << " resolves to " << identifiedType << std::endl;
            return identifiedType;
        }
    }
    return nullptr;
}

const TxType* TxGenSpecTypeNode::define_generic_specialization_type() {
    auto baseTypeDecl = lookup_type( this->context().scope(), *this->ident );
    const TxType* baseType = baseTypeDecl ? baseTypeDecl->get_definer()->resolve_type() : nullptr;
    if (! baseType) {
        CERROR(this, "Unknown type: " << this->ident << " (from " << this->context().scope() << ")");
        return nullptr;
    }
    auto tmp = std::vector<const TxTypeArgumentNode*>( this->typeArgs->size() );
    std::copy( this->typeArgs->cbegin(), this->typeArgs->cend(), tmp.begin() );
    return this->types().get_type_specialization( this, baseType, &tmp );
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

    TxTypeExpressionNode* superTypeExprN;
    if (this->baseTypes->empty())
        superTypeExprN = new TxIdentifiedTypeNode( this->parseLocation, "tx.Tuple" );
    else
        superTypeExprN = new TxTypeExprWrapperNode( this->baseTypes->at(0) );
    //TxTypeExpressionNode* superTypeExprN = new TxSuperTypeNode(this->parseLocation, new TxTypeExprWrapperNode(this));
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
            lexContext.scope()->LOGGER().debug("Implicitly declaring Array Element modifiable at %s", this->str().c_str());
            maybeModElem->set_modifiable( true );
        }
    }

    TxTypeExpressionNode::symbol_declaration_pass( defContext, lexContext, owningDeclaration );
}


void TxMaybeModTypeNode::symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext,
                                                  const TxTypeDeclaration* owningDeclaration) {
    // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
    if (! this->is_modifiable()) {
        if (auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>(this->baseType))
            if (typeid(*arrayBaseType->elementTypeNode->typeExprNode) == typeid(TxModifiableTypeNode)) {
                lexContext.scope()->LOGGER().debug("Implicitly declaring Array modifiable at %s", this->str().c_str());
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




//static bool arg_type_matches( const TxType *expectedType, const TxType* providedType ) {
//    // mimics behavior of inner_validate_wrap_convert()   FUTURE: merge code
//    if (providedType->auto_converts_to(*expectedType))
//        return true;
//    if (auto refType = dynamic_cast<const TxReferenceType*>(expectedType)) {
//        auto refTargetType = refType->target_type();
//        if (refTargetType && providedType->is_a(*refTargetType)) {
//            if (! refTargetType->is_modifiable()) {
//                // originalExpr will be auto-wrapped with a reference-to node
//                return true;
//            }
////            else {
////                if (!originalType->is_modifiable())
////                    LOGGER().debug("Cannot convert reference with non-mod-target to one with mod target: %s -> %s",
////                                   originalType->to_string().c_str(), requiredType->to_string().c_str());
////                else
////                    LOGGER().debug("Cannot implicitly convert to reference with modifiable target: %s -> %s",
////                                   originalType->to_string().c_str(), requiredType->to_string().c_str());
////            }
//        }
//    }
////    LOGGER().debug("Can't auto-convert value\n\tFrom: %80s\n\tTo:   %80s",
////                   originalType->to_string().c_str(), requiredType->to_string().c_str());
//    return false;
//}

static int get_reinterpretation_degree( const TxType *expectedType, const TxType* providedType ) {
    if (*expectedType == *providedType) {
        //std::cerr << "Types equal: " << expectedType << "   ==   " << providedType << std::endl;
        return 0;
    }

    // TODO: check if provided type is narrower than the expected type

    if (providedType->auto_converts_to(*expectedType))
        return 2;

    if (auto refType = dynamic_cast<const TxReferenceType*>( expectedType )) {
        if (auto refTargetType = refType->target_type()) {
            if (providedType->is_a( *refTargetType )) {
                if (! refTargetType->is_modifiable())
                    return 3;  // expression will be auto-wrapped with a reference-to node
            }
        }
    }

    return -1;  // does not match
}


/** Attempts to resolve an identified entity symbol, that is potentially overloaded,
 * to a specific field by matching with the provided arguments' types.
 * The closest matching, valid field is picked. If no field matched, NULL is returned.
 * If a field was matched, and implicit conversions were needed for any arguments,
 * those conversions are inserted for those arguments within this call.
 *
 * All included fields that have the matching number of arguments and compatible argument types are candidates.
 * Candidate selection is done by counting the number and degree of argument reinterpretations necessary to match it.
 * (A single 2nd degree reinterpretation is "further away" than many 1st degree reinterpretations.)
 *
 * Degrees of reinterpretation (to be thought of as degrees of "distance"):
 * 0: Argument and receiver have the exact same type
 * 1: Argument and receiver have equivalent types (according to narrowing/widening type rules)
 * 2: Argument can be implicitly converted to the receiver's type (e.g. Int -> Long)
 * 3: Argument can be transformed via implicit operation to the receiver's type (e.g. implicit referencing)
 *
 * Note: This function doesn't generate compiler errors; if no match is found null is returned.
 */
static TxFieldDeclaration* resolve_field( const TxParseOrigin& origin, TxEntitySymbol* entitySymbol,
                                          const std::vector<TxMaybeConversionNode*>* arguments ) {
    if (! arguments) {
        if (entitySymbol->field_count() == 1)
            return entitySymbol->get_first_field_decl();
        if (entitySymbol->field_count() > 1)
            entitySymbol->LOGGER().note("%s must be matched using type parameters", entitySymbol->str().c_str());
        return nullptr;
    }

    if (entitySymbol->field_count() == 0)
        return nullptr;

    // prepare vector of provided arguments' original types:
    std::vector<const TxType*> argTypes( arguments->size() );
    std::transform( arguments->cbegin(), arguments->cend(), argTypes.begin(),
                    []( TxMaybeConversionNode* n ) -> const TxType*  {  return n->resolve_type();  } );
    // should we check for a null-resolved arg here?

    TxFieldDeclaration* closestDecl = nullptr;
    uint64_t closestReint = UINT64_MAX;

    for (auto fieldCandidateI = entitySymbol->fields_cbegin();
              fieldCandidateI != entitySymbol->fields_cend(); fieldCandidateI++) {
        if (auto field = (*fieldCandidateI)->get_definer()->resolve_field()) {

            // first screen the fields that are of function type and take the correct number of arguments:
            if (auto funcType = dynamic_cast<const TxFunctionType*>( field->get_type() )) {
                if (funcType->argumentTypes.size() == arguments->size()) {
                    //entitySymbol->LOGGER().trace("Candidate function: %s", funcType->to_string().c_str());

                    // next check that the argument types match, and how close they match:
                    uint16_t reint[4] = { 0, 0, 0, 0 };
                    auto argTypeI = argTypes.cbegin();
                    for (auto argDef : funcType->argumentTypes) {
                        auto argType = *argTypeI;
                        int degree = get_reinterpretation_degree( argDef, argType );
                        if (degree < 0) {
                            //entitySymbol->LOGGER().trace("Argument mismatch, can't convert\n\tFrom: %80s\n\tTo:   %80s",
                            //                             argType->to_string(true).c_str(), argDef->to_string(true).c_str());
                            goto NEXT_CANDIDATE;
                        }
                        reint[ degree ]++;
                        argTypeI++;
                    }

                    entitySymbol->LOGGER().debug("Arguments match for %s: %-32s: %d, %d, %d, %d", field->str().c_str(), funcType->str().c_str(),
                                                 reint[0], reint[1], reint[2], reint[3] );
                    uint64_t candReint = ( ((uint64_t)reint[3])<<48 | ((uint64_t)reint[2])<<32 | ((uint64_t)reint[1])<<16 | reint[0] );
                    if (candReint <= closestReint) {
                        if (candReint == closestReint) {
                            // Note, multiple functions with the exact same signature is checked elsewhere.
                            // If arguments for multiple "equal" top candidates are matched via reinterpretation, we just pick the first one found.
                            // TODO: Pick the narrowest match, not the first found match
                            //CWARNING(origin, "Ambiguous function call to " << entitySymbol->get_full_name() << ": "
                            //         << field->get_type() << ", multiple signatures match equally well "
                            //         << "[ " << reint[0] << ", " << reint[1] << ", " << reint[2] << ", " << reint[3] << " ]");
                        }
                        else {
                            closestDecl = *fieldCandidateI;
                            closestReint = candReint;
                        }
                    }
                }
            }
        }

        NEXT_CANDIDATE:
        ;
    }

    if (closestDecl) {
//        // apply implicit reinterpretations on arguments:
//        auto funcType = static_cast<const TxFunctionType*>( closestDecl->get_definer()->get_type() );
//        auto argExprI = arguments->begin();
//        for (auto argDefType : funcType->argumentTypes) {
//            (*argExprI)->insert_conversion( argDefType );  // FIXME
//            argExprI++;
//        }

        return closestDecl;
    }

    entitySymbol->LOGGER().debug("Arguments do not match any overloaded candidate of %s", entitySymbol->str().c_str());
    return nullptr;
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
            symbol = baseType->lookup_inherited_instance_member(vantageScope, this->symbolName->str());
        }
        else if (auto baseSymbolNode = dynamic_cast<TxFieldValueNode*>(this->baseExpr)) {
            if (auto baseSymbol = baseSymbolNode->resolve_symbol()) {
                symbol = lookup_member(vantageScope, baseSymbol, *this->symbolName);
            }
        }
    }
    else {
        symbol = lookup_symbol(this->context().scope(), *this->symbolName);
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
                if (auto fieldDecl = resolve_field( *this, entitySymbol, this->appliedFuncArgs )) {
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
                if (this->appliedFuncArgs) {
                    if (auto allocType = typeDecl->get_definer()->resolve_type()) {
                        if (auto constructorSymbol = allocType->get_instance_base_type()->get_instance_member("$init"))  // (constructors aren't inherited)
                            if (auto constructorDecl = resolve_field( *this, constructorSymbol, this->appliedFuncArgs )) {
                                ASSERT(constructorDecl->get_decl_flags() & TXD_CONSTRUCTOR, "field named $init is not flagged as TXD_CONSTRUCTOR: " << constructorDecl->str());
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
    ASSERT(this->appliedFuncArgs, "appliedFuncArgTypes of TxConstructorCalleeExprNode not initialized");
    if (auto allocType = this->objectExpr->resolve_type()) {
        // find the constructor
        if (auto constructorSymbol = allocType->get_instance_base_type()->get_instance_member("$init")) {  // (constructors aren't inherited)
            if (auto constructorDecl = resolve_field( *this, constructorSymbol, this->appliedFuncArgs)) {
                ASSERT(constructorDecl->get_decl_flags() & TXD_CONSTRUCTOR, "field named $init is not flagged as TXD_CONSTRUCTOR: " << constructorDecl->str());
                this->declaration = constructorDecl;
                if (auto constructorField = constructorDecl->get_definer()->resolve_field())
                    return constructorField->get_type();
            }
        }
        if (this->appliedFuncArgs->size() == 0) {
            // TODO: support default value constructor
        }
        else if (this->appliedFuncArgs->size() == 1) {
            // TODO: support default assignment constructor
        }
        CERROR(this, "No matching constructor for type " << allocType);
    }
    return nullptr;
}


TxFunctionCallNode::TxFunctionCallNode(const TxLocation& parseLocation, TxExpressionNode* callee, const std::vector<TxExpressionNode*>* argsExprList)
        : TxExpressionNode(parseLocation), callee(callee), origArgsExprList(argsExprList), argsExprList( make_args_vec( argsExprList ) ) {
    if (auto fieldValueNode = dynamic_cast<TxFieldValueNode*>(this->callee)) {
        // handle direct constructor invocation - self() and super()
        auto identifier = fieldValueNode->get_full_identifier();
        if (identifier == "self" || identifier == "super") {
            auto objectDeref = new TxReferenceDerefNode(this->parseLocation, this->callee);
            this->callee = new TxConstructorCalleeExprNode(this->parseLocation, objectDeref);
            this->isSelfSuperConstructorInvocation = true;
        }
    }
}

void TxFunctionCallNode::symbol_declaration_pass( LexicalContext& lexContext) {
    this->set_context( lexContext);
    this->callee->symbol_declaration_pass( lexContext);
    for (auto argExpr : *this->argsExprList) {
        argExpr->symbol_declaration_pass( lexContext);
    }

    if (this->isSelfSuperConstructorInvocation) {
        if (! this->context().get_constructed())
            CERROR(this, "self() / super() constructor may only be invoked from within the type's other constructors");
        // TODO: shall only be legal as first statement within constructor body
    }
}

const TxType* TxFunctionCallNode::define_type() {
    // The resolution here shall resolve to the function signature that *closest* matches the argument types,
    // but also takes automatic (implicit) type conversions into account (if needed).
    // The automatic type conversions thus considered shall then be applied upon function invocation.
    // Prepare for resolving possible function overloading by registering actual function signature with
    // the callee node, BEFORE the callee node type is resolved:
    ASSERT (!callee->get_applied_func_args(), "callee already has applied func arg types: " << callee);
    callee->set_applied_func_args( this->argsExprList );
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

    if (auto constructorType = dynamic_cast<const TxConstructorType*>(this->funcType)) {
        // Stack construction syntactically looks like a function call, e.g. Int(42)
        // If the callee is a constructor, we substitute this function call with a stack construction expression:
        if (! dynamic_cast<TxConstructorCalleeExprNode*>( this->callee )) {  // (prevents infinite recursion)
            auto typeDeclNode = new TxTypeDeclWrapperNode( this->parseLocation, constructorType->get_constructed_type_decl() );
            this->inlinedExpression = new TxStackConstructionNode( this->parseLocation, typeDeclNode, this->origArgsExprList );
            this->inlinedExpression->symbol_declaration_pass( this->context());
            this->inlinedExpression->symbol_resolution_pass();
            return;
        }
    }

    // Verify matching function signature, and apply implicit conversions if needed:
    if (this->funcType) {
        if (this->funcType->argumentTypes.size() != this->argsExprList->size()) {
            CERROR(this, "Callee of function call expression has mismatching argument count: " << this->funcType);
        }
        else {
            auto argExprI = this->argsExprList->begin();
            for (auto argDefType : this->funcType->argumentTypes) {
                // note: similar rules to assignment
                // TODO: check dataspace rules if function arg is a reference
                (*argExprI)->insert_conversion( argDefType );  // generates compilation error upon mismatch
                argExprI++;
            }
        }
    }

    if (auto inlineCalleeType = dynamic_cast<const TxInlineFunctionType*>(this->funcType)) {
        this->inlinedExpression = inlineCalleeType->make_inline_expr( this->callee, this->argsExprList );
    }

    if (this->inlinedExpression) {
        this->inlinedExpression->symbol_resolution_pass();
        return;
    }

    callee->symbol_resolution_pass();
    for (auto argExpr : *this->argsExprList)
        argExpr->symbol_resolution_pass();
}
