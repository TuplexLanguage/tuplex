#pragma once


class TxSpecializationPass {
public:
    LexicalContext lexContext;
    LexicalContext defContext;
    const TxEntityDeclaration* declaration = nullptr;
    const TxType* type = nullptr;
    const TxField* field = nullptr;
    bool isResolving = false;  // during development - guard against recursive resolution
    bool hasResolved = false;  // to prevent multiple identical error messages
    TxExpressionNode* inlinedExpression = nullptr;  // substitutes the function/constructor call if non-null

    inline const TxTypeDeclaration*  get_type_decl()  const { return static_cast<const TxTypeDeclaration* >(this->declaration); }
    inline const TxFieldDeclaration* get_field_decl() const { return static_cast<const TxFieldDeclaration*>(this->declaration); }

//    inline const TxType*  get_type()  const { return static_cast<const TxType* >(this->entity); }
//    inline const TxField* get_field() const { return static_cast<const TxField*>(this->entity); }

    TxSpecializationPass() : lexContext(), defContext()  { }
};

//class TxNode;
//class TxTypeExpressionNode;
//class TxExpressionNode;
//class TxFieldDefNode;
//class TxAssigneeNode;
//class TxTypeArgumentNode;
//class TxPredefinedTypeNode;
//class TxFunctionTypeNode;
//class TxFieldValueNode;
//class TxReferenceDerefNode;
//class TxBinaryOperatorNode;
//class TxFunctionCallNode;
//class TxConstructorCalleeExprNode;
//class TxMakeObjectNode;


//class TxNode : public virtual TxParseOrigin, public Printable {
//    LexicalContext lexContext;

//class TxTypeExpressionNode : public TxNode, public TxTypeDefiner {
//    bool gettingType = false;  // during development - guard against recursive calls to get_type()
//    bool gottenType = false;  // to prevent multiple identical error messages
//    TxType const * cachedType = nullptr;
//    TxTypeDeclaration* typeDeclaration = nullptr;  // null unless initialized in symbol declaration pass

//class TxExpressionNode : public TxNode, public TxTypeDefiner {
//    bool gettingType = false;  // during development - guard against recursive calls to get_type()
//    bool gottenType = false;  // to prevent multiple identical error messages
//    TxType const * cachedType = nullptr;
//    std::vector<const TxType*>* appliedFuncArgTypes = nullptr; // injected by expression context if applicable

//class TxFieldDefNode : public TxNode, public TxFieldDefiner {
//    TxField const * cachedField = nullptr;
//    TxType const * cachedType = nullptr;
//    TxFieldDeclaration* fieldDeclaration = nullptr;  // null until initialized in symbol declaration pass

//class TxAssigneeNode : public TxNode {
//    bool resolved = false;
//    TxType const * cachedType = nullptr;

//class TxTypeArgumentNode : public TxNode {
//    LexicalContext defContext;
//    TxTypeDeclNode* typeDeclNode;
//    TxFieldDeclNode* fieldDeclNode;
//    bool bound = false;  // during development: checks invocation order

//class TxPredefinedTypeNode : public TxTypeExpressionNode {
//    LexicalContext defContext;

//class TxFunctionTypeNode : public TxTypeExpressionNode {
//    /** If non-null, this function is a constructor and this is the object type that is being constructed. */
//    TxTypeDeclaration* constructedObjTypeDecl = nullptr;


//class TxFieldValueNode : public TxExpressionNode {
//    TxScopeSymbol* cachedSymbol = nullptr;
//    const TxField* cachedField = nullptr;
//    bool hasRunResolve = false;

//class TxReferenceDerefNode : public TxExpressionNode {
    /** internal "cache" to prevent multiple code generations */
    //mutable llvm::Value* refExprValue = nullptr;

//class TxBinaryOperatorNode : public TxOperatorValueNode {
//    bool reference_operands = false;

//class TxFunctionCallNode : public TxExpressionNode {
//    TxExpressionNode* inlinedExpression = nullptr;  // substitutes the function call if non-null

//class TxConstructorCalleeExprNode : public TxExpressionNode {
//    /** The constructor method's declaration */
//    const TxFieldDeclaration* constructorDecl = nullptr;
    //mutable llvm::Value* objectPtrV = nullptr;

//class TxMakeObjectNode : public TxExpressionNode {
//    TxExpressionNode* inlinedInitializer = nullptr;  // substitutes the constructor call if non-null
