#pragma once

#include <typeinfo>
#include <vector>

#include "util/assert.hpp"
#include "util/logging.hpp"

#include "context.hpp"
#include "tx_error.hpp"
#include "tx_operations.hpp"
#include "identifier.hpp"
#include "driver.hpp"
#include "location.hpp"

#include "symbol/package.hpp"
#include "symbol/module.hpp"
#include "symbol/entity.hpp"


/* forward declarations pertaining to LLVM code generation */
class LlvmGenerationContext;
class GenScope;
namespace llvm {
    class Value;
}


/** Helper function that makes a deep-copy of a vector of nodes. */
template<class N>
std::vector<N*>* make_node_vec_copy( const std::vector<N*>* nodeVec ) {
    if (! nodeVec)
        return nullptr;
    std::vector<N*>* copyVec = new std::vector<N*>( nodeVec->size() );
    std::transform( nodeVec->cbegin(), nodeVec->cend(), copyVec->begin(),
                    []( N* n ) -> N*  {  return n->make_ast_copy();  } );
    return copyVec;
}


/** Represents a value that can be statically computed (in compile time). */
class TxConstantProxy : public TxTypeProxy {
public:
    virtual ~TxConstantProxy() = default;

    /** Gets the TxType instance representing the type of the constant. */
    virtual const TxType* get_type() const override = 0;

    virtual uint32_t get_value_UInt() const = 0;

    virtual llvm::Constant* code_gen(LlvmGenerationContext& context, GenScope* scope) const = 0;

    virtual bool operator==(const TxConstantProxy& other) const;

    inline virtual bool operator!=(const TxConstantProxy& other) const final {
        return ! this->operator==(other);
    }
};



class TxNode : public virtual TxParseOrigin, public Printable {
    static Logger& LOG;
    static unsigned nextNodeId;

    const unsigned nodeId;
    //TxNode const * const originalNode;

    LexicalContext lexContext;

protected:
    TxNode(const TxLocation& parseLocation) : nodeId(nextNodeId++), lexContext(), parseLocation(parseLocation) { }

    virtual ~TxNode() = default;

    void set_context(LexicalContext&& context) {
        ASSERT(!this->is_context_set(), "lexicalContext already initialized in " << this->to_string());
        this->lexContext = context;
    }

    inline TypeRegistry& types() { return this->lexContext.package()->types(); }

public:
    const TxLocation parseLocation;

    virtual const TxLocation& get_parse_location() const override final {
        return this->parseLocation;
    }

    virtual TxDriver* get_driver() const override final {
        return this->parseLocation.parserCtx->get_driver();
    }

    inline unsigned get_node_id() const { return this->nodeId; }

//    /** Returns nullptr if this node is the original copy (constructed from first lexical source parse). */
//    inline const TxNode* original_node() const { return this->originalNode; }

    /** Creates a copy of this node and all its descendants for purpose of generic specialization. */
    virtual TxNode* make_ast_copy() const = 0;


    inline bool is_context_set() const { return this->lexContext.scope(); }

    void set_context(const LexicalContext& context) {
        ASSERT(!this->is_context_set(), "lexicalContext already initialized in " << this->to_string());
        this->lexContext = context;
    }

    inline const LexicalContext& context() const {
        ASSERT(this->is_context_set(), "lexicalContext not initialized in " << this->to_string());
        return this->lexContext;
    }
    inline LexicalContext& context() {
        return const_cast<LexicalContext&>(static_cast<const TxNode *>(this)->context());
    }


    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const = 0;


    virtual std::string to_string() const override;

    std::string parse_loc_string() const;


    inline Logger& LOGGER() const { return this->LOG; }
};



//class TxSpecializableNode : public TxNode {
//    TxSpecializationPass firstSpec;
//    std::vector<TxSpecializationPass*> specializations;
//
//protected:
//    inline const TxSpecializationPass* get_spec(TxSpecializationIndex six) const {
//        ASSERT(six < this->specializations.size(), "Non-existant specialization index " << six << " in " << this);
//        return this->specializations.at(six);
//    }
//    inline TxSpecializationPass* get_spec(TxSpecializationIndex six) {
//        while (six >= this->specializations.size()) {
//            //std::cerr << "EXTENDING SPEC VEC TO six " << six << " OF " << this << std::endl;
//            this->specializations.push_back(new TxSpecializationPass());
//        }
//        return this->specializations.at(six);
//    }
//
//
//    inline TypeRegistry& types(TxSpecializationIndex six) { return this->context().package()->types(); }
//
//public:
//    TxSpecializableNode(const TxLocation& parseLocation) : TxNode(parseLocation), specializations({ &this->firstSpec }) { }
//
//    TxSpecializationIndex next_spec_index() const {
//        return this->specializations.size();
//    }
//
//    inline bool is_context_set(TxSpecializationIndex six) const {
//        return this->specializations.size() > six && this->specializations.at(six)->lexContext.scope();
//    }
//
//    inline void set_context(TxSpecializationIndex six, const LexicalContext& context) {
//        ASSERT(!this->is_context_set(six), "lexicalContext already initialized for s-ix " << six << " in " << this->to_string());
//        this->get_spec(six)->lexContext = context;
//        //std::cerr << "Set context for s-ix " << six << " in " << this->to_string() << std::endl;
//    }
//
//    inline const LexicalContext& context(TxSpecializationIndex six) const {
//        ASSERT(this->is_context_set(six), "lexicalContext not initialized for s-ix " << six << " in " << this->to_string());
//        return this->get_spec(six)->lexContext;
//    }
//    inline LexicalContext& context(TxSpecializationIndex six) {
//        return const_cast<LexicalContext&>(static_cast<const TxSpecializableNode *>(this)->context());
//    }
//
//    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const = 0;
//};



bool validateTypeName (TxNode* node, TxDeclarationFlags declFlags, const std::string& name);
bool validateFieldName(TxNode* node, TxDeclarationFlags declFlags, const std::string& name);


class TxIdentifierNode : public TxNode {
public:
    const TxIdentifier ident;

    TxIdentifierNode(const TxLocation& parseLocation, const TxIdentifier* ident)
        : TxNode(parseLocation), ident(*ident)  { }

    TxIdentifierNode(const TxLocation& parseLocation, const TxIdentifier& ident)
        : TxNode(parseLocation), ident(ident)  { }

    virtual TxIdentifierNode* make_ast_copy() const override { return new TxIdentifierNode(this->parseLocation, this->ident); }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }

    virtual std::string to_string() const {
        return TxNode::to_string() + " '" + this->ident.to_string() + "'";
    }
};



class TxImportNode : public TxNode {
public:
    const TxIdentifierNode* identNode;

    TxImportNode(const TxLocation& parseLocation, const TxIdentifierNode* identifier)
        : TxNode(parseLocation), identNode(identifier)  { }

    virtual TxImportNode* make_ast_copy() const override { return new TxImportNode(this->parseLocation, this->identNode->make_ast_copy()); }

    virtual void symbol_declaration_pass(TxModule* module) {
        this->set_context(LexicalContext(module));
        if (! identNode->ident.is_qualified())
            CERROR(this, "can't import unqualified identifier '" << identNode->ident << "'");
        module->register_import(identNode->ident);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }
};


class TxDeclarationNode : public TxNode {  // either type or field
protected:
    const TxDeclarationFlags declFlags;

public:
    TxDeclarationNode(const TxLocation& parseLocation, const TxDeclarationFlags declFlags)
        : TxNode(parseLocation), declFlags(declFlags) { }

    virtual TxDeclarationNode* make_ast_copy() const override = 0;

    inline TxDeclarationFlags get_decl_flags() const { return this->declFlags; }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) = 0;

    virtual void symbol_resolution_pass() = 0;

    virtual const TxEntityDeclaration* get_declaration() const = 0;
};


class TxModuleNode : public TxNode {
    const TxIdentifierNode* identNode;
    std::vector<TxImportNode*>* imports;
    std::vector<TxDeclarationNode*>* members;
    std::vector<TxModuleNode*>* subModules;
    TxModule* module = nullptr;

public:
    TxModuleNode(const TxLocation& parseLocation, const TxIdentifierNode* identifier,
                 std::vector<TxImportNode*>* imports, std::vector<TxDeclarationNode*>* members,
                 std::vector<TxModuleNode*>* subModules)
        : TxNode(parseLocation), identNode(identifier), imports(imports), members(members), subModules(subModules)  {
        ASSERT(identifier, "NULL identifier");  // (sanity check on parser)
    }

    virtual TxModuleNode* make_ast_copy() const override {
        return new TxModuleNode( this->parseLocation, this->identNode->make_ast_copy(),
                                 make_node_vec_copy( imports ),
                                 make_node_vec_copy( members ),
                                 make_node_vec_copy( subModules ) );
    }

    virtual void symbol_declaration_pass(TxModule* parent) {
        this->module = parent->declare_module(this->identNode->ident);

        if (this->imports) {
            for (auto imp : *this->imports)
                imp->symbol_declaration_pass( this->module );
        }
        if (this->members) {
            LexicalContext subCtx(this->module);
            for (auto mem : *this->members)
                mem->symbol_declaration_pass( subCtx );
        }
        if (this->subModules) {
            for (auto mod : *this->subModules)
                mod->symbol_declaration_pass( this->module );
        }
    }

    virtual void symbol_resolution_pass() {
        if (this->members) {
            for (auto mem : *this->members)
                mem->symbol_resolution_pass();
        }
        if (this->subModules) {
            for (auto mod : *this->subModules)
                mod->symbol_resolution_pass();
        }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


/** Represents a parsing unit, i.e. a given source text input (e.g. source file). */
class TxParsingUnitNode : public TxNode {
    TxModuleNode* module;
public:
    TxParsingUnitNode(const TxLocation& parseLocation, TxModuleNode* module)
        : TxNode( parseLocation ), module( module )  { }

    virtual TxDeclarationNode* make_ast_copy() const override {
        ASSERT(false, "Can't make AST copy of TxParsingUnitNode " << this);
        return nullptr;
    }

    virtual void symbol_declaration_pass(TxPackage* package) {
        this->module->symbol_declaration_pass(package);
    }

    virtual void symbol_resolution_pass() {
        this->module->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



class TxStatementNode : public TxNode {
public:
    TxStatementNode(const TxLocation& parseLocation) : TxNode(parseLocation) { }

    virtual TxStatementNode* make_ast_copy() const override = 0;

    virtual void symbol_declaration_pass( LexicalContext& lexContext) = 0;
    virtual void symbol_resolution_pass() = 0;

    /** Returns true if this statement / compound statement *may* end with a break or continue statement. */
    virtual bool may_end_with_non_return_stmt() const { return false; }

    /** Returns true if this statement / compound statement always ends with an explicit terminal statement
     * (return, break, continue).
     * This means that any successor statement in the same suite will never be reached. */
    virtual bool ends_with_terminal_stmt() const { return false; }

    /** Returns true if this statement / compound statement always ends with an explicit return statement. */
    virtual bool ends_with_return_stmt() const { return false; }
};



class TxTypeDefiningNode : public TxNode, public TxTypeDefiner {
    const TxType* type = nullptr;
    bool startedRslv = false;  // during development - guard against recursive resolution
    bool hasResolved = false;  // to prevent multiple identical error messages

protected:
    /** Defines the type of this expression (as specific as can be known), constructing/obtaining the TxType instance.
     * The implementation should only traverse the minimum nodes needed to define the type
     * (e.g. not require the actual target type of a reference to be defined).
     * This should only be invoked once, from the TxTypeDefiningNode class. */
    virtual const TxType* define_type() = 0;

public:
    TxTypeDefiningNode(const TxLocation& parseLocation) : TxNode(parseLocation) { }

    virtual TxTypeDefiningNode* make_ast_copy() const override = 0;

    virtual TxTypeDefiningNode* get_node() override { return this; }

    /** Returns the type (as specific as can be known) of the value this expression produces. */
    virtual const TxType* resolve_type() override final {
        if (!this->type && !this->hasResolved) {
            LOGGER().trace("resolving type of %s", this->to_string().c_str());
            ASSERT(!this->startedRslv, "Recursive invocation of resolve_type() of " << this);
            this->startedRslv = true;
            this->type = this->define_type();
            this->hasResolved = true;
        }
        return this->type;
    }

    virtual const TxType* attempt_get_type() const override final { return this->type; }
    virtual const TxType* get_type        () const override final {
        ASSERT(this->hasResolved, "entity definer not resolved: " << this); return this->type; }
};


class TxFieldDefiningNode : public TxNode, public TxFieldDefiner {
    const TxType* type = nullptr;
    const TxField* field = nullptr;
    bool startedRslv = false;  // during development - guard against recursive resolution
    bool hasResolved = false;  // to prevent multiple identical error messages

protected:
    /** Defines the type of this field (as specific as can be known), constructing/obtaining the TxType instance.
     * The implementation should only traverse the minimum nodes needed to define the type
     * (e.g. not require the actual target type of a reference to be defined).
     * This should only be invoked once, from the TxFieldDefiningNode class. */
    virtual const TxType* define_type() = 0;

    /** Defines the field of this node, constructing/obtaining the TxField instance.
     * This should only be invoked once, from the TxFieldDefiningNode class. */
    virtual const TxField* define_field() = 0;

public:
    TxFieldDefiningNode(const TxLocation& parseLocation) : TxNode(parseLocation) { }

    virtual TxFieldDefiningNode* make_ast_copy() const override = 0;

    /** Resolves the type and returns the field entity of this field-defining node. */
    virtual const TxField* resolve_field() override final {
        if (!this->field && !this->hasResolved) {
            LOGGER().trace("resolving field of %s", this->to_string().c_str());
            ASSERT(!this->startedRslv, "Recursive invocation of resolve_field() of " << this);
            this->startedRslv = true;
            this->type = this->define_type();
            if (this->type)
                this->field = this->define_field();
            this->hasResolved = true;
        }
        return this->field;
    }

    /** Returns the type (as specific as can be known) of the value this field-defining node produces. */
    virtual const TxType* resolve_type() override final {
        this->resolve_field();
        return this->type;
    }

    virtual const TxType*  attempt_get_type() const override final { return this->type; }
    virtual const TxType*  get_type        () const override final {
        ASSERT(this->hasResolved, "entity definer not resolved: " << this); return this->type; }
    virtual const TxField* get_field       () const override final {
        ASSERT(this->hasResolved, "entity definer not resolved: " << this); return this->field; }
//    virtual const TxExpressionNode* get_init_expression() const = 0;
};


/**
 * The context of this node refers to its outer scope. This node's entity, if any, refers to its inner scope.
 */
class TxTypeExpressionNode : public TxTypeDefiningNode {
    const TxTypeDeclaration* declaration = nullptr;
    /** if parent node is a type declaration that declares type parameters, these will be set by it */
    const std::vector<TxDeclarationNode*>* typeParamDeclNodes = nullptr;

protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext,
                                                      LexicalContext& lexContext, TxDeclarationFlags declFlags ) = 0;

public:
    TxTypeExpressionNode(const TxLocation& parseLocation) : TxTypeDefiningNode(parseLocation) { }

    virtual TxTypeExpressionNode* make_ast_copy() const override = 0;

    /** Returns true if this type expression is a directly identified type
     * (i.e. a previously declared type, does not construct a new type). */
    virtual bool has_predefined_type() const { return false; }

    /** Gets the type declaration of this type expression, if any. */
    inline const TxTypeDeclaration* get_declaration() const { return this->declaration; }

    /** Performs the symbol declaration pass for this type expression.
     * Type expressions evaluate within a "definition context", representing their "outer" scope,
     * and a "lexical context", within which they declare their constituent sub-expressions.
     * The definition context is used for named types lookups, to avoid conflation with names of the sub-expressions.
     */
    virtual void symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext,
                                          TxDeclarationFlags declFlags, const std::string designatedTypeName,
                                          const std::vector<TxDeclarationNode*>* typeParamDeclNodes );

    virtual void symbol_resolution_pass() {
        if (typeParamDeclNodes) {
            for (auto paramDeclNode : *typeParamDeclNodes) {
                paramDeclNode->symbol_resolution_pass();
            }
        }
        this->resolve_type();
    }
};

/** Wraps a TxTypeDefiner or a TxTypeExpressionNode within another TxTypeExpressionNode.
 * If a TxTypeDefiner is wrapped, the same type will be resolved regardless of specialization index.
 * If a TxTypeDefiningNode is wrapped, the declaration and resolution pass calls won't be forwarded,
 * allowing the wrapped node to be added as a TxTypeExpressionNode child to additional parent nodes. */
class TxTypeExprWrapperNode : public TxTypeExpressionNode {
    TxTypeDefiner* const typeDefiner;
    TxTypeDefiningNode* const typeDefNode;
protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext,
                                                      TxDeclarationFlags declFlags ) override { }

    virtual const TxType* define_type() override {
        auto type = ( typeDefiner ? this->typeDefiner->resolve_type() : this->typeDefNode->resolve_type() );
        if (!type)
            return nullptr;
        else if (auto declEnt = this->get_declaration()) {
            if (! type->is_modifiable())
                // create empty specialization (uniquely named but identical type)
                return this->types().get_empty_specialization(declEnt, type);
        }
        return type;
    }

public:
    TxTypeExprWrapperNode(const TxLocation& parseLocation, TxTypeDefiner* typeDefiner)
        : TxTypeExpressionNode(parseLocation), typeDefiner(typeDefiner), typeDefNode()  { }

    TxTypeExprWrapperNode(TxTypeDefiningNode* typeDefNode)
        : TxTypeExpressionNode(typeDefNode->parseLocation), typeDefiner(), typeDefNode(typeDefNode)  { }

    virtual TxTypeExprWrapperNode* make_ast_copy() const override {
        if (this->typeDefiner)
            return new TxTypeExprWrapperNode( this->parseLocation, this->typeDefiner );
        else
            return new TxTypeExprWrapperNode( this->typeDefNode );
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }
};



class TxFieldDefNode;

class TxExpressionNode : public TxTypeDefiningNode {
protected:
    /** injected by field definition if known and applicable */
    const TxFieldDefNode* fieldDefNode = nullptr;

    /** injected by outer expression if applicable */
    std::vector<const TxType*>* appliedFuncArgTypes = nullptr;

public:
    TxExpressionNode(const TxLocation& parseLocation) : TxTypeDefiningNode(parseLocation) { }

    virtual TxExpressionNode* make_ast_copy() const override = 0;

    /** Injected by field definition if known and applicable. */
    virtual void set_field_def_node(const TxFieldDefNode* fieldDefNode) {
        this->fieldDefNode = fieldDefNode;
    }

    /** Returns true if this value expression is of a directly identified type
     * (i.e. does not construct a new type), e.g. value literals and directly identified fields. */
    virtual bool has_predefined_type() const = 0;

    virtual void symbol_declaration_pass( LexicalContext& lexContext) = 0;

    virtual void symbol_resolution_pass() {
        this->resolve_type();
    }

    /** Returns true if this expression is a stack allocation expression,
     * i.e. its result is in newly allocated stack space, and the allocation's type is the type of this expression.
     * Note that sub-expressions may perform allocations without this expression being an allocation. */
    // TODO: review combinatorial expressions that maybe should return true if any of their sub-expressions return true
    virtual bool is_stack_allocation_expression() const { return false; }

    /** Returns true if this expression is a constant expression that can be evaluated at compile time. */
    virtual bool is_statically_constant() const { return false; }

    /** If this expression can currently be statically evaluated,
     * a TxConstantProxy representing its value is returned, otherwise nullptr.
     * In future, this should return non-null for all expressions for which is_statically_constant() returns true.
     */
    virtual const TxConstantProxy* get_static_constant_proxy() const {
        return nullptr;
    }

    virtual std::vector<const TxType*>* get_applied_func_arg_types() {
        return this->appliedFuncArgTypes;
    }
    virtual void set_applied_func_arg_types( std::vector<const TxType*>* appliedFuncArgTypes ) {
        this->appliedFuncArgTypes = appliedFuncArgTypes;
    }

    /** Generates code that produces the type id (as opposed to the value) of this expression. */
    virtual llvm::Value* code_gen_typeid(LlvmGenerationContext& context, GenScope* scope) const;
};


/** A conversion placeholder node which can wrap a specific conversion around an expression if necessary. */
class TxMaybeConversionNode : public TxExpressionNode {
    TxExpressionNode* conversionExpr = nullptr;

protected:
    virtual const TxType* define_type() override {
        auto expr = this->get_spec_expression();
        return expr->resolve_type();
    }

public:
    TxExpressionNode* const originalExpr;

    TxMaybeConversionNode(TxExpressionNode* originalExpr)
            : TxExpressionNode(originalExpr->parseLocation), originalExpr(originalExpr) {
        ASSERT( originalExpr, "NULL originalExpr" );
        ASSERT( !dynamic_cast<TxMaybeConversionNode*>( originalExpr ),
                "Can't wrap a TxMaybeConversionNode with another TxMaybeConversionNode: " << originalExpr );
    }

    virtual TxMaybeConversionNode* make_ast_copy() const override {
        ASSERT(false, "Can't make AST copy of a TxMaybeConversionNode: " << this);
        return nullptr;
    }

    void insert_conversion( const TxType* resultType, bool _explicit=false );

    inline TxExpressionNode* get_spec_expression() const {
        return (this->conversionExpr ? this->conversionExpr : this->originalExpr);
    }

    virtual bool has_predefined_type() const override { return false; }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
        auto expr = this->get_spec_expression();
        if (! expr->is_context_set())
            expr->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        auto expr = this->get_spec_expression();
        expr->symbol_resolution_pass();
    }

    virtual bool is_statically_constant() const override {
        return this->get_spec_expression()->is_statically_constant();
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override {
        return this->get_spec_expression()->get_static_constant_proxy();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};


///** Wraps a TxExpressionNode within another node which will not forward declaration and resolution pass calls.
// * This allows the wrapped node to be added as a child to additional parent nodes / specializations. */
//// FUTURE: Perhaps make a TxValueDefiner instead, analogous to TxTypeDefiner
//class TxExprWrapperNode : public TxExpressionNode {
//    TxExpressionNode* const expr;
//    TxSpecializationIndex const six;
//protected:
//    virtual const TxType* define_type() override {
//        return this->expr->resolve_type();
//    }
//
//public:
//    TxExprWrapperNode(TxExpressionNode* expr, TxSpecializationIndex six)
//        : TxExpressionNode(expr->parseLocation), expr(expr), six(six)  { }
//
//    inline TxExpressionNode* get_wrapped() const { return this->expr; }
//    inline TxSpecializationIndex get_six() const { return this->six; }
//
//    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
//        this->set_context( lexContext); }
//
//    virtual bool is_statically_constant() const { return this->expr->is_statically_constant(); }
//    virtual bool has_predefined_type() const override { return this->expr->has_predefined_type(); }
//    virtual const TxConstantProxy* get_static_constant_proxy() const override { return this->expr->get_static_constant_proxy(); }
//
//    virtual std::vector<const TxType*>* get_applied_func_arg_types(TxSpecializationIndex six) {
//        return this->expr->get_applied_func_arg_types(this->six); }
//    virtual void set_applied_func_arg_types(TxSpecializationIndex six, std::vector<const TxType*>* appliedFuncArgTypes) {
//        this->expr->set_applied_func_arg_types(this->six, appliedFuncArgTypes); }
//
//    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override {
//        return this->expr->code_gen(context, scope); }
//    virtual llvm::Value* code_gen_typeid(LlvmGenerationContext& context, GenScope* scope) const override {
//        return this->expr->code_gen_typeid(context, scope); }
//};


class TxFieldDefNode : public TxFieldDefiningNode {
    const std::string fieldName;  // the original source field name
    std::string declName = "";  // the declared field name

    const TxFieldDeclaration* declaration = nullptr;

    void symbol_declaration_pass( LexicalContext& outerContext, LexicalContext& innerContext, TxDeclarationFlags declFlags) {
        this->set_context( outerContext);
        auto typeDeclFlags = (declFlags & (TXD_PUBLIC | TXD_PROTECTED)) | TXD_IMPLICIT;
        if (this->typeExpression) {
            // unless the type expression is a directly named type, declare implicit type entity for this field's type:
            std::string implTypeName = ( this->typeExpression->has_predefined_type() ? "" : this->get_decl_field_name() + "$type" );
            this->typeExpression->symbol_declaration_pass( innerContext, innerContext, typeDeclFlags, implTypeName, nullptr);
        }
        if (this->initExpression) {
// TODO: delegate this to the expression nodes
//            if (!this->typeExpression && !this->initExpression->has_predefined_type()) {
//                // declare implicit type entity for this field's type:
//                TxTypeEntity* typeEntity = lexContext.scope()->declare_type(implTypeName, this->typeExpression, typeDeclFlags);
//                if (!typeEntity)
//                    CERROR(this, "Failed to declare implicit type %s for field %s", implTypeName.c_str(), this->fieldName.c_str());
//            }
            this->initExpression->symbol_declaration_pass( outerContext);
        }
    };

protected:
    virtual const TxType* define_type() override {
        LOGGER().trace("resolving type of %s", this->to_string().c_str());
        const TxType* type;
        if (this->typeExpression) {
            type = this->typeExpression->resolve_type();
        }
//        else if (this->typeDefiner) {
//            type = this->typeDefiner->resolve_type();
//        }
        else {
            type = this->initExpression->resolve_type();
            if (type) {
                if (this->modifiable) {
                    if (! type->is_modifiable())
                        type = this->types().get_modifiable_type(nullptr, type);
                }
                else if (type->is_modifiable())
                    // if initialization expression is modifiable type, and modifiable not explicitly specified,
                    // lose modifiable attribute (modifiability must be explicit)
                    type = type->get_semantic_base_type();
            }
        }
        if (! type)
            CERROR(this, "No type defined for field");
        return type;
    }

    virtual const TxField* define_field() override {
        LOGGER().trace("resolving field of %s", this->to_string().c_str());
        ASSERT(this->attempt_get_type(), "Expected non-NULL type in " << this);
        if (this->declaration)
            return new TxField(this->declaration, this->attempt_get_type());
        // else is not an error - function type's arguments & return type lack field declarations
        return nullptr;
    }

public:
    const bool modifiable;  // true if field name explicitly declared modifiable
    TxTypeExpressionNode* typeExpression;
    TxMaybeConversionNode* initExpression;

    TxFieldDefNode(const TxLocation& parseLocation, const std::string& fieldName,
                   TxTypeExpressionNode* typeExpression, TxExpressionNode* initExpression, bool modifiable=false)
            : TxFieldDefiningNode(parseLocation), fieldName(fieldName), modifiable(modifiable) /*, typeDefiner()*/ {
        this->typeExpression = typeExpression;
        if (initExpression) {
            initExpression->set_field_def_node(this);
            this->initExpression = new TxMaybeConversionNode(initExpression);
        }
        else {
            ASSERT(typeExpression, "At least one of typeExpression and initExpression must be specified");
            this->initExpression = nullptr;
        }
    }

    virtual TxFieldDefNode* make_ast_copy() const override {
        TxTypeExpressionNode* typeExpr = ( this->typeExpression ? this->typeExpression->make_ast_copy() : nullptr );
        TxExpressionNode*     initExpr = ( this->initExpression ? this->initExpression->originalExpr->make_ast_copy() : nullptr );
        return new TxFieldDefNode( this->parseLocation, this->fieldName, typeExpr, initExpr, this->modifiable );
    }

    void symbol_declaration_pass_local_field( LexicalContext& lexContext, bool create_local_scope, TxDeclarationFlags declFlags=TXD_NONE ) {
        LexicalContext outerCtx(lexContext);  // prevents init expr from referring to this field
        if (create_local_scope)
            lexContext.scope(lexContext.scope()->create_code_block_scope());
        this->declName = this->fieldName;
        TxDeclarationFlags tmpFlags = declFlags | lexContext.decl_flags();
        this->declaration = lexContext.scope()->declare_field(this->declName, this, tmpFlags, TXS_STACK, TxIdentifier(""));
        this->symbol_declaration_pass( outerCtx, lexContext, declFlags);
    }

    void symbol_declaration_pass_nonlocal_field( LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                                 TxFieldStorage storage, const TxIdentifier& dataspace ) {
        TxDeclarationFlags tmpFlags = declFlags | lexContext.decl_flags();
        if (this->fieldName != "self")
            this->declName = this->fieldName;
        else {
            // handle constructor declaration
            this->declName = "$init";
            tmpFlags = tmpFlags | TXD_CONSTRUCTOR;
        }

        this->declaration = lexContext.scope()->declare_field(this->declName, this, tmpFlags, storage, dataspace);
        this->symbol_declaration_pass( lexContext, lexContext, declFlags);
    }

    void symbol_declaration_pass_functype_arg( LexicalContext& lexContext ) {
        this->symbol_declaration_pass( lexContext, lexContext, TXD_NONE);
    }

    virtual void symbol_resolution_pass() {
        this->resolve_field();
        if (this->typeExpression) {
            this->typeExpression->symbol_resolution_pass();
        }
        auto field = this->get_field();
        if (this->initExpression) {
            if (this->typeExpression && field)
                this->initExpression->insert_conversion( field->get_type() );
            this->initExpression->symbol_resolution_pass();
            if (field && field->is_statically_constant())
                    if (! this->initExpression->is_statically_constant())
                        CERROR(this, "Non-constant initializer for constant global/static field.");
        }

        if (field) {
            if (! field->get_type()->is_concrete())
                CERROR(this, "Field type is not concrete (size potentially unknown): " << field->get_type());
            if (this->get_decl_field_name() == "$init") {
                if (this->get_declaration()->get_storage() != TXS_INSTANCEMETHOD)
                    CERROR(this, "Illegal declaration name for non-constructor member: " << this->fieldName);
                // TODO: check that constructor function type has void return value
            }
        }
    }

    virtual const TxExpressionNode* get_init_expression() const override {
        return this->initExpression;
    }

    /** Gets the plain name of this field as defined in the source text. */
    inline const std::string& get_source_field_name() const {
        return this->fieldName;
    }

    /** Gets the plain name of this field as actually declared in the symbol table. */
    inline const std::string& get_decl_field_name() const {
        return this->declName;
    }

    const TxFieldDeclaration* get_declaration() const {
        ASSERT(this->declaration, "field declaration not initialized for " << this->fieldName);
        return this->declaration;
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual std::string to_string() const override {
        return TxFieldDefiningNode::to_string() + " '" + this->get_source_field_name() + "'";
    }
};

/** Non-local field declaration */
class TxFieldDeclNode : public TxDeclarationNode {
    const bool isMethodSyntax = false;
public:
    TxFieldDefNode* field;

    TxFieldDeclNode(const TxLocation& parseLocation, const TxDeclarationFlags declFlags, TxFieldDefNode* field,
                    bool isMethodSyntax=false)
            : TxDeclarationNode(parseLocation, declFlags), isMethodSyntax(isMethodSyntax), field(field) { }

    virtual TxFieldDeclNode* make_ast_copy() const override {
        return new TxFieldDeclNode( this->parseLocation, this->declFlags, this->field->make_ast_copy(), this->isMethodSyntax );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override;

    virtual void symbol_resolution_pass() override;

    virtual const TxFieldDeclaration* get_declaration() const override {
        return this->field->get_declaration();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual std::string to_string() const {
        return TxDeclarationNode::to_string() + " '" + this->field->get_source_field_name() + "'";
    }
};


/** Non-local type declaration */
class TxTypeDeclNode : public TxDeclarationNode {
public:
    const std::string typeName;
    const bool interfaceKW;
    const std::vector<TxDeclarationNode*>* typeParamDecls;
    TxTypeExpressionNode* typeExpression;

    TxTypeDeclNode(const TxLocation& parseLocation, const TxDeclarationFlags declFlags,
                   const std::string typeName, const std::vector<TxDeclarationNode*>* typeParamDecls,
                   TxTypeExpressionNode* typeExpression, bool interfaceKW=false)
        : TxDeclarationNode(parseLocation, declFlags),
          typeName(typeName), interfaceKW(interfaceKW), typeParamDecls(typeParamDecls), typeExpression(typeExpression) {
        validateTypeName(this, declFlags, typeName);
    }

    virtual TxTypeDeclNode* make_ast_copy() const override {
        return new TxTypeDeclNode( this->parseLocation, this->declFlags, this->typeName,
                                   make_node_vec_copy( this->typeParamDecls ),
                                   this->typeExpression->make_ast_copy(), this->interfaceKW);
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->symbol_declaration_pass( lexContext, lexContext);
    }
    virtual void symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext);

    virtual void symbol_resolution_pass() override {
        this->typeExpression->symbol_resolution_pass();
        if (this->typeParamDecls)
            for (auto paramDecl : *this->typeParamDecls)
                paramDecl->symbol_resolution_pass();
    }

    virtual const TxTypeDeclaration* get_declaration() const override {
        return this->typeExpression->get_declaration();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual std::string to_string() const {
        return TxDeclarationNode::to_string() + " '" + this->typeName + "'";
    }
};


class TxAssigneeNode : public TxTypeDefiningNode {
public:
    TxAssigneeNode(const TxLocation& parseLocation) : TxTypeDefiningNode(parseLocation) { }

    virtual TxAssigneeNode* make_ast_copy() const override = 0;

    virtual void symbol_declaration_pass( LexicalContext& lexContext) = 0;

    virtual void symbol_resolution_pass() {
        this->resolve_type();
    }
};



class TxExpErrDeclNode : public TxDeclarationNode {
    const int expected_error_count;
    const int prev_encountered_errors;
    int encountered_error_count;
public:
    TxDeclarationNode* body;

    TxExpErrDeclNode(const TxLocation& parseLocation, int expected_error_count, int prev_encountered_errors, TxDeclarationNode* body)
        : TxDeclarationNode(parseLocation, (body ? body->get_decl_flags() : TXD_NONE) | TXD_EXPERRBLOCK),
          expected_error_count(expected_error_count), prev_encountered_errors(prev_encountered_errors),
          encountered_error_count(prev_encountered_errors), body(body)  {
    }

    virtual TxExpErrDeclNode* make_ast_copy() const override {
        return new TxExpErrDeclNode( this->parseLocation, this->expected_error_count, this->prev_encountered_errors, this->body->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        LexicalContext experrBlockContext(lexContext, lexContext.scope(), true);
        this->set_context( experrBlockContext);
        if (this->body) {
            if (true /*this->is_original_node()*/) {
                experrBlockContext.package()->driver().begin_exp_err(this->parseLocation);
                this->body->symbol_declaration_pass( experrBlockContext);
                this->encountered_error_count += experrBlockContext.package()->driver().end_exp_err(this->parseLocation);
            }
            else
                this->body->symbol_declaration_pass( experrBlockContext);
        }
    }

    virtual void symbol_resolution_pass() override {
        auto ctx = this->context();
        if (true /*this->is_original_node()*/) {
            ctx.package()->driver().begin_exp_err(this->parseLocation);
            if (this->body)
                this->body->symbol_resolution_pass();
            this->encountered_error_count += ctx.package()->driver().end_exp_err(this->parseLocation);
            if ( this->expected_error_count <  0 ) {
                if ( this->encountered_error_count == 0 )
                    CERROR(this, "COMPILER TEST FAIL: Expected one or more compilation errors but encountered " << this->encountered_error_count);
            }
            else if ( this->expected_error_count != this->encountered_error_count )
                CERROR(this, "COMPILER TEST FAIL: Expected " << this->expected_error_count
                              << " compilation errors but encountered " << this->encountered_error_count);
        }
        else if (this->body)
            this->body->symbol_resolution_pass();
    }

    virtual const TxEntityDeclaration* get_declaration() const override {
        return body->get_declaration();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }
};
