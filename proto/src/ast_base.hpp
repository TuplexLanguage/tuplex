#pragma once

#include <typeinfo>
#include <vector>

#include "assert.hpp"
#include "context.hpp"
#include "logging.hpp"
#include "tx_error.hpp"

#include "tx_operations.hpp"
#include "identifier.hpp"
#include "package.hpp"
#include "module.hpp"
#include "entity.hpp"
#include "driver.hpp"
#include "location.hh"


/* forward declarations pertaining to LLVM code generation */
class LlvmGenerationContext;
class GenScope;
namespace llvm {
    class Value;
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

protected:
    TxNode(const yy::location& parseLocation) : nodeId(nextNodeId++), parseLocation(parseLocation) { }

    virtual ~TxNode() = default;

public:
    const yy::location parseLocation;

    virtual const yy::location& get_parse_location() const override {
        return this->parseLocation;
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const = 0;

    virtual std::string to_string() const override;

    std::string parse_loc_string() const;

    unsigned get_node_id() const { return this->nodeId; }

    inline Logger& LOGGER() const { return this->LOG; }
};


class TxNonSpecializableNode : public TxNode {
    LexicalContext lexContext;

protected:
    void set_context(const LexicalContext& context) {
        ASSERT(!this->is_context_set(), "lexicalContext already initialized in " << this->to_string());
        this->lexContext = context;
    }
    /** Sets the lexical context of this node to the current context of the module. */
    void set_context(TxScopeSymbol* lexContext) {
        this->set_context(LexicalContext(lexContext));
    }

public:
    TxNonSpecializableNode(const yy::location& parseLocation) : TxNode(parseLocation), lexContext() { }

    inline bool is_context_set() const { return this->lexContext.scope(); }

    inline const LexicalContext& context() const {
        ASSERT(this->is_context_set(), "lexicalContext not initialized in " << this->to_string());
        return this->lexContext;
    }
    inline LexicalContext& context() {
        return const_cast<LexicalContext&>(static_cast<const TxNonSpecializableNode *>(this)->context());
    }

    virtual TxDriver* get_driver() const override {
        return ( this->is_context_set() ? &this->context().package()->driver() : nullptr );
    }
};


class TxSpecializableNode : public TxNode {
    std::vector<TxSpecializationPass> specializations;

protected:
    inline const TxSpecializationPass& get_spec(TxSpecializationIndex six) const {
        ASSERT(six < this->specializations.size(), "Non-existant specialization index " << six << " in " << this);
        return this->specializations.at(six);
    }
    inline TxSpecializationPass& get_spec(TxSpecializationIndex six) {
        while (six >= this->specializations.size())
            this->specializations.emplace_back();
        return this->specializations.at(six);
    }


    inline TypeRegistry& types(TxSpecializationIndex six=0) { return this->context(six).package()->types(); }

public:
    TxSpecializableNode(const yy::location& parseLocation) : TxNode(parseLocation), specializations(1) { }

    TxSpecializationIndex next_spec_index() const {
        return this->specializations.size();
    }

    inline bool is_context_set(TxSpecializationIndex six) const {
        return this->specializations.size() > six && this->specializations.at(six).lexContext.scope();
    }

    inline void set_context(TxSpecializationIndex six, const LexicalContext& context) {
        ASSERT(!this->is_context_set(six), "lexicalContext already initialized for s-ix " << six << " in " << this->to_string());
        this->get_spec(six).lexContext = context;
        //std::cerr << "Set context for s-ix " << six << " in " << this->to_string() << std::endl;
    }

    inline const LexicalContext& context(TxSpecializationIndex six) const {
        ASSERT(this->is_context_set(six), "lexicalContext not initialized for s-ix " << six << " in " << this->to_string());
        return this->get_spec(six).lexContext;
    }
    inline LexicalContext& context(TxSpecializationIndex six) {
        return const_cast<LexicalContext&>(static_cast<const TxSpecializableNode *>(this)->context(six));
    }

    virtual TxDriver* get_driver() const override {
        return ( this->is_context_set(0) ? &this->context(0).package()->driver() : nullptr );
    }
};



bool validateTypeName (TxSpecializableNode* node, TxDeclarationFlags declFlags, const std::string& name);
bool validateFieldName(TxSpecializableNode* node, TxDeclarationFlags declFlags, const std::string& name);


class TxIdentifierNode : public TxNonSpecializableNode {
public:
    enum IdentifierClass { UNSPECIFIED, MODULE_ID, IMPORT_ID, TYPE_ID, FIELD_ID, DATASPACE_ID };
    const IdentifierClass idClass;
    const TxIdentifier ident;

    TxIdentifierNode(const yy::location& parseLocation, const TxIdentifier* ident, IdentifierClass identifierClass=UNSPECIFIED)
        : TxNonSpecializableNode(parseLocation), idClass(identifierClass), ident(*ident)  { }

    TxIdentifierNode(const yy::location& parseLocation, const TxIdentifier& ident)
        : TxNonSpecializableNode(parseLocation), idClass(UNSPECIFIED), ident(ident)  { }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }

    virtual std::string to_string() const {
        return TxNonSpecializableNode::to_string() + " '" + this->ident.to_string() + "'";
    }
};



class TxImportNode : public TxNonSpecializableNode {
public:
    const TxIdentifierNode* identNode;

    TxImportNode(const yy::location& parseLocation, const TxIdentifierNode* identifier)
        : TxNonSpecializableNode(parseLocation), identNode(identifier)  { }

    virtual void symbol_declaration_pass(TxModule* module) {
        this->set_context(module);
        if (! identNode->ident.is_qualified())
            CERROR(this, "can't import unqualified identifier '" << identNode->ident << "'");
        module->register_import(identNode->ident);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) {
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }
};


class TxDeclarationNode : public TxSpecializableNode {  // either type or field
public:
    const TxDeclarationFlags declFlags;

    TxDeclarationNode(const yy::location& parseLocation, const TxDeclarationFlags declFlags)
        : TxSpecializableNode(parseLocation), declFlags(declFlags) { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) = 0;

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) = 0;
};


class TxModuleNode : public TxNonSpecializableNode {
    const TxIdentifierNode* identNode;
    std::vector<TxImportNode*>* imports;
    std::vector<TxDeclarationNode*>* members;
    std::vector<TxModuleNode*>* subModules;
    TxModule* module;  // set in symbol table pass
public:
    TxModuleNode(const yy::location& parseLocation, const TxIdentifierNode* identifier,
                 std::vector<TxImportNode*>* imports, std::vector<TxDeclarationNode*>* members,
                 std::vector<TxModuleNode*>* subModules)
        : TxNonSpecializableNode(parseLocation), identNode(identifier), imports(imports), members(members),
          subModules(subModules), module()  {
        ASSERT(identifier, "NULL identifier");  // (sanity check on parser)
    }

    virtual void symbol_declaration_pass(TxModule* parent) {
        this->set_context(parent);
        this->module = parent->declare_module(identNode->ident);
        if (this->imports) {
            for (auto elem : *this->imports)
                elem->symbol_declaration_pass(this->module);
        }
        if (this->members) {
            LexicalContext lexContext(this->module);
            for (auto elem : *this->members)
                elem->symbol_declaration_pass(0, lexContext);
        }
        if (this->subModules) {
            for (auto mod : *this->subModules)
                mod->symbol_declaration_pass(this->module);
        }
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) {
        if (this->imports) {
            for (auto elem : *this->imports)
                elem->symbol_resolution_pass(resCtx);
        }
        if (this->members) {
            for (auto elem : *this->members)
                elem->symbol_resolution_pass(0, resCtx);
        }
        if (this->subModules) {
            for (auto mod : *this->subModules)
                mod->symbol_resolution_pass(resCtx);
        }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


/** Represents a parsing unit, i.e. a given source text input (e.g. source file). */
class TxParsingUnitNode : public TxNonSpecializableNode {
    std::vector<TxModuleNode*> modules;
public:

    TxParsingUnitNode(const yy::location& parseLocation) : TxNonSpecializableNode(parseLocation) { }

    void add_module(TxModuleNode* module) {
        this->modules.push_back(module);
    }

    virtual void symbol_declaration_pass(TxPackage* package) {
        for (auto mod : this->modules)
            mod->symbol_declaration_pass(package);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) {
        for (auto mod : this->modules)
            mod->symbol_resolution_pass(resCtx);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



class TxStatementNode : public TxSpecializableNode {
public:
    TxStatementNode(const yy::location& parseLocation) : TxSpecializableNode(parseLocation) { }
    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) = 0;
    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) = 0;
};



class TxTypeDefiningNode : public TxSpecializableNode, public TxSpecializableTypeDefiner {
    std::vector<TxSpecializationTypeDefiner*> specializationDefs;

protected:
    TxSpecializationTypeDefiner* get_spec_type_def(TxSpecializationIndex six) {
        for (unsigned s = this->specializationDefs.size(); s <= six; s++)
            this->specializationDefs.push_back(new TxSpecializationTypeDefiner(this, s));
        return this->specializationDefs.at(six);
    }

    /** Defines the type of this expression (as specific as can be known), constructing/obtaining the TxType instance.
     * The implementation should only traverse the minimum nodes needed to define the type
     * (e.g. not require the actual target type of a reference to be defined).
     * This should only be invoked once, from the TxTypeDefiningNode class. */
    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) = 0;

public:
    TxTypeDefiningNode(const yy::location& parseLocation) : TxSpecializableNode(parseLocation) { }

    inline TxTypeDefiner* get_type_definer(TxSpecializationIndex six) { return this->get_spec_type_def(six); }

    /** Returns the type (as specific as can be known) of the value this expression produces. */
    virtual const TxType* resolve_type(TxSpecializationIndex six, ResolutionContext& resCtx) override final {
        auto & spec = this->get_spec(six);
        if (!spec.type && !spec.hasResolved) {
            LOGGER().trace("resolving type of %s (s-ix %u)", this->to_string().c_str(), six);
            ASSERT(!spec.startedRslv, "Recursive invocation of resolve_type() of " << this);
            spec.startedRslv = true;
            spec.type = this->define_type(six, resCtx);
            spec.hasResolved = true;
            if (spec.type && !spec.type->is_prepared())
                const_cast<TxType*>(spec.type)->prepare_type_members();
        }
        return spec.type;
    }

    virtual const TxType* attempt_get_type(TxSpecializationIndex six) const override final { return this->get_spec(six).type; }
    virtual const TxType* get_type        (TxSpecializationIndex six) const override final { return this->get_spec(six).type; }
};


class TxFieldDefiningNode : public TxSpecializableNode, public TxSpecializableFieldDefiner {
    std::vector<TxSpecializationFieldDefiner*> specializationDefs;

protected:
    TxSpecializationFieldDefiner* get_spec_field_def(TxSpecializationIndex six) {
        for (unsigned s = this->specializationDefs.size(); s <= six; s++)
            this->specializationDefs.push_back(new TxSpecializationFieldDefiner(this, s));
        return this->specializationDefs.at(six);
    }

    /** Defines the field of this node, constructing/obtaining the TxField instance.
     * This should only be invoked once, from the TxFieldDefiningNode class. */
    virtual const TxField* define_field(TxSpecializationIndex six, ResolutionContext& resCtx) = 0;

public:
    TxFieldDefiningNode(const yy::location& parseLocation) : TxSpecializableNode(parseLocation) { }

    TxFieldDefiner* get_field_definer(TxSpecializationIndex six) { return this->get_spec_field_def(six); }

    /** Returns the type (as specific as can be known) of the value this expression produces. */
    virtual const TxField* resolve_field(TxSpecializationIndex six, ResolutionContext& resCtx) override final {
        auto & spec = this->get_spec(six);
        if (!spec.field && !spec.hasResolved) {
            LOGGER().trace("resolving field of %s (s-ix %u)", this->to_string().c_str(), six);
            ASSERT(!spec.startedRslv, "Recursive invocation of resolve_field() of " << this);
            spec.startedRslv = true;
            spec.field = this->define_field(six, resCtx);
            spec.hasResolved = true;
        }
        return spec.field;
    }

    virtual const TxType* resolve_type(TxSpecializationIndex six, ResolutionContext& resCtx) override final {
        this->resolve_field(six, resCtx);
        return this->get_spec(six).type;
    }

    virtual const TxType*  attempt_get_type(TxSpecializationIndex six) const override final { return this->get_spec(six).type; }
    virtual const TxType*  get_type        (TxSpecializationIndex six) const override final { return this->get_spec(six).type; }
    virtual const TxField* get_field       (TxSpecializationIndex six) const override final { return this->get_spec(six).field; }
//    virtual const TxExpressionNode* get_init_expression() const = 0;
};


/**
 * The context of this node refers to its outer scope. This node's entity, if any, refers to its inner scope.
 */
class TxTypeExpressionNode : public TxTypeDefiningNode {
//    bool isResolving = false;  // during development - guard against recursive calls to get_type()
//    bool hasResolved = false;  // to prevent multiple identical error messages
//    TxType const * type = nullptr;
//    TxTypeDeclaration* declaration = nullptr;  // null unless initialized in symbol declaration pass

    /** if parent node is a type declaration that declares type parameters, these will be set by it */
    std::vector<const std::vector<TxDeclarationNode*>*> typeParamDeclNodes;

    void setTypeParamDeclNodes(TxSpecializationIndex six, const std::vector<TxDeclarationNode*>* typeParamDeclNodes) {
        while (six >= this->typeParamDeclNodes.size())
            this->typeParamDeclNodes.push_back(nullptr);
        this->typeParamDeclNodes[six] = typeParamDeclNodes;
    }

protected:
    inline const std::vector<TxDeclarationNode*>* getTypeParamDeclNodes(TxSpecializationIndex six) const {
        return ( six < this->typeParamDeclNodes.size() ? this->typeParamDeclNodes.at(six) : nullptr );
    }

    /** constructs a newly allocated TxTypeParam vector for this type expression's type parameter declaration nodes */
    std::vector<TxTypeParam>* makeDeclTypeParams(TxSpecializationIndex six);

    /** Gets the type declaration of this type expression, if any. */
    inline const TxTypeDeclaration* get_declaration(TxSpecializationIndex six) const {
        return dynamic_cast<const TxTypeDeclaration*>(this->get_spec(six).declaration);
    }

    virtual void symbol_declaration_pass_descendants(TxSpecializationIndex six, LexicalContext& defContext,
                                                     LexicalContext& lexContext, TxDeclarationFlags declFlags) = 0;

public:
    TxTypeExpressionNode(const yy::location& parseLocation) : TxTypeDefiningNode(parseLocation), typeParamDeclNodes(1)  { }

    /** Returns true if this type expression is a directly identified type
     * (i.e. a previously declared type, does not construct a new type). */
    virtual bool has_predefined_type() const { return false; }


    /** Performs the symbol declaration pass for this type expression.
     * Type expressions evaluate within a "definition context", representing their "outer" scope,
     * and a "lexical context", within which they declare their constituent sub-expressions.
     * The definition context is used for named types lookups, to avoid conflation with names of the sub-expressions.
     */
    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& defContext, LexicalContext& lexContext,
                                         TxDeclarationFlags declFlags, const std::string designatedTypeName,
                                         const std::vector<TxDeclarationNode*>* typeParamDeclNodes);

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) {
        this->resolve_type(six, resCtx);
    }
};

/** Wraps a TxTypeDefiner or a TxTypeExpressionNode within another TxTypeExpressionNode.
 * If a TxTypeDefiner is wrapped, the same type will be resolved regardless of specialization index.
 * If a TxTypeExpressionNode is wrapped, the declaration and resolution pass calls won't be forwarded,
 * allowing the wrapped node to be added as a child to additional parent nodes. */
class TxTypeExprWrapperNode : public TxTypeExpressionNode {
    TxTypeDefiner* const typeDefiner;
    TxTypeExpressionNode* const typeExpr;
protected:
    virtual void symbol_declaration_pass_descendants(TxSpecializationIndex six, LexicalContext& defContext,
                                                     LexicalContext& lexContext, TxDeclarationFlags declFlags) override { }

    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        auto type = ( typeDefiner ? this->typeDefiner->resolve_type(resCtx) : this->typeExpr->resolve_type(six, resCtx) );
        if (!type)
            return nullptr;
        else if (auto declEnt = this->get_declaration(six)) {
            if (! type->is_modifiable())
                // create empty specialization (uniquely named but identical type)
                return this->types(six).get_type_specialization(declEnt, TxTypeSpecialization(type));
        }
        return type;
    }

public:
    TxTypeExprWrapperNode(const yy::location& parseLocation, TxTypeDefiner* typeDefiner)
        : TxTypeExpressionNode(parseLocation), typeDefiner(typeDefiner), typeExpr()  { }

    TxTypeExprWrapperNode(TxTypeExpressionNode* typeExpr)
        : TxTypeExpressionNode(typeExpr->parseLocation), typeDefiner(), typeExpr(typeExpr)  { }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }
};



class TxFieldDefNode;

class TxExpressionNode : public TxTypeDefiningNode {
//    bool isResolving = false;  // during development - guard against recursive calls to get_type()
//    bool hasResolved = false;  // to prevent multiple identical error messages
//    TxType const * type = nullptr;
//    /** injected by expression context if applicable */
//    std::vector<const TxType*>* appliedFuncArgTypes = nullptr;

protected:
    /** injected by field definition if known and applicable */
    const TxFieldDefNode* fieldDefNode = nullptr;

public:
    TxExpressionNode(const yy::location& parseLocation) : TxTypeDefiningNode(parseLocation) { }

    TxExpressionNode* get_value_definer(TxSpecializationIndex six);

    /** Injected by field definition if known and applicable. */
    virtual void set_field_def_node(const TxFieldDefNode* fieldDefNode) {
        this->fieldDefNode = fieldDefNode;
    }

    /** Returns true if this value expression is of a directly identified type
     * (i.e. does not construct a new type), e.g. value literals and directly identified fields. */
    virtual bool has_predefined_type() const = 0;

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) = 0;

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) {
        this->resolve_type(six, resCtx);
    }

    /** Returns true if this expression is a constant expression that can be evaluated at compile time. */
    virtual bool is_statically_constant() const { return false; }

    /** If this expression can currently be statically evaluated,
     * a TxConstantProxy representing its value is returned, otherwise nullptr.
     * In future, this should return non-null for all expressions for which is_statically_constant() returns true.
     */
    virtual const TxConstantProxy* get_static_constant_proxy() const {
        //throw std::logic_error("Getting constant proxy not supported for expression node " + this->to_string());
        return nullptr;
    }

    virtual std::vector<const TxType*>* get_applied_func_arg_types(TxSpecializationIndex six) {
        return this->get_spec(six).appliedFuncArgTypes;
    }
    virtual void set_applied_func_arg_types(TxSpecializationIndex six, std::vector<const TxType*>* appliedFuncArgTypes) {
        this->get_spec(six).appliedFuncArgTypes = appliedFuncArgTypes;
    }

    /** Generates code that produces the type id (as opposed to the value) of this expression. */
    virtual llvm::Value* code_gen_typeid(LlvmGenerationContext& context, GenScope* scope) const;
};


/** Wraps a TxExpressionNode within another node which will not forward declaration and resolution pass calls.
 * This allows the wrapped node to be added as a child to additional parent nodes / specializations. */
// FUTURE: Perhaps make a TxValueDefiner instead, analogous to TxTypeDefiner
class TxExprWrapperNode : public TxExpressionNode {
    TxExpressionNode* const expr;
    TxSpecializationIndex const six;
protected:
    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        return this->expr->resolve_type(this->six, resCtx);
    }

public:
    TxExprWrapperNode(TxExpressionNode* expr, TxSpecializationIndex six)
        : TxExpressionNode(expr->parseLocation), expr(expr), six(six)  { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext); }

    virtual bool is_statically_constant() const { return this->expr->is_statically_constant(); }
    virtual bool has_predefined_type() const override { return this->expr->has_predefined_type(); }
    virtual const TxConstantProxy* get_static_constant_proxy() const override { return this->expr->get_static_constant_proxy(); }

    virtual std::vector<const TxType*>* get_applied_func_arg_types(TxSpecializationIndex six) {
        return this->expr->get_applied_func_arg_types(this->six); }
    virtual void set_applied_func_arg_types(TxSpecializationIndex six, std::vector<const TxType*>* appliedFuncArgTypes) {
        this->expr->set_applied_func_arg_types(this->six, appliedFuncArgTypes); }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override {
        return this->expr->code_gen(context, scope); }
    virtual llvm::Value* code_gen_typeid(LlvmGenerationContext& context, GenScope* scope) const override {
        return this->expr->code_gen_typeid(context, scope); }
};


/** Checks that an expression has a type that matches the required type, and wraps
 * a value & type conversion node around it if permitted and necessary.
 *
 * Assumes that originalExpr symbol registration pass has already run.
 * Will run symbol registration and symbol resolution passes on any inserted nodes.
 * Does not run the semantic pass on inserted nodes.
 */
TxExpressionNode* validate_wrap_convert(TxSpecializationIndex six, ResolutionContext& resCtx, TxExpressionNode* originalExpr,
                                        const TxType* requiredType, bool _explicit=false);

/** Checks that an rvalue expression of an assignment or argument to a funciton call
 * has a type that matches the required type,
 * and wraps a value & type conversion node around it if permitted and necessary.
 *
 * Assumes that originalExpr symbol registration pass has already run.
 * Will run symbol registration and symbol resolution passes on any inserted nodes.
 * Does not run the semantic pass on inserted nodes.
 */
TxExpressionNode* validate_wrap_assignment(TxSpecializationIndex six, ResolutionContext& resCtx, TxExpressionNode* rValueExpr,
                                           const TxType* requiredType);


class TxFieldDefNode : public TxFieldDefiningNode {
//    TxFieldDeclaration* declaration = nullptr;  // null until initialized in symbol declaration pass
//    TxType const * type = nullptr;
//    TxField const * field = nullptr;
    const std::string fieldName;  // the original source field name
    std::string declName = "";  // the declared field name

    void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& outerContext, LexicalContext& innerContext,
                                 TxDeclarationFlags declFlags) {
        this->set_context(six, outerContext);
        auto typeDeclFlags = (declFlags & (TXD_PUBLIC | TXD_PROTECTED)) | TXD_IMPLICIT;
        if (this->typeExpression) {
            // unless the type expression is a directly named type, declare implicit type entity for this field's type:
            std::string implTypeName = ( this->typeExpression->has_predefined_type() ? "" : this->get_decl_field_name() + "$type" );
            this->typeExpression->symbol_declaration_pass(six, innerContext, innerContext, typeDeclFlags, implTypeName, nullptr);
        }
        if (this->initExpression) {
// TODO: delegate this to the expression nodes
//            if (!this->typeExpression && !this->initExpression->has_predefined_type()) {
//                // declare implicit type entity for this field's type:
//                TxTypeEntity* typeEntity = lexContext.scope()->declare_type(implTypeName, this->typeExpression, typeDeclFlags);
//                if (!typeEntity)
//                    CERROR(this, "Failed to declare implicit type %s for field %s", implTypeName.c_str(), this->fieldName.c_str());
//            }
            this->initExpression->symbol_declaration_pass(six, outerContext);
        }
    };

protected:
    virtual const TxField* define_field(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        LOGGER().trace("resolving type of %s (s-ix %u)", this->to_string().c_str(), six);
        const TxType* type;
        if (this->typeExpression) {
            type = this->typeExpression->resolve_type(six, resCtx);
        }
        else if (this->typeDefiner) {
            type = this->typeDefiner->resolve_type(resCtx);
        }
        else {
            type = this->initExpression->resolve_type(six, resCtx);
            if (type) {
                if (this->modifiable) {
                    if (! type->is_modifiable())
                        type = this->types().get_modifiable_type(nullptr, type);
                }
                else if (type->is_modifiable())
                    // if initialization expression is modifiable type, and modifiable not explicitly specified,
                    // lose modifiable attribute (modifiability must be explicit)
                    type = type->get_base_type();
            }
        }

        if (type) {
            auto & spec = this->get_spec(six);
            spec.type = type;
            if (auto decl = static_cast<const TxFieldDeclaration*>(spec.declaration))
                return new TxField(decl, type);
            // else is not an error - function type's arguments & return type lack field declarations
        }
        else
            CERROR(this, "No type defined for field");
        return nullptr;
    }

public:
    const bool modifiable;  // true if field name explicitly declared modifiable
    TxTypeDefiner* typeDefiner;  // optional, non-code-generating type definer (can't be specified at same time as typeExpression)
    TxTypeExpressionNode* typeExpression;
    TxExpressionNode* initExpression;

    TxFieldDefNode(const yy::location& parseLocation, const std::string& fieldName,
                   TxTypeExpressionNode* typeExpression, TxExpressionNode* initExpression)
            : TxFieldDefiningNode(parseLocation), fieldName(fieldName), modifiable(false), typeDefiner() {
        this->typeExpression = typeExpression;
        this->initExpression = initExpression;
        if (this->initExpression)
            this->initExpression->set_field_def_node(this);
    }
    TxFieldDefNode(const yy::location& parseLocation, const std::string& fieldName,
                   TxExpressionNode* initExpression, bool modifiable=false, TxTypeDefiner* typeDefiner=nullptr)
            : TxFieldDefiningNode(parseLocation), fieldName(fieldName), modifiable(modifiable), typeDefiner(typeDefiner) {
        this->typeExpression = nullptr;
        this->initExpression = initExpression;
        if (this->initExpression)
            this->initExpression->set_field_def_node(this);
    }

    void symbol_declaration_pass_local_field(TxSpecializationIndex six, LexicalContext& lexContext, bool create_local_scope) {
        auto outerCtx = lexContext;  // prevents init expr from referring to this field
        if (create_local_scope)
            lexContext.scope(lexContext.scope()->create_code_block_scope());
        TxDeclarationFlags declFlags = TXD_NONE;
        this->declName = this->fieldName;
        this->get_spec(six).declaration = lexContext.scope()->declare_field(this->declName, this->get_spec_field_def(six),
                                                                            declFlags, TXS_STACK, TxIdentifier(""));
        this->symbol_declaration_pass(six, outerCtx, lexContext, declFlags);
    }

    void symbol_declaration_pass_nonlocal_field(TxSpecializationIndex six, LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                                TxFieldStorage storage, const TxIdentifier& dataspace) {
        if (this->fieldName != "self")
            this->declName = this->fieldName;
        else {
            // handle constructor declaration
            this->declName = "$init";
            declFlags = declFlags | TXD_CONSTRUCTOR;
        }

        this->get_spec(six).declaration = lexContext.scope()->declare_field(this->declName, this->get_spec_field_def(six),
                                                                            declFlags, storage, dataspace);
        this->symbol_declaration_pass(six, lexContext, lexContext, declFlags);
    }

    void symbol_declaration_pass_functype_arg(TxSpecializationIndex six, LexicalContext& lexContext) {
        this->symbol_declaration_pass(six, lexContext, lexContext, TXD_NONE);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) {
        this->resolve_field(six, resCtx);
        if (this->typeExpression) {
            this->typeExpression->symbol_resolution_pass(six, resCtx);
        }
        if (this->initExpression) {
            this->initExpression->symbol_resolution_pass(six, resCtx);
            if ((this->typeExpression || this->typeDefiner) && this->get_spec(six).type)
                this->initExpression = validate_wrap_convert(six, resCtx, this->initExpression, this->get_spec(six).type);
            if (this->get_spec(six).field && this->get_spec(six).field->is_statically_constant())
                    if (! this->initExpression->is_statically_constant())
                        CERROR(this, "Non-constant initializer for constant global/static field.");
        }

        if (auto type = this->get_type(six)) {
            if (! type->is_concrete())
                CERROR(this, "Field type is not concrete (size potentially unknown): " << type);
            if (this->get_decl_field_name() == "$init") {
                if (this->get_declaration(six)->get_storage() != TXS_INSTANCEMETHOD)
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

    const TxFieldDeclaration* get_declaration(TxSpecializationIndex six) const {
        ASSERT(this->get_spec(six).declaration, "field declaration not initialized for " << this->fieldName);
        return dynamic_cast<const TxFieldDeclaration*>(this->get_spec(six).declaration);
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

    TxFieldDeclNode(const yy::location& parseLocation, const TxDeclarationFlags declFlags, TxFieldDefNode* field,
                    bool isMethodSyntax=false)
            : TxDeclarationNode(parseLocation, declFlags), isMethodSyntax(isMethodSyntax), field(field) { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override;

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override;

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

    TxTypeDeclNode(const yy::location& parseLocation, const TxDeclarationFlags declFlags,
                   const std::string typeName, const std::vector<TxDeclarationNode*>* typeParamDecls,
                   TxTypeExpressionNode* typeExpression, bool interfaceKW=false)
        : TxDeclarationNode(parseLocation, declFlags),
          typeName(typeName), interfaceKW(interfaceKW), typeParamDecls(typeParamDecls), typeExpression(typeExpression) {
        validateTypeName(this, declFlags, typeName);
    }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->symbol_declaration_pass(six, lexContext, lexContext);
    }
    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& defContext, LexicalContext& lexContext);

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        this->typeExpression->symbol_resolution_pass(six, resCtx);
        if (this->typeParamDecls)
            for (auto paramDecl : *this->typeParamDecls)
                paramDecl->symbol_resolution_pass(six, resCtx);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual std::string to_string() const {
        return TxDeclarationNode::to_string() + " '" + this->typeName + "'";
    }
};


class TxAssigneeNode : public TxTypeDefiningNode {
//    bool hasResolved = false;
//    TxType const * type = nullptr;

public:
    TxAssigneeNode(const yy::location& parseLocation) : TxTypeDefiningNode(parseLocation) { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) = 0;

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) {
        this->resolve_type(six, resCtx);
    }
};
