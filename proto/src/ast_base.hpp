#pragma once

#include <typeinfo>
#include <vector>

#include "txassert.hpp"
#include "logging.hpp"

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


class TxNode : public Printable {
private:
    LexicalContext lexContext;

protected:
    void set_context(const LexicalContext& context) {
        ASSERT(!this->is_context_set(), "lexicalContext already initialized in " << this->to_string());
        this->lexContext = context;
    }
    /** Sets the lexical context of this node to the current context of the module. */
    void set_context(TxSymbolScope* lexContext) {
        this->set_context(LexicalContext(lexContext));
    }

//    inline const TypeRegistry& types() const {
//        return this->context().get_package()->types();
//    }
    inline TypeRegistry& types() const {
        return const_cast<TxPackage*>(this->context().package())->types();  // hackish... review type creation approach
    }
    inline TxDriver& driver() const {
        return this->context().package()->driver();
    }

public:
    const yy::location parseLocation;

    TxNode(const yy::location& parseLocation) : lexContext(), parseLocation(parseLocation) { }

    virtual ~TxNode() {
        if (this->is_context_set())
            LOGGER().debug("Running destructor of %s", this->to_string().c_str());
    }

    inline bool is_context_set() const { return this->lexContext.scope(); }

    /** Sets the lexical context of this node to be equal to that of the provided node. */
    void set_context(const TxNode* node) {
        this->set_context(node->context());
    }

    inline const LexicalContext& context() const {
        ASSERT(this->is_context_set(), "lexicalContext not initialized in " << this->to_string());
        return this->lexContext;
    }
    inline LexicalContext& context() {
        return const_cast<LexicalContext&>(static_cast<const TxNode *>(this)->context());
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const = 0;

    virtual std::string to_string() const;

    std::string parse_loc_string() const;

    virtual void cerror(char const *fmt, ...) const;
    virtual void cwarning(char const *fmt, ...) const;

    inline Logger& LOGGER() const {
        // FUTURE: improve
        if (this->is_context_set())
            return this->context().scope()->LOGGER();
        else
            return Logger::get("PARSER");
    }
};


bool validateTypeName(TxNode* node, TxDeclarationFlags declFlags, const std::string& name);
bool validateFieldName(TxNode* node, TxDeclarationFlags declFlags, const std::string& name);


class TxIdentifierNode : public TxNode {
public:
    enum IdentifierClass { UNSPECIFIED, MODULE_ID, IMPORT_ID, TYPE_ID, FIELD_ID, DATASPACE_ID };
    const IdentifierClass idClass;
    const TxIdentifier& ident;

    TxIdentifierNode(const yy::location& parseLocation, const TxIdentifier* ident, IdentifierClass identifierClass=UNSPECIFIED)
        : TxNode(parseLocation), idClass(identifierClass), ident(*ident)  { }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const { return nullptr; }

    virtual std::string to_string() const {
        return TxNode::to_string() + " '" + this->ident.to_string() + "'";
    }
};



class TxImportNode : public TxNode {
public:
    const TxIdentifierNode* identNode;

    TxImportNode(const yy::location& parseLocation, const TxIdentifierNode* identifier)
        : TxNode(parseLocation), identNode(identifier)  { }

    virtual void symbol_declaration_pass(TxModule* module) {
        this->set_context(module);
        if (! identNode->ident.is_qualified())
            cerror("can't import unqualified identifier '%s'", identNode->ident.to_string().c_str());
        module->register_import(identNode->ident);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) {
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const { return nullptr; }
};


class TxDeclarationNode : public TxNode {  // either type or field
public:
    const TxDeclarationFlags declFlags;

    TxDeclarationNode(const yy::location& parseLocation, const TxDeclarationFlags declFlags)
        : TxNode(parseLocation), declFlags(declFlags) { }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) = 0;

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) = 0;

    virtual void semantic_pass() = 0;
};


class TxModuleNode : public TxNode {
    const TxIdentifierNode* identNode;
    std::vector<TxImportNode*>* imports;
    std::vector<TxDeclarationNode*>* members;
    std::vector<TxModuleNode*>* subModules;
    TxModule* module;  // set in symbol table pass
public:
    TxModuleNode(const yy::location& parseLocation, const TxIdentifierNode* identifier,
                 std::vector<TxImportNode*>* imports, std::vector<TxDeclarationNode*>* members,
                 std::vector<TxModuleNode*>* subModules)
        : TxNode(parseLocation), identNode(identifier), imports(imports), members(members), subModules(subModules), module()  {
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
                elem->symbol_declaration_pass(lexContext);
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
                elem->symbol_resolution_pass(resCtx);
        }
        if (this->subModules) {
            for (auto mod : *this->subModules)
                mod->symbol_resolution_pass(resCtx);
        }
    }

    virtual void semantic_pass() {
        if (this->members)
            for (auto elem : *this->members) {
                elem->semantic_pass();
            }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};


/** Represents a parsing unit, i.e. a given source text input (e.g. source file). */
class TxParsingUnitNode : public TxNode {
    std::vector<TxModuleNode*> modules;
public:

    TxParsingUnitNode(const yy::location& parseLocation) : TxNode(parseLocation) { }

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

    virtual void semantic_pass() {
        for (auto mod : this->modules)
            mod->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};



class TxStatementNode : public TxNode {
public:
    TxStatementNode(const yy::location& parseLocation) : TxNode(parseLocation) { }
    virtual void symbol_declaration_pass(LexicalContext& lexContext) = 0;
    virtual void symbol_resolution_pass(ResolutionContext& resCtx) = 0;
    virtual void semantic_pass() = 0;
};



/**
 * The context of this node refers to its outer scope. This node's entity, if any, refers to its inner scope.
 */
class TxTypeExpressionNode : public TxNode, public TxTypeDefiner {
    bool gettingType = false;  // during development - guard against recursive calls to get_type()
    bool gottenType = false;  // to prevent multiple identical error messages
    TxType const * cachedType = nullptr;
    TxTypeEntity* declaredEntity = nullptr;  // null unless initialized in symbol declaration pass

    static const std::vector<TxTypeParam>* makeTypeParams(const std::vector<TxDeclarationNode*>* typeParamDecls);

protected:
    const std::vector<TxTypeParam>* declTypeParams = nullptr;    // null unless set in symbol declaration pass

    virtual void set_entity(TxTypeEntity* entity) {
        ASSERT(!this->declaredEntity, "declaredEntity already set in " << this);
        this->declaredEntity = entity;
    }

    /** Gets the type entity declared with this type expression, if any. */
    virtual TxTypeEntity* get_entity() const { return this->declaredEntity; }

    virtual void symbol_declaration_pass_descendants(LexicalContext& lexContext, TxDeclarationFlags declFlags) = 0;

    /** Defines the type of this type expression, constructing/obtaining the TxType instance.
     * The implementation should only traverse the minimum nodes needed to define the type
     * (e.g. not require the actual target type of a reference to be defined).
     * This should only be invoked once, from the TxTypeExpressionNode class. */
    virtual const TxType* define_type(ResolutionContext& resCtx) = 0;

public:
    TxTypeExpressionNode(const yy::location& parseLocation) : TxNode(parseLocation)  { }

    /** Returns true if this type expression is a directly identified type
     * (i.e. a previously declared type, does not construct a new type). */
    virtual bool has_predefined_type() const { return false; }


    virtual void symbol_declaration_pass(LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                         const std::string designatedTypeName = std::string(),
                                         const std::vector<TxDeclarationNode*>* typeParamDecls = nullptr);

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) {
        this->resolve_type(resCtx);
    }

    virtual const TxType* resolve_type(ResolutionContext& resCtx) override final {
        if (!cachedType && !gottenType) {
            LOGGER().trace("resolving type of %s", this->to_string().c_str());
            ASSERT(!gettingType, "Recursive invocation of resolve_type() of " << this);
            this->gettingType = true;
            this->cachedType = this->define_type(resCtx);
            this->gottenType = true;
            this->gettingType = false;
//            if (this->cachedType && this->declaredEntity)
//                ASSERT(this->cachedType->entity()==this->declaredEntity || this->declaredEntity->get_alias(),
//                        "entity " << this->cachedType->entity() << " (of type " << this->cachedType
//                        << ") is not same as declared entity " << this->declaredEntity
//                        << " (of node " << *this << ")");
        }
        return cachedType;
    }

    virtual const TxType* attempt_get_type() const override final {
        return cachedType;
    }
    inline virtual const TxType* get_type() const override final {
        ASSERT(this->is_context_set(), "Can't call get_type() before symbol table pass has completed: "  << this);
        ASSERT(cachedType, "Type not set in " << this);
        return cachedType;
    }

    virtual void semantic_pass() = 0;
    //virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};

class TxFieldDefNode;

class TxExpressionNode : public TxNode, public TxTypeDefiner {
    bool gettingType = false;  // during development - guard against recursive calls to get_type()
    bool gottenType = false;  // to prevent multiple identical error messages
    TxType const * cachedType = nullptr;
protected:
    const std::vector<const TxType*>* appliedFuncArgTypes = nullptr; // injected by expression context if applicable

    /** Defines the type of this expression (as specific as can be known), constructing/obtaining the TxType instance.
     * The implementation should only traverse the minimum nodes needed to define the type
     * (e.g. not require the actual target type of a reference to be defined).
     * This should only be invoked once, from the TxExpressionNode class. */
    virtual const TxType* define_type(ResolutionContext& resCtx) = 0;

public:
    const TxFieldDefNode* fieldDefNode = nullptr; // injected by field definition if known and applicable

    TxExpressionNode(const yy::location& parseLocation) : TxNode(parseLocation) { }

    /** Returns true if this value expression is of a directly identified type
     * (i.e. does not construct a new type), e.g. value literals and directly identified fields. */
    virtual bool has_predefined_type() const = 0;

    virtual void symbol_declaration_pass(LexicalContext& lexContext) = 0;

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) {
        this->resolve_type(resCtx);
    }

    virtual void semantic_pass() = 0;

    /** Returns the type (as specific as can be known) of the value this expression produces. */
    virtual const TxType* resolve_type(ResolutionContext& resCtx) override final {
        if (!cachedType && !gottenType) {
            LOGGER().trace("resolving type of %s", this->to_string().c_str());
            ASSERT(!gettingType, "Recursive invocation of resolve_type() of " << this);
            this->gettingType = true;
            this->cachedType = this->define_type(resCtx);
            this->gottenType = true;
            this->gettingType = false;
            //if (! this->cachedType)
            //    LOGGER().warning("%s: resolve_type() yielded NULL", this->parse_loc_string().c_str());
        }
        return cachedType;
    }

    virtual const TxType* attempt_get_type() const override final {
        return cachedType;
    }
    /** Returns the type (as specific as can be known) of the value this expression produces. */
    virtual const TxType* get_type() const override final {
        // (for now) not a strict requirement, these nodes are sometimes added dynamically (e.g. conversions):
        //ASSERT(this->is_context_set(), "Can't call get_type() before symbol table pass has completed: "  << this);
        return cachedType;
    }

    ///** Returns true if this expression represents a specific storage location (e.g. a field). */
    //virtual bool has_address() const { return false; }

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

    virtual bool hasAppliedFuncArgTypes()  { return this->appliedFuncArgTypes; }
    virtual void set_applied_func_arg_types(std::vector<const TxType*>* appliedFuncArgTypes) {
        this->appliedFuncArgTypes = appliedFuncArgTypes;
    }

    /** Generates code that produces the type id (as opposed to the value) of this expression. */
    virtual llvm::Value* code_gen_typeid(LlvmGenerationContext& context, GenScope* scope) const;
};


/** Checks that an expression has a type that matches the required type, and wraps
 * a value & type conversion node around it if permitted and necessary.
 *
 * Assumes that originalExpr symbol registration pass has already run.
 * Will run symbol registration and symbol resolution passes on any inserted nodes.
 * Does not run the semantic pass on inserted nodes.
 */
TxExpressionNode* validate_wrap_convert(ResolutionContext& resCtx, TxExpressionNode* originalExpr, const TxType* requiredType, bool _explicit=false);

/** Checks that an rvalue expression of an assignment or argument to a funciton call
 * has a type that matches the required type,
 * and wraps a value & type conversion node around it if permitted and necessary.
 *
 * Assumes that originalExpr symbol registration pass has already run.
 * Will run symbol registration and symbol resolution passes on any inserted nodes.
 * Does not run the semantic pass on inserted nodes.
 */
TxExpressionNode* validate_wrap_assignment(ResolutionContext& resCtx, TxExpressionNode* rValueExpr, const TxType* requiredType);


class TxFieldDefNode : public TxNode, public TxFieldDefiner {
    TxType const * cachedType = nullptr;

    bool modifiable;  // true if field name explicitly declared modifiable
    TxTypeDefiner* typeDefiner;  // optional, non-code-generating type definer (can't be specified at same time as typeExpression)
    TxDeclarationFlags declFlags = TXD_NONE;
    TxFieldEntity* declaredEntity;  // null until initialized in symbol registration pass

    void symbol_declaration_pass(LexicalContext& outerContext, LexicalContext& innerContext) {
        this->set_context(outerContext);
        auto typeDeclFlags = (this->declFlags & (TXD_PUBLIC | TXD_PROTECTED)) | TXD_IMPLICIT;
        if (this->typeExpression) {
            // unless the type expression is a directly named type, declare implicit type entity for this field's type:
            if (this->typeExpression->has_predefined_type())
                this->typeExpression->symbol_declaration_pass(innerContext, typeDeclFlags);
            else {
                auto implTypeName = this->fieldName + "$type";
                this->typeExpression->symbol_declaration_pass(innerContext, typeDeclFlags, implTypeName);
            }
        }
        if (this->initExpression) {
// TODO: delegate this to the expression nodes
//            if (!this->typeExpression && !this->initExpression->has_predefined_type()) {
//                // declare implicit type entity for this field's type:
//                TxTypeEntity* typeEntity = lexContext.scope()->declare_type(implTypeName, this->typeExpression, typeDeclFlags);
//                if (!typeEntity)
//                    cerror("Failed to declare implicit type %s for field %s", implTypeName.c_str(), this->fieldName.c_str());
//            }
            this->initExpression->fieldDefNode = this;
            this->initExpression->symbol_declaration_pass(outerContext);
        }
    };

public:
    const std::string fieldName;
    TxTypeExpressionNode* typeExpression;
    TxExpressionNode* initExpression;

    TxFieldDefNode(const yy::location& parseLocation, const std::string& fieldName,
                   TxTypeExpressionNode* typeExpression, TxExpressionNode* initExpression)
            : TxNode(parseLocation), modifiable(false), typeDefiner(), declaredEntity(), fieldName(fieldName) {
        validateFieldName(this, declFlags, fieldName);
        this->typeExpression = typeExpression;
        this->initExpression = initExpression;
    }
    TxFieldDefNode(const yy::location& parseLocation, const std::string& fieldName,
                   TxExpressionNode* initExpression, bool modifiable=false, TxTypeDefiner* typeDefiner=nullptr)
            : TxNode(parseLocation), modifiable(modifiable), typeDefiner(typeDefiner), declaredEntity(), fieldName(fieldName) {
        validateFieldName(this, declFlags, fieldName);
        this->typeExpression = nullptr;
        this->initExpression = initExpression;
    }

    void symbol_declaration_pass_local_field(LexicalContext& lexContext, bool create_local_scope) {
        auto outerCtx = lexContext;  // prevents init expr from referring to this field
        if (create_local_scope)
            lexContext.scope(lexContext.scope()->create_code_block_scope());
        this->declFlags = TXD_NONE;
        this->declaredEntity = lexContext.scope()->declare_field(this->fieldName, this, declFlags, TXS_STACK, TxIdentifier(""));
        this->symbol_declaration_pass(outerCtx, lexContext);
    }

    void symbol_declaration_pass_nonlocal_field(LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                                 TxFieldStorage storage, const TxIdentifier& dataspace) {
        this->declFlags = declFlags;
        this->declaredEntity = lexContext.scope()->declare_field(this->fieldName, this, declFlags, storage, dataspace);
        this->symbol_declaration_pass(lexContext, lexContext);
    }

    void symbol_declaration_pass_functype_arg(LexicalContext& lexContext) {
        this->symbol_declaration_pass(lexContext, lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) {
        this->resolve_type(resCtx);
        if (this->typeExpression) {
            this->typeExpression->symbol_resolution_pass(resCtx);
        }
        if (this->initExpression) {
            this->initExpression->symbol_resolution_pass(resCtx);
            if ((this->typeExpression || this->typeDefiner) && this->cachedType)
                this->initExpression = validate_wrap_convert(resCtx, this->initExpression, this->cachedType);
            if (this->get_entity()->is_statically_constant())
                if (! this->initExpression->is_statically_constant())
                    cerror("Non-constant initializer for constant global/static field.");
        }
        if (auto type = this->get_type())
            if (! type->is_concrete())
                cerror("Field type is not concrete (size potentially unknown): %s", type->to_string().c_str());
    }

    virtual const TxType* resolve_type(ResolutionContext& resCtx) override {
        if (! cachedType) {
            LOGGER().trace("resolving type of %s", this->to_string().c_str());
            if (this->typeExpression) {
                this->cachedType = this->typeExpression->resolve_type(resCtx);
            }
            else if (this->typeDefiner) {
                this->cachedType = this->typeDefiner->resolve_type(resCtx);
            }
            else {
                this->cachedType = this->initExpression->resolve_type(resCtx);
                if (this->cachedType) {
                    if (this->modifiable) {
                        if (! this->cachedType->is_modifiable())
                            this->cachedType = this->types().get_modifiable_type(nullptr, this->cachedType);
                    }
                    else if (this->cachedType->is_modifiable())
                        // if initialization expression is modifiable type, and modifiable not explicitly specified,
                        // lose modifiable attribute (modifiability must be explicit)
                        this->cachedType = this->cachedType->get_base_type();
                }
            }
        }
        return this->cachedType;
    }

    virtual const TxType* attempt_get_type() const override final {
        return cachedType;
    }
    virtual const TxType* get_type() const override final {
        ASSERT(this->is_context_set(), "Can't call get_type() before symbol table pass has completed: "  << this);
        return cachedType;
    }

    virtual const TxExpressionNode* get_init_expression() const override {
        return this->initExpression;
    }

    TxFieldEntity* get_entity() const {
        ASSERT(this->declaredEntity, "Declared field entity not initialized");
        return this->declaredEntity;
    }

    void semantic_pass() {
        if (this->typeExpression)
            this->typeExpression->semantic_pass();
        if (this->initExpression)
            this->initExpression->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;

    virtual std::string to_string() const {
        return TxNode::to_string() + " '" + this->fieldName + "'";
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

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override;

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        this->field->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() override {
        this->field->semantic_pass();
        if (auto type = this->field->get_type()) {
            auto storage = this->field->get_entity()->get_storage();
            if (type->is_modifiable()) {
                if (storage == TXS_GLOBAL)
                    cerror("Global fields may not be modifiable: %s", field->fieldName.c_str());
            }
            else if (! this->field->initExpression) {
                if (storage == TXS_GLOBAL || storage == TXS_STATIC)
                    if (! (this->field->get_entity()->get_decl_flags() & TXD_GENPARAM))
                        cerror("Non-modifiable field must have an initializer");
                // FUTURE: ensure TXS_INSTANCE fields are initialized either here or in every constructor
                // FUTURE: check that TXS_STACK fields are initialized before first use
            }
        }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};


/** Non-local type declaration */
class TxTypeDeclNode : public TxDeclarationNode {
public:
    const std::string typeName;
    const std::vector<TxDeclarationNode*>* typeParamDecls;
    TxTypeExpressionNode* typeExpression;

    TxTypeDeclNode(const yy::location& parseLocation, const TxDeclarationFlags declFlags,
                   const std::string typeName, const std::vector<TxDeclarationNode*>* typeParamDecls,
                   TxTypeExpressionNode* typeExpression)
        : TxDeclarationNode(parseLocation, declFlags),
          typeName(typeName), typeParamDecls(typeParamDecls), typeExpression(typeExpression) {
        validateTypeName(this, declFlags, typeName);
    }

    virtual void symbol_declaration_pass(LexicalContext& lexContext);

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) {
        this->typeExpression->symbol_resolution_pass(resCtx);
        if (this->typeParamDecls)
            for (auto paramDecl : *this->typeParamDecls)
                paramDecl->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() {
        this->typeExpression->semantic_pass();
        if (this->typeParamDecls)
            for (auto paramDecl : *this->typeParamDecls)
                paramDecl->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};


class TxAssigneeNode : public TxNode {
    bool resolved = false;
    TxType const * cachedType = nullptr;

    /** Defines/obtains the type (as specific as can be known) of the value of this assignee.
     * Must be defined by all TxAssigneeNode subclasses, but should only be invoked from TxAssigneeNode. */
    virtual const TxType* define_type(ResolutionContext& resCtx) = 0;

public:
    TxAssigneeNode(const yy::location& parseLocation) : TxNode(parseLocation) { }
    virtual void symbol_declaration_pass(LexicalContext& lexContext) = 0;

    virtual const TxType* resolve_type(ResolutionContext& resCtx) final {
        if (! resolved) {
            resolved = true;
            this->cachedType = this->define_type(resCtx);
        }
        return cachedType;
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) {
        this->resolve_type(resCtx);
    }

    virtual void semantic_pass() = 0;

    /** Gets the type of this assignee. */
    virtual const TxType* get_type() const final {
        return this->cachedType;
    }
};
