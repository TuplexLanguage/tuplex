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
#include "location.hh"

#include "symbol/package.hpp"
#include "symbol/module.hpp"
#include "symbol/entity.hpp"


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

    virtual std::string to_string() const override;

    std::string parse_loc_string() const;

    unsigned get_node_id() const { return this->nodeId; }

    virtual TxDriver* get_driver() const override {  // TODO: refactor
        return nullptr;
    }

    inline Logger& LOGGER() const { return this->LOG; }
};


/** Represents a 'semantic' node, i.e. a context-dependent interpretation of a literal node.
 */
class TxSemNode {
protected:
    LexicalContext _lexCtx;

    /** Copy constructor. */
    TxSemNode(const TxSemNode& snode) : _lexCtx(snode._lexCtx) { }

    virtual ~TxSemNode() = default;

public:
    TxSemNode(const LexicalContext& lexCtx) : _lexCtx(lexCtx) { }

    //inline TxNode* lit_node() const { return this->_litNode; }

    inline const LexicalContext& context() const {
        return this->_lexCtx;
    }
    inline LexicalContext& context() {
        return const_cast<LexicalContext&>(static_cast<const TxSemNode *>(this)->context());
    }


    virtual void symbol_resolution_pass() = 0;

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const = 0;
};


//class TxNonSpecializableNode : public TxNode {
//    LexicalContext lexContext;
//
//protected:
//    void set_context(const LexicalContext& context) {
//        ASSERT(!this->is_context_set(), "lexicalContext already initialized in " << this->to_string());
//        this->lexContext = context;
//    }
//    /** Sets the lexical context of this node to the current context of the module. */
//    void set_context(TxModule* module) {
//        this->set_context(LexicalContext(module));
//    }
//
//public:
//    TxNonSpecializableNode(const yy::location& parseLocation) : TxNode(parseLocation), lexContext() { }
//
//    inline bool is_context_set() const { return this->lexContext.scope(); }
//
//    inline const LexicalContext& context() const {
//        ASSERT(this->is_context_set(), "lexicalContext not initialized in " << this->to_string());
//        return this->lexContext;
//    }
//    inline LexicalContext& context() {
//        return const_cast<LexicalContext&>(static_cast<const TxNonSpecializableNode *>(this)->context());
//    }
//
//    virtual TxDriver* get_driver() const override {
//        return ( this->is_context_set() ? &this->context().package()->driver() : nullptr );
//    }
//};


class TxSpecializableNode : public TxNode {
    TxSpecializationPass firstSpec;
    std::vector<TxSpecializationPass*> specializations;
    TxDriver* driver = nullptr;

protected:
    inline const TxSpecializationPass* get_spec(TxSpecializationIndex six) const {
        ASSERT(six < this->specializations.size(), "Non-existant specialization index " << six << " in " << this);
        return this->specializations.at(six);
    }
    inline TxSpecializationPass* get_spec(TxSpecializationIndex six) {
        while (six >= this->specializations.size()) {
            //std::cerr << "EXTENDING SPEC VEC TO six " << six << " OF " << this << std::endl;
            this->specializations.push_back(new TxSpecializationPass());
        }
        return this->specializations.at(six);
    }


    inline TypeRegistry& types(TxSpecializationIndex six) { return this->context(six).package()->types(); }

public:
    TxSpecializableNode(const yy::location& parseLocation) : TxNode(parseLocation), specializations({ &this->firstSpec }) { }

    TxSpecializationIndex next_spec_index() const {
        return this->specializations.size();
    }

    inline bool is_context_set(TxSpecializationIndex six) const {
        return this->specializations.size() > six && this->specializations.at(six)->lexContext.scope();
    }

    inline void set_context(TxSpecializationIndex six, const LexicalContext& context) {
        ASSERT(!this->is_context_set(six), "lexicalContext already initialized for s-ix " << six << " in " << this->to_string());
        this->get_spec(six)->lexContext = context;
        this->driver = &context.package()->driver();
        //std::cerr << "Set context for s-ix " << six << " in " << this->to_string() << std::endl;
    }

    inline const LexicalContext& context(TxSpecializationIndex six) const {
        ASSERT(this->is_context_set(six), "lexicalContext not initialized for s-ix " << six << " in " << this->to_string());
        return this->get_spec(six)->lexContext;
    }
    inline LexicalContext& context(TxSpecializationIndex six) {
        return const_cast<LexicalContext&>(static_cast<const TxSpecializableNode *>(this)->context(six));
    }

    virtual TxDriver* get_driver() const override {
        return this->driver;
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const = 0;
};



bool validateTypeName (TxSpecializableNode* node, TxDeclarationFlags declFlags, const std::string& name);
bool validateFieldName(TxSpecializableNode* node, TxDeclarationFlags declFlags, const std::string& name);


class TxIdentifierNode : public TxNode {
public:
    const TxIdentifier ident;

    TxIdentifierNode(const yy::location& parseLocation, const TxIdentifier* ident)
        : TxNode(parseLocation), ident(*ident)  { }

    TxIdentifierNode(const yy::location& parseLocation, const TxIdentifier& ident)
        : TxNode(parseLocation), ident(ident)  { }

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
        //this->set_context(module);
        if (! identNode->ident.is_qualified())
            CERROR(this, "can't import unqualified identifier '" << identNode->ident << "'");
        module->register_import(identNode->ident);
    }
};


class TxDeclarationNode : public TxSpecializableNode {  // either type or field
protected:
    TxDeclarationFlags declFlags;

public:
    TxDeclarationNode(const yy::location& parseLocation, const TxDeclarationFlags declFlags)
        : TxSpecializableNode(parseLocation), declFlags(declFlags) { }

    inline TxDeclarationFlags get_decl_flags() const { return this->declFlags; }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) = 0;

    virtual void symbol_resolution_pass(TxSpecializationIndex six) = 0;

    virtual const TxEntityDeclaration* get_declaration(TxSpecializationIndex six) const = 0;
};


class TxModuleNode : public TxNode {
    const TxIdentifierNode* identNode;
    std::vector<TxImportNode*>* imports;
    std::vector<TxDeclarationNode*>* members;
    std::vector<TxModuleNode*>* subModules;

public:
    class SemNode;

    TxModuleNode(const yy::location& parseLocation, const TxIdentifierNode* identifier,
                 std::vector<TxImportNode*>* imports, std::vector<TxDeclarationNode*>* members,
                 std::vector<TxModuleNode*>* subModules)
        : TxNode(parseLocation), identNode(identifier), imports(imports), members(members), subModules(subModules)  {
        ASSERT(identifier, "NULL identifier");  // (sanity check on parser)
    }

    virtual SemNode* symbol_declaration_pass(TxModule* parent) {
        return new SemNode( parent, this );
    }

    class SemNode : public TxSemNode {
        TxModuleNode* node;
        TxModule* module;
        std::vector<TxDeclarationNode*> members;
        std::vector<TxModuleNode::SemNode*> modules;
    public:
        SemNode(TxModule* parent, TxModuleNode* node) : TxSemNode( LexicalContext(parent) ), node(node) {
            this->module = parent->declare_module(node->identNode->ident);

            if (node->imports) {
                for (auto imp : *node->imports)
                    imp->symbol_declaration_pass( this->module );
            }
            if (node->members) {
                LexicalContext subCtx(this->module);
                for (auto mem : *node->members)
                {
                    // TODO: members.push_back( mem->symbol_declaration_pass( 0, subCtx ) );
                    mem->symbol_declaration_pass( 0, subCtx );
                    members.push_back( mem );
                }
            }
            if (node->subModules) {
                for (auto mod : *node->subModules)
                    modules.push_back( mod->symbol_declaration_pass( this->module ) );
            }
        }

        virtual void symbol_resolution_pass() {
            for (auto mem : this->members)
                mem->symbol_resolution_pass(0);
            for (auto mod : this->modules)
                mod->symbol_resolution_pass();
        }

        virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
    };
};


/** Represents a parsing unit, i.e. a given source text input (e.g. source file). */
class TxParsingUnitNode : public TxNode {
    TxModuleNode* module;
public:
    class SemNode;

    TxParsingUnitNode(const yy::location& parseLocation, TxModuleNode* module)
        : TxNode( parseLocation ), module( module )  { }

    virtual SemNode* symbol_declaration_pass(TxPackage* package) {
        return new SemNode(package, this);
    }

    class SemNode : public TxSemNode {
        TxParsingUnitNode* node;
        TxModuleNode::SemNode* module;
    public:
        SemNode(TxPackage* package, TxParsingUnitNode* node) : TxSemNode( LexicalContext(package) ), node(node) {
            module = node->module->symbol_declaration_pass(package);
        }

        virtual void symbol_resolution_pass() {
            this->module->symbol_resolution_pass();
        }

        virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
    };
};



class TxStatementNode : public TxSpecializableNode {
public:
    TxStatementNode(const yy::location& parseLocation) : TxSpecializableNode(parseLocation) { }
    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) = 0;
    virtual void symbol_resolution_pass(TxSpecializationIndex six) = 0;

    /** Returns true if this statement / compound statement *may* end with a break or continue statement. */
    virtual bool may_end_with_non_return_stmt() const { return false; }

    /** Returns true if this statement / compound statement always ends with an explicit terminal statement
     * (return, break, continue).
     * This means that any successor statement in the same suite will never be reached. */
    virtual bool ends_with_terminal_stmt() const { return false; }

    /** Returns true if this statement / compound statement always ends with an explicit return statement. */
    virtual bool ends_with_return_stmt() const { return false; }
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
    virtual const TxType* define_type(TxSpecializationIndex six) = 0;

public:
    TxTypeDefiningNode(const yy::location& parseLocation) : TxSpecializableNode(parseLocation) { }

    inline TxTypeDefiner* get_type_definer(TxSpecializationIndex six) { return this->get_spec_type_def(six); }

    /** Returns the type (as specific as can be known) of the value this expression produces. */
    virtual const TxType* resolve_type(TxSpecializationIndex six) override final;

    virtual const TxType* attempt_get_type(TxSpecializationIndex six) const override final { return this->get_spec(six)->type; }
    virtual const TxType* get_type        (TxSpecializationIndex six) const override final {
        auto spec = this->get_spec(six); ASSERT(spec->hasResolved, "entity definer not resolved: " << this); return spec->type; }
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
    virtual const TxField* define_field(TxSpecializationIndex six) = 0;

public:
    TxFieldDefiningNode(const yy::location& parseLocation) : TxSpecializableNode(parseLocation) { }

    TxFieldDefiner* get_field_definer(TxSpecializationIndex six) { return this->get_spec_field_def(six); }

    /** Resolves the type and returns the field entity of this field-defining node. */
    virtual const TxField* resolve_field(TxSpecializationIndex six) override final {
        auto spec = this->get_spec(six);
        if (!spec->field && !spec->hasResolved) {
            LOGGER().trace("resolving field of %s (s-ix %u)", this->to_string().c_str(), six);
            ASSERT(!spec->startedRslv, "Recursive invocation of resolve_field() of " << this);
            spec->startedRslv = true;
            spec->field = this->define_field(six);
            spec->hasResolved = true;
        }
        return spec->field;
    }

    /** Returns the type (as specific as can be known) of the value this field-defining node produces. */
    virtual const TxType* resolve_type(TxSpecializationIndex six) override final {
        this->resolve_field(six);
        return this->get_spec(six)->type;
    }

    virtual const TxType*  attempt_get_type(TxSpecializationIndex six) const override final { return this->get_spec(six)->type; }
    virtual const TxType*  get_type        (TxSpecializationIndex six) const override final {
        auto spec = this->get_spec(six); ASSERT(spec->hasResolved, "entity definer not resolved: " << this); return spec->type; }
    virtual const TxField* get_field       (TxSpecializationIndex six) const override final {
        auto spec = this->get_spec(six); ASSERT(spec->hasResolved, "entity definer not resolved: " << this); return spec->field; }
//    virtual const TxExpressionNode* get_init_expression() const = 0;
};


/**
 * The context of this node refers to its outer scope. This node's entity, if any, refers to its inner scope.
 */
class TxTypeExpressionNode : public TxTypeDefiningNode {
    /** if parent node is a type declaration that declares type parameters, these will be set by it */
    std::vector<const std::vector<TxDeclarationNode*>*> typeParamDeclNodes;

    void set_type_param_decl_nodes(TxSpecializationIndex six, const std::vector<TxDeclarationNode*>* typeParamDeclNodes) {
        while (six >= this->typeParamDeclNodes.size())
            this->typeParamDeclNodes.push_back(nullptr);
        this->typeParamDeclNodes[six] = typeParamDeclNodes;
    }

protected:
    inline bool has_type_param_decl_nodes(TxSpecializationIndex six) const {
        if ( six >= this->typeParamDeclNodes.size() ) return false;
        if ( auto nodes = this->typeParamDeclNodes.at(six) ) return ! nodes->empty();
        return false;
    }

    inline const std::vector<TxDeclarationNode*>* get_type_param_decl_nodes(TxSpecializationIndex six) const {
        return ( six < this->typeParamDeclNodes.size() ? this->typeParamDeclNodes.at(six) : nullptr );
    }

    virtual void symbol_declaration_pass_descendants(TxSpecializationIndex six, LexicalContext& defContext,
                                                     LexicalContext& lexContext, TxDeclarationFlags declFlags) = 0;

public:
    TxTypeExpressionNode(const yy::location& parseLocation) : TxTypeDefiningNode(parseLocation), typeParamDeclNodes(1)  { }

    /** Returns true if this type expression is a directly identified type
     * (i.e. a previously declared type, does not construct a new type). */
    virtual bool has_predefined_type() const { return false; }

    /** Gets the type declaration of this type expression, if any. */
    inline const TxTypeDeclaration* get_declaration(TxSpecializationIndex six) const {
        return dynamic_cast<const TxTypeDeclaration*>(this->get_spec(six)->declaration);
    }

    /** Performs the symbol declaration pass for this type expression.
     * Type expressions evaluate within a "definition context", representing their "outer" scope,
     * and a "lexical context", within which they declare their constituent sub-expressions.
     * The definition context is used for named types lookups, to avoid conflation with names of the sub-expressions.
     */
    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& defContext, LexicalContext& lexContext,
                                         TxDeclarationFlags declFlags, const std::string designatedTypeName,
                                         const std::vector<TxDeclarationNode*>* typeParamDeclNodes);

    virtual void symbol_resolution_pass(TxSpecializationIndex six) {
        if (auto typeParamDeclNodes = this->get_type_param_decl_nodes(six)) {
            for (auto paramDeclNode : *typeParamDeclNodes) {
                paramDeclNode->symbol_resolution_pass(six);
            }
        }
        this->resolve_type(six);
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
    virtual void symbol_declaration_pass_descendants(TxSpecializationIndex six, LexicalContext& defContext,
                                                     LexicalContext& lexContext, TxDeclarationFlags declFlags) override { }

    virtual const TxType* define_type(TxSpecializationIndex six) override {
        auto type = ( typeDefiner ? this->typeDefiner->resolve_type() : this->typeDefNode->resolve_type(six) );
        if (!type)
            return nullptr;
        else if (auto declEnt = this->get_declaration(six)) {
            if (! type->is_modifiable())
                // create empty specialization (uniquely named but identical type)
                return this->types(six).get_empty_specialization(declEnt, type);
        }
        return type;
    }

public:
    TxTypeExprWrapperNode(const yy::location& parseLocation, TxTypeDefiner* typeDefiner)
        : TxTypeExpressionNode(parseLocation), typeDefiner(typeDefiner), typeDefNode()  { }

    TxTypeExprWrapperNode(TxTypeDefiningNode* typeDefNode)
        : TxTypeExpressionNode(typeDefNode->parseLocation), typeDefiner(), typeDefNode(typeDefNode)  { }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }
};



class TxFieldDefNode;

class TxExpressionNode : public TxTypeDefiningNode {
protected:
    /** injected by field definition if known and applicable */
    const TxFieldDefNode* fieldDefNode = nullptr;

public:
    TxExpressionNode(const yy::location& parseLocation) : TxTypeDefiningNode(parseLocation) { }

    /** Injected by field definition if known and applicable. */
    virtual void set_field_def_node(const TxFieldDefNode* fieldDefNode) {
        this->fieldDefNode = fieldDefNode;
    }

    /** Returns true if this value expression is of a directly identified type
     * (i.e. does not construct a new type), e.g. value literals and directly identified fields. */
    virtual bool has_predefined_type() const = 0;

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) = 0;

    virtual void symbol_resolution_pass(TxSpecializationIndex six) {
        this->resolve_type(six);
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

    virtual std::vector<const TxType*>* get_applied_func_arg_types(TxSpecializationIndex six) {
        return this->get_spec(six)->appliedFuncArgTypes;
    }
    virtual void set_applied_func_arg_types(TxSpecializationIndex six, std::vector<const TxType*>* appliedFuncArgTypes) {
        this->get_spec(six)->appliedFuncArgTypes = appliedFuncArgTypes;
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
    virtual const TxType* define_type(TxSpecializationIndex six) override {
        return this->expr->resolve_type(this->six);
    }

public:
    TxExprWrapperNode(TxExpressionNode* expr, TxSpecializationIndex six)
        : TxExpressionNode(expr->parseLocation), expr(expr), six(six)  { }

    inline TxExpressionNode* get_wrapped() const { return this->expr; }
    inline TxSpecializationIndex get_six() const { return this->six; }

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


/** wrapper for class declaration order reasons... */
TxExpressionNode* make_generic_conversion_node(TxExpressionNode*);

/** wrapper for class declaration order reasons... */
void insert_conversion(TxExpressionNode* genericExprNode, TxSpecializationIndex six, const TxType* resultType, bool _explicit=false);


class TxFieldDefNode : public TxFieldDefiningNode {
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
    virtual const TxField* define_field(TxSpecializationIndex six) override {
        LOGGER().trace("resolving type of %s (s-ix %u)", this->to_string().c_str(), six);
        const TxType* type;
        if (this->typeExpression) {
            type = this->typeExpression->resolve_type(six);
        }
        else if (this->typeDefiner) {
            type = this->typeDefiner->resolve_type();
        }
        else {
            type = this->initExpression->resolve_type(six);
            if (type) {
                if (this->modifiable) {
                    if (! type->is_modifiable())
                        type = this->types(six).get_modifiable_type(nullptr, type);
                }
                else if (type->is_modifiable())
                    // if initialization expression is modifiable type, and modifiable not explicitly specified,
                    // lose modifiable attribute (modifiability must be explicit)
                    type = type->get_semantic_base_type();
            }
        }

        if (type) {
            auto spec = this->get_spec(six);
            spec->type = type;
            if (auto decl = static_cast<const TxFieldDeclaration*>(spec->declaration))
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
        if (initExpression) {
            initExpression->set_field_def_node(this);
            this->initExpression = make_generic_conversion_node(initExpression);
        }
        else {
            ASSERT(typeExpression, "At least one of typeExpression and initExpression must be specified");
            this->initExpression = nullptr;
        }
    }
    TxFieldDefNode(const yy::location& parseLocation, const std::string& fieldName,
                   TxExpressionNode* initExpression, bool modifiable=false, TxTypeDefiner* typeDefiner=nullptr)
            : TxFieldDefiningNode(parseLocation), fieldName(fieldName), modifiable(modifiable), typeDefiner(typeDefiner) {
        this->typeExpression = nullptr;
        if (initExpression) {
            initExpression->set_field_def_node(this);
            this->initExpression = make_generic_conversion_node(initExpression);
        }
        else {
            ASSERT(typeDefiner, "At least one of typeDefiner and initExpression must be specified");
            this->initExpression = nullptr;
        }
    }

    void symbol_declaration_pass_local_field(TxSpecializationIndex six, LexicalContext& lexContext, bool create_local_scope,
                                             TxDeclarationFlags declFlags=TXD_NONE) {
        LexicalContext outerCtx(lexContext);  // prevents init expr from referring to this field
        if (create_local_scope)
            lexContext.scope(lexContext.scope()->create_code_block_scope());
        this->declName = this->fieldName;
        TxDeclarationFlags tmpFlags = declFlags | lexContext.decl_flags();
        this->get_spec(six)->declaration = lexContext.scope()->declare_field(this->declName, this->get_spec_field_def(six),
                                                                             tmpFlags, TXS_STACK, TxIdentifier(""));
        this->symbol_declaration_pass(six, outerCtx, lexContext, declFlags);
    }

    void symbol_declaration_pass_nonlocal_field(TxSpecializationIndex six, LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                                TxFieldStorage storage, const TxIdentifier& dataspace) {
        TxDeclarationFlags tmpFlags = declFlags | lexContext.decl_flags();
        if (this->fieldName != "self")
            this->declName = this->fieldName;
        else {
            // handle constructor declaration
            this->declName = "$init";
            tmpFlags = tmpFlags | TXD_CONSTRUCTOR;
        }

        this->get_spec(six)->declaration = lexContext.scope()->declare_field(this->declName, this->get_spec_field_def(six),
                                                                             tmpFlags, storage, dataspace);
        this->symbol_declaration_pass(six, lexContext, lexContext, declFlags);
    }

    void symbol_declaration_pass_functype_arg(TxSpecializationIndex six, LexicalContext& lexContext) {
        this->symbol_declaration_pass(six, lexContext, lexContext, TXD_NONE);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) {
        this->resolve_field(six);
        if (this->typeExpression) {
            this->typeExpression->symbol_resolution_pass(six);
        }
        if (this->initExpression) {
            auto spec = this->get_spec(six);
            if ((this->typeExpression || this->typeDefiner) && spec->type)
                insert_conversion(this->initExpression, six, spec->type);
            this->initExpression->symbol_resolution_pass(six);
            if (spec->field && spec->field->is_statically_constant())
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
        ASSERT(this->get_spec(six)->declaration, "field declaration not initialized for " << this->fieldName);
        return dynamic_cast<const TxFieldDeclaration*>(this->get_spec(six)->declaration);
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

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override;

    virtual const TxFieldDeclaration* get_declaration(TxSpecializationIndex six) const override {
        return this->field->get_declaration(six);
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

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        this->typeExpression->symbol_resolution_pass(six);
        if (this->typeParamDecls)
            for (auto paramDecl : *this->typeParamDecls)
                paramDecl->symbol_resolution_pass(six);
    }

    virtual const TxTypeDeclaration* get_declaration(TxSpecializationIndex six) const override {
        return this->typeExpression->get_declaration(six);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual std::string to_string() const {
        return TxDeclarationNode::to_string() + " '" + this->typeName + "'";
    }
};


class TxAssigneeNode : public TxTypeDefiningNode {
public:
    TxAssigneeNode(const yy::location& parseLocation) : TxTypeDefiningNode(parseLocation) { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) = 0;

    virtual void symbol_resolution_pass(TxSpecializationIndex six) {
        this->resolve_type(six);
    }
};



class TxExpErrDeclNode : public TxDeclarationNode {
    const int expected_error_count;
    int encountered_error_count;
public:
    TxDeclarationNode* body;

    TxExpErrDeclNode(const yy::location& parseLocation, int expected_error_count, int prev_encountered_errors, TxDeclarationNode* body)
        : TxDeclarationNode(parseLocation, (body ? body->get_decl_flags() : TXD_NONE) | TXD_EXPERRBLOCK),
          expected_error_count(expected_error_count), encountered_error_count(prev_encountered_errors), body(body)  {
        //if (body)
        //    body->set_decl_flags(this->declFlags);
    }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        LexicalContext experrBlockContext(lexContext, lexContext.scope(), true);
        this->set_context(six, experrBlockContext);
        if (this->body) {
            if (six == 0) {
                experrBlockContext.package()->driver().begin_exp_err(this->parseLocation);
                this->body->symbol_declaration_pass(six, experrBlockContext);
                this->encountered_error_count += experrBlockContext.package()->driver().end_exp_err(this->parseLocation);
            }
            else
                this->body->symbol_declaration_pass(six, experrBlockContext);
        }
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        auto ctx = this->context(six);
        if (six == 0) {
            ctx.package()->driver().begin_exp_err(this->parseLocation);
            if (this->body)
                this->body->symbol_resolution_pass(six);
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
            this->body->symbol_resolution_pass(six);
    }

    virtual const TxEntityDeclaration* get_declaration(TxSpecializationIndex six) const override {
        return body->get_declaration(six);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }
};
