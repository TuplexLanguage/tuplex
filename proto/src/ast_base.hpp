#pragma once

#include <typeinfo>
#include <vector>

#include "util/assert.hpp"
#include "util/logging.hpp"

#include "tx_error.hpp"
#include "tx_operations.hpp"
#include "location.hpp"
#include "identifier.hpp"

#include "driver.hpp"

#include "symbol/package.hpp"
#include "symbol/module.hpp"

#include "context.hpp"

class TxType;
class TxField;

/* forward declarations pertaining to LLVM code generation */
class LlvmGenerationContext;
class GenScope;
namespace llvm {
    class Constant;
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
class TxConstantProxy {
public:
    virtual ~TxConstantProxy() = default;

    /** Gets the TxType instance representing the type of the constant. */
    virtual const TxType* get_type() const = 0;

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

    inline unsigned get_node_id() const { return this->nodeId; }

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

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorDecl=false ) = 0;

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



class TxEntityDefiningNode : public TxNode {
public:
    TxEntityDefiningNode(const TxLocation& parseLocation) : TxNode(parseLocation) { }

    virtual TxEntityDefiningNode* make_ast_copy() const override = 0;

    /** Returns the type (as specific as can be known) of the value this node produces/uses. */
    virtual const TxType* resolve_type    ()       = 0;
    virtual const TxType* attempt_get_type() const = 0;
    virtual const TxType* get_type        () const = 0;
};


class TxTypeDefiningNode : public TxEntityDefiningNode {
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
    TxTypeDefiningNode(const TxLocation& parseLocation) : TxEntityDefiningNode(parseLocation) { }

    virtual TxTypeDefiningNode* make_ast_copy() const override = 0;

    /** Returns the type (as specific as can be known) of the value this node produces/uses. */
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


class TxExpressionNode;

class TxFieldDefiningNode : public TxEntityDefiningNode {
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
    TxFieldDefiningNode(const TxLocation& parseLocation) : TxEntityDefiningNode(parseLocation) { }

    virtual TxFieldDefiningNode* make_ast_copy() const override = 0;

    /** Resolves the type and returns the field entity of this field-defining node. */
    virtual const TxField* resolve_field() final {
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

    /** Returns the type (as specific as can be known) of the value this field-defining node produces/uses. */
    virtual const TxType* resolve_type() final {
        this->resolve_field();
        return this->type;
    }

    virtual const TxType*  attempt_get_type() const override final { return this->type; }
    virtual const TxType*  get_type        () const override final {
        ASSERT(this->hasResolved, "entity definer not resolved: " << this); return this->type; }

    virtual const TxField* get_field       () const final {
        ASSERT(this->hasResolved, "entity definer not resolved: " << this); return this->field; }

    virtual const TxExpressionNode* get_init_expression() const = 0;
};
