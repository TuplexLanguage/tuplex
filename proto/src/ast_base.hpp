#pragma once

#include <typeinfo>
#include <vector>
#include <functional>

#include "util/assert.hpp"

#include "tx_logging.hpp"
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
class TxNode;

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
    if ( !nodeVec )
        return nullptr;
    std::vector<N*>* copyVec = new std::vector<N*>( nodeVec->size() );
    std::transform( nodeVec->cbegin(), nodeVec->cend(), copyVec->begin(),
                    []( N* n ) -> N* {return n->make_ast_copy();} );
    return copyVec;
}

bool validateTypeName( TxNode* node, TxDeclarationFlags declFlags, const std::string& name );
bool validateFieldName( TxNode* node, const std::string& name );

/** Returns the "degree of reinterpretation" required to implicitly transform a provided value
 * to a value of an expected type.
 * Returns 0 if the types are effectively equal (no conversion or casting required);
 * >0 if the provided type can be cast or implicitly converted to the expected type
 * (the higher the value, the greater the "degree of conversion" that is required
 * ("distance" between the types);
 * or <0 if it can't be cast or implicitly converted to the expected type.
 */
int get_reinterpretation_degree( const TxType *expectedType, const TxType* providedType );

struct AstCursor {
    const AstCursor* parent;
    const TxNode* node;
    unsigned depth;  // 0 if parent is null
    AstCursor( const TxNode* node )
            : parent(), node( node ), depth() {
    }
    AstCursor( const AstCursor* parent, const TxNode* node )
            : parent( parent ), node( node ), depth( parent->depth + 1 ) {
    }
};

/** type of the AST visitor callable */
typedef std::function<void( TxNode* node, const AstCursor& parent, const std::string& role, void* context )> AstVisitor;


/** Runs the declaration pass on a node and its subtree.
 * @parentNode the parent of node; must not be null */
extern void run_declaration_pass( TxNode* node, const TxNode* parentNode, const std::string& role="");

/** Runs the declaration pass on a node and its subtree. */
extern void run_declaration_pass( TxNode* node, const LexicalContext& lexContext );


//template<class N>
//const N* enclosing_node(const TxNode* node) const {
//    for ( const TxNode* node = node->parent(); node; node = node->parent() ) {
//        if ( auto stmtNode = dynamic_cast<const N*>( node ) )
//            return stmtNode;
//    }
//    return nullptr;
//}

class TxNode : public virtual TxParseOrigin, public Printable {
    static Logger& _LOG;
    static unsigned nextNodeId;

    const unsigned nodeId;

    /** this node's parent node (null for TxParsingUnitNode), this is set in the declaration pass */
    const TxNode* parentNode = nullptr;

protected:
    /** the semantic context this node represents/produces for its sub-AST, this is set in the declaration pass */
    LexicalContext lexContext;

    TxNode( const TxLocation& parseLocation )
            : nodeId( nextNodeId++ ), lexContext(), parseLocation( parseLocation ) {
    }

    virtual ~TxNode() = default;

    inline void set_context( LexicalContext&& context ) {
        ASSERT( !this->is_context_set(), "lexicalContext already initialized in " << this->str() );
        this->lexContext = context;
    }

    /** sets the parent node reference of this node and copies the parent's context to this node */
    inline void set_context( const TxNode* parentNode ) {
        ASSERT( !this->is_context_set(), "lexicalContext already initialized in " << this->str() );
        this->parentNode = parentNode;
        // TODO: make private and non-const, or separate parserCtx from TxLocation into separate TxNode member:
        const_cast<TxLocation&>( this->parseLocation ).parserCtx = parentNode->parseLocation.parserCtx;
        this->lexContext = this->parentNode->context();
    }

    /** Performs the declaration pass operations specific to this node.
     * The default implementation does nothing; to be overridden by subclasses as necessary.
     * This is invoked exactly once by run_declaration_pass() and should not be invoked from elsewhere.
     * This node's parent and context are set prior to this call.
     */
    virtual void declaration_pass() {
    }

public:
    const TxLocation parseLocation;

    virtual const TxNode* get_origin_node() const override final {
        return this;
    }

    virtual const TxLocation& get_parse_location() const override final {
        return this->parseLocation;
    }

    virtual ExpectedErrorClause* exp_err_ctx() const override final {
        return this->lexContext.exp_error();
    }

    inline unsigned get_node_id() const {
        return this->nodeId;
    }

    /** Creates a copy of this node and all its descendants for purpose of generic specialization. */
    virtual TxNode* make_ast_copy() const = 0;

    inline bool is_context_set() const {
        return this->lexContext.scope();
    }

    inline const LexicalContext& context() const {
        ASSERT( this->is_context_set(), "lexicalContext not initialized in " << this );
        return this->lexContext;
    }

    /** Gets the AST parent of this node. Populated in the beginning of the declaration pass. */
    inline const TxNode* parent() const {
        return this->parentNode;
    }


    /** Gets the nearest parent node of the specified type, starting with this node's parent.
     * Returns null if this node has no ancestor of the desired type. */
    template<class N>
    const N* enclosing_node() const {
        for ( const TxNode* node = this->parent(); node; node = node->parent() ) {
            if ( auto soughtNode = dynamic_cast<const N*>( node ) )
                return soughtNode;
        }
        return nullptr;
    }

    /** Runs the resolution pass on this node and its subtree. */
    virtual void symbol_resolution_pass() = 0;

//    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const = 0;

    /** Visits this node and its subtree with the provided visitor and context object. */
    void visit_ast( AstVisitor visitor, void* context );

    /** Invoked from visit_descendants() of parent node. */
    void visit_ast( AstVisitor visitor, const AstCursor& parent, const std::string& role, void* context );

    /** To be implemented by subclasses. */
    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) = 0;

    /** Runs the declaration pass on this node (its subtree is not processed).
     * @parentNode the parent of node; must not be null */
    inline void node_declaration_pass( const TxNode* parentNode ) {
        ASSERT( parentNode, "NULL parentNode" );
        if (this->is_context_set()) {
            LOG( this->LOGGER(), ERROR, "Context already set, skipping decl pass for node " << this << " (parent: " << this->parent() << ")" );
            return;
        }
        this->set_context( parentNode );
        this->declaration_pass();
    }

    /** Returns the identifier owned by this node, if any, otherwise an empty string. */
    virtual std::string get_identifier() const {
        return std::string();
    }

    virtual std::string str() const override;

    std::string parse_loc_string() const;

    inline TypeRegistry& registry() const {
        return this->context().package()->registry();
    }

    inline Logger* LOGGER() const {
        return &this->_LOG;
    }
};

class TxImportNode : public TxNode {
protected:
    virtual void declaration_pass() override {
        static_cast<TxModule*>( this->context().scope() )->register_import( *this, *this->ident );
    }

public:
    const TxIdentifier* ident;

    TxImportNode( const TxLocation& parseLocation, const TxIdentifier* identifier )
            : TxNode( parseLocation ), ident( identifier ) {
        // imports need to be added to the parser context upon AST creation, so that they will be parsed before the declaration pass:
        if ( !this->ident->is_qualified() )
            CERROR( this, "can't import unqualified identifier '" << this->ident << "'" );
        else {
            if ( !this->parseLocation.parserCtx->add_import( this->ident->parent() ) )
                CERROR( this, "Failed to import module (source not found): " << this->ident->parent() );
        }
    }

    virtual TxImportNode* make_ast_copy() const override {
        return new TxImportNode( this->parseLocation, this->ident );
    }

    virtual void symbol_resolution_pass() override { }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
    }

    virtual std::string get_identifier() const override {
        return this->ident->str();
    }
};

/** Common superclass for non-local entity declaration nodes. */
class TxDeclarationNode : public TxNode {
    TxDeclarationFlags declFlags;
    // these set TXD_EXPERRBLOCK if this is an exp-err declaration:
    friend class TxExpErrDeclNode;
    friend class TxTypeStmtNode;  // forwards from TxExpErrStmtNode

public:
    TxDeclarationNode( const TxLocation& parseLocation, const TxDeclarationFlags declFlags )
            : TxNode( parseLocation ), declFlags( declFlags ) {
    }

    virtual TxDeclarationNode* make_ast_copy() const override = 0;

    inline TxDeclarationFlags get_decl_flags() const {
        return this->declFlags;
    }

    virtual const TxEntityDeclaration* get_declaration() const = 0;

    virtual void code_gen( LlvmGenerationContext& context ) const = 0;
};

class TxModuleNode : public TxNode {
    const TxIdentifier* ident;
    std::vector<TxImportNode*>* imports;
    std::vector<TxDeclarationNode*>* members;
    std::vector<TxModuleNode*>* subModules;
    bool builtin;

protected:
    virtual void declaration_pass() override {
        this->lexContext._scope = static_cast<TxModule*>( this->lexContext._scope )->declare_module( *this, *this->ident, this->builtin );
    }

public:
    TxModuleNode( const TxLocation& parseLocation, const TxIdentifier* identifier,
                  std::vector<TxImportNode*>* imports,
                  std::vector<TxDeclarationNode*>* members,
                  std::vector<TxModuleNode*>* subModules,
                  bool builtin = false )
            : TxNode( parseLocation ), ident( identifier ), imports( imports ), members( members ), subModules( subModules ), builtin( builtin ) {
        ASSERT( identifier, "NULL identifier" );  // (sanity check on parser)
    }

    virtual TxModuleNode* make_ast_copy() const override {
        return new TxModuleNode( this->parseLocation, this->ident,
                                 make_node_vec_copy( imports ),
                                 make_node_vec_copy( members ),
                                 make_node_vec_copy( subModules ),
                                 builtin );
    }

    virtual void symbol_resolution_pass() {
        if ( this->members ) {
            for ( auto mem : *this->members )
                mem->symbol_resolution_pass();
        }
        if ( this->subModules ) {
            for ( auto mod : *this->subModules )
                mod->symbol_resolution_pass();
        }
    }

    void code_gen( LlvmGenerationContext& context ) const;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( this->imports ) {
            for ( auto imp : *this->imports )
                imp->visit_ast( visitor, thisCursor, "import", context );
        }
        if ( this->members ) {
            for ( auto mem : *this->members )
                mem->visit_ast( visitor, thisCursor, "member", context );
        }
        if ( this->subModules ) {
            for ( auto mod : *this->subModules )
                mod->visit_ast( visitor, thisCursor, "module", context );
        }
    }

    virtual std::string get_identifier() const override {
        return this->ident->str();
    }
};

/** Represents a parsing unit, i.e. a given source text input (e.g. source file). */
class TxParsingUnitNode : public TxNode {
public:
    TxModuleNode* module;

    TxParsingUnitNode( const TxLocation& parseLocation, TxModuleNode* module )
            : TxNode( parseLocation ), module( module ) {
    }

    virtual TxDeclarationNode* make_ast_copy() const override {
        ASSERT( false, "Can't make AST copy of TxParsingUnitNode " << this );
        return nullptr;
    }

    inline void set_context( TxPackage* package ) {
        ASSERT( !this->is_context_set(), "lexicalContext already initialized in " << this->str() );
        this->lexContext = LexicalContext( package );
    }

    virtual void symbol_resolution_pass() {
        this->module->symbol_resolution_pass();
    }

    void code_gen( LlvmGenerationContext& context ) const;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->module->visit_ast( visitor, thisCursor, "module", context );
    }
};

/** Root class for AST nodes that resolve a program entity - a type or a field.
 * To resolve means to either produce a new entity (define a new type or field)
 * or to use one produced elsewhere.
 *
 * Type and field definitions, as well as value expressions, produce and/or use an entity.
 * If the node resolves a field entity, it will also resolve the type of that field.
 * This means all instances of this node will resolve a type, both type-defining and field-defining ones.
 * The entity is produced/resolved during the resolution pass.
 */
class TxEntityDefiningNode : public TxNode {
public:
    TxEntityDefiningNode( const TxLocation& parseLocation )
            : TxNode( parseLocation ) {
    }

    virtual TxEntityDefiningNode* make_ast_copy() const override = 0;

    /** Resolves and returns the type of the entity/value this node produces/uses.
     * If this node's entity has not already been resolved, it will be resolved in this invocation.
     * If the resolution fails, an error message will have been generated and resolution_error exception is thrown.
     * This method never returns NULL. */
    virtual const TxType* resolve_type() = 0;

    /** Returns the type of the entity/value this node produces/uses.
     * This node must have been resolved before this call.
     * This method never returns NULL, provided this node has been successfully resolved. */
    virtual const TxType* get_type() const = 0;

    /** Returns the type of the entity/value this node produces/uses if already successfully resolved,
     * otherwise NULL. */
    virtual const TxType* attempt_get_type() const = 0;
};

class TxTypeDefiningNode : public TxEntityDefiningNode {
    const TxType* type = nullptr;
    bool startedRslv = false;  // guard against recursive resolution
    bool hasResolved = false;  // to prevent multiple identical error messages

protected:
    /** Defines the type of this expression (as specific as can be known), constructing/obtaining the TxType instance.
     * The implementation should only traverse the minimum nodes needed to define the type
     * (e.g. not require the actual target type of a reference to be defined).
     * This should only be invoked once, from the TxTypeDefiningNode class.
     * @return a valid type pointer (exception must be thrown upon failure) */
    virtual const TxType* define_type() = 0;

public:
    TxTypeDefiningNode( const TxLocation& parseLocation )
            : TxEntityDefiningNode( parseLocation ) {
    }

    virtual TxTypeDefiningNode* make_ast_copy() const override = 0;

    /** Returns the type (as specific as can be known) of the value this node produces/uses.
     * @return a valid type pointer (exception is thrown upon failure) */
    virtual const TxType* resolve_type() override final;

    virtual const TxType* attempt_get_type() const override {
        return this->type;
    }
    virtual const TxType* get_type() const override {
        ASSERT( this->type, "entity definer not resolved: " << this );
        return this->type;
    }

    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const = 0;
};

class TxExpressionNode;

class TxFieldDefiningNode : public TxEntityDefiningNode {
    const TxType* type = nullptr;
    const TxField* field = nullptr;
    bool startedRslv = false;  // guard against recursive resolution
    bool hasResolved = false;  // to prevent multiple identical error messages

    mutable llvm::Constant* cachedConstantInitializer = nullptr;

protected:
    /** Defines the type of this field (as specific as can be known), constructing/obtaining the TxType instance.
     * The implementation should only traverse the minimum nodes needed to define the type
     * (e.g. not require the actual target type of a reference to be defined).
     * This should only be invoked once, from the TxFieldDefiningNode class.
     * @return a valid type pointer (exception must be thrown upon failure) */
    virtual const TxType* define_type() = 0;

    /** Defines the field of this node, constructing/obtaining the TxField instance.
     * This should only be invoked once, from the TxFieldDefiningNode class. */
    virtual const TxField* define_field() = 0;

public:
    TxFieldDefiningNode( const TxLocation& parseLocation )
            : TxEntityDefiningNode( parseLocation ) {
    }

    virtual TxFieldDefiningNode* make_ast_copy() const override = 0;

    /** Resolves the type and returns the field entity of this field-defining node.
     * @return a valid field pointer (exception is thrown upon failure) */
    virtual const TxField* resolve_field() final;

    /** Returns the type (as specific as can be known) of the value this field-defining node produces/uses.
     * @return a valid type pointer (exception is thrown upon failure) */
    virtual const TxType* resolve_type() final {
        this->resolve_field();
        return this->type;
    }

    virtual const TxType* attempt_get_type() const override final {
        return this->type;
    }
    virtual const TxType* get_type() const override final {
        ASSERT( this->type, "entity definer not resolved: " << this );
        return this->type;
    }

    virtual const TxField* attempt_get_field() const final {
        return this->field;
    }
    virtual const TxField* get_field() const final {
        ASSERT( this->field, "entity definer not resolved: " << this );
        return this->field;
    }

    virtual const TxExpressionNode* get_init_expression() const = 0;

    /** Generates / retrieves the code generated constant value of this field's init expression,
     * if it has one and it is constant.
     * May be called multiple times, it caches the result to ensures the constant value is only generated once.
     * Only valid to call on nodes for which is_statically_constant() returns true. */
    virtual llvm::Constant* code_gen_constant_init_expr( LlvmGenerationContext& context ) const;
};
