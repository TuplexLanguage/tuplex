#pragma once

#include "parser/location.hpp"
#include "tx_logging.hpp"
#include "tx_error.hpp"

#include "lexcontext.hpp"


/* forward declarations pertaining to LLVM code generation */
class LlvmGenerationContext;
class GenScope;
namespace llvm {
class Constant;
class Value;
}


class TypeRegistry;

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
typedef std::function<void( TxNode* node, const AstCursor& parent, const std::string& role, void* context )> AstVisitorFunc;

typedef struct {
    AstVisitorFunc preFunc;
    AstVisitorFunc postFunc;
} AstVisitor;


enum TxPass {
    TXP_NIL, TXP_PARSE, TXP_DECLARATION, TXP_TYPE, TXP_RESOLUTION, TXP_VERIFICATION, TXP_CODEGEN
};

/** Used as parameter to certain AST methods to carry current analysis pass information. */
typedef unsigned TxPassInfo;

inline bool is_full_resolution( TxPassInfo pi) { return ( pi >= TXP_RESOLUTION ); }


class TxNode : public virtual TxParseOrigin, public Printable {
    friend class TxDriver;

    static const std::string EMPTY_STRING;
    static Logger& _LOG;
    static unsigned nextNodeId;

    const unsigned nodeId;

    /** this node's parent node (null for TxParsingUnitNode), this is set in the declaration pass */
    const TxNode* parentNode = nullptr;

    /** indicates whether this node has encountered compilation errors in previous passes */
    unsigned compilationErrors = 0;

protected:
    /** the semantic context this node represents/produces for its sub-AST, this is set in the declaration pass */
    LexicalContext lexContext;

    TxNode( const TxLocation& ploc )
            : nodeId( nextNodeId++ ), lexContext(), ploc( ploc ) {
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
        const_cast<TxLocation&>( this->ploc ).parserCtx = parentNode->ploc.parserCtx;
        this->lexContext = this->parentNode->context();
    }

    /** Performs the declaration pass operations specific to this node.
     * The default implementation does nothing; to be overridden by subclasses as necessary.
     * This is invoked exactly once by node_declaration_pass() and should not be invoked from elsewhere.
     * This node's parent and context are set prior to this call.
     */
    virtual void declaration_pass() {
    }

    /** Performs the type pass operations specific to this node.
     * The default implementation does nothing; to be overridden by subclasses as necessary.
     * This is invoked exactly once by node_type_pass() and should not be invoked from elsewhere.
     */
    virtual void type_pass() {
    }

    /** Performs the resolution pass operations specific to this node.
     * The default implementation does nothing; to be overridden by subclasses as necessary.
     * This is invoked exactly once by node_resolution_pass() and should not be invoked from elsewhere.
     */
    virtual void resolution_pass() {
    }

    /** Performs the verification pass operations specific to this node.
     * The default implementation does nothing; to be overridden by subclasses as necessary.
     * This is invoked exactly once by node_verification_pass() and should not be invoked from elsewhere.
     */
    virtual void verification_pass() const {
    }

public:
    const TxLocation ploc;

    virtual const TxNode* get_origin_node() const override final {
        return this;
    }

    virtual const TxLocation& get_parse_location() const override final {
        return this->ploc;
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

    /** Visits this node and its subtree with the provided visitor and context object. */
    void visit_ast( const AstVisitor& visitor, void* context );

    /** Invoked from visit_descendants() of parent node. */
    void visit_ast( const AstVisitor& visitor, const AstCursor& parent, const std::string& role, void* context );

    /** To be implemented by subclasses. */
    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) = 0;

    /** Runs the declaration pass on this specific node (its subtree is not processed).
     * @parentNode the parent of node; must not be null */
    inline void node_declaration_pass( const TxNode* parentNode ) {
        ASSERT( parentNode, "NULL parentNode" );
        this->set_context( parentNode );
        this->declaration_pass();
    }

    /** Runs the type pass on this specific node (its subtree is not processed). */
    inline void node_type_pass() {
        this->type_pass();
    }

    /** Runs the resolution pass on this specific node (its subtree is not processed). */
    inline void node_resolution_pass() {
        this->resolution_pass();
    }

    /** Runs the verification pass on this specific node (its subtree is not processed). */
    inline void node_verification_pass() const {
        this->verification_pass();
    }

    /** Returns a source-similar descriptor of this node, e.g. the identifier or literal. Otherwise an empty string. */
    virtual const std::string& get_descriptor() const {
        return EMPTY_STRING;
    }

    virtual std::string str() const override;

    std::string parse_loc_string() const;

    TypeRegistry& registry() const;

    inline Logger* LOGGER() const {
        return &this->_LOG;
    }
};
