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
    const TxNode* parentNode;
    // does not need to hold current node - implicitly known via 'this' in called methods
    unsigned depth;
    explicit AstCursor( const TxNode* parentNode )
            : parentNode( parentNode ), depth() {
    }
    AstCursor( const AstCursor& parentCursor, const TxNode* parentNode )
            : parentNode( parentNode ), depth( parentCursor.depth + 1 ) {
    }
} __attribute__((aligned(16)));

/** type of the AST visitor callable */
typedef std::function<void( TxNode* node, const AstCursor& cursor, const std::string& role, void* aux )> AstVisitorFunc;

/** Compilation pass, used in AST visitor traversal.
 * For traversals that don't constitute a compilation pass, TXP_NIL is used.
 */
enum TxPass {
    TXP_NIL, TXP_PARSE, TXP_DECLARATION, TXP_TYPE_CREATION, TXP_RESOLUTION, TXP_VERIFICATION, TXP_CODEGEN
};

typedef struct {
    AstVisitorFunc preFunc;
    AstVisitorFunc postFunc;
    TxPass pass;
    bool insertedNodes;
} __attribute__((aligned(128))) AstVisitor;


/** Used as parameter to type resolution methods to specify current / needed resolution level:
 * resolve_type(), define_type()
 * get_constructed_type(), insert_conversion(), insert_qual_conversion()
 */
enum TxTypeResLevel {
    TXR_TYPE_CREATION, TXR_FULL_RESOLUTION
};


class TxNode : public virtual TxParseOrigin, public Printable {
    static const std::string EMPTY_STRING;
    static Logger& ASTLOGGER;
    static unsigned nextNodeId;

    const unsigned nodeId;

    /** this node's parent node (null for TxParsingUnitNode), this is set in the declaration pass */
    const TxNode* parentNode = nullptr;

    /** indicates the last compiler / visitor pass successfully executed for this node.
     * Note that this is set after the visitor pre-function, and the children, have been processed,
     * but before the vistor post-function on this node. */
    TxPass lastPass = TXP_NIL;

    /** indicates whether this node has encountered compilation errors in previous passes */
    unsigned compilationErrors = 0;

    /** sets the parent node reference of this node and copies the parent's context to this node */
    inline void set_context( const TxNode* parent ) {
        this->parentNode = parent;
        this->lexContext = this->parentNode->context();
    }

protected:
    /** the semantic context this node represents/produces for its sub-AST, this is set in the declaration pass */
    LexicalContext lexContext;

    explicit TxNode( const TxLocation& ploc )
            : nodeId( nextNodeId++ ), lexContext(), ploc( ploc ) {
    }

    ~TxNode() override = default;

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

    inline const TxNode* get_origin_node() const final {
        return this;
    }

    inline const TxLocation& get_parse_location() const final {
        return this->ploc;
    }

    inline ExpectedErrorClause* exp_err_ctx() const final {
        return this->lexContext.exp_error();
    }

    inline unsigned get_node_id() const {
        return this->nodeId;
    }

    inline TxPass get_last_pass() const {
        return this->lastPass;
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


    /** To be implemented by subclasses.
     * This shall invoke visit_ast for all (owned) descendants.
     * It shall typically pass the same visitor, cursor, and aux, but with a bespoke role descriptor that
     * indicates the role of that descendant node from this parent node's perspective. For example:
     *     this->_typeNode->visit_ast( visitor, cursor, "type", aux );
     */
    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) = 0;

    /** To be invoked from the visit_descendants() implementation of the parent node. For example:
     *     this->_typeNode->visit_ast( visitor, cursor, "type", aux );
     */
    void visit_ast( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux );


    /** Runs the declaration pass on this specific node (its subtree is not processed).
     * Invoked by the declaration-pass visitor.
     * @param parent the parent of node; must not be null */
    inline void node_declaration_pass( const TxNode* parent ) {
        this->set_context( parent );
        this->declaration_pass();
    }

    /** Runs the type pass on this specific node (its subtree is not processed).
     * Invoked by the type-pass visitor. */
    inline void node_type_pass() {
        this->type_pass();
    }

    /** Runs the resolution pass on this specific node (its subtree is not processed).
     * Invoked by the resolution-pass visitor. */
    inline void node_resolution_pass() {
        this->resolution_pass();
    }

    /** Runs the verification pass on this specific node (its subtree is not processed).
     * Invoked by the verification-pass visitor. */
    inline void node_verification_pass() const {
        this->verification_pass();
    }


    /** Returns a source-similar descriptor or tag for this node, e.g. the identifier or literal.
     * Otherwise an empty string. */
    virtual const std::string& get_descriptor() const {
        return EMPTY_STRING;
    }

    std::string str() const override;

    std::string parse_loc_string() const;

    TypeRegistry& registry() const;

    inline static Logger* LOGGER() {
        return &TxNode::ASTLOGGER;
    }

    static unsigned nodes_created_count() {
        return TxNode::nextNodeId;
    }
};
