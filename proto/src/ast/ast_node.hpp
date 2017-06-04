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
typedef std::function<void( TxNode* node, const AstCursor& parent, const std::string& role, void* context )> AstVisitor;

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

    TypeRegistry& registry() const;

    inline Logger* LOGGER() const {
        return &this->_LOG;
    }
};
