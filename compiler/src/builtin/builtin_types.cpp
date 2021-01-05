#include "builtin_types.hpp"

#include "../ast/ast_entitydecls.hpp"
#include "util/assert.hpp"

#include "llvm_generator.hpp"
#include "parsercontext.hpp"
#include "driver.hpp"

#include "ast/ast_util.hpp"
#include "ast/ast_modbase.hpp"
#include "ast/ast_declpass.hpp"
#include "ast/type/ast_types.hpp"
#include "ast/type/ast_qualtypes.hpp"
#include "ast/expr/ast_lit.hpp"
#include "ast/expr/ast_string.hpp"
#include "ast/expr/ast_array.hpp"
#include "ast/expr/ast_ref.hpp"
#include "ast/expr/ast_field.hpp"
#include "ast/expr/ast_lambda_node.hpp"
#include "ast/expr/ast_op_exprs.hpp"
#include "ast/stmt/ast_stmts.hpp"

/*--- statically allocated built-in type objects ---*/

std::vector<std::string> BUILTIN_TYPE_NAMES = {
                                                "Any",
                                                "Void",
                                                "Interface",
                                                "Elementary",
                                                "Scalar",
                                                "Integer",
                                                "Signed",
                                                "Byte",
                                                "Short",
                                                "Int",
                                                "Long",
                                                "Unsigned",
                                                "UByte",
                                                "UShort",
                                                "UInt",
                                                "ULong",
                                                "Floatingpoint",
                                                "Half",
                                                "Float",
                                                "Double",
                                                "Bool",
                                                "Reference",
                                                "Array",
                                                "Function",
                                                "Tuple",
};


/*------ built-in type defining AST nodes ------*/

static Logger& _LOGGER = Logger::get( "BUILTINS" );

static std::string thisSrcFileName( __FILE__ );

std::string get_builtins_file_name() {
    auto slashPos = thisSrcFileName.find_last_of( '/' );
    if ( slashPos == std::string::npos )
        return thisSrcFileName;
    else
        return thisSrcFileName.substr( slashPos+1, thisSrcFileName.size() );
}

#define PLOC(parserContext)  (TxLocation( __LINE__, 0, (parserContext) ))


/** Abstract superclass for the built-in type defining nodes. */
class TxBuiltinTypeDefiningNode : public TxTypeCreatingNode {
    TxTypeDeclNode* superRefTypeNode = nullptr;

    /** creates nodes for the implicit type member 'Super' */
    void make_super_type_node() {
        // (Note, 'Self' is created in the symbol table for all types, as an alias directly to the type.)
        if ( this->baseTypeNode ) {
            auto superTypeExprN = new TxAliasTypeNode( this->ploc, new TxTypeExprWrapperNode( this->baseTypeNode ) );
            auto superTypeName = new TxIdentifierNode( this->ploc, "Super" );
            this->superRefTypeNode = new TxTypeDeclNode( this->ploc, TXD_IMPLICIT, superTypeName, nullptr, superTypeExprN );
        }
        else {
            // let tx.Any.Super refer to itself
            auto superTypeExprN = new TxAliasTypeNode( this->ploc, new TxTypeExprWrapperNode( this ) );
            auto superTypeName = new TxIdentifierNode( this->ploc, "Super" );
            this->superRefTypeNode = new TxTypeDeclNode( this->ploc, TXD_IMPLICIT, superTypeName, nullptr, superTypeExprN );
        }
    }

    void merge_builtin_type_definers( TxDerivedTypeNode* sourcecodeDef ) {
        ASSERT( this->is_context_set(), "Builtin type node hasn't run declaration pass: " << this );
        ASSERT( !this->attempt_qtype(), "Builtin type already resolved: " << this );
        sourcecodeDef->set_builtin_type_definer( this );
        this->sourcecodeDefiner = sourcecodeDef;
    }

    friend void merge_builtin_type_definers( TxDerivedTypeNode* sourcecodeDefiner, TxTypeResolvingNode* builtinDefiner );

protected:
    const BuiltinTypeId builtinTypeId;
    TxBuiltinTypeDefiningNode const * const original;

    /** Specifies the base type of this type (null for the Any type). */
    TxTypeExpressionNode* const baseTypeNode;
    /** Declarations within this type's namespace. */
    std::vector<TxDeclarationNode*> declNodes;

    /** behavior defined via source code */
    TxDerivedTypeNode* sourcecodeDefiner = nullptr;

    /** for use by make_ast_copy() in subclasses */
    TxBuiltinTypeDefiningNode( const TxLocation& ploc, const TxBuiltinTypeDefiningNode* original,
                               TxTypeExpressionNode* baseTypeNode, const std::vector<TxDeclarationNode*>& declNodes,
                               TxDerivedTypeNode* sourcecodeDefiner )
            : TxTypeCreatingNode( ploc ), builtinTypeId( TXBT_NOTSET ), original( original ),
              baseTypeNode( baseTypeNode ), declNodes( declNodes ), sourcecodeDefiner( sourcecodeDefiner ) {
        make_super_type_node();
        if (sourcecodeDefiner)
            sourcecodeDefiner->set_builtin_type_definer( this );
    }

    void typeexpr_declaration_pass() override {
        if ( this->sourcecodeDefiner ) {
            // "pass through" entity declaration to the source code definer
            this->sourcecodeDefiner->set_declaration( this->get_declaration() );
        }
    }

    TxActualType* create_type( TxPassInfo passInfo ) final {
        TxActualType* actType;
        if ( this->sourcecodeDefiner ) {
            // copy vector because of const conversion:
            std::vector<const TxTypeExpressionNode*> ifNodes( this->sourcecodeDefiner->interfaces->cbegin(),
                                                              this->sourcecodeDefiner->interfaces->cend() );
            actType = this->make_builtin_type( this->get_declaration(), ifNodes, this->requires_mutable_type() );
        }
        else {
            actType = this->make_builtin_type( this->get_declaration(), {}, this->requires_mutable_type() );
        }
        actType->runtimeTypeId = this->builtinTypeId;
        this->registry().add_type( actType );
        return actType;
    }

    virtual TxActualType* make_builtin_type( const TxTypeDeclaration* declaration, const std::vector<const TxTypeExpressionNode*>& ifNodes,
                                             bool mutableType ) = 0;

public:
    TxBuiltinTypeDefiningNode( const TxLocation& ploc, BuiltinTypeId builtinTypeId,
                               TxTypeExpressionNode* baseTypeNode,
                               std::vector<TxDeclarationNode*> declNodes )
            : TxTypeCreatingNode( ploc ), builtinTypeId( builtinTypeId ), original( nullptr ),
              baseTypeNode( baseTypeNode ),
              declNodes(std::move( declNodes )) {
        make_super_type_node();
    }

    TxBuiltinTypeDefiningNode* make_ast_copy() const override {
        // (only valid for Ref and Array, which override this method)
        ASSERT( false, "Can't make AST copy of built-in type definer " << this );
        return nullptr;
    }

    void code_gen_type( LlvmGenerationContext& context ) const override {
        if ( this->baseTypeNode ) {
            this->baseTypeNode->code_gen_type( context );
        }
        this->superRefTypeNode->code_gen( context );
        for ( auto decl : this->declNodes )
            decl->code_gen( context );
        if ( this->sourcecodeDefiner )
            this->sourcecodeDefiner->code_gen_builtin_type( context );
        for ( auto implConstr : this->qtype()->get_implicit_constructors() )
            implConstr->code_gen( context );
    }

    void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( this->baseTypeNode ) {
            this->baseTypeNode->visit_ast( visitor, thisCursor, "basetype", context );
        }
        this->superRefTypeNode->visit_ast( visitor, thisCursor, "super", context );
        for ( auto decl : this->declNodes )
            decl->visit_ast( visitor, thisCursor, "decl", context );
        if ( this->sourcecodeDefiner )
            this->sourcecodeDefiner->visit_ast( visitor, thisCursor, "source", context );
        if ( auto qtype = this->attempt_qtype() ) {
            for ( auto implConstr : qtype->get_implicit_constructors() )
                implConstr->visit_ast( visitor, thisCursor, "initializer", context );
        }
    }

    const std::string& get_descriptor() const override {
        return this->parent()->get_descriptor();  // the descriptor of the type declaration
    }
};

void merge_builtin_type_definers( TxDerivedTypeNode* sourcecodeDefiner, TxTypeResolvingNode* builtinDefiner ) {
    auto builtinNode = dynamic_cast<TxBuiltinTypeDefiningNode*>( builtinDefiner );
    if ( !builtinNode )
        THROW_LOGIC( "Expected builtin type definer to be of type TxBuiltinTypeDefiningNode: " << builtinDefiner );
    builtinNode->merge_builtin_type_definers( sourcecodeDefiner );
}


/** Single-purpose node that defines the Any root type. */
class TxAnyTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    TxActualType* make_builtin_type( const TxTypeDeclaration* declaration, const std::vector<const TxTypeExpressionNode*>& ifNodes,
                                             bool mutableType ) override {
        return new TxActualType( get_type_class_handler( TXTC_ANY ), declaration, true );
    }

public:
    TxAnyTypeDefNode( const TxLocation& ploc, const std::vector<TxDeclarationNode*>& declNodes )
            : TxBuiltinTypeDefiningNode( ploc, TXBT_ANY, nullptr, declNodes ) {
    }
};

/** Single-purpose node that defines the Void type. */
class TxVoidTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    TxActualType* make_builtin_type( const TxTypeDeclaration* declaration, const std::vector<const TxTypeExpressionNode*>& ifNodes,
                                             bool mutableType ) override {
        return new TxActualType( get_type_class_handler( TXTC_VOID ), declaration, false );
    }

public:
    TxVoidTypeDefNode( const TxLocation& ploc, TxTypeExpressionNode* baseTypeNode, const std::vector<TxDeclarationNode*>& declNodes = { } )
            : TxBuiltinTypeDefiningNode( ploc, TXBT_VOID, baseTypeNode, declNodes ) {
    }
};

/** Used to define the built-in types' abstract base types. */
class TxBuiltinAbstractTypeDefNode final : public TxBuiltinTypeDefiningNode {
    const TxTypeClass typeClass;

protected:
    TxActualType* make_builtin_type( const TxTypeDeclaration* declaration, const std::vector<const TxTypeExpressionNode*>& ifNodes,
                                             bool mutableType ) override {
//        return new TxBuiltinBaseType( this->typeClass, declaration, baseType, ifSpecs );
        return new TxActualType( get_type_class_handler( this->typeClass ),
                                 declaration, true, this->baseTypeNode, ifNodes );
    }
public:
    TxBuiltinAbstractTypeDefNode( const TxLocation& ploc, BuiltinTypeId builtinTypeId, TxTypeExpressionNode* baseTypeNode,
                                  TxTypeClass typeClass, const std::vector<TxDeclarationNode*>& declNodes )
            : TxBuiltinTypeDefiningNode( ploc, builtinTypeId, baseTypeNode, declNodes ), typeClass( typeClass ) {
    }
};

class TxIntegerTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    TxActualType* make_builtin_type( const TxTypeDeclaration* declaration, const std::vector<const TxTypeExpressionNode*>& ifNodes,
                                             bool mutableType ) override {
        return new TxIntegerType( declaration, this->baseTypeNode, ifNodes, this->size, this->sign );
    }
public:
    const int size;
    const bool sign;
    TxIntegerTypeDefNode( const TxLocation& ploc, BuiltinTypeId builtinTypeId, TxTypeExpressionNode* baseTypeNode, int size, bool sign,
                          const std::vector<TxDeclarationNode*>& declNodes )
            : TxBuiltinTypeDefiningNode( ploc, builtinTypeId, baseTypeNode, declNodes ), size( size ), sign( sign ) {
    }
};

class TxFloatingTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    TxActualType* make_builtin_type( const TxTypeDeclaration* declaration,
                                             const std::vector<const TxTypeExpressionNode*>& ifNodes, bool mutableType ) override {
        return new TxFloatingType( declaration, this->baseTypeNode, ifNodes, this->size );
    }
public:
    const int size;
    TxFloatingTypeDefNode( const TxLocation& ploc, BuiltinTypeId builtinTypeId, TxTypeExpressionNode* baseTypeNode, int size,
                           const std::vector<TxDeclarationNode*>& declNodes )
            : TxBuiltinTypeDefiningNode( ploc, builtinTypeId, baseTypeNode, declNodes ), size( size ) {
    }
};

class TxBoolTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    TxActualType* make_builtin_type( const TxTypeDeclaration* declaration,
                                             const std::vector<const TxTypeExpressionNode*>& ifNodes, bool mutableType ) override {
        return new TxBoolType( declaration, this->baseTypeNode, ifNodes );
    }
public:
    TxBoolTypeDefNode( const TxLocation& ploc, TxTypeExpressionNode* baseTypeNode,
                       const std::vector<TxDeclarationNode*>& declNodes )
            : TxBuiltinTypeDefiningNode( ploc, TXBT_BOOL, baseTypeNode, declNodes ) {
    }
};

class TxTupleTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    TxActualType* make_builtin_type( const TxTypeDeclaration* declaration,
                                             const std::vector<const TxTypeExpressionNode*>& ifNodes, bool mutableType ) override {
        return new TxActualType( get_type_class_handler( TXTC_TUPLE ), declaration, mutableType, this->baseTypeNode, ifNodes );
    }
public:
    TxTupleTypeDefNode( const TxLocation& ploc, TxTypeExpressionNode* baseTypeNode,
                        const std::vector<TxDeclarationNode*>& declNodes )
            : TxBuiltinTypeDefiningNode( ploc, TXBT_TUPLE, baseTypeNode, declNodes ) {
    }
};

class TxInterfaceTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    TxActualType* make_builtin_type( const TxTypeDeclaration* declaration,
                                             const std::vector<const TxTypeExpressionNode*>& ifNodes, bool mutableType ) override {
        return new TxActualType( get_type_class_handler( TXTC_INTERFACE ), declaration, mutableType, this->baseTypeNode, ifNodes );
    }
public:
    TxInterfaceTypeDefNode( const TxLocation& ploc, TxTypeExpressionNode* baseTypeNode,
                            const std::vector<TxDeclarationNode*>& declNodes )
            : TxBuiltinTypeDefiningNode( ploc, TXBT_INTERFACE, baseTypeNode, declNodes ) {
    }
};

class TxFunctionTypeDefNode final : public TxBuiltinTypeDefiningNode {
    /** Used solely for the abstract Function base type. */
    class TxAbstractFunctionTypeClassHandler : public TxTypeClassHandler {
    public:
        TxAbstractFunctionTypeClassHandler() : TxTypeClassHandler( TXTC_FUNCTION )  { }

        llvm::Type* make_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const override {
            return llvm::StructType::get( context.get_voidPtrT(), context.get_closureRefT() );
        }
    };

    static const TxAbstractFunctionTypeClassHandler abstrFunctionTypeClassHandler;

    protected:
    TxActualType* make_builtin_type( const TxTypeDeclaration* declaration,
                                             const std::vector<const TxTypeExpressionNode*>& ifNodes, bool mutableType ) override {
        return new TxActualType( &abstrFunctionTypeClassHandler,
                                 declaration, mutableType, this->baseTypeNode, ifNodes );
    }
public:
    TxFunctionTypeDefNode( const TxLocation& ploc, TxTypeExpressionNode* baseTypeNode,
                           const std::vector<TxDeclarationNode*>& declNodes )
            : TxBuiltinTypeDefiningNode( ploc, TXBT_FUNCTION, baseTypeNode, declNodes ) {
    }
};

const TxFunctionTypeDefNode::TxAbstractFunctionTypeClassHandler TxFunctionTypeDefNode::abstrFunctionTypeClassHandler;


class TxRefTypeDefNode final : public TxBuiltinTypeDefiningNode {
    TxRefTypeDefNode( const TxLocation& ploc, const TxRefTypeDefNode* original,
                      TxTypeExpressionNode* baseTypeNode, const std::vector<TxDeclarationNode*>& declNodes, TxDerivedTypeNode* sourcecodeDefiner )
            : TxBuiltinTypeDefiningNode( ploc, original, baseTypeNode, declNodes, sourcecodeDefiner ) {
    }
protected:
    TxActualType* make_builtin_type( const TxTypeDeclaration* declaration,
                                             const std::vector<const TxTypeExpressionNode*>& ifNodes, bool mutableType ) override {
        return new TxActualType( get_type_class_handler( TXTC_REFERENCE ), declaration, mutableType, this->baseTypeNode, ifNodes );
    }
public:
    TxRefTypeDefNode( const TxLocation& ploc, TxTypeExpressionNode* baseTypeNode,
                      const std::vector<TxDeclarationNode*>& declNodes )
            : TxBuiltinTypeDefiningNode( ploc, TXBT_REFERENCE, baseTypeNode, declNodes ) {
    }

    TxRefTypeDefNode* make_ast_copy() const override {
        return new TxRefTypeDefNode( this->ploc, this, this->baseTypeNode->make_ast_copy(), make_node_vec_copy( this->declNodes ),
                                     ( this->sourcecodeDefiner ? this->sourcecodeDefiner->make_ast_copy() : nullptr ) );
    }
};

class TxArrayTypeDefNode final : public TxBuiltinTypeDefiningNode {
    TxArrayTypeDefNode( const TxLocation& ploc, const TxArrayTypeDefNode* original,
                        TxTypeExpressionNode* baseTypeNode, const std::vector<TxDeclarationNode*>& declNodes, TxDerivedTypeNode* sourcecodeDefiner )
            : TxBuiltinTypeDefiningNode( ploc, original, baseTypeNode, declNodes, sourcecodeDefiner ) {
    }
protected:
    TxActualType* make_builtin_type( const TxTypeDeclaration* declaration,
                                             const std::vector<const TxTypeExpressionNode*>& ifNodes, bool mutableType ) override {
        return new TxActualType( get_type_class_handler( TXTC_ARRAY ), declaration, mutableType, this->baseTypeNode, ifNodes );
    }
public:
    TxArrayTypeDefNode( const TxLocation& ploc, TxTypeExpressionNode* baseTypeNode,
                        const std::vector<TxDeclarationNode*>& declNodes )
            : TxBuiltinTypeDefiningNode( ploc, TXBT_ARRAY, baseTypeNode, declNodes ) {
    }

    TxArrayTypeDefNode* make_ast_copy() const override {
        return new TxArrayTypeDefNode( this->ploc, this, this->baseTypeNode->make_ast_copy(), make_node_vec_copy( this->declNodes ),
                                       ( this->sourcecodeDefiner ? this->sourcecodeDefiner->make_ast_copy() : nullptr ) );
    }
};

/*----- built-in constructor / initializer type defining AST nodes -----*/

class TxBuiltinConstructorTypeDefNode : public TxFunctionTypeNode {
protected:

public:
    TxBuiltinConstructorTypeDefNode( const TxLocation& ploc, std::vector<TxArgTypeDefNode*>* arguments, TxTypeExpressionNode* returnType )
            : TxFunctionTypeNode( ploc, false, arguments, returnType ) {
    }

    TxBuiltinConstructorTypeDefNode* make_ast_copy() const override = 0;
};

class TxDefConstructorTypeDefNode final : public TxBuiltinConstructorTypeDefNode {
protected:
    TxExpressionNode* initExprNode;

    TxActualType* create_type( TxPassInfo passInfo ) override {
        auto actType = new TxBuiltinDefaultConstructorType( this->get_declaration(), this->registry().get_builtin_type( TXBT_FUNCTION ),
                                                            this->returnField->resolve_type( passInfo ).type(),
                                                            initExprNode );
        return actType;
    }

public:
    TxDefConstructorTypeDefNode( const TxLocation& ploc, TxTypeExpressionNode* returnTypeNode, TxExpressionNode* initExprNode )
            : TxBuiltinConstructorTypeDefNode( ploc, new std::vector<TxArgTypeDefNode*>(), returnTypeNode ), initExprNode( initExprNode ) {
    }

    TxDefConstructorTypeDefNode* make_ast_copy() const override {
        return new TxDefConstructorTypeDefNode( this->ploc, this->returnField->typeExpression->make_ast_copy(),
                                                initExprNode->make_ast_copy() );
    }

    void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        TxBuiltinConstructorTypeDefNode::visit_descendants( visitor, thisCursor, role, context );
        this->initExprNode->visit_ast( visitor, thisCursor, "initializer", context );
    }
};

class TxConvConstructorTypeDefNode final : public TxBuiltinConstructorTypeDefNode {
protected:
    TxActualType* create_type( TxPassInfo passInfo ) override {
        auto actType = new TxBuiltinConversionFunctionType( this->get_declaration(), this->registry().get_builtin_type( TXBT_FUNCTION ),
                                                            this->arguments->at( 0 )->resolve_type( passInfo ).type(),
                                                            this->returnField->resolve_type( passInfo ).type() );
        return actType;
    }

public:
    TxConvConstructorTypeDefNode( const TxLocation& ploc, TxArgTypeDefNode* fromTypeArg, TxTypeExpressionNode* returnTypeNode )
            : TxBuiltinConstructorTypeDefNode( ploc, new std::vector<TxArgTypeDefNode*>( { fromTypeArg } ), returnTypeNode ) {
    }

    TxConvConstructorTypeDefNode* make_ast_copy() const override {
        return new TxConvConstructorTypeDefNode( this->ploc, this->arguments->at( 0 )->make_ast_copy(),
                                                 this->returnField->typeExpression->make_ast_copy() );
    }
};

/** Assignment initializer, argument type is the same as the return type. */
class TxAssignmentConstructorTypeDefNode final : public TxBuiltinConstructorTypeDefNode {
protected:
    TxActualType* create_type( TxPassInfo passInfo ) override {
        auto actType = new TxBuiltinAssignInitializerType( this->get_declaration(), this->registry().get_builtin_type( TXBT_FUNCTION ),
                                                           this->arguments->at( 0 )->resolve_type( passInfo ).type() );
        return actType;
    }

public:
    TxAssignmentConstructorTypeDefNode( const TxLocation& ploc, TxArgTypeDefNode* fromTypeArg )
            : TxBuiltinConstructorTypeDefNode( ploc, new std::vector<TxArgTypeDefNode*>( { fromTypeArg } ),
                                               new TxTypeExprWrapperNode( fromTypeArg->typeExpression ) ) {
    }

    TxAssignmentConstructorTypeDefNode* make_ast_copy() const override {
        return new TxAssignmentConstructorTypeDefNode( this->ploc, this->arguments->at( 0 )->make_ast_copy() );
    }
};

/*----- helper functions creating the built-in elementary types' declarations and constructors -----*/

static TxTypeDeclNode* make_builtin_abstract( const TxLocation& parseLoc, TxTypeClass typeClass, BuiltinTypeId id, BuiltinTypeId parentId ) {
    auto baseTypeNode = new TxNamedTypeNode( parseLoc, BUILTIN_TYPE_NAMES[parentId] );
    auto typeDecl = new TxTypeDeclNode( parseLoc, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT,
                                        new TxIdentifierNode( parseLoc, BUILTIN_TYPE_NAMES[id] ), nullptr,
                                        new TxBuiltinAbstractTypeDefNode( parseLoc, id, baseTypeNode, typeClass, { } ), false, true );
    return typeDecl;
}

static TxTypeDeclNode* make_builtin_integer( const TxLocation& parseLoc, BuiltinTypeId id, BuiltinTypeId parentId,
                                             std::vector<std::vector<TxDeclarationNode*>>& constructors,
                                             int size, bool sign ) {
    auto baseTypeNode = new TxNamedTypeNode( parseLoc, BUILTIN_TYPE_NAMES[parentId] );
    auto typeDecl = new TxTypeDeclNode( parseLoc, TXD_PUBLIC | TXD_BUILTIN | TXD_FINAL,
                                        new TxIdentifierNode( parseLoc, BUILTIN_TYPE_NAMES[id] ), nullptr,
                                        new TxIntegerTypeDefNode( parseLoc, id, baseTypeNode, size, sign, constructors[id] ), false, true );
    return typeDecl;
}

static TxTypeDeclNode* make_builtin_floating( const TxLocation& parseLoc, BuiltinTypeId id, BuiltinTypeId parentId,
                                              std::vector<std::vector<TxDeclarationNode*>>& constructors,
                                              int size ) {
    auto baseTypeNode = new TxNamedTypeNode( parseLoc, BUILTIN_TYPE_NAMES[parentId] );
    auto typeDecl = new TxTypeDeclNode( parseLoc, TXD_PUBLIC | TXD_BUILTIN | TXD_FINAL,
                                        new TxIdentifierNode( parseLoc, BUILTIN_TYPE_NAMES[id] ), nullptr,
                                        new TxFloatingTypeDefNode( parseLoc, id, baseTypeNode, size, constructors[id] ), false, true );
    return typeDecl;
}

static TxFieldDeclNode* make_default_initializer( const TxLocation& loc, BuiltinTypeId toTypeId, TxExpressionNode* initializerExpr ) {
    auto toTypeNode = new TxNamedTypeNode( loc, BUILTIN_TYPE_NAMES[toTypeId] );
    return new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_VIRTUAL | TXD_BUILTIN | TXD_INITIALIZER,
                                new TxNonLocalFieldDefNode( loc, new TxIdentifierNode( loc, CONSTR_IDENT ),
                                                            new TxDefConstructorTypeDefNode( loc, toTypeNode, initializerExpr ),
                                                            nullptr ),  // no function body, initialization is inlined
                                false );  // not method syntax since elementary types' initializers are inlineable, pure functions
}

static TxFieldDeclNode* make_conversion_initializer( const TxLocation& loc, BuiltinTypeId fromTypeId, BuiltinTypeId toTypeId ) {
    auto toTypeNode = new TxNamedTypeNode( loc, BUILTIN_TYPE_NAMES[toTypeId] );
    auto fromTypeNode = new TxNamedTypeNode( loc, BUILTIN_TYPE_NAMES[fromTypeId] );
    return new TxFieldDeclNode(
            loc, TXD_PUBLIC | TXD_VIRTUAL | TXD_BUILTIN | TXD_INITIALIZER,
            new TxNonLocalFieldDefNode( loc, new TxIdentifierNode( loc, CONSTR_IDENT ),
                                        new TxConvConstructorTypeDefNode(
                                                loc, new TxArgTypeDefNode( loc, "arg", fromTypeNode ), toTypeNode ),
                                        nullptr ),  // no function body, initialization is inlined
            false );  // not method syntax since elementary types' initializers are inlineable, pure functions
}

static std::vector<TxDeclarationNode*> make_any_methods( TxParserContext* pctx ) {
/*  Built-in definitions corresponding to the following Tuplex code for the Any root type:

### @return self by default
key() -> &Any
    return self;

### Compares key object keys' identity by default
equals( other : &Any )->Bool
    return self.key() == other.key();
*/
    std::vector<TxDeclarationNode*> methods;
    { //  define key() -> &Any
        auto retStmt = new TxReturnStmtNode( PLOC(pctx), new TxNamedFieldNode( PLOC(pctx), "self" ) );
        auto methodType = new TxFunctionTypeNode( PLOC(pctx), false, new std::vector<TxArgTypeDefNode*>( {} ),
                                                  new TxReferenceTypeNode( PLOC(pctx), nullptr, new TxNamedTypeNode( PLOC(pctx), "tx.Any" ) ) );
        auto lambdaExpr = new TxLambdaExprNode( PLOC(pctx), methodType,
                                                new TxSuiteNode( PLOC(pctx), new std::vector<TxStatementNode*>( { retStmt } ) ), true );
        methods.push_back( new TxFieldDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_BUILTIN,
                                                new TxNonLocalFieldDefNode( PLOC(pctx), new TxIdentifierNode( PLOC(pctx), "key" ),
                                                                            lambdaExpr, false ),
                                                true ) );  // method syntax
    }
    { //  define equals( other: &Any ) -> Bool
        auto selfKeyCall = new TxFunctionCallNode( PLOC(pctx), new TxNamedFieldNode( PLOC(pctx), "self.key" ),
                                                   new std::vector<TxExpressionNode*>( { } ) );
        auto otherKeyCall = new TxFunctionCallNode( PLOC(pctx), new TxNamedFieldNode( PLOC(pctx), "other.key" ),
                                                    new std::vector<TxExpressionNode*>( { } ) );
        auto eqStmt = new TxReturnStmtNode( PLOC(pctx), new TxEqualityOperatorNode( PLOC(pctx), selfKeyCall, otherKeyCall ) );

        auto argNode = new TxArgTypeDefNode( PLOC(pctx), "other", new TxReferenceTypeNode( PLOC(pctx), nullptr, new TxNamedTypeNode( PLOC(pctx), "tx.Any" ) ) );
        auto methodType = new TxFunctionTypeNode( PLOC(pctx), false, new std::vector<TxArgTypeDefNode*>( { argNode } ),
                                                  new TxNamedTypeNode( PLOC(pctx), "tx.Bool" ) );
        auto lambdaExpr = new TxLambdaExprNode( PLOC(pctx), methodType,
                                                new TxSuiteNode( PLOC(pctx), new std::vector<TxStatementNode*>( { eqStmt } ) ), true );
        methods.push_back( new TxFieldDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_BUILTIN,
                                                new TxNonLocalFieldDefNode( PLOC(pctx), new TxIdentifierNode( PLOC(pctx), "equals" ),
                                                                            lambdaExpr, false ),
                                                true ) );  // method syntax
    }
    return methods;
}

static std::vector<TxDeclarationNode*> make_array_methods( TxParserContext* pctx ) {
    std::vector<TxDeclarationNode*> methods;
    { // default constructor - this does nothing (it presumes memory allocation logic will initialize Array.C)
        auto constrType = new TxFunctionTypeNode( PLOC(pctx), false, new std::vector<TxArgTypeDefNode*>(), nullptr );
        auto lambdaExpr = new TxLambdaExprNode( PLOC(pctx), constrType, new TxSuiteNode( PLOC(pctx) ), true );
        methods.push_back( new TxFieldDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_BUILTIN | TXD_CONSTRUCTOR,
                                                new TxNonLocalFieldDefNode( PLOC(pctx), new TxIdentifierNode( PLOC(pctx), CONSTR_IDENT ),
                                                                            lambdaExpr, false ),
                                                true ) );  // method syntax since regular constructor
    }
    { // copy constructor
        auto selfSuperStmt = new TxSelfSuperFieldsStmtNode( PLOC(pctx) );
        auto copyStmt = new TxAssignStmtNode( PLOC(pctx),
                                              new TxDerefAssigneeNode( PLOC(pctx), new TxNamedFieldNode( PLOC(pctx), "self" ) ),
                                              new TxReferenceDerefNode( PLOC(pctx), new TxNamedFieldNode( PLOC(pctx), "src" ) ) );
        auto arrayTypeNode = new TxArrayTypeNode( PLOC(pctx), new TxConstTypeNode( PLOC(pctx), new TxNamedTypeNode( PLOC(pctx), "E" ) ) );
        auto argTypeNode = new TxReferenceTypeNode( PLOC(pctx), nullptr, arrayTypeNode );
        auto argNode = new TxArgTypeDefNode( PLOC(pctx), "src", argTypeNode );
        auto constrType = new TxFunctionTypeNode( PLOC(pctx), false, new std::vector<TxArgTypeDefNode*>( { argNode } ), nullptr );
        auto suite = new TxSuiteNode( PLOC(pctx), new std::vector<TxStatementNode*>( { selfSuperStmt, copyStmt } ) );
        auto lambdaExpr = new TxLambdaExprNode( PLOC(pctx), constrType, suite, true );
        methods.push_back( new TxFieldDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_BUILTIN | TXD_CONSTRUCTOR,
                                                new TxNonLocalFieldDefNode( PLOC(pctx), new TxIdentifierNode( PLOC(pctx), CONSTR_IDENT ),
                                                                            lambdaExpr, false ),
                                                true ) );  // method syntax since regular constructor
    }
    { //  override clear() ~ (from Collection{E})
        auto clearStmt = new TxAssignStmtNode( PLOC(pctx),
                                               new TxArrayLenAssigneeNode( PLOC(pctx), new TxNamedFieldNode( PLOC(pctx), "self" ) ),
                                               new TxIntegerLitNode( PLOC(pctx), 0, false, TXBT_UINT ) );
        auto methodType = new TxFunctionTypeNode( PLOC(pctx), true, new std::vector<TxArgTypeDefNode*>(), nullptr );
        auto lambdaExpr = new TxLambdaExprNode( PLOC(pctx), methodType,
                                                new TxSuiteNode( PLOC(pctx), new std::vector<TxStatementNode*>( { clearStmt } ) ), true );
        methods.push_back( new TxFieldDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_BUILTIN | TXD_OVERRIDE,
                                                new TxNonLocalFieldDefNode( PLOC(pctx), new TxIdentifierNode( PLOC(pctx), "clear" ),
                                                                            lambdaExpr, false ),
                                                true ) );  // method syntax
    }
    return methods;
}

static std::vector<TxDeclarationNode*> make_panic_functions( const TxLocation& loc ) {
    std::vector<TxDeclarationNode*> functions;
    { // tx.panic( message : &[]UByte )
        TxSuiteNode* suiteNode;
        {
            auto msgExpr = new TxNamedFieldNode( loc, "msg" );
            auto stderrArg = new TxNamedFieldNode( loc, "tx.c.stderr" );
            auto putsCallee = new TxNamedFieldNode( loc, "tx.c.fputs" );
            auto putsCallExpr = new TxFunctionCallNode( loc, putsCallee, new std::vector<TxExpressionNode*>( { msgExpr, stderrArg } ) );
            TxStatementNode* putsStmt = new TxCallStmtNode( loc, putsCallExpr );

            // we call c library abort() upon assertion failure
            auto abortCallee = new TxNamedFieldNode( loc, "tx.c.abort" );
            auto abortCallExpr = new TxFunctionCallNode( loc, abortCallee, new std::vector<TxExpressionNode*>(), true );
            TxStatementNode* abortStmt = new TxCallStmtNode( loc, abortCallExpr );

            suiteNode = new TxSuiteNode( loc, new std::vector<TxStatementNode*>( { putsStmt, abortStmt } ) );
        }

        auto argTypeNode = new TxReferenceTypeNode( loc, nullptr, new TxArrayTypeNode( loc, new TxNamedTypeNode( loc, "tx.UByte" ) ) );
        auto argNode = new TxArgTypeDefNode( loc, "msg", argTypeNode );
        auto funcType = new TxFunctionTypeNode( loc, false, new std::vector<TxArgTypeDefNode*>( { argNode } ), nullptr );
        auto lambdaExpr = new TxLambdaExprNode( loc, funcType, suiteNode, false );
        lambdaExpr->add_function_attribute( llvm::Attribute::AttrKind::NoInline );

        functions.push_back( new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_BUILTIN,
                                                  new TxNonLocalFieldDefNode( loc, new TxIdentifierNode( loc, "panic" ),
                                                                              lambdaExpr, false ),
                                                  false ) );
    }

    { // tx.panic( message : &[]UByte, value : ULong )
        TxSuiteNode* suiteNode;
        {
            auto msgExpr = new TxNamedFieldNode( loc, "msg" );
            auto valExpr = new TxNamedFieldNode( loc, "val" );
            auto stderrArg = new TxNamedFieldNode( loc, "tx.c.stderr" );
            auto printfCallee = new TxNamedFieldNode( loc, "tx.c.fprintf" );
            auto printfCallExpr = new TxFunctionCallNode( loc, printfCallee, new std::vector<TxExpressionNode*>( { stderrArg, msgExpr, valExpr } ) );
            TxStatementNode* putsStmt = new TxCallStmtNode( loc, printfCallExpr );

            // we call c library abort() upon assertion failure
            auto abortCallee = new TxNamedFieldNode( loc, "tx.c.abort" );
            auto abortCallExpr = new TxFunctionCallNode( loc, abortCallee, new std::vector<TxExpressionNode*>(), true );
            TxStatementNode* abortStmt = new TxCallStmtNode( loc, abortCallExpr );

            suiteNode = new TxSuiteNode( loc, new std::vector<TxStatementNode*>( { putsStmt, abortStmt } ) );
        }

        auto msgArgTypeNode = new TxReferenceTypeNode( loc, nullptr, new TxArrayTypeNode( loc, new TxNamedTypeNode( loc, "tx.UByte" ) ) );
        auto msgArgNode = new TxArgTypeDefNode( loc, "msg", msgArgTypeNode );
        auto valArgTypeNode = new TxNamedTypeNode( loc, "tx.ULong" );
        auto valArgNode = new TxArgTypeDefNode( loc, "val", valArgTypeNode );
        auto funcType = new TxFunctionTypeNode( loc, false, new std::vector<TxArgTypeDefNode*>( { msgArgNode, valArgNode } ), nullptr );
        auto lambdaExpr = new TxLambdaExprNode( loc, funcType, suiteNode, false );
        lambdaExpr->add_function_attribute( llvm::Attribute::AttrKind::NoInline );

        functions.push_back( new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_BUILTIN,
                                                  new TxNonLocalFieldDefNode( loc, new TxIdentifierNode( loc, "panic" ),
                                                                              lambdaExpr, false ),
                                                  false ) );
    }

    /*
    if ( !loc.parserCtx->driver().get_options().txPath.empty() )  // if foundation library included in compilation
    { // tx.panic( message : &String )
        const unsigned NOF_ARGS = 3;
        TxSuiteNode* suiteNode;
        {
            std::vector<TxExpressionNode*> stringers;
            for ( unsigned i = 0; i < NOF_ARGS; i++ ) {
                stringers.push_back( new TxNamedFieldNode( loc, "msg"+std::to_string(i) ) );
            }
            auto panicMsgExpr = new TxConcatenateStringsNode( loc, stringers );
            auto printCallee = new TxNamedFieldNode( loc, "tx.print_err" );
            auto printCallExpr = new TxFunctionCallNode( loc, printCallee, new std::vector<TxExpressionNode*>( { panicMsgExpr } ) );
            TxStatementNode* printStmt = new TxCallStmtNode( loc, printCallExpr );

            // we call c library abort() upon assertion failure
            auto abortCallee = new TxNamedFieldNode( loc, "tx.c.abort" );
            auto abortCallExpr = new TxFunctionCallNode( loc, abortCallee, new std::vector<TxExpressionNode*>(), true );
            TxStatementNode* abortStmt = new TxCallStmtNode( loc, abortCallExpr );

            suiteNode = new TxSuiteNode( loc, new std::vector<TxStatementNode*>( { printStmt, abortStmt } ) );
        }

        auto argDefs = new std::vector<TxArgTypeDefNode*>();
        for ( unsigned i = 0; i < NOF_ARGS; i++ ) {
            auto argTypeNode = new TxReferenceTypeNode( loc, nullptr, new TxNamedTypeNode( loc, "tx.Stringer" ) );
            argDefs->push_back( new TxArgTypeDefNode( loc, "msg"+std::to_string(i), argTypeNode ) );
        }
        auto funcType = new TxFunctionTypeNode( loc, false, argDefs, nullptr );
        auto lambdaExpr = new TxLambdaExprNode( loc, funcType, suiteNode, false );

        functions.push_back( new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_BUILTIN,
                                                  new TxNonLocalFieldDefNode( loc, "panic", lambdaExpr, false ),
                                                  false ) );
    }
    */
    return functions;
}

TxParsingUnitNode* BuiltinTypes::createTxModuleAST() {
    auto pctx = this->builtinParserContext;

    { // create the Any root type:
        auto anyMembers = make_any_methods( pctx );
        auto anyTypeDecl = new TxTypeDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, new TxIdentifierNode( PLOC(pctx), "Any" ), nullptr,
                                               new TxAnyTypeDefNode( PLOC(pctx), anyMembers ), false, true );
        this->builtinTypes[TXBT_ANY] = anyTypeDecl;
    }

    { // create the Void type:
        auto voidBaseTypeNode = new TxNamedTypeNode( PLOC(pctx), BUILTIN_TYPE_NAMES[TXBT_ANY] );
        auto voidTypeDecl = new TxTypeDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT | TXD_FINAL, new TxIdentifierNode( PLOC(pctx), "Void" ), nullptr,
                                                new TxVoidTypeDefNode( PLOC(pctx), voidBaseTypeNode ), false, false );
        this->builtinTypes[TXBT_VOID] = voidTypeDecl;
    }

    // default initializers for elementary, concrete built-ins:
    std::vector<std::vector<TxDeclarationNode*>> constructors( BuiltinTypeId_COUNT );
    constructors[TXBT_BOOL].push_back( make_default_initializer( PLOC(pctx), TXBT_BOOL, new TxBoolLitNode( PLOC(pctx), false ) ) );
    constructors[TXBT_BYTE].push_back( make_default_initializer( PLOC(pctx), TXBT_BYTE, new TxIntegerLitNode( PLOC(pctx), 0, true, TXBT_BYTE ) ) );
    constructors[TXBT_SHORT].push_back( make_default_initializer( PLOC(pctx), TXBT_SHORT, new TxIntegerLitNode( PLOC(pctx), 0, true, TXBT_SHORT ) ) );
    constructors[TXBT_INT].push_back( make_default_initializer( PLOC(pctx), TXBT_INT, new TxIntegerLitNode( PLOC(pctx), 0, true, TXBT_INT ) ) );
    constructors[TXBT_LONG].push_back( make_default_initializer( PLOC(pctx), TXBT_LONG, new TxIntegerLitNode( PLOC(pctx), 0, true, TXBT_LONG ) ) );
    constructors[TXBT_UBYTE].push_back( make_default_initializer( PLOC(pctx), TXBT_UBYTE, new TxIntegerLitNode( PLOC(pctx), 0, false, TXBT_UBYTE ) ) );
    constructors[TXBT_USHORT].push_back( make_default_initializer( PLOC(pctx), TXBT_USHORT, new TxIntegerLitNode( PLOC(pctx), 0, false, TXBT_USHORT ) ) );
    constructors[TXBT_UINT].push_back( make_default_initializer( PLOC(pctx), TXBT_UINT, new TxIntegerLitNode( PLOC(pctx), 0, false, TXBT_UINT ) ) );
    constructors[TXBT_ULONG].push_back( make_default_initializer( PLOC(pctx), TXBT_ULONG, new TxIntegerLitNode( PLOC(pctx), 0, false, TXBT_ULONG ) ) );
    constructors[TXBT_HALF].push_back( make_default_initializer( PLOC(pctx), TXBT_HALF, new TxFloatingLitNode( PLOC(pctx), TXBT_HALF ) ) );
    constructors[TXBT_FLOAT].push_back( make_default_initializer( PLOC(pctx), TXBT_FLOAT, new TxFloatingLitNode( PLOC(pctx), TXBT_FLOAT ) ) );
    constructors[TXBT_DOUBLE].push_back( make_default_initializer( PLOC(pctx), TXBT_DOUBLE, new TxFloatingLitNode( PLOC(pctx), TXBT_DOUBLE ) ) );

    // built-in conversion-initializers between the concrete elementary types (BOOL and the scalar types):
    const BuiltinTypeId CONCRETE_ELEM_TYPE_IDS[] = {
                                                     TXBT_BOOL,
                                                     TXBT_BYTE,
                                                     TXBT_SHORT,
                                                     TXBT_INT,
                                                     TXBT_LONG,
                                                     TXBT_UBYTE,
                                                     TXBT_USHORT,
                                                     TXBT_UINT,
                                                     TXBT_ULONG,
                                                     TXBT_HALF,
                                                     TXBT_FLOAT,
                                                     TXBT_DOUBLE,
    };
    for ( auto toTypeId : CONCRETE_ELEM_TYPE_IDS ) {
        for ( auto fromTypeId : CONCRETE_ELEM_TYPE_IDS )
            constructors[toTypeId].push_back( make_conversion_initializer( PLOC(pctx), fromTypeId, toTypeId ) );
    }

    // create the built-in abstract base types:
    this->builtinTypes[TXBT_ELEMENTARY] = make_builtin_abstract( PLOC(pctx), TXTC_ELEMENTARY, TXBT_ELEMENTARY, TXBT_ANY );
    this->builtinTypes[TXBT_SCALAR] = make_builtin_abstract( PLOC(pctx), TXTC_ELEMENTARY, TXBT_SCALAR, TXBT_ELEMENTARY );
    this->builtinTypes[TXBT_INTEGER] = make_builtin_abstract( PLOC(pctx), TXTC_ELEMENTARY, TXBT_INTEGER, TXBT_SCALAR );
    this->builtinTypes[TXBT_SIGNED] = make_builtin_abstract( PLOC(pctx), TXTC_ELEMENTARY, TXBT_SIGNED, TXBT_INTEGER );
    this->builtinTypes[TXBT_UNSIGNED] = make_builtin_abstract( PLOC(pctx), TXTC_ELEMENTARY, TXBT_UNSIGNED, TXBT_INTEGER );
    this->builtinTypes[TXBT_FLOATINGPOINT] = make_builtin_abstract( PLOC(pctx), TXTC_ELEMENTARY, TXBT_FLOATINGPOINT, TXBT_SCALAR );

    // create the built-in concrete scalar types:
    this->builtinTypes[TXBT_BYTE] = make_builtin_integer( PLOC(pctx), TXBT_BYTE, TXBT_SIGNED, constructors, 1, true );
    this->builtinTypes[TXBT_SHORT] = make_builtin_integer( PLOC(pctx), TXBT_SHORT, TXBT_SIGNED, constructors, 2, true );
    this->builtinTypes[TXBT_INT] = make_builtin_integer( PLOC(pctx), TXBT_INT, TXBT_SIGNED, constructors, 4, true );
    this->builtinTypes[TXBT_LONG] = make_builtin_integer( PLOC(pctx), TXBT_LONG, TXBT_SIGNED, constructors, 8, true );
    this->builtinTypes[TXBT_UBYTE] = make_builtin_integer( PLOC(pctx), TXBT_UBYTE, TXBT_UNSIGNED, constructors, 1, false );
    this->builtinTypes[TXBT_USHORT] = make_builtin_integer( PLOC(pctx), TXBT_USHORT, TXBT_UNSIGNED, constructors, 2, false );
    this->builtinTypes[TXBT_UINT] = make_builtin_integer( PLOC(pctx), TXBT_UINT, TXBT_UNSIGNED, constructors, 4, false );
    this->builtinTypes[TXBT_ULONG] = make_builtin_integer( PLOC(pctx), TXBT_ULONG, TXBT_UNSIGNED, constructors, 8, false );
    this->builtinTypes[TXBT_HALF] = make_builtin_floating( PLOC(pctx), TXBT_HALF, TXBT_FLOATINGPOINT, constructors, 2 );
    this->builtinTypes[TXBT_FLOAT] = make_builtin_floating( PLOC(pctx), TXBT_FLOAT, TXBT_FLOATINGPOINT, constructors, 4 );
    this->builtinTypes[TXBT_DOUBLE] = make_builtin_floating( PLOC(pctx), TXBT_DOUBLE, TXBT_FLOATINGPOINT, constructors, 8 );

    // create the boolean type:
    this->builtinTypes[TXBT_BOOL] = new TxTypeDeclNode(
            PLOC(pctx), TXD_PUBLIC | TXD_BUILTIN | TXD_FINAL, new TxIdentifierNode( PLOC(pctx), "Bool" ), nullptr,
            new TxBoolTypeDefNode( PLOC(pctx), new TxNamedTypeNode( PLOC(pctx), "Elementary" ), constructors[TXBT_BOOL] ), false, true );

    // create the function base type:
    this->builtinTypes[TXBT_FUNCTION] = new TxTypeDeclNode(
            PLOC(pctx), TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, new TxIdentifierNode( PLOC(pctx), "Function" ), nullptr,
            new TxFunctionTypeDefNode( PLOC(pctx), new TxNamedTypeNode( PLOC(pctx), "Any" ), { } ), false, true );
            //new TxBuiltinAbstractTypeDefNode( PLOC(pctx), TXBT_FUNCTION, new TxNamedTypeNode( PLOC(pctx), "Any" ), TXTC_FUNCTION, { } ), false, true );

    // create the tuple base type:
    this->builtinTypes[TXBT_TUPLE] = new TxTypeDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_BUILTIN,
                                                         new TxIdentifierNode( PLOC(pctx), "Tuple" ),
                                                         nullptr,
                                                         new TxTupleTypeDefNode( PLOC(pctx), new TxNamedTypeNode( PLOC(pctx), "Any" ), { } ),
                                                         false, true );

    // create the reference base type:
    {
        auto refInitializer = new TxFieldDeclNode(
                PLOC( pctx ),
                TXD_PUBLIC | TXD_VIRTUAL | TXD_BUILTIN | TXD_INITIALIZER,
                new TxNonLocalFieldDefNode(
                        PLOC( pctx ),
                        new TxIdentifierNode( PLOC( pctx ), CONSTR_IDENT ),
                        new TxAssignmentConstructorTypeDefNode(
                                PLOC( pctx ), new TxArgTypeDefNode( PLOC( pctx ), "val", new TxNamedTypeNode( PLOC( pctx ), "Self" ) ) ),
                        nullptr ),  // no function body, initialization is inlined
                false );  // not method syntax since built-in types' initializers are inlineable, pure functions

        auto paramNodes = new std::vector<TxDeclarationNode*>( {
            new TxTypeDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_GENPARAM, new TxIdentifierNode( PLOC(pctx), "T" ), nullptr,
                                new TxGenParamTypeNode( PLOC(pctx), new TxNamedTypeNode( PLOC(pctx), "Any" ) ) )
        } );
        this->builtinTypes[TXBT_REFERENCE] = new TxTypeDeclNode(
                PLOC( pctx ), TXD_PUBLIC | TXD_BUILTIN, new TxIdentifierNode( PLOC( pctx ), "Ref" ), paramNodes,
                new TxRefTypeDefNode( PLOC( pctx ), new TxNamedTypeNode( PLOC( pctx ), "Any" ), { refInitializer } ),
                false, true );
    }

    // create the array base type:
    {
        auto arrayMembers = make_array_methods( pctx );
        arrayMembers.push_back( new TxFieldDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_IMPLICIT | TXD_BUILTIN,
                                                     new TxNonLocalFieldDefNode( PLOC(pctx), new TxIdentifierNode( PLOC(pctx), "L" ),
                                                                                 new TxNamedTypeNode( PLOC(pctx), "UInt" ), nullptr ) ) );

        auto paramNodes = new std::vector<TxDeclarationNode*>( {
            new TxTypeDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_GENPARAM, new TxIdentifierNode( PLOC(pctx), "E" ), nullptr,
                                new TxGenParamTypeNode( PLOC(pctx), new TxNamedTypeNode( PLOC(pctx), "Any" ) ) ),
            new TxFieldDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_GENPARAM,
                                 new TxNonLocalFieldDefNode( PLOC(pctx), new TxIdentifierNode( PLOC(pctx), "C" ),
                                                             new TxNamedTypeNode( PLOC(pctx), "UInt" ), nullptr ) ),
        } );
        this->builtinTypes[TXBT_ARRAY] = new TxTypeDeclNode(
                PLOC(pctx), TXD_PUBLIC | TXD_BUILTIN, new TxIdentifierNode( PLOC(pctx), "Array" ), paramNodes,
                new TxArrayTypeDefNode( PLOC(pctx), new TxNamedTypeNode( PLOC(pctx), "Any" ), arrayMembers ), false, true );
    }

    // create the interface base type:
    {
        // the adaptee type id virtual field member, which is abstract here but concrete in adapter subtypes:
        const TxDeclarationFlags adapteeIdFieldFlags = TXD_PUBLIC | TXD_BUILTIN | TXD_VIRTUAL | TXD_ABSTRACT | TXD_IMPLICIT;
        auto adapteeIdFType = new TxNamedTypeNode( PLOC(pctx), "UInt" );
        auto adapteeIdField = new TxNonLocalFieldDefNode( PLOC(pctx), new TxIdentifierNode( PLOC(pctx), "$adTypeId" ), adapteeIdFType, nullptr );
        auto adapteeIdFDecl = new TxFieldDeclNode( PLOC(pctx), adapteeIdFieldFlags, adapteeIdField );

        auto ifTypeDef = new TxInterfaceTypeDefNode( PLOC(pctx), new TxNamedTypeNode( PLOC(pctx), "Any" ), { adapteeIdFDecl } );
        this->builtinTypes[TXBT_INTERFACE] = new TxTypeDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT,
                                                                 new TxIdentifierNode( PLOC(pctx), "Interface" ),
                                                                 nullptr, ifTypeDef, true, true );
    }

    // put it all together as the AST for the tx module:

    std::vector<TxImportNode*>* imports = nullptr;
    std::vector<TxModuleNode*>* subModules = new std::vector<TxModuleNode*>();
    std::vector<TxDeclarationNode*>* members = new std::vector<TxDeclarationNode*>();

    for ( unsigned id = 0; id < BuiltinTypeId_COUNT; id++ ) {
        ASSERT( this->builtinTypes[id], "not yet coded builtin type id: " << id );
        members->push_back( this->builtinTypes[id] );
    }

    for ( auto func : make_panic_functions( PLOC(pctx) ) ) {
        members->push_back( func );
    }

    subModules->push_back( this->create_tx_c_module() );

    auto module = new TxModuleNode( this->builtinLocation, new TxIdentifier( BUILTIN_NS ),
                                    imports,
                                    members, subModules, true );
    auto parsingUnit = new TxParsingUnitNode( this->builtinLocation, module );
    return parsingUnit;
}

/** declares the built-in entities of the tx.c module */
TxModuleNode* BuiltinTypes::create_tx_c_module() {
    auto pctx = this->builtinParserContext;
    std::vector<TxDeclarationNode*>* members = new std::vector<TxDeclarationNode*>();

    {   // declare tx.c.puts:
        auto cstrArgType = new TxReferenceTypeNode( PLOC(pctx), nullptr, new TxArrayTypeNode( PLOC(pctx), new TxNamedTypeNode( PLOC(pctx), "tx.UByte" ) ) );
        auto args = new std::vector<TxArgTypeDefNode*>( { new TxArgTypeDefNode( PLOC(pctx), "cstr", cstrArgType ) } );
        auto putsDecl = new TxFieldDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_EXTERNC | TXD_BUILTIN,
                                             new TxNonLocalFieldDefNode( PLOC(pctx), new TxIdentifierNode( PLOC(pctx), "puts" ),
                                                                         new TxFunctionTypeNode( PLOC(pctx), false, args,
                                                                                                 new TxNamedTypeNode( PLOC(pctx), "tx.Int" ) ),
                                                                         nullptr ) );
        members->push_back( putsDecl );
    }

    {   // declare tx.c.fputs:
        auto cstrArgType = new TxReferenceTypeNode( PLOC(pctx), nullptr, new TxArrayTypeNode( PLOC(pctx), new TxNamedTypeNode( PLOC(pctx), "tx.UByte" ) ) );
        auto fileArgType = new TxNamedTypeNode( PLOC(pctx), "tx.ULong" );
        auto args = new std::vector<TxArgTypeDefNode*>( { new TxArgTypeDefNode( PLOC(pctx), "cstr", cstrArgType ),
                                                          new TxArgTypeDefNode( PLOC(pctx), "file", fileArgType ) } );
        auto fputsDecl = new TxFieldDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_EXTERNC | TXD_BUILTIN,
                                              new TxNonLocalFieldDefNode( PLOC(pctx), new TxIdentifierNode( PLOC(pctx), "fputs" ),
                                                                          new TxFunctionTypeNode( PLOC(pctx), false, args,
                                                                                                  new TxNamedTypeNode( PLOC(pctx), "tx.Int" ) ),
                                                                          nullptr ) );
        members->push_back( fputsDecl );
    }

    {   // declare tx.c.fprintf:
        auto fileArgType = new TxNamedTypeNode( PLOC(pctx), "tx.ULong" );
        auto cstrArgType = new TxReferenceTypeNode( PLOC(pctx), nullptr, new TxArrayTypeNode( PLOC(pctx), new TxNamedTypeNode( PLOC(pctx), "tx.UByte" ) ) );
        auto val1ArgType = new TxNamedTypeNode( PLOC(pctx), "tx.ULong" );
        auto args = new std::vector<TxArgTypeDefNode*>( { new TxArgTypeDefNode( PLOC(pctx), "file", fileArgType ),
                                                          new TxArgTypeDefNode( PLOC(pctx), "cstr", cstrArgType ),
                                                          new TxArgTypeDefNode( PLOC(pctx), "val1", val1ArgType ) } );
        auto fprintfDecl = new TxFieldDeclNode(
                PLOC(pctx), TXD_PUBLIC | TXD_EXTERNC | TXD_BUILTIN,
                new TxNonLocalFieldDefNode( PLOC(pctx), new TxIdentifierNode( PLOC(pctx), "fprintf" ),
                                            new TxFunctionTypeNode( PLOC(pctx), false, args,
                                                                    new TxNamedTypeNode( PLOC(pctx), "tx.Int" ) ),
                                            nullptr ) );
        members->push_back( fprintfDecl );
    }

    {   // declare tx.c.stdout and tx.c.stderr:
        auto stdoutDecl = new TxFieldDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_EXTERNC | TXD_BUILTIN,
                                               new TxNonLocalFieldDefNode( PLOC(pctx), new TxIdentifierNode( PLOC(pctx), "stdout" ),
                                                                           new TxNamedTypeNode( PLOC(pctx), "tx.ULong" ), nullptr ) );
        auto stderrDecl = new TxFieldDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_EXTERNC | TXD_BUILTIN,
                                               new TxNonLocalFieldDefNode( PLOC(pctx), new TxIdentifierNode( PLOC(pctx), "stderr" ),
                                                                           new TxNamedTypeNode( PLOC(pctx), "tx.ULong" ), nullptr ) );
        members->push_back( stdoutDecl );
        members->push_back( stderrDecl );
    }

    {  // declare tx.c.abort:
        auto args = new std::vector<TxArgTypeDefNode*>( { } );
        auto abortDecl = new TxFieldDeclNode( PLOC(pctx), TXD_PUBLIC | TXD_EXTERNC | TXD_BUILTIN,
                                              new TxNonLocalFieldDefNode( PLOC(pctx), new TxIdentifierNode( PLOC(pctx), "abort" ),
                                                                          new TxFunctionTypeNode( PLOC(pctx), false, args, nullptr ),
                                                                          nullptr ) );
        members->push_back( abortDecl );
    }

    {  // declare tx.c.memcmp:
        auto data1ArgType = new TxReferenceTypeNode( PLOC(pctx), nullptr, new TxArrayTypeNode( PLOC(pctx), new TxNamedTypeNode( PLOC(pctx), "tx.UByte" ) ) );
        auto data2ArgType = new TxReferenceTypeNode( PLOC(pctx), nullptr, new TxArrayTypeNode( PLOC(pctx), new TxNamedTypeNode( PLOC(pctx), "tx.UByte" ) ) );
        auto lenArgType = new TxNamedTypeNode( PLOC(pctx), "tx.ULong" );
        auto args = new std::vector<TxArgTypeDefNode*>( { new TxArgTypeDefNode( PLOC(pctx), "data1", data1ArgType ),
                                                          new TxArgTypeDefNode( PLOC(pctx), "data2", data2ArgType ),
                                                          new TxArgTypeDefNode( PLOC(pctx), "len",   lenArgType ) } );
        auto memcmpDecl = new TxFieldDeclNode(
                PLOC(pctx), TXD_PUBLIC | TXD_EXTERNC | TXD_BUILTIN,
                new TxNonLocalFieldDefNode( PLOC(pctx), new TxIdentifierNode( PLOC(pctx), "memcmp" ),
                                            new TxFunctionTypeNode( PLOC(pctx), false, args,
                                                                    new TxNamedTypeNode( PLOC(pctx), "tx.Int" ) ),
                                            nullptr ) );
        members->push_back( memcmpDecl );
    }

    auto tx_c_module = new TxModuleNode( this->builtinLocation, new TxIdentifier( "c" ), nullptr, members, nullptr, true );
    return tx_c_module;
}


/** Initializes the built-in symbols. */
void BuiltinTypes::resolveBuiltinSymbols() {
    LOG_DEBUG( &_LOGGER, "Resolving built-in types" );

    // ensure that all built-in types are created and resolved:
    for ( unsigned id = 0; id < BuiltinTypeId_COUNT; id++ ) {
        ASSERT( this->builtinTypes[id], "Uninitialized built-in type! id=" << id );
        this->builtinTypes[id]->typeCreatingNode->resolve_type( TXP_FULL_RESOLUTION );
    }
}

static TxPackage* make_root_package( TxParserContext* parserContext );

BuiltinTypes::BuiltinTypes( TxParserContext* builtinParserContext )
        : builtinParserContext( builtinParserContext ), package( make_root_package( builtinParserContext ) ),
          builtinLocation( package->root_origin().get_parse_location() ) {
}

const TxActualType* BuiltinTypes::get_builtin_type( const BuiltinTypeId id ) const {
    return this->builtinTypes[id]->typeCreatingNode->qtype().type();
}


/*=== root package definer ===*/

/** Represents the definition of the package, i.e. the root module. */
class TxPackageDefinerNode : public TxNode {
protected:
    void declaration_pass() override {
        this->lexContext._scope = new TxPackage( this->get_parser_context()->driver(), *this );
    }

public:
    TxPackageDefinerNode( const TxLocation& ploc )
            : TxNode( ploc ) {
    }

    TxNode* make_ast_copy() const override {
        ASSERT( false, "Can't make AST copy of TxPackageDefinerNode " << this );
        return nullptr;
    }

    TxPackage* get_package() const {
        return static_cast<TxPackage*>( this->context().scope() );
    }

    /** This node has a special declaration pass implementation since it has neither parent node or parent scope. */
    void custom_declaration_pass() {
        this->declaration_pass();
    }

    void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
    }
};

/** Creates the package, i.e. the root namespace module (""). */
static TxPackage* make_root_package( TxParserContext* pctx ) {
    auto packageDefiner = new TxPackageDefinerNode( PLOC( pctx) );
    packageDefiner->custom_declaration_pass();
    return packageDefiner->get_package();
}
