#include "builtin_types.hpp"

#include "../ast/ast_entitydecls.hpp"
#include "util/assert.hpp"

#include "llvm_generator.hpp"
#include "parsercontext.hpp"

#include "ast/ast_util.hpp"
#include "ast/ast_modbase.hpp"
#include "ast/ast_declpass.hpp"
#include "ast/type/ast_types.hpp"
#include "ast/expr/ast_lit.hpp"
#include "ast/expr/ast_array.hpp"
#include "ast/expr/ast_lambda_node.hpp"
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

/** Abstract superclass for the built-in type defining nodes. */
class TxBuiltinTypeDefiningNode : public TxTypeExpressionNode {
    TxTypeDeclNode* superRefTypeNode = nullptr;

    /** creates nodes for the implicit type member 'Super' */
    TxTypeDeclNode* make_super_type_node() const {
        // (Note, 'Self' is created in the symbol table for all types, as an alias directly to the type.)
        TxTypeExpressionNode* superTypeExprN = new TxTypeExprWrapperNode( this->baseTypeNode );
        auto superRefTypeExprN = new TxReferenceTypeNode( this->ploc, nullptr, superTypeExprN );
        const std::string superTypeName = "Super";
        return new TxTypeDeclNode( this->ploc, TXD_IMPLICIT, superTypeName, nullptr, superRefTypeExprN );
    }

    /** helper method for subclasses that constructs a vector of TxType of this instance's interface expressions */
    std::vector<const TxType*> resolve_interfaces() const {
        std::vector<const TxType*> ifs;
        if ( this->sourcecodeDefiner ) {
            for ( auto ifDef : *this->sourcecodeDefiner->interfaces )
                ifs.emplace_back( ifDef->resolve_type()->type() );
        }
        return ifs;
    }

    /** helper method for subclasses that constructs a vector of TxTypeSpecialization of this instance's interface expressions */
    static std::vector<const TxActualType*> resolve_interface_specs( const std::vector<const TxType*>& interfaces ) {
        std::vector<const TxActualType*> ifSpecs;
        for ( auto interface : interfaces )
            ifSpecs.emplace_back( interface->acttype() );
        return ifSpecs;
    }

    void merge_builtin_type_definers( TxDerivedTypeNode* sourcecodeDefiner ) {
        ASSERT( this->is_context_set(), "Builtin type node hasn't run declaration pass: " << this );
        ASSERT( !this->attempt_qualtype(), "Builtin type already resolved: " << this );
        sourcecodeDefiner->set_builtin_type_definer( this );
        this->sourcecodeDefiner = sourcecodeDefiner;
    }

    friend void merge_builtin_type_definers( TxDerivedTypeNode* sourcecodeDefiner, TxTypeDefiningNode* builtinDefiner );

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
            : TxTypeExpressionNode( ploc ), builtinTypeId( TXBT_NOTSET ), original( original ),
              baseTypeNode( baseTypeNode ), declNodes( declNodes ), sourcecodeDefiner( sourcecodeDefiner ) {
        if ( this->baseTypeNode ) {
            this->superRefTypeNode = make_super_type_node();
        }
        if (sourcecodeDefiner)
            sourcecodeDefiner->set_builtin_type_definer( this );
    }

    virtual void typeexpr_declaration_pass() override {
        if ( this->sourcecodeDefiner ) {
            // "pass through" entity declaration to the source code definer
            this->sourcecodeDefiner->set_declaration( this->get_declaration() );
        }
    }

    virtual const TxQualType* define_type() override final {
// This used to be necessary but hindered providing members to Array and Ref in source code:
//        if ( this->original ) {  // true when this is a reinterpreted copy
//            // Note: This applies to Ref and Array specializations and deviates from reinterpretation of user types:
//            //   Specializations of user types will not have the generic base type as their base type,
//            //   instead they will have the generic base type's parent as their base type.
//            //   Here that would be  this->baseTypeNode->resolve_type()  instead of  this->original->get_type() .
//            //   That would however not work with the current type class implementation (TxReferenceType and TxArrayType).
//            return this->registry().make_type_entity( this->registry().make_actual_type( this->get_declaration(),
//                                                                                         this->original->get_type()->type() ) );
//        }
        const TxType* baseType = ( this->baseTypeNode ? this->baseTypeNode->resolve_type()->type() : nullptr );
        const std::vector<const TxType*> interfaces = this->resolve_interfaces();
        return new TxQualType( new TxType( this,
                           [ this, baseType, interfaces ] () {
                                auto ifSpecs = resolve_interface_specs( interfaces );
                                TxActualType* actType = this->make_builtin_type( this->get_declaration(), baseType, ifSpecs,
                                                                                 this->requires_mutable_type() );
                                actType->formalTypeId = this->builtinTypeId;
                                actType->vtableId = this->builtinTypeId;
                                this->registry().add_type( actType );
                                return actType;
                           } ) );
    }

    virtual TxActualType* make_builtin_type( const TxTypeDeclaration* declaration, const TxType* baseType,
                                             const std::vector<const TxActualType*>& ifSpecs, bool mutableType ) = 0;

//    /** Returns true if this type expression requires the produced type to be mutable. Used by subclasses upon type creation. */
//    bool requires_mutable_type() const {
//    }

public:
    TxBuiltinTypeDefiningNode( const TxLocation& ploc, BuiltinTypeId builtinTypeId,
                               TxTypeExpressionNode* baseTypeNode,
                               const std::vector<TxDeclarationNode*>& declNodes )
            : TxTypeExpressionNode( ploc ), builtinTypeId( builtinTypeId ), original( nullptr ),
              baseTypeNode( baseTypeNode ),
              declNodes( declNodes ) {
        if ( this->baseTypeNode ) {
            this->superRefTypeNode = make_super_type_node();
        }
    }

    virtual TxBuiltinTypeDefiningNode* make_ast_copy() const override {
        // (only valid for Ref and Array, which override this method)
        ASSERT( false, "Can't make AST copy of built-in type definer " << this );
        return nullptr;
    }

    virtual void symbol_resolution_pass() override {
        TxTypeExpressionNode::symbol_resolution_pass();
        if ( this->baseTypeNode ) {
            this->superRefTypeNode->symbol_resolution_pass();
        }
        for ( auto decl : this->declNodes )
            decl->symbol_resolution_pass();
        if ( this->sourcecodeDefiner )
            this->sourcecodeDefiner->symbol_resolution_pass();
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override {
        if ( this->baseTypeNode ) {
            this->baseTypeNode->code_gen_type( context );
            this->superRefTypeNode->code_gen( context );
        }
        for ( auto decl : this->declNodes )
            decl->code_gen( context );
        if ( this->sourcecodeDefiner )
            this->sourcecodeDefiner->code_gen_builtin_type( context );
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( this->baseTypeNode ) {
            this->baseTypeNode->visit_ast( visitor, thisCursor, "basetype", context );
            this->superRefTypeNode->visit_ast( visitor, thisCursor, "super", context );
        }
        for ( auto decl : this->declNodes )
            decl->visit_ast( visitor, thisCursor, "decl", context );
        if ( this->sourcecodeDefiner )
            this->sourcecodeDefiner->visit_ast( visitor, thisCursor, "source", context );
    }

    virtual const std::string& get_descriptor() const override {
        return this->parent()->get_descriptor();  // the descriptor of the type declaration
    }
};

void merge_builtin_type_definers( TxDerivedTypeNode* sourcecodeDefiner, TxTypeDefiningNode* builtinDefiner ) {
    auto builtinNode = dynamic_cast<TxBuiltinTypeDefiningNode*>( builtinDefiner );
    if ( !builtinNode )
        THROW_LOGIC( "Expected builtin type definer to be of type TxBuiltinTypeDefiningNode: " << builtinDefiner );
    builtinNode->merge_builtin_type_definers( sourcecodeDefiner );
}

/** Single-purpose node that defines the Any root type. */
class TxAnyTypeDefNode final : public TxBuiltinTypeDefiningNode {
    /** Used solely for the Any root type object. */
    class TxAnyType final : public TxActualType {
        TxAnyType( const TxTypeDeclaration* declaration, const TxActualType* baseType )
                : TxActualType( TXTC_ANY, declaration, baseType, true ) {
        }

        TxAnyType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                          bool mutableType, const std::vector<const TxActualType*>& interfaces ) const override {
            // Note: Only for equivalent derivations - e.g. empty, 'modifiable', and GENPARAM constraints.
            if ( !dynamic_cast<const TxAnyType*>( baseType ) )
                throw std::logic_error( "Specified a base type for TxAnyType that was not a TxAnyType: " + baseType->str() );
            return new TxAnyType( declaration, baseType );
        }

    public:
        TxAnyType( const TxTypeDeclaration* declaration )
                : TxActualType( TXTC_ANY, declaration, true ) {
        }

        virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override {
            return llvm::StructType::get( context.llvmContext );  // abstract type
        }
    };

protected:
    virtual TxActualType* make_builtin_type( const TxTypeDeclaration* declaration, const TxType* baseType,
                                             const std::vector<const TxActualType*>& ifSpecs, bool mutableType ) override {
        return new TxAnyType( declaration );
    }

public:
    TxAnyTypeDefNode( const TxLocation& ploc, const std::vector<TxDeclarationNode*>& declNodes = { } )
            : TxBuiltinTypeDefiningNode( ploc, TXBT_ANY, nullptr, declNodes ) {
    }
};

/** Single-purpose node that defines the Void type. */
class TxVoidTypeDefNode final : public TxBuiltinTypeDefiningNode {
    /** Used solely for the Void type object. */
    class TxVoidType final : public TxActualType {
        TxVoidType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                           bool mutableType, const std::vector<const TxActualType*>& interfaces ) const override {
            throw std::logic_error( "Can't specialize Void" );
        }

    public:
        TxVoidType( const TxTypeDeclaration* declaration )
                : TxActualType( TXTC_VOID, declaration, false ) {
        }

        virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override {
            LOG_TRACE( context.LOGGER(), "LLVM type for abstract type " << this << " is VOID" );
            return llvm::Type::getVoidTy( context.llvmContext );
        }
    };

protected:
    virtual TxActualType* make_builtin_type( const TxTypeDeclaration* declaration, const TxType* baseType,
                                             const std::vector<const TxActualType*>& ifSpecs, bool mutableType ) override {
        return new TxVoidType( declaration );
    }

public:
    TxVoidTypeDefNode( const TxLocation& ploc, TxTypeExpressionNode* baseTypeNode, const std::vector<TxDeclarationNode*>& declNodes = { } )
            : TxBuiltinTypeDefiningNode( ploc, TXBT_VOID, baseTypeNode, declNodes ) {
    }
};

/** Used to define the built-in types' abstract base types. */
class TxBuiltinAbstractTypeDefNode final : public TxBuiltinTypeDefiningNode {
    /** Used for the built-in types' abstract base types. */
    class TxBuiltinBaseType final : public TxActualType {
        TxBuiltinBaseType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                                  bool mutableType, const std::vector<const TxActualType*>& interfaces ) const override {
            if ( !dynamic_cast<const TxBuiltinBaseType*>( baseType ) )
                throw std::logic_error( "Specified a base type for TxBuiltinBaseType that was not a TxBuiltinBaseType: " + baseType->str() );
            return new TxBuiltinBaseType( baseType->get_type_class(), declaration, baseType, interfaces );
        }

    public:
        TxBuiltinBaseType( TxTypeClass typeClass, const TxTypeDeclaration* declaration, const TxActualType* baseType,
                           const std::vector<const TxActualType*>& interfaces )
                : TxActualType( typeClass, declaration, baseType, true, interfaces ) {
        }

        virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override {
            LOG_TRACE( context.LOGGER(), "LLVM type for abstract type " << this << " is VOID" );
            return llvm::Type::getVoidTy( context.llvmContext );
        }
    };

protected:
    virtual TxActualType* make_builtin_type( const TxTypeDeclaration* declaration, const TxType* baseType,
                                             const std::vector<const TxActualType*>& ifSpecs, bool mutableType ) override {
        return new TxBuiltinBaseType( this->typeClass, declaration, baseType->acttype(), ifSpecs );
    }
public:
    const TxTypeClass typeClass;
    TxBuiltinAbstractTypeDefNode( const TxLocation& ploc, BuiltinTypeId builtinTypeId, TxTypeExpressionNode* baseTypeNode,
                                  TxTypeClass typeClass,
                                  const std::vector<TxDeclarationNode*>& declNodes )
            : TxBuiltinTypeDefiningNode( ploc, builtinTypeId, baseTypeNode, declNodes ), typeClass( typeClass ) {
    }
};

class TxIntegerTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    virtual TxActualType* make_builtin_type( const TxTypeDeclaration* declaration, const TxType* baseType,
                                             const std::vector<const TxActualType*>& ifSpecs, bool mutableType ) override {
        return new TxIntegerType( declaration, baseType->acttype(), ifSpecs, this->size, this->sign );
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
    virtual TxActualType* make_builtin_type( const TxTypeDeclaration* declaration, const TxType* baseType,
                                             const std::vector<const TxActualType*>& ifSpecs, bool mutableType ) override {
        return new TxFloatingType( declaration, baseType->acttype(), ifSpecs, this->size );
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
    virtual TxActualType* make_builtin_type( const TxTypeDeclaration* declaration, const TxType* baseType,
                                             const std::vector<const TxActualType*>& ifSpecs, bool mutableType ) override {
        return new TxBoolType( declaration, baseType->acttype(), ifSpecs );
    }
public:
    TxBoolTypeDefNode( const TxLocation& ploc, TxTypeExpressionNode* baseTypeNode,
                       const std::vector<TxDeclarationNode*>& declNodes )
            : TxBuiltinTypeDefiningNode( ploc, TXBT_BOOL, baseTypeNode, declNodes ) {
    }
};

class TxTupleTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    virtual TxActualType* make_builtin_type( const TxTypeDeclaration* declaration, const TxType* baseType,
                                             const std::vector<const TxActualType*>& ifSpecs, bool mutableType ) override {
        return new TxTupleType( declaration, baseType->acttype(), ifSpecs, mutableType );
    }
public:
    TxTupleTypeDefNode( const TxLocation& ploc, TxTypeExpressionNode* baseTypeNode,
                        const std::vector<TxDeclarationNode*>& declNodes )
            : TxBuiltinTypeDefiningNode( ploc, TXBT_TUPLE, baseTypeNode, declNodes ) {
    }
};

class TxInterfaceTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    virtual TxActualType* make_builtin_type( const TxTypeDeclaration* declaration, const TxType* baseType,
                                             const std::vector<const TxActualType*>& ifSpecs, bool mutableType ) override {
        return new TxInterfaceType( declaration, baseType->acttype(), ifSpecs );
    }
public:
    TxInterfaceTypeDefNode( const TxLocation& ploc, TxTypeExpressionNode* baseTypeNode,
                            const std::vector<TxDeclarationNode*>& declNodes )
            : TxBuiltinTypeDefiningNode( ploc, TXBT_INTERFACE, baseTypeNode, declNodes ) {
    }
};

class TxRefTypeDefNode final : public TxBuiltinTypeDefiningNode {
    TxRefTypeDefNode( const TxLocation& ploc, const TxRefTypeDefNode* original,
                      TxTypeExpressionNode* baseTypeNode, const std::vector<TxDeclarationNode*>& declNodes, TxDerivedTypeNode* sourcecodeDefiner )
            : TxBuiltinTypeDefiningNode( ploc, original, baseTypeNode, declNodes, sourcecodeDefiner ) {
    }
protected:
    virtual TxActualType* make_builtin_type( const TxTypeDeclaration* declaration, const TxType* baseType,
                                             const std::vector<const TxActualType*>& ifSpecs, bool mutableType ) override {
        return new TxReferenceType( declaration, baseType->acttype(), ifSpecs );
    }
public:
    TxRefTypeDefNode( const TxLocation& ploc, TxTypeExpressionNode* baseTypeNode,
                      const std::vector<TxDeclarationNode*>& declNodes,
                      const TxRefTypeDefNode* original = nullptr )
            : TxBuiltinTypeDefiningNode( ploc, TXBT_REFERENCE, baseTypeNode, declNodes ) {
    }

    virtual TxRefTypeDefNode* make_ast_copy() const override {
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
    virtual TxActualType* make_builtin_type( const TxTypeDeclaration* declaration, const TxType* baseType,
                                             const std::vector<const TxActualType*>& ifSpecs, bool mutableType ) override {
        return new TxArrayType( declaration, baseType->acttype(), mutableType, ifSpecs );
    }
public:
    TxArrayTypeDefNode( const TxLocation& ploc, TxTypeExpressionNode* baseTypeNode,
                        const std::vector<TxDeclarationNode*>& declNodes,
                        const TxArrayTypeDefNode* original = nullptr )
            : TxBuiltinTypeDefiningNode( ploc, TXBT_ARRAY, baseTypeNode, declNodes ) {
    }

    virtual TxArrayTypeDefNode* make_ast_copy() const override {
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

    virtual TxBuiltinConstructorTypeDefNode* make_ast_copy() const override = 0;
};

class TxDefConstructorTypeDefNode final : public TxBuiltinConstructorTypeDefNode {
protected:
    TxExpressionNode* initExprNode;

    virtual const TxQualType* define_type() override {
        auto actType = new TxBuiltinDefaultConstructorType( this->get_declaration(), this->registry().get_builtin_type( TXBT_FUNCTION )->acttype(),
                                                            this->returnField->resolve_type()->type()->acttype(),
                                                            initExprNode );
        return new TxQualType( this->registry().make_type_entity( actType ) );
    }

public:
    TxDefConstructorTypeDefNode( const TxLocation& ploc, TxTypeExpressionNode* returnTypeNode, TxExpressionNode* initExprNode )
            : TxBuiltinConstructorTypeDefNode( ploc, new std::vector<TxArgTypeDefNode*>(), returnTypeNode ), initExprNode( initExprNode ) {
    }

    virtual TxDefConstructorTypeDefNode* make_ast_copy() const override {
        return new TxDefConstructorTypeDefNode( this->ploc, this->returnField->typeExpression->make_ast_copy(),
                                                initExprNode->make_ast_copy() );
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        TxBuiltinConstructorTypeDefNode::visit_descendants( visitor, thisCursor, role, context );
        this->initExprNode->visit_ast( visitor, thisCursor, "initializer", context );
    }
};

class TxConvConstructorTypeDefNode final : public TxBuiltinConstructorTypeDefNode {
protected:
    virtual const TxQualType* define_type() override {
        auto actType = new TxBuiltinConversionFunctionType( this->get_declaration(), this->registry().get_builtin_type( TXBT_FUNCTION )->acttype(),
                                                            this->arguments->at( 0 )->resolve_type()->type()->acttype(),
                                                            this->returnField->resolve_type()->type()->acttype() );
        return new TxQualType( this->registry().make_type_entity( actType ) );
    }

public:
    TxConvConstructorTypeDefNode( const TxLocation& ploc, TxArgTypeDefNode* fromTypeArg, TxTypeExpressionNode* returnTypeNode )
            : TxBuiltinConstructorTypeDefNode( ploc, new std::vector<TxArgTypeDefNode*>( { fromTypeArg } ), returnTypeNode ) {
    }

    virtual TxConvConstructorTypeDefNode* make_ast_copy() const override {
        return new TxConvConstructorTypeDefNode( this->ploc, this->arguments->at( 0 )->make_ast_copy(),
                                                 this->returnField->typeExpression->make_ast_copy() );
    }
};

class TxArrayConstructorTypeDefNode final : public TxBuiltinConstructorTypeDefNode {
protected:
    virtual const TxQualType* define_type() override {
        auto actType = new TxBuiltinArrayInitializerType( this->get_declaration(), this->registry().get_builtin_type( TXBT_FUNCTION )->acttype(),
                                                          this->arguments->at( 0 )->resolve_type()->type()->acttype(),
                                                          this->returnField->resolve_type()->type()->acttype() );
        return new TxQualType( this->registry().make_type_entity( actType ) );
    }

public:
    TxArrayConstructorTypeDefNode( const TxLocation& ploc, TxArgTypeDefNode* fromTypeArg, TxTypeExpressionNode* returnTypeNode )
            : TxBuiltinConstructorTypeDefNode( ploc, new std::vector<TxArgTypeDefNode*>( { fromTypeArg } ), returnTypeNode ) {
    }

    virtual TxArrayConstructorTypeDefNode* make_ast_copy() const override {
        return new TxArrayConstructorTypeDefNode( this->ploc, this->arguments->at( 0 )->make_ast_copy(),
                                                  this->returnField->typeExpression->make_ast_copy() );
    }
};

/*----- helper functions creating the built-in elementary types' declarations and constructors -----*/

static TxTypeDeclNode* make_builtin_abstract( const TxLocation& parseLoc, TxTypeClass typeClass, BuiltinTypeId id, BuiltinTypeId parentId ) {
    auto baseTypeNode = new TxNamedTypeNode( parseLoc, BUILTIN_TYPE_NAMES[parentId] );
    auto typeDecl = new TxTypeDeclNode( parseLoc, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, BUILTIN_TYPE_NAMES[id], nullptr,
                                        new TxBuiltinAbstractTypeDefNode( parseLoc, id, baseTypeNode, typeClass, { } ), false, true );
    return typeDecl;
}

static TxTypeDeclNode* make_builtin_integer( const TxLocation& parseLoc, BuiltinTypeId id, BuiltinTypeId parentId,
                                             std::vector<std::vector<TxDeclarationNode*>>& constructors,
                                             int size, bool sign ) {
    auto baseTypeNode = new TxNamedTypeNode( parseLoc, BUILTIN_TYPE_NAMES[parentId] );
    auto typeDecl = new TxTypeDeclNode( parseLoc, TXD_PUBLIC | TXD_BUILTIN | TXD_FINAL, BUILTIN_TYPE_NAMES[id], nullptr,
                                        new TxIntegerTypeDefNode( parseLoc, id, baseTypeNode, size, sign, constructors[id] ), false, true );
    return typeDecl;
}

static TxTypeDeclNode* make_builtin_floating( const TxLocation& parseLoc, BuiltinTypeId id, BuiltinTypeId parentId,
                                              std::vector<std::vector<TxDeclarationNode*>>& constructors,
                                              int size ) {
    auto baseTypeNode = new TxNamedTypeNode( parseLoc, BUILTIN_TYPE_NAMES[parentId] );
    auto typeDecl = new TxTypeDeclNode( parseLoc, TXD_PUBLIC | TXD_BUILTIN | TXD_FINAL, BUILTIN_TYPE_NAMES[id], nullptr,
                                        new TxFloatingTypeDefNode( parseLoc, id, baseTypeNode, size, constructors[id] ), false, true );
    return typeDecl;
}

static TxFieldDeclNode* make_default_initializer( const TxLocation& loc, BuiltinTypeId toTypeId, TxExpressionNode* initializerExpr ) {
    auto toTypeNode = new TxNamedTypeNode( loc, BUILTIN_TYPE_NAMES[toTypeId] );
    return new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_VIRTUAL | TXD_BUILTIN | TXD_INITIALIZER,
                                new TxNonLocalFieldDefNode( loc, CONSTR_IDENT,
                                                            new TxDefConstructorTypeDefNode( loc, toTypeNode, initializerExpr ),
                                                            nullptr ),  // no function body, initialization is inlined
                                false );  // not method syntax since elementary types' initializers are inlineable, pure functions
}

static TxFieldDeclNode* make_conversion_initializer( const TxLocation& loc, BuiltinTypeId fromTypeId, BuiltinTypeId toTypeId ) {
    auto toTypeNode = new TxNamedTypeNode( loc, BUILTIN_TYPE_NAMES[toTypeId] );
    auto fromTypeNode = new TxNamedTypeNode( loc, BUILTIN_TYPE_NAMES[fromTypeId] );
    return new TxFieldDeclNode(
            loc, TXD_PUBLIC | TXD_VIRTUAL | TXD_BUILTIN | TXD_INITIALIZER,
            new TxNonLocalFieldDefNode( loc, CONSTR_IDENT,
                                        new TxConvConstructorTypeDefNode( loc, new TxArgTypeDefNode( loc, "arg", fromTypeNode ), toTypeNode ),
                                        nullptr ),  // no function body, initialization is inlined
            false );  // not method syntax since elementary types' initializers are inlineable, pure functions
}

static std::vector<TxDeclarationNode*> make_array_constructors( const TxLocation& loc ) {
    std::vector<TxDeclarationNode*> constructors;
    { // default constructor - this does nothing (it presumes memory allocation logic will initialize Array.C)
        auto constrType = new TxFunctionTypeNode( loc, false, new std::vector<TxArgTypeDefNode*>(), nullptr );
        auto lambdaExpr = new TxLambdaExprNode( loc, constrType, new TxSuiteNode( loc ), true );
        constructors.push_back( new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_BUILTIN | TXD_CONSTRUCTOR,
                                                     new TxNonLocalFieldDefNode( loc, CONSTR_IDENT,
                                                                                 nullptr, lambdaExpr ),
                                                     true ) );  // method syntax since regular constructor
    }
    { // copy constructor
        auto argNode = new TxArgTypeDefNode( loc, "val", new TxNamedTypeNode( loc, "Self" ) );
        auto returnTypeNode = new TxNamedTypeNode( loc, "Self" );
        constructors.push_back( new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_VIRTUAL | TXD_BUILTIN | TXD_INITIALIZER,
                                                     new TxNonLocalFieldDefNode( loc, CONSTR_IDENT,
                                                                                 new TxArrayConstructorTypeDefNode( loc, argNode, returnTypeNode ),
                                                                                 nullptr ),  // no function body, initialization is inlined
                                                     false ) );  // not method syntax since this initializer is an inlineable, pure function
    }
    return constructors;
}

TxParsingUnitNode* BuiltinTypes::createTxModuleAST() {
    auto & loc = this->builtinLocation;

    { // create the Any root type:
        auto anyTypeDecl = new TxTypeDeclNode( loc, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, "Any", nullptr,
                                               new TxAnyTypeDefNode( loc ), false, true );
        this->builtinTypes[TXBT_ANY] = anyTypeDecl;
    }

    { // create the Void type:
        auto voidBaseTypeNode = new TxNamedTypeNode( loc, BUILTIN_TYPE_NAMES[TXBT_ANY] );
        auto voidTypeDecl = new TxTypeDeclNode( loc, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT | TXD_FINAL, "Void", nullptr,
                                                new TxVoidTypeDefNode( loc, voidBaseTypeNode ), false, false );
        this->builtinTypes[TXBT_VOID] = voidTypeDecl;
    }

    // default initializers for elementary, concrete built-ins:
    std::vector<std::vector<TxDeclarationNode*>> constructors( BuiltinTypeId_COUNT );
    constructors[TXBT_BOOL].push_back( make_default_initializer( loc, TXBT_BOOL, new TxBoolLitNode( loc, false ) ) );
    constructors[TXBT_BYTE].push_back( make_default_initializer( loc, TXBT_BYTE, new TxIntegerLitNode( loc, 0, true, TXBT_BYTE ) ) );
    constructors[TXBT_SHORT].push_back( make_default_initializer( loc, TXBT_SHORT, new TxIntegerLitNode( loc, 0, true, TXBT_SHORT ) ) );
    constructors[TXBT_INT].push_back( make_default_initializer( loc, TXBT_INT, new TxIntegerLitNode( loc, 0, true, TXBT_INT ) ) );
    constructors[TXBT_LONG].push_back( make_default_initializer( loc, TXBT_LONG, new TxIntegerLitNode( loc, 0, true, TXBT_LONG ) ) );
    constructors[TXBT_UBYTE].push_back( make_default_initializer( loc, TXBT_UBYTE, new TxIntegerLitNode( loc, 0, false, TXBT_UBYTE ) ) );
    constructors[TXBT_USHORT].push_back( make_default_initializer( loc, TXBT_USHORT, new TxIntegerLitNode( loc, 0, false, TXBT_USHORT ) ) );
    constructors[TXBT_UINT].push_back( make_default_initializer( loc, TXBT_UINT, new TxIntegerLitNode( loc, 0, false, TXBT_UINT ) ) );
    constructors[TXBT_ULONG].push_back( make_default_initializer( loc, TXBT_ULONG, new TxIntegerLitNode( loc, 0, false, TXBT_ULONG ) ) );
    constructors[TXBT_HALF].push_back( make_default_initializer( loc, TXBT_HALF, new TxFloatingLitNode( loc, TXBT_HALF ) ) );
    constructors[TXBT_FLOAT].push_back( make_default_initializer( loc, TXBT_FLOAT, new TxFloatingLitNode( loc, TXBT_FLOAT ) ) );
    constructors[TXBT_DOUBLE].push_back( make_default_initializer( loc, TXBT_DOUBLE, new TxFloatingLitNode( loc, TXBT_DOUBLE ) ) );

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
            constructors[toTypeId].push_back( make_conversion_initializer( loc, fromTypeId, toTypeId ) );
    }

    // create the built-in abstract base types:
    this->builtinTypes[TXBT_ELEMENTARY] = make_builtin_abstract( loc, TXTC_ELEMENTARY, TXBT_ELEMENTARY, TXBT_ANY );
    this->builtinTypes[TXBT_SCALAR] = make_builtin_abstract( loc, TXTC_ELEMENTARY, TXBT_SCALAR, TXBT_ELEMENTARY );
    this->builtinTypes[TXBT_INTEGER] = make_builtin_abstract( loc, TXTC_ELEMENTARY, TXBT_INTEGER, TXBT_SCALAR );
    this->builtinTypes[TXBT_SIGNED] = make_builtin_abstract( loc, TXTC_ELEMENTARY, TXBT_SIGNED, TXBT_INTEGER );
    this->builtinTypes[TXBT_UNSIGNED] = make_builtin_abstract( loc, TXTC_ELEMENTARY, TXBT_UNSIGNED, TXBT_INTEGER );
    this->builtinTypes[TXBT_FLOATINGPOINT] = make_builtin_abstract( loc, TXTC_ELEMENTARY, TXBT_FLOATINGPOINT, TXBT_SCALAR );

    // create the built-in concrete scalar types:
    this->builtinTypes[TXBT_BYTE] = make_builtin_integer( loc, TXBT_BYTE, TXBT_SIGNED, constructors, 1, true );
    this->builtinTypes[TXBT_SHORT] = make_builtin_integer( loc, TXBT_SHORT, TXBT_SIGNED, constructors, 2, true );
    this->builtinTypes[TXBT_INT] = make_builtin_integer( loc, TXBT_INT, TXBT_SIGNED, constructors, 4, true );
    this->builtinTypes[TXBT_LONG] = make_builtin_integer( loc, TXBT_LONG, TXBT_SIGNED, constructors, 8, true );
    this->builtinTypes[TXBT_UBYTE] = make_builtin_integer( loc, TXBT_UBYTE, TXBT_UNSIGNED, constructors, 1, false );
    this->builtinTypes[TXBT_USHORT] = make_builtin_integer( loc, TXBT_USHORT, TXBT_UNSIGNED, constructors, 2, false );
    this->builtinTypes[TXBT_UINT] = make_builtin_integer( loc, TXBT_UINT, TXBT_UNSIGNED, constructors, 4, false );
    this->builtinTypes[TXBT_ULONG] = make_builtin_integer( loc, TXBT_ULONG, TXBT_UNSIGNED, constructors, 8, false );
    this->builtinTypes[TXBT_HALF] = make_builtin_floating( loc, TXBT_HALF, TXBT_FLOATINGPOINT, constructors, 2 );
    this->builtinTypes[TXBT_FLOAT] = make_builtin_floating( loc, TXBT_FLOAT, TXBT_FLOATINGPOINT, constructors, 4 );
    this->builtinTypes[TXBT_DOUBLE] = make_builtin_floating( loc, TXBT_DOUBLE, TXBT_FLOATINGPOINT, constructors, 8 );

    // create the boolean type:
    this->builtinTypes[TXBT_BOOL] = new TxTypeDeclNode(
            loc, TXD_PUBLIC | TXD_BUILTIN | TXD_FINAL, "Bool", nullptr,
            new TxBoolTypeDefNode( loc, new TxNamedTypeNode( loc, "Elementary" ), constructors[TXBT_BOOL] ), false, true );

    // create the function base type:
    this->builtinTypes[TXBT_FUNCTION] = new TxTypeDeclNode(
            loc, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, "Function", nullptr,
            new TxBuiltinAbstractTypeDefNode( loc, TXBT_FUNCTION, new TxNamedTypeNode( loc, "Any" ), TXTC_FUNCTION, { } ), false, true );

    // create the tuple base type:
    this->builtinTypes[TXBT_TUPLE] = new TxTypeDeclNode( loc, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, "Tuple", nullptr,
                                                         new TxTupleTypeDefNode( loc, new TxNamedTypeNode( loc, "Any" ), { } ), false, true );

    // create the reference base type:
    {
        auto paramNodes = new std::vector<TxDeclarationNode*>(
                {
                  new TxTypeDeclNode( loc, TXD_PUBLIC | TXD_GENPARAM, "T", nullptr, new TxNamedTypeNode( loc, "Any" ) )
                } );
        this->builtinTypes[TXBT_REFERENCE] = new TxTypeDeclNode( loc, TXD_PUBLIC | TXD_BUILTIN, "Ref", paramNodes,
                                                                 new TxRefTypeDefNode( loc, new TxNamedTypeNode( loc, "Any" ), { } ), false, true );
    }

    // create the array base type:
    {
        auto arrayMembers = make_array_constructors( loc );
        arrayMembers.push_back( new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_IMPLICIT | TXD_BUILTIN,
                                                     new TxNonLocalFieldDefNode( loc, "L", new TxNamedTypeNode( loc, "UInt" ), nullptr, false ) ) );

        auto paramNodes = new std::vector<TxDeclarationNode*>(
                {
                  new TxTypeDeclNode( loc, TXD_PUBLIC | TXD_GENPARAM, "E", nullptr, new TxNamedTypeNode( loc, "Any" ) ),
                  new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_GENPARAM,
                                       new TxNonLocalFieldDefNode( loc, "C", new TxNamedTypeNode( loc, "UInt" ), nullptr, false ) ),
                } );
        this->builtinTypes[TXBT_ARRAY] = new TxTypeDeclNode(
                loc, TXD_PUBLIC | TXD_BUILTIN, "Array", paramNodes,
                new TxArrayTypeDefNode( loc, new TxNamedTypeNode( loc, "Any" ), arrayMembers ), false, true );
    }

    // create the interface base type:
    {
        // the adaptee type id virtual field member, which is abstract here but concrete in adapter subtypes:
        const TxDeclarationFlags adapteeIdFieldFlags = TXD_PUBLIC | TXD_BUILTIN | TXD_VIRTUAL | TXD_ABSTRACT | TXD_IMPLICIT;
        auto adapteeIdFType = new TxNamedTypeNode( loc, "UInt" );
        auto adapteeIdField = new TxNonLocalFieldDefNode( loc, "$adTypeId", adapteeIdFType, nullptr );
        auto adapteeIdFDecl = new TxFieldDeclNode( loc, adapteeIdFieldFlags, adapteeIdField );

        auto ifTypeDef = new TxInterfaceTypeDefNode( loc, new TxNamedTypeNode( loc, "Any" ), { adapteeIdFDecl } );
        this->builtinTypes[TXBT_INTERFACE] = new TxTypeDeclNode( loc, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, "Interface", nullptr,
                                                                 ifTypeDef, true, true );
    }

    // put it all together as the AST for the tx module:

    std::vector<TxImportNode*>* imports = nullptr;
    std::vector<TxModuleNode*>* subModules = new std::vector<TxModuleNode*>();
    std::vector<TxDeclarationNode*>* members = new std::vector<TxDeclarationNode*>();

    for ( unsigned id = 0; id < BuiltinTypeId_COUNT; id++ ) {
        ASSERT( this->builtinTypes[id], "not yet coded builtin type id: " << id );
        members->push_back( this->builtinTypes[id] );
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
    auto & loc = this->builtinLocation;
    std::vector<TxDeclarationNode*>* members = new std::vector<TxDeclarationNode*>();

    {   // declare tx.c.puts:
        auto cstrArgType = new TxReferenceTypeNode( loc, nullptr, new TxArrayTypeNode( loc, new TxNamedTypeNode( loc, "tx.UByte" ) ) );
        auto args = new std::vector<TxArgTypeDefNode*>( { new TxArgTypeDefNode( loc, "cstr", cstrArgType ) } );
        auto putsDecl = new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_EXTERNC | TXD_BUILTIN,
                                             new TxNonLocalFieldDefNode( loc, "puts", new TxFunctionTypeNode( loc, false, args,
                                                                                                              new TxNamedTypeNode( loc, "tx.Int" ) ),
                                                                         nullptr ) );
        members->push_back( putsDecl );
    }

    {   // declare tx.c.fputs:
        auto cstrArgType = new TxReferenceTypeNode( loc, nullptr, new TxArrayTypeNode( loc, new TxNamedTypeNode( loc, "tx.UByte" ) ) );
        auto fileArgType = new TxNamedTypeNode( loc, "tx.ULong" );
        auto args = new std::vector<TxArgTypeDefNode*>( { new TxArgTypeDefNode( loc, "cstr", cstrArgType ),
                                                          new TxArgTypeDefNode( loc, "file", fileArgType ) } );
        auto fputsDecl = new TxFieldDeclNode(
                loc, TXD_PUBLIC | TXD_EXTERNC | TXD_BUILTIN,
                new TxNonLocalFieldDefNode( loc, "fputs", new TxFunctionTypeNode( loc, false, args,
                                                                                  new TxNamedTypeNode( loc, "tx.Int" ) ),
                                            nullptr ) );
        members->push_back( fputsDecl );
    }

    {   // declare tx.c.stdout and tx.c.stderr:
        auto stdoutDecl = new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_EXTERNC | TXD_BUILTIN,
                                               new TxNonLocalFieldDefNode( loc, "stdout", new TxNamedTypeNode( loc, "tx.ULong" ), nullptr ) );
        auto stderrDecl = new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_EXTERNC | TXD_BUILTIN,
                                               new TxNonLocalFieldDefNode( loc, "stderr", new TxNamedTypeNode( loc, "tx.ULong" ), nullptr ) );
        members->push_back( stdoutDecl );
        members->push_back( stderrDecl );
    }

    {  // declare tx.c.abort:
        auto args = new std::vector<TxArgTypeDefNode*>( { } );
        auto abortDecl = new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_EXTERNC | TXD_BUILTIN,
                                              new TxNonLocalFieldDefNode( loc, "abort", new TxFunctionTypeNode( loc, false, args, nullptr ),
                                                                          nullptr ) );
        members->push_back( abortDecl );
    }

    auto tx_c_module = new TxModuleNode( this->builtinLocation, new TxIdentifier( "c" ), nullptr, members, nullptr, true );
    return tx_c_module;
}


/** Initializes the built-in symbols. */
void BuiltinTypes::resolveBuiltinSymbols() {
    LOG_INFO( this->registry.LOGGER(), "Resolving built-in symbols" );

    for ( unsigned id = 0; id < BuiltinTypeId_COUNT; id++ ) {
        // ensure that all built-in types are created and resolved:
        ASSERT( this->builtinTypes[id], "Uninitialized built-in type! id=" << id );
        this->builtinTypes[id]->typeExpression->resolve_type()->type()->acttype();
    }
}

BuiltinTypes::BuiltinTypes( TypeRegistry& registry )
        : registry( registry ), builtinLocation( registry.package().root_origin().get_parse_location() ) {
}

const TxType* BuiltinTypes::get_builtin_type( const BuiltinTypeId id ) const {
    return this->builtinTypes[id]->typeExpression->qualtype()->type();
}


/*=== root package definer ===*/

/** Represents the definition of the package, i.e. the root module. */
class TxPackageDefinerNode : public TxNode {
protected:
    virtual void declaration_pass() override {
        this->lexContext._scope = new TxPackage( this->get_parser_context()->driver(), *this );
    }

public:
    TxPackageDefinerNode( const TxLocation& ploc )
            : TxNode( ploc ) {
    }

    virtual TxNode* make_ast_copy() const override {
        ASSERT( false, "Can't make AST copy of TxPackageDefinerNode " << this );
        return nullptr;
    }

    TxPackage* get_package() const {
        return static_cast<TxPackage*>( this->context().scope() );
    }

    /** This node has a special declaration pass implementation since it has neither parent node or parent scope. */
    void node_declaration_pass() {
        this->declaration_pass();
    }

    virtual void symbol_resolution_pass() override {
    }

//    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override {
//        return nullptr;
//    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
    }
};

TxPackage* make_root_package( TxParserContext* parserContext ) {
    auto packageDefiner = new TxPackageDefinerNode( TxLocation( nullptr, 0, 0, parserContext ) );
    packageDefiner->node_declaration_pass();
    return packageDefiner->get_package();
}
