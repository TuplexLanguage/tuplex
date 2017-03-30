#include "builtin_types.hpp"

#include "util/assert.hpp"

#include "ast.hpp"

#include "llvm_generator.hpp"


/*--- statically allocated built-in type objects ---*/

std::vector<std::string> BUILTIN_TYPE_NAMES = {
    "Any",
    "Void",
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
    "Interface"
};



/** Helper function that makes a deep-copy of a vector of nodes to an initializer list. */
template<class N>
std::vector<N*> make_node_vec_copy( const std::vector<N*>& nodeVec ) {
    std::vector<N*> copyVec( nodeVec.size() );
    std::transform( nodeVec.cbegin(), nodeVec.cend(), copyVec.begin(),
                    []( N* n ) -> N*  {  return n->make_ast_copy();  } );
    return copyVec;
}



/*--- private classes providing indirection for fetching the built-in type objects ---*/

class TxBuiltinFieldDefNode final : public TxFieldDefiningNode {
    const TxField* builtinField;

protected:
    virtual const TxType* define_type() override {
        return this->builtinField->get_type();
    }

    virtual const TxField* define_field() override {
        return this->builtinField;
    }

public:

    TxBuiltinFieldDefNode( const TxLocation& parseLocation ) : TxFieldDefiningNode( parseLocation ), builtinField() { }

    virtual TxFieldDefiningNode* make_ast_copy() const override {
        ASSERT( false, "Can't make AST copy of a TxBuiltinFieldDefNode: " << this);
        return nullptr;
    }

    void set_field(const TxField* field) {
        this->builtinField = field;
        this->set_context( LexicalContext( field->get_symbol()->get_outer(), nullptr, false ) );  // emulate declaration pass
        this->resolve_field();  // auto-resolves
    }

    virtual const TxExpressionNode* get_init_expression() const { return nullptr; }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override {
        return nullptr;
    }

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {}
};




/*------ built-in type defining AST nodes ------*/


/** Abstract superclass for the built-in type defining nodes. */
class TxBuiltinTypeDefiningNode : public TxTypeExpressionNode {
protected:
    const BuiltinTypeId builtinTypeId;
    TxBuiltinTypeDefiningNode const * const original;

    /** Specifies the base type of this type (null for the Any type). */
    TxTypeExpressionNode* const baseTypeNode;
    /** Declarations within this type's namespace. */
    std::vector<TxDeclarationNode*> declNodes;

    /** for use by make_ast_copy() in subclasses */
    TxBuiltinTypeDefiningNode( const TxLocation& parseLocation, const TxBuiltinTypeDefiningNode* original,
                               TxTypeExpressionNode* baseTypeNode, const std::vector<TxDeclarationNode*>& declNodes )
        : TxTypeExpressionNode( parseLocation ), builtinTypeId( TXBTID_NOTSET ), original(original),
          baseTypeNode( baseTypeNode ), declNodes( declNodes )  { }


    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext ) override {
        if (this->baseTypeNode)
            this->baseTypeNode->symbol_declaration_pass( defContext, lexContext, nullptr );
        for (auto decl : this->declNodes)
            decl->symbol_declaration_pass( lexContext, false );
    }

    virtual const TxType* define_type() override final {
        if (this->original) {  // true when this is a reinterpreted copy
            // Note: This applies to Ref and Array specializations and deviates from reinterpretation of user types:
            //   Specializations of user types will not have the generic base type as their base type,
            //   instead they will have the generic base type's parent as their base type.
            //   Here that would be  this->baseTypeNode->resolve_type()  instead of  this->original->get_type() .
            //   That would however not work with the current type class implementation (TxReferenceType and TxArrayType).
            return this->registry().make_type_entity( this->registry().make_actual_type( this->get_declaration(), this->original->get_type()->type() ) );
        }
        else {
            auto actType = this->define_builtin_type();
            actType->staticTypeId = builtinTypeId;
            this->registry().add_type( actType );
            return this->registry().make_type_entity( actType );
        }
    }

    virtual TxActualType* define_builtin_type() = 0;

public:
    TxBuiltinTypeDefiningNode( const TxLocation& parseLocation, BuiltinTypeId builtinTypeId,
                               TxTypeExpressionNode* baseTypeNode, const std::vector<TxDeclarationNode*>& declNodes )
        : TxTypeExpressionNode( parseLocation ), builtinTypeId( builtinTypeId ), original( nullptr ),
          baseTypeNode( baseTypeNode ), declNodes( declNodes )  { }

    virtual TxBuiltinTypeDefiningNode* make_ast_copy() const override {
        ASSERT(false, "Can't make AST copy of built-in type definer " << this);
        return nullptr;
    }

    virtual std::string get_auto_type_name() const override final {
        return this->get_declaration()->get_unique_full_name();
    }

    virtual void symbol_resolution_pass() override {
        TxTypeExpressionNode::symbol_resolution_pass();
        for (auto decl : this->declNodes)
            decl->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override {
        if (this->baseTypeNode)
            this->baseTypeNode->code_gen( context, scope );
        for (auto decl : this->declNodes)
            decl->code_gen( context, scope );
        return nullptr;
    }

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        if (this->baseTypeNode)
            this->baseTypeNode->visit_ast( visitor, thisAsParent, "basetype", context );
        for (auto decl : this->declNodes)
            decl->visit_ast( visitor, thisAsParent, "decl", context );
    }
};



/** Single-purpose node that defines the Any root type. */
class TxAnyTypeDefNode final : public TxBuiltinTypeDefiningNode {
    /** Used solely for the Any root type object. */
    class TxAnyType final : public TxActualType {
        TxAnyType(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec)
                : TxActualType(TXTC_ANY, declaration, baseTypeSpec)  { }

        TxAnyType* make_specialized_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                      const std::vector<TxTypeSpecialization>& interfaces) const override {
            if (! dynamic_cast<const TxAnyType*>(baseTypeSpec.type))
                throw std::logic_error("Specified a base type for TxAnyType that was not a TxAnyType: " + baseTypeSpec.type->str());
            return new TxAnyType(declaration, baseTypeSpec);
        }

    public:
        TxAnyType(const TxTypeDeclaration* declaration) : TxActualType(TXTC_ANY, declaration) { }

        virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override {
            //ASSERT(false, "Can't contruct LLVM type for abstract type " << this->to_string());
            LOG_DEBUG(context.LOGGER(), "LLVM type for abstract type " << this << " is VOID");
            return context.get_voidT();
        }
    };

protected:
    virtual TxActualType* define_builtin_type() override {
        return new TxAnyType( this->get_declaration() );
    }

public:
    TxAnyTypeDefNode( const TxLocation& parseLocation, const std::vector<TxDeclarationNode*>& declNodes={} )
        : TxBuiltinTypeDefiningNode( parseLocation, ANY, nullptr, declNodes ) { }
};



/** Single-purpose node that defines the Void type. */
class TxVoidTypeDefNode final : public TxBuiltinTypeDefiningNode {
    /** Used solely for the Void type object. */
    class TxVoidType final : public TxActualType {
        TxVoidType* make_specialized_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                          const std::vector<TxTypeSpecialization>& interfaces) const override {
            throw std::logic_error("Can't specialize Void");
        }

    public:
        TxVoidType( const TxTypeDeclaration* declaration ) : TxActualType( TXTC_VOID, declaration ) { }

        virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override {
            //ASSERT(false, "Can't contruct LLVM type for Void type " << this->to_string());
            LOG_DEBUG(context.LOGGER(), "LLVM type for abstract type " << this << " is VOID");
            return context.get_voidT();
        }
    };

protected:
    virtual TxActualType* define_builtin_type() override {
        return new TxVoidType( this->get_declaration() );
    }

public:
    TxVoidTypeDefNode( const TxLocation& parseLocation, TxTypeExpressionNode* baseTypeNode, const std::vector<TxDeclarationNode*>& declNodes={} )
        : TxBuiltinTypeDefiningNode( parseLocation, VOID, baseTypeNode, declNodes ) { }
};



/** Used to define the built-in types' abstract base types. */
class TxBuiltinAbstractTypeDefNode final : public TxBuiltinTypeDefiningNode {
    /** Used for the built-in types' abstract base types. */
    class TxBuiltinBaseType final : public TxActualType {
        TxBuiltinBaseType* make_specialized_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                      const std::vector<TxTypeSpecialization>& interfaces) const override {
            if (! dynamic_cast<const TxBuiltinBaseType*>(baseTypeSpec.type))
                throw std::logic_error("Specified a base type for TxBuiltinBaseType that was not a TxBuiltinBaseType: " + baseTypeSpec.type->str());
            return new TxBuiltinBaseType(baseTypeSpec.type->get_type_class(), declaration, baseTypeSpec);
        }

    public:
        TxBuiltinBaseType(TxTypeClass typeClass, const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec)
            : TxActualType(typeClass, declaration, baseTypeSpec)  { }
        TxBuiltinBaseType(TxTypeClass typeClass, const TxTypeDeclaration* declaration, const TxActualType* baseType)
            : TxActualType(typeClass, declaration, TxTypeSpecialization(baseType))  { }

        virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override {
            //ASSERT(false, "Can't contruct LLVM type for abstract type " << this->to_string());
            LOG_DEBUG(context.LOGGER(), "LLVM type for abstract type " << this << " is VOID");
            return context.get_voidT();
        }
    };

protected:
    virtual TxActualType* define_builtin_type() override {
        auto baseType = this->baseTypeNode->resolve_type();
        return new TxBuiltinBaseType( this->typeClass, this->get_declaration(), baseType->type() );
    }
public:
    const TxTypeClass typeClass;
    TxBuiltinAbstractTypeDefNode( const TxLocation& parseLocation, BuiltinTypeId builtinTypeId, TxTypeExpressionNode* baseTypeNode, TxTypeClass typeClass,
                                  const std::vector<TxDeclarationNode*>& declNodes )
        : TxBuiltinTypeDefiningNode( parseLocation, builtinTypeId, baseTypeNode, declNodes ), typeClass( typeClass ) { }
};



class TxIntegerTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    virtual TxActualType* define_builtin_type() override {
        auto baseType = this->baseTypeNode->resolve_type();
        return new TxIntegerType( this->get_declaration(), baseType->type(), this->size, this->sign);
    }
public:
    const int size;
    const bool sign;
    TxIntegerTypeDefNode( const TxLocation& parseLocation, BuiltinTypeId builtinTypeId, TxTypeExpressionNode* baseTypeNode, int size, bool sign,
                          const std::vector<TxDeclarationNode*>& declNodes )
        : TxBuiltinTypeDefiningNode( parseLocation, builtinTypeId, baseTypeNode, declNodes ), size( size ), sign( sign )  { }
};


class TxFloatingTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    virtual TxActualType* define_builtin_type() override {
        auto baseType = this->baseTypeNode->resolve_type();
        return new TxFloatingType( this->get_declaration(), baseType->type(), this->size );
    }
public:
    const int size;
    TxFloatingTypeDefNode( const TxLocation& parseLocation, BuiltinTypeId builtinTypeId, TxTypeExpressionNode* baseTypeNode, int size,
                           const std::vector<TxDeclarationNode*>& declNodes )
        : TxBuiltinTypeDefiningNode( parseLocation, builtinTypeId, baseTypeNode, declNodes ), size( size )  { }
};


class TxBoolTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    virtual TxActualType* define_builtin_type() override {
        auto baseType = this->baseTypeNode->resolve_type();
        return new TxBoolType( this->get_declaration(), baseType->type() );
    }
public:
    TxBoolTypeDefNode( const TxLocation& parseLocation, TxTypeExpressionNode* baseTypeNode,
                       const std::vector<TxDeclarationNode*>& declNodes )
        : TxBuiltinTypeDefiningNode( parseLocation, BOOL, baseTypeNode, declNodes )  { }
};


class TxTupleTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    virtual TxActualType* define_builtin_type() override {
        auto baseType = this->baseTypeNode->resolve_type();
        return new TxTupleType( this->get_declaration(), baseType->type(), true );
    }
public:
    TxTupleTypeDefNode( const TxLocation& parseLocation, TxTypeExpressionNode* baseTypeNode,
                        const std::vector<TxDeclarationNode*>& declNodes )
        : TxBuiltinTypeDefiningNode( parseLocation, TUPLE, baseTypeNode, declNodes )  { }
};


class TxInterfaceTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    virtual TxActualType* define_builtin_type() override {
        auto baseType = this->baseTypeNode->resolve_type();
        return new TxInterfaceType( this->get_declaration(), baseType->type() );
    }
public:
    TxInterfaceTypeDefNode( const TxLocation& parseLocation, TxTypeExpressionNode* baseTypeNode,
                            const std::vector<TxDeclarationNode*>& declNodes )
        : TxBuiltinTypeDefiningNode( parseLocation, INTERFACE, baseTypeNode, declNodes )  { }
};


class TxRefTypeDefNode final : public TxBuiltinTypeDefiningNode {
    TxRefTypeDefNode( const TxLocation& parseLocation, const TxRefTypeDefNode* original,
                        TxTypeExpressionNode* baseTypeNode, const std::vector<TxDeclarationNode*>& declNodes )
        : TxBuiltinTypeDefiningNode( parseLocation, original, baseTypeNode, declNodes )  { }
protected:
    virtual TxActualType* define_builtin_type() override {
        auto baseType = this->baseTypeNode->resolve_type();
        return new TxReferenceType( this->get_declaration(), baseType->type() );
    }
public:
    TxRefTypeDefNode( const TxLocation& parseLocation, TxTypeExpressionNode* baseTypeNode,
                      const std::vector<TxDeclarationNode*>& declNodes, const TxRefTypeDefNode* original=nullptr )
        : TxBuiltinTypeDefiningNode( parseLocation, REFERENCE, baseTypeNode, declNodes )  { }

    virtual TxRefTypeDefNode* make_ast_copy() const override {
        return new TxRefTypeDefNode( this->parseLocation, this, this->baseTypeNode->make_ast_copy(), make_node_vec_copy( this->declNodes ) );
    }
};


class TxArrayTypeDefNode final : public TxBuiltinTypeDefiningNode {
    TxArrayTypeDefNode( const TxLocation& parseLocation, const TxArrayTypeDefNode* original,
                        TxTypeExpressionNode* baseTypeNode, const std::vector<TxDeclarationNode*>& declNodes )
        : TxBuiltinTypeDefiningNode( parseLocation, original, baseTypeNode, declNodes )  { }
protected:
    virtual TxActualType* define_builtin_type() override {
        auto baseType = this->baseTypeNode->resolve_type();
        return new TxArrayType( this->get_declaration(), baseType->type() );
    }
public:
    TxArrayTypeDefNode( const TxLocation& parseLocation, TxTypeExpressionNode* baseTypeNode,
                        const std::vector<TxDeclarationNode*>& declNodes, const TxArrayTypeDefNode* original=nullptr )
        : TxBuiltinTypeDefiningNode( parseLocation, ARRAY, baseTypeNode, declNodes )  { }

    virtual TxArrayTypeDefNode* make_ast_copy() const override {
        return new TxArrayTypeDefNode( this->parseLocation, this, this->baseTypeNode->make_ast_copy(), make_node_vec_copy( this->declNodes ) );
    }
};




/*----- built-in constructor / initializer type defining AST nodes -----*/


class TxDefConstructorTypeDefNode final : public TxFunctionTypeNode {
protected:
    TxExpressionNode* initExprNode;

    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext ) override {
        TxFunctionTypeNode::symbol_declaration_pass_descendants( defContext, lexContext );
        this->initExprNode->symbol_declaration_pass( this->context() );
    }

    virtual const TxType* define_type() override {
        auto actType = new TxBuiltinDefaultConstructorType( this->get_declaration(), this->registry().get_builtin_type( FUNCTION )->type(),
                                                            this->returnField->resolve_type()->type(), initExprNode );
        return this->registry().make_type_entity( actType );
    }

public:
    TxDefConstructorTypeDefNode( const TxLocation& parseLocation, TxTypeExpressionNode* returnTypeNode, TxExpressionNode* initExprNode)
        : TxFunctionTypeNode( parseLocation, false, new std::vector<TxFieldTypeDefNode*>(), returnTypeNode ), initExprNode( initExprNode )  { }

    virtual void symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext, const TxTypeDeclaration* owningDecl ) override {
        // overrides in order to create implicit declaration for the function type
        ASSERT(!owningDecl, "Expected NULL owningDeclaration: " << owningDecl);

        std::string funcTypeName = "$ftype";
        funcTypeName = lexContext.scope()->make_unique_name( funcTypeName );
        auto declaration = lexContext.scope()->declare_type( funcTypeName, this, TXD_PUBLIC | TXD_IMPLICIT );
        if (! declaration) {
            CERROR(this, "Failed to declare type " << funcTypeName);
            return;
        }
        //LOG(this->LOGGER(), INFO, this << ": Declared type " << declaration);

        TxTypeExpressionNode::symbol_declaration_pass( defContext, lexContext, declaration );
    }

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->initExprNode->visit_ast( visitor, thisAsParent, "initializer", context );
    }
};


class TxConvConstructorTypeDefNode final : public TxFunctionTypeNode {
protected:
    virtual const TxType* define_type() override {
        auto actType = new TxBuiltinConversionFunctionType( this->get_declaration(), this->registry().get_builtin_type( FUNCTION )->type(),
                                                            this->arguments->at(0)->resolve_type()->type(), this->returnField->resolve_type()->type() );
        return this->registry().make_type_entity( actType );
    }

public:
    TxConvConstructorTypeDefNode( const TxLocation& parseLocation, TxFieldTypeDefNode* fromTypeArg, TxTypeExpressionNode* returnTypeNode )
        : TxFunctionTypeNode( parseLocation, false, new std::vector<TxFieldTypeDefNode*>( { fromTypeArg } ), returnTypeNode )  { }

    virtual void symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext, const TxTypeDeclaration* owningDecl ) override {
        // overrides in order to create implicit declaration for the function type
        ASSERT(!owningDecl, "Expected NULL owningDeclaration: " << owningDecl);

        std::string funcTypeName = "$ftype";
        funcTypeName = lexContext.scope()->make_unique_name( funcTypeName );
        auto declaration = lexContext.scope()->declare_type( funcTypeName, this, TXD_PUBLIC | TXD_IMPLICIT );
        if (! declaration) {
            CERROR(this, "Failed to declare type " << funcTypeName);
            return;
        }
        //LOG(this->LOGGER(), INFO, this << ": Declared type " << declaration);

        TxTypeExpressionNode::symbol_declaration_pass( defContext, lexContext, declaration );
    }
};




/*----- helper functions creating the built-in elementary types' declarations and constructors -----*/


static TxTypeDeclNode* make_builtin_abstract( const TxLocation& parseLoc, TxTypeClass typeClass, BuiltinTypeId id, BuiltinTypeId parentId ) {
    auto baseTypeNode = new TxIdentifiedTypeNode( parseLoc, BUILTIN_TYPE_NAMES[parentId] );
    auto typeDecl = new TxTypeDeclNode( parseLoc, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, BUILTIN_TYPE_NAMES[id], nullptr,
                                        new TxBuiltinAbstractTypeDefNode( parseLoc, id, baseTypeNode, typeClass, {} ) );
    return typeDecl;
}

static TxTypeDeclNode* make_builtin_integer( const TxLocation& parseLoc, BuiltinTypeId id, BuiltinTypeId parentId,
                                             std::vector<std::vector<TxDeclarationNode*>>& constructors, int size, bool sign ) {
    auto baseTypeNode = new TxIdentifiedTypeNode( parseLoc, BUILTIN_TYPE_NAMES[parentId] );
    auto typeDecl = new TxTypeDeclNode( parseLoc, TXD_PUBLIC | TXD_BUILTIN, BUILTIN_TYPE_NAMES[id], nullptr,
                                        new TxIntegerTypeDefNode( parseLoc, id, baseTypeNode, size, sign, constructors[id] ) );
    return typeDecl;
}

static TxTypeDeclNode* make_builtin_floating( const TxLocation& parseLoc, BuiltinTypeId id, BuiltinTypeId parentId,
                                              std::vector<std::vector<TxDeclarationNode*>>& constructors, int size ) {
    auto baseTypeNode = new TxIdentifiedTypeNode( parseLoc, BUILTIN_TYPE_NAMES[parentId] );
    auto typeDecl = new TxTypeDeclNode( parseLoc, TXD_PUBLIC | TXD_BUILTIN, BUILTIN_TYPE_NAMES[id], nullptr,
                                        new TxFloatingTypeDefNode( parseLoc, id, baseTypeNode, size, constructors[id] ) );
    return typeDecl;
}



static TxFieldDeclNode* make_default_initializer( const TxLocation& loc, BuiltinTypeId toTypeId, TxExpressionNode* initializerExpr ) {
    auto toTypeNode = new TxIdentifiedTypeNode( loc, BUILTIN_TYPE_NAMES[toTypeId] );
    return new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_STATIC | TXD_BUILTIN | TXD_INITIALIZER,
                new TxFieldDefNode( loc, CONSTR_IDENT,
                                    new TxDefConstructorTypeDefNode( loc, toTypeNode, initializerExpr ),
                                    nullptr ),  // no function body, initialization is inlined
//                        new TxLambdaExprNode( loc,
//                                new TxDefConstructorTypeDefNode( loc, toTypeNode, initializerExpr ),
//                                new TxSuiteNode( loc, new std::vector<TxStatementNode*>( {
//                                                            new TxReturnStmtNode( loc, initializerExpr )
//                                                        } ) ),
//                                false )  // not method syntax since elementary types' initializers are inlineable, pure functions
//                        ),
                false );  // not method syntax since elementary types' initializers are inlineable, pure functions
}


static TxFieldDeclNode* make_conversion_initializer( const TxLocation& loc, BuiltinTypeId fromTypeId, BuiltinTypeId toTypeId ) {
    auto   toTypeNode = new TxIdentifiedTypeNode( loc, BUILTIN_TYPE_NAMES[toTypeId] );
    auto fromTypeNode = new TxIdentifiedTypeNode( loc, BUILTIN_TYPE_NAMES[fromTypeId] );
    return new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_STATIC | TXD_BUILTIN | TXD_INITIALIZER,
                new TxFieldDefNode( loc, CONSTR_IDENT,
                                    new TxConvConstructorTypeDefNode( loc, new TxFieldTypeDefNode( loc, "arg", fromTypeNode ), toTypeNode ),
                                    nullptr ),  // no function body, initialization is inlined
                false );  // not method syntax since elementary types' initializers are inlineable, pure functions
}



TxParsingUnitNode* BuiltinTypes::createTxModuleAST() {
    auto & loc = this->builtinLocation;

    { // create the Any root type:
        auto anyTypeDecl = new TxTypeDeclNode( loc, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, "Any", nullptr,
                                               new TxAnyTypeDefNode( loc ) );
        this->builtinTypes[ANY] = anyTypeDecl;
        // candidate members of the Any root type:
        // public static _typeid() UInt
        // public final _address() ULong
    }


    { // create the Void type:
        auto voidBaseTypeNode = new TxIdentifiedTypeNode( loc, BUILTIN_TYPE_NAMES[ANY] );
        auto voidTypeDecl = new TxTypeDeclNode( loc, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, "Void", nullptr,
                                                new TxVoidTypeDefNode( loc, voidBaseTypeNode ) );
        this->builtinTypes[VOID] = voidTypeDecl;
    }


    // default initializers for elementary, concrete built-ins:
    std::vector<std::vector<TxDeclarationNode*>> constructors( BuiltinTypeId_COUNT );
    constructors[BOOL]  .push_back( make_default_initializer( loc, BOOL,   new TxBoolLitNode( loc, false ) ) );
    constructors[BYTE]  .push_back( make_default_initializer( loc, BYTE,   new TxIntegerLitNode( loc, 0, true,  BYTE) ) );
    constructors[SHORT] .push_back( make_default_initializer( loc, SHORT,  new TxIntegerLitNode( loc, 0, true,  SHORT) ) );
    constructors[INT]   .push_back( make_default_initializer( loc, INT,    new TxIntegerLitNode( loc, 0, true,  INT) ) );
    constructors[LONG]  .push_back( make_default_initializer( loc, LONG,   new TxIntegerLitNode( loc, 0, true,  LONG) ) );
    constructors[UBYTE] .push_back( make_default_initializer( loc, UBYTE,  new TxIntegerLitNode( loc, 0, false, UBYTE) ) );
    constructors[USHORT].push_back( make_default_initializer( loc, USHORT, new TxIntegerLitNode( loc, 0, false, USHORT) ) );
    constructors[UINT]  .push_back( make_default_initializer( loc, UINT,   new TxIntegerLitNode( loc, 0, false, UINT) ) );
    constructors[ULONG] .push_back( make_default_initializer( loc, ULONG,  new TxIntegerLitNode( loc, 0, false, ULONG) ) );
    constructors[HALF]  .push_back( make_default_initializer( loc, HALF,   new TxFloatingLitNode( loc, HALF) ) );
    constructors[FLOAT] .push_back( make_default_initializer( loc, FLOAT,  new TxFloatingLitNode( loc, FLOAT) ) );
    constructors[DOUBLE].push_back( make_default_initializer( loc, DOUBLE, new TxFloatingLitNode( loc, DOUBLE) ) );

    // built-in conversion-initializers between the concrete elementary types (BOOL and the scalar types):
    const BuiltinTypeId CONCRETE_ELEM_TYPE_IDS[] = {
            BOOL,
            BYTE,
            SHORT,
            INT,
            LONG,
            UBYTE,
            USHORT,
            UINT,
            ULONG,
            HALF,
            FLOAT,
            DOUBLE,
    };
    for (auto toTypeId : CONCRETE_ELEM_TYPE_IDS) {
        for (auto fromTypeId : CONCRETE_ELEM_TYPE_IDS)
            constructors[toTypeId].push_back( make_conversion_initializer( loc, fromTypeId, toTypeId ) );
    }


    // create the built-in abstract base types:
    this->builtinTypes[ELEMENTARY]    = make_builtin_abstract( loc, TXTC_ELEMENTARY, ELEMENTARY,    ANY );
    this->builtinTypes[SCALAR]        = make_builtin_abstract( loc, TXTC_ELEMENTARY, SCALAR,        ELEMENTARY );
    this->builtinTypes[INTEGER]       = make_builtin_abstract( loc, TXTC_ELEMENTARY, INTEGER,       SCALAR );
    this->builtinTypes[SIGNED]        = make_builtin_abstract( loc, TXTC_ELEMENTARY, SIGNED,        INTEGER );
    this->builtinTypes[UNSIGNED]      = make_builtin_abstract( loc, TXTC_ELEMENTARY, UNSIGNED,      INTEGER );
    this->builtinTypes[FLOATINGPOINT] = make_builtin_abstract( loc, TXTC_ELEMENTARY, FLOATINGPOINT, SCALAR );

    // create the built-in concrete scalar types:
    this->builtinTypes[BYTE]          = make_builtin_integer ( loc, BYTE,            SIGNED,        constructors, 1, true );
    this->builtinTypes[SHORT]         = make_builtin_integer ( loc, SHORT,           SIGNED,        constructors, 2, true );
    this->builtinTypes[INT]           = make_builtin_integer ( loc, INT,             SIGNED,        constructors, 4, true );
    this->builtinTypes[LONG]          = make_builtin_integer ( loc, LONG,            SIGNED,        constructors, 8, true );
    this->builtinTypes[UBYTE]         = make_builtin_integer ( loc, UBYTE,           UNSIGNED,      constructors, 1, false );
    this->builtinTypes[USHORT]        = make_builtin_integer ( loc, USHORT,          UNSIGNED,      constructors, 2, false );
    this->builtinTypes[UINT]          = make_builtin_integer ( loc, UINT,            UNSIGNED,      constructors, 4, false );
    this->builtinTypes[ULONG]         = make_builtin_integer ( loc, ULONG,           UNSIGNED,      constructors, 8, false );
    this->builtinTypes[HALF]          = make_builtin_floating( loc, HALF,            FLOATINGPOINT, constructors, 2 );
    this->builtinTypes[FLOAT]         = make_builtin_floating( loc, FLOAT,           FLOATINGPOINT, constructors, 4 );
    this->builtinTypes[DOUBLE]        = make_builtin_floating( loc, DOUBLE,          FLOATINGPOINT, constructors, 8 );


    // create the boolean type:
    this->builtinTypes[BOOL] = new TxTypeDeclNode( loc, TXD_PUBLIC | TXD_BUILTIN, "Bool", nullptr,
            new TxBoolTypeDefNode( loc, new TxIdentifiedTypeNode( loc, "Elementary" ), constructors[BOOL] ) );


    // create the function base type:
    this->builtinTypes[FUNCTION] = new TxTypeDeclNode( loc, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, "Function", nullptr,
           new TxBuiltinAbstractTypeDefNode( loc, FUNCTION, new TxIdentifiedTypeNode( loc, "Any" ), TXTC_FUNCTION, {} ) );

    // create the tuple base type:
    this->builtinTypes[TUPLE] = new TxTypeDeclNode( loc, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, "Tuple", nullptr,
            new TxTupleTypeDefNode( loc, new TxIdentifiedTypeNode( loc, "Any" ), {} ) );

    // create the reference base type:
    {
        auto paramNodes = new std::vector<TxDeclarationNode*>( {
                new TxTypeDeclNode( loc, TXD_PUBLIC | TXD_GENPARAM, "T", nullptr, new TxIdentifiedTypeNode( loc, "Any") )
            } );
        this->builtinTypes[REFERENCE] = new TxTypeDeclNode( loc, TXD_PUBLIC | TXD_BUILTIN, "Ref", paramNodes,
                new TxRefTypeDefNode( loc, new TxIdentifiedTypeNode( loc, "Any" ), {} ) );
    }

    // create the array base type:
    {
        auto lTypeNode = new TxIdentifiedTypeNode( loc, "UInt");
        auto paramNodes = new std::vector<TxDeclarationNode*>( {
                new TxTypeDeclNode ( loc, TXD_PUBLIC | TXD_GENPARAM, "E", nullptr, new TxIdentifiedTypeNode( loc, "Any") ),
                new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_GENPARAM, new TxFieldDefNode( loc, "L", lTypeNode, nullptr, false) )
            } );
        this->builtinTypes[ARRAY] = new TxTypeDeclNode( loc, TXD_PUBLIC | TXD_BUILTIN, "Array", paramNodes,
                new TxArrayTypeDefNode( loc, new TxIdentifiedTypeNode( loc, "Any" ), {} ) );
    }

    // create the interface base type:
    {
        // the adaptee type id virtual field member, which is abstract here but concrete in adapter subtypes:
        const TxDeclarationFlags adapteeIdFieldFlags = TXD_PUBLIC | TXD_BUILTIN | TXD_STATIC | TXD_ABSTRACT | TXD_IMPLICIT;
        auto adapteeIdFType = new TxIdentifiedTypeNode( loc, "UInt" );
        auto adapteeIdField = new TxFieldDefNode( loc, "$adTypeId", adapteeIdFType, nullptr );
        auto adapteeIdFDecl = new TxFieldDeclNode( loc, adapteeIdFieldFlags, adapteeIdField );

        auto ifTypeDef = new TxInterfaceTypeDefNode( loc, new TxIdentifiedTypeNode( loc, "Any" ), { adapteeIdFDecl } );
        this->builtinTypes[INTERFACE] = new TxTypeDeclNode( loc, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, "Interface", nullptr,
                                                            ifTypeDef, true );
    }



    // put it all together as the AST for the tx module:

    std::vector<TxImportNode*>* imports = nullptr;
    std::vector<TxModuleNode*>* subModules = new std::vector<TxModuleNode*>();
    std::vector<TxDeclarationNode*>* members = new std::vector<TxDeclarationNode*>();

    for (unsigned id = 0; id < BuiltinTypeId_COUNT; id++) {
        ASSERT(this->builtinTypes[id], "not yet coded builtin type id: " << id);
        members->push_back( this->builtinTypes[id] );
    }

    subModules->push_back( new TxModuleNode( this->builtinLocation, new TxIdentifier("c"),
                                             nullptr, nullptr, nullptr, true ) );

    auto module = new TxModuleNode( this->builtinLocation, new TxIdentifier(BUILTIN_NS),
                                    imports, members, subModules, true );
    auto parsingUnit = new TxParsingUnitNode( this->builtinLocation, module );
    return parsingUnit;
}



/** Initializes the built-in symbols. */
void BuiltinTypes::initializeBuiltinSymbols() {

    // FIXME: remove when initializeBuiltinSymbols() no longer creates entities needing these to be pre-resolved:
    for (unsigned id = 0; id < BuiltinTypeId_COUNT; id++) {
        // verify that all built-in types are initialized:
        ASSERT(this->builtinTypes[id], "Uninitialized built-in type! id=" << id);
        this->builtinTypes[id]->typeExpression->resolve_type();
    }

    declare_tx_functions();
}

void BuiltinTypes::declare_tx_functions() {

//    auto txCfuncModule = this->registry.get_package().declare_module( this->registry.get_package().root_origin(), TxIdentifier(BUILTIN_NS ".c"), true);
    auto txCfuncModule = this->registry.package().lookup_module("tx.c");
    LexicalContext ctx(txCfuncModule);
    {   // declare tx.c.puts:
        auto refTypeNode = new TxReferenceTypeNode(this->builtinLocation, nullptr, new TxIdentifiedTypeNode(this->builtinLocation, "tx.UByte"));
        auto argNode = new TxFieldTypeDefNode( this->builtinLocation, "str", refTypeNode );
        auto c_puts_func_type_def = new TxFunctionTypeNode( this->builtinLocation, false, new std::vector<TxFieldTypeDefNode*>( { argNode } ),
                                                            new TxIdentifiedTypeNode( this->builtinLocation, "tx.Int" ) );
        auto c_puts_func_type_decl = new TxTypeDeclNode( this->builtinLocation, TXD_PUBLIC | TXD_IMPLICIT, "puts$func", nullptr,
                                                         c_puts_func_type_def );
        c_puts_func_type_decl->symbol_declaration_pass( ctx );
        c_puts_func_type_decl->symbol_resolution_pass();

        auto c_puts_def = new TxBuiltinFieldDefNode( this->builtinLocation );
        auto c_puts_decl = txCfuncModule->declare_field( "puts", c_puts_def, TXD_PUBLIC, TXS_GLOBAL, TxIdentifier("") );
        c_puts_def->set_field( TxField::make_field( c_puts_decl, c_puts_func_type_def->resolve_type() ) );
    }
    {  // declare tx.c.abort:
//        std::vector<const TxActualType*> argumentTypes( {} );
//        auto funcType = new TxFunctionType( nullptr, this->builtinTypes[FUNCTION]->typeExpression->resolve_type()->type(), argumentTypes,
//                                            this->builtinTypes[VOID]->typeExpression->resolve_type()->type() );
//        auto type = new TxType( funcType );
        auto c_abort_func_type_def = new TxFunctionTypeNode( this->builtinLocation, false, new std::vector<TxFieldTypeDefNode*>( {} ), nullptr );
        auto c_abort_func_type_decl = new TxTypeDeclNode( this->builtinLocation, TXD_PUBLIC | TXD_IMPLICIT, "abort$func", nullptr,
                                                          c_abort_func_type_def );
        c_abort_func_type_decl->symbol_declaration_pass( ctx );
        c_abort_func_type_decl->symbol_resolution_pass();

        auto c_abort_def = new TxBuiltinFieldDefNode( this->builtinLocation );
        auto c_abort_decl = txCfuncModule->declare_field( "abort", c_abort_def, TXD_PUBLIC, TXS_GLOBAL, TxIdentifier("") );
        c_abort_def->set_field( TxField::make_field( c_abort_decl, c_abort_func_type_def->resolve_type() ) );
    }

//    auto module = this->registry.get_package().lookup_module("tx");
//    LexicalContext moduleCtx( module );

    // public _address( r : Ref ) ULong
// FIXME
//    std::vector<const TxType*> argumentTypes( { this->builtinTypes[REFERENCE]->typeExpression->get_type() } );
//    auto funcTypeDef = new TxBuiltinFieldDefNode( this->builtinLocation );
//    auto funcDecl = module->declare_field("_address", funcTypeDef, TXD_PUBLIC | TXD_BUILTIN, TXS_GLOBAL, TxIdentifier(""));
//    auto type = new TxFunctionType(nullptr, this->builtinTypes[FUNCTION]->typeExpression->get_type(), argumentTypes,
//                                   this->builtinTypes[ULONG]->typeExpression->get_type());
//    funcTypeDef->set_field( TxField::make_field(funcDecl, type) );
}




BuiltinTypes::BuiltinTypes( TypeRegistry& registry )
        : registry(registry), builtinLocation( registry.package().root_origin().get_parse_location() ) {
}


const TxType* BuiltinTypes::get_builtin_type( const BuiltinTypeId id ) const {
    return this->builtinTypes[id]->typeExpression->get_type();
}




/*----- interface adapter -----*/

// TODO: can we refactor out these special node classes?

class TxImplicitTypeDefiningNode final : public TxTypeExpressionNode {
    const TxImplicitTypeDefiningNode* originalNode;
    const TxType* baseType;

    TxImplicitTypeDefiningNode( const TxImplicitTypeDefiningNode* original )
            : TxTypeExpressionNode(original->parseLocation), originalNode(original), baseType() { }

protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext ) override { }

    virtual const TxType* define_type() override {
        if (this->originalNode) {
            ASSERT(!this->baseType, "type already set");
            this->baseType = this->registry().make_type_entity( this->registry().make_actual_type( this->get_declaration(), this->originalNode->baseType->type() ) );
        }
        return this->baseType;
    }

public:
    TxImplicitTypeDefiningNode( const TxLocation& parseLocation )
            : TxTypeExpressionNode(parseLocation), originalNode(), baseType() { }

    /** Creates a copy of this node and all its descendants for purpose of generic specialization. */
    virtual TxImplicitTypeDefiningNode* make_ast_copy() const override {
        return new TxImplicitTypeDefiningNode( this );
    }

    virtual std::string get_auto_type_name() const override {
        return (this->get_declaration() ? this->get_declaration()->get_unique_full_name() : "");
    }

    void set_type(const TxType* type) {
        ASSERT(!this->baseType, "type already set");
        this->baseType = type;
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        if (this->originalNode)
            this->originalNode->visit_ast( visitor, thisAsParent, "originalnode", context );
    }
};

class TxImplicitTypeDeclNode : public TxTypeDeclNode {
public:
    TxImplicitTypeDeclNode( const TxLocation& parseLocation, const std::string plainName, TxDeclarationFlags declFlags=(TXD_PUBLIC | TXD_IMPLICIT),
                            const std::vector<TxDeclarationNode*>* typeParamDeclNodes=nullptr )
        : TxTypeDeclNode( parseLocation, declFlags, plainName, typeParamDeclNodes, new TxImplicitTypeDefiningNode(parseLocation) ) { }

    void set_type(const TxType* type) {
        static_cast<TxImplicitTypeDefiningNode*>(this->typeExpression)->set_type(type);
    }

    const TxType* get_type() const { return this->typeExpression->get_type(); }
};



const TxType* BuiltinTypes::inner_get_interface_adapter(const TxActualType* interfaceType, const TxActualType* adaptedType) {
    ASSERT(! interfaceType->is_modifiable(), "Shouldn't create adapter for 'modifiable' interface type: " << interfaceType);
    ASSERT(!   adaptedType->is_modifiable(), "Shouldn't create adapter for 'modifiable' adaptee type: "   << adaptedType);
    ASSERT(*interfaceType != *adaptedType,   "Shouldn't create adapter between equivalent types");

    // we want to bypass empty, implicit derivations:
    while (adaptedType->is_empty_derivation() && !adaptedType->is_explicit_nongen_declaration())
        adaptedType = adaptedType->get_base_type();

//    // (for now) we want to bypass equivalent reinterpretations:
//    // FIXME: experimental: is this correct when only one of these is reinterpreted?
//    while (interfaceType->is_equivalent_reinterpreted_specialization()) {
//        auto tmpType = interfaceType->get_declaration()->get_definer()->get_node()->get_type();
//        //std::cerr << "**** reverted from reinterpreted i/f type " << interfaceType << " to " << tmpType << std::endl;
//        interfaceType = tmpType;
//    }
//    while (adaptedType->is_equivalent_reinterpreted_specialization()) {
//        auto tmpType = adaptedType->get_declaration()->get_definer()->get_node()->get_type();
//        //std::cerr << "**** reverted from reinterpreted obj type " << adaptedType << " to " << tmpType << std::endl;
//        adaptedType = tmpType;
//    }

    auto ifDecl = interfaceType->get_declaration();
    auto scope = ifDecl->get_symbol()->get_outer();
    std::string adapterName = ifDecl->get_unique_name() + "$if$" + encode_type_name( adaptedType->get_declaration() );

    if (auto existingAdapterSymbol = dynamic_cast<TxEntitySymbol*>(scope->get_member_symbol(adapterName))) {
        if (auto typeDecl = existingAdapterSymbol->get_type_decl()) {
            auto adapterType = typeDecl->get_definer()->resolve_type();
            //std::cerr << "Getting existing interface adapter: " << adapterType << std::endl;
            return adapterType;
        }
    }

    //std::cerr << "Creating interface adapter: " << adapterName << "\n\tfrom " << adaptedType << "\n\tto   " << interfaceType << std::endl;
    // TODO: combine flags from adapted and adaptee types, including TXD_EXPERRBLOCK

    auto adapterDefiner = new TxImplicitTypeDeclNode( this->get_builtin_location(), adapterName );
    {
        auto ifDefiner = ifDecl->get_definer();
        auto & ctx = ifDefiner->context();
        adapterDefiner->symbol_declaration_pass(ctx);
    }
    //auto typeDecl = scope->declare_type(adapterName, adapterDefiner, TXD_PUBLIC | TXD_IMPLICIT);
    auto typeDecl = adapterDefiner->get_declaration();

    {   // override the adaptee type id virtual field member:
        const TxType* fieldType = this->get_builtin_type( UINT );
        auto fieldDef = new TxBuiltinFieldDefNode( this->get_builtin_location() );
        auto fieldDecl = typeDecl->get_symbol()->declare_field("$adTypeId", fieldDef, TXD_PUBLIC | TXD_STATIC | TXD_OVERRIDE | TXD_IMPLICIT,
                                                               TXS_VIRTUAL, "");
        fieldDef->set_field( TxField::make_field(fieldDecl, fieldType) );
    }

    auto adapterActType = new TxInterfaceAdapterType( typeDecl, interfaceType, adaptedType );
    auto adapterType = this->registry.make_type_entity( adapterActType );
    adapterDefiner->set_type( adapterType );
    adapterDefiner->symbol_resolution_pass();
    this->registry.add_type( adapterActType );
    return adapterType;
}

