#include "builtin_types.hpp"

#include "util/assert.hpp"

#include "ast.hpp"

#include "llvm_generator.hpp"


/*--- statically allocated built-in type objects ---*/

static const BuiltinTypeId SCALAR_TYPE_IDS[] = {
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

//static const BuiltinTypeId INTEGER_TYPE_IDS[] = {
//        BYTE,
//        SHORT,
//        INT,
//        LONG,
//        UBYTE,
//        USHORT,
//        UINT,
//        ULONG,
//};



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

    virtual std::string get_declared_name() const override {
        return this->builtinField->get_unique_name();
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
            this->baseType = this->types().make_specialized_type(this->get_declaration(), TxTypeSpecialization(this->originalNode->baseType), {});
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
        return this->get_declared_name();
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

class TxBuiltinTypeDeclNode final : public TxImplicitTypeDeclNode {
public:
    const BuiltinTypeId id;

    TxBuiltinTypeDeclNode( const TxLocation& parseLocation, std::string plainName, BuiltinTypeId id, TxDeclarationFlags declFlags,
                           const std::vector<TxDeclarationNode*>* typeParamDeclNodes=nullptr )
        : TxImplicitTypeDeclNode(parseLocation, plainName, declFlags, typeParamDeclNodes), id(id)  { }
};



/** Used solely for the Any root type object. */
class TxAnyType final : public TxType {
    TxAnyType(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec)
            : TxType(TXTC_ANY, declaration, baseTypeSpec)  { }

    TxType* make_specialized_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                  const std::vector<TxTypeSpecialization>& interfaces) const override {
        if (! dynamic_cast<const TxAnyType*>(baseTypeSpec.type))
            throw std::logic_error("Specified a base type for TxAnyType that was not a TxAnyType: " + baseTypeSpec.type->str());
        return new TxAnyType(declaration, baseTypeSpec);
    }

public:
    TxAnyType(const TxTypeDeclaration* declaration) : TxType(TXTC_ANY, declaration) { }

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override {
        //ASSERT(false, "Can't contruct LLVM type for abstract type " << this->to_string());
        context.LOG.debug("LLVM type for abstract type %s is VOID", this->str().c_str());
        return context.get_voidT();
    }
};


/** Used for the built-in types' abstract base types. */
class TxBuiltinBaseType final : public TxType {
    TxType* make_specialized_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                  const std::vector<TxTypeSpecialization>& interfaces) const override {
        if (! dynamic_cast<const TxBuiltinBaseType*>(baseTypeSpec.type))
            throw std::logic_error("Specified a base type for TxBuiltinBaseType that was not a TxBuiltinBaseType: " + baseTypeSpec.type->str());
        return new TxBuiltinBaseType(baseTypeSpec.type->get_type_class(), declaration, baseTypeSpec);
    }

public:
    TxBuiltinBaseType(TxTypeClass typeClass, const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec)
        : TxType(typeClass, declaration, baseTypeSpec)  { }
    TxBuiltinBaseType(TxTypeClass typeClass, const TxTypeDeclaration* declaration, const TxType* baseType)
        : TxType(typeClass, declaration, TxTypeSpecialization(baseType))  { }

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override {
        //ASSERT(false, "Can't contruct LLVM type for abstract type " << this->to_string());
        context.LOG.debug("LLVM type for abstract type %s is VOID", this->str().c_str());
        return context.get_voidT();
    }
};



/*-----------------------------------*/

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
            return this->types().make_specialized_type( this->get_declaration(), TxTypeSpecialization( this->original->get_type() ) );
        }
        else {
            auto type = this->define_builtin_type();
            //type->prepare_type_members();
            type->staticTypeId = builtinTypeId;
            this->types().add_type( type );
            return type;
        }
    }

    virtual TxType* define_builtin_type() = 0;

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

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        if (this->baseTypeNode)
            this->baseTypeNode->visit_ast( visitor, thisAsParent, "basetype", context );
        for (auto decl : this->declNodes)
            decl->visit_ast( visitor, thisAsParent, "decl", context );
    }
};



class TxAnyTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    virtual TxType* define_builtin_type() override {
        return new TxAnyType( this->get_declaration() );
    }

public:
    TxAnyTypeDefNode( const TxLocation& parseLocation, const std::vector<TxDeclarationNode*>& declNodes={} )
        : TxBuiltinTypeDefiningNode( parseLocation, ANY, nullptr, declNodes ) { }
};


class TxBuiltinAbstractTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    virtual TxType* define_builtin_type() override {
        auto baseType = this->baseTypeNode->resolve_type();
        return new TxBuiltinBaseType( this->typeClass, this->get_declaration(), baseType );
    }
public:
    const TxTypeClass typeClass;
    TxBuiltinAbstractTypeDefNode( const TxLocation& parseLocation, BuiltinTypeId builtinTypeId, TxTypeExpressionNode* baseTypeNode, TxTypeClass typeClass,
                                  const std::vector<TxDeclarationNode*>& declNodes )
        : TxBuiltinTypeDefiningNode( parseLocation, builtinTypeId, baseTypeNode, declNodes ), typeClass( typeClass ) { }
};


class TxIntegerTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    virtual TxType* define_builtin_type() override {
        auto baseType = this->baseTypeNode->resolve_type();
        return new TxIntegerType( this->get_declaration(), baseType, this->size, this->sign);
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
    virtual TxType* define_builtin_type() override {
        auto baseType = this->baseTypeNode->resolve_type();
        return new TxFloatingType( this->get_declaration(), baseType, this->size );
    }
public:
    const int size;
    TxFloatingTypeDefNode( const TxLocation& parseLocation, BuiltinTypeId builtinTypeId, TxTypeExpressionNode* baseTypeNode, int size,
                           const std::vector<TxDeclarationNode*>& declNodes )
        : TxBuiltinTypeDefiningNode( parseLocation, builtinTypeId, baseTypeNode, declNodes ), size( size )  { }
};


class TxBoolTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    virtual TxType* define_builtin_type() override {
        auto baseType = this->baseTypeNode->resolve_type();
        return new TxBoolType( this->get_declaration(), baseType );
    }
public:
    TxBoolTypeDefNode( const TxLocation& parseLocation, TxTypeExpressionNode* baseTypeNode,
                       const std::vector<TxDeclarationNode*>& declNodes )
        : TxBuiltinTypeDefiningNode( parseLocation, BOOL, baseTypeNode, declNodes )  { }
};


class TxTupleTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    virtual TxType* define_builtin_type() override {
        auto baseType = this->baseTypeNode->resolve_type();
        return new TxTupleType( this->get_declaration(), baseType, true );
    }
public:
    TxTupleTypeDefNode( const TxLocation& parseLocation, TxTypeExpressionNode* baseTypeNode,
                        const std::vector<TxDeclarationNode*>& declNodes )
        : TxBuiltinTypeDefiningNode( parseLocation, TUPLE, baseTypeNode, declNodes )  { }
};


class TxInterfaceTypeDefNode final : public TxBuiltinTypeDefiningNode {
protected:
    virtual TxType* define_builtin_type() override {
        auto baseType = this->baseTypeNode->resolve_type();
        return new TxInterfaceType( this->get_declaration(), baseType );
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
    virtual TxType* define_builtin_type() override {
        auto baseType = this->baseTypeNode->resolve_type();
        return new TxReferenceType( this->get_declaration(), baseType );
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
    virtual TxType* define_builtin_type() override {
        auto baseType = this->baseTypeNode->resolve_type();
        return new TxArrayType( this->get_declaration(), baseType );
    }
public:
    TxArrayTypeDefNode( const TxLocation& parseLocation, TxTypeExpressionNode* baseTypeNode,
                        const std::vector<TxDeclarationNode*>& declNodes, const TxArrayTypeDefNode* original=nullptr )
        : TxBuiltinTypeDefiningNode( parseLocation, ARRAY, baseTypeNode, declNodes )  { }

    virtual TxArrayTypeDefNode* make_ast_copy() const override {
        return new TxArrayTypeDefNode( this->parseLocation, this, this->baseTypeNode->make_ast_copy(), make_node_vec_copy( this->declNodes ) );
    }
};


//class TxDefConstructorTypeDefNode final : public TxBuiltinTypeDefiningNode {  // TODO: WIP
//protected:
//    TxTypeExpressionNode* returnTypeNode;
//    TxExpressionNode* initExprNode;
//
//    virtual TxType* define_builtin_type() override {
//        // hackish but works since they don't really declare anything:
//        this->returnTypeNode->symbol_declaration_pass( this->context(), this->context(), nullptr );
//        this->initExprNode->symbol_declaration_pass( this->context() );
//
//        auto baseType = this->baseTypeNode->resolve_type();
//        auto returnType = this->returnTypeNode->resolve_type();
//        return new TxBuiltinDefaultConstructorType( this->get_declaration(), baseType, returnType, initExprNode );
//    }
//
//public:
//    TxDefConstructorTypeDefNode( const TxLocation& parseLocation, TxTypeExpressionNode* baseTypeNode,
//                                 TxTypeExpressionNode* returnTypeNode, TxExpressionNode* initExprNode,
//                                 const std::vector<TxDeclarationNode*>& declNodes )
//        : TxBuiltinTypeDefiningNode( parseLocation, TXBTID_NOTSET, baseTypeNode, declNodes ), returnTypeNode( returnTypeNode ), initExprNode( initExprNode )  { }
//
//    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
//        this->returnTypeNode->visit_ast( visitor, thisAsParent, "returntype", context );
//        this->initExprNode->visit_ast( visitor, thisAsParent, "initializer", context );
//    }
//};




TxTypeDeclNode* BuiltinTypes::make_builtin_abstract( TxTypeClass typeClass, BuiltinTypeId id, std::string plainName, std::string parentName ) const {
    auto baseTypeNode = new TxIdentifiedTypeNode( this->builtinLocation, parentName );
    auto typeDecl = new TxTypeDeclNode( this->builtinLocation, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, plainName, nullptr,
                                        new TxBuiltinAbstractTypeDefNode( this->builtinLocation, id, baseTypeNode, typeClass, {} ) );
    return typeDecl;
}

TxTypeDeclNode* BuiltinTypes::make_builtin_integer( BuiltinTypeId id, std::string plainName, std::string parentName, int size, bool sign ) const {
    auto baseTypeNode = new TxIdentifiedTypeNode( this->builtinLocation, parentName );
    auto typeDecl = new TxTypeDeclNode( this->builtinLocation, TXD_PUBLIC | TXD_BUILTIN, plainName, nullptr,
                                        new TxIntegerTypeDefNode( this->builtinLocation, id, baseTypeNode, size, sign, {} ) );
    return typeDecl;
}

TxTypeDeclNode* BuiltinTypes::make_builtin_floating( BuiltinTypeId id, std::string plainName, std::string parentName, int size ) const {
    auto baseTypeNode = new TxIdentifiedTypeNode( this->builtinLocation, parentName );
    auto typeDecl = new TxTypeDeclNode( this->builtinLocation, TXD_PUBLIC | TXD_BUILTIN, plainName, nullptr,
                                        new TxFloatingTypeDefNode( this->builtinLocation, id, baseTypeNode, size, {} ) );
    return typeDecl;
}



//TxFieldDeclNode* BuiltinTypes::make_default_constructor( const std::string& toTypeName, TxExpressionNode* initValueExpr ) const {
//    // constructors for built-in elementary types are really inline conversion expressions
//
//    // the function body created here is just a dummy, will not get invoked
//    //auto funcArgs = new std::vector<TxFieldDefNode*>( { new TxFieldDefNode( this->builtinLocation, "arg", new TxIdentifiedTypeNode( this->builtinLocation, toTypeName ), nullptr ) } );
//    auto funcTypeNode = new TxFunctionTypeNode( this->builtinLocation, false, new std::vector<TxFieldDefNode*>(), new TxIdentifiedTypeNode( this->builtinLocation, toTypeName ) );
//    auto constructorBody = new TxLambdaExprNode( this->builtinLocation, funcTypeNode, new TxSuiteNode( this->builtinLocation ) );
//
//    auto constructorType = new TxDefConstructorTypeDefNode( this->builtinLocation, new TxIdentifiedTypeNode( this->builtinLocation, "Function" ),
//                                                            new TxIdentifiedTypeNode( this->builtinLocation, toTypeName ), initValueExpr, {} );
//    auto constructorField = new TxFieldDefNode( this->builtinLocation, "$init", constructorType, constructorBody );
//    auto constructorDecl = new TxFieldDeclNode( this->builtinLocation, TXD_PUBLIC | TXD_BUILTIN | TXD_CONSTRUCTOR, constructorField );
//    return constructorDecl;
//}


void BuiltinTypes::declare_default_constructor(LexicalContext& ctx, BuiltinTypeId toTypeId, TxExpressionNode* initValueExpr) {
    initValueExpr->symbol_declaration_pass( ctx );

    // constructors for built-in elementary types are really inline conversion expressions
    auto typeDecl = this->builtinTypes[toTypeId]->get_declaration();
    auto constructorDefiner = new TxBuiltinFieldDefNode( this->builtinLocation );
    auto constructorDecl = typeDecl->get_symbol()->declare_field("$init", constructorDefiner, TXD_PUBLIC | TXD_BUILTIN | TXD_CONSTRUCTOR, TXS_INSTANCEMETHOD, TxIdentifier(""));
    auto type = new TxBuiltinDefaultConstructorType(nullptr, this->builtinTypes[FUNCTION]->typeExpression->get_type(),
                                                    this->builtinTypes[toTypeId]->typeExpression->get_type(),
                                                    initValueExpr);
    type->prepare_members();
    constructorDefiner->set_field( TxField::make_field(constructorDecl, type) );
}

void BuiltinTypes::declare_conversion_constructor(BuiltinTypeId fromTypeId, BuiltinTypeId toTypeId) {
    // constructors for built-in elementary types are really inline conversion expressions
    auto typeDecl = this->builtinTypes[toTypeId]->get_declaration();
    auto constructorDefiner = new TxBuiltinFieldDefNode( this->builtinLocation );
    auto constructorDecl = typeDecl->get_symbol()->declare_field("$init", constructorDefiner, TXD_PUBLIC | TXD_BUILTIN | TXD_CONSTRUCTOR, TXS_INSTANCEMETHOD, TxIdentifier(""));
    auto type = new TxBuiltinConversionFunctionType(nullptr, this->builtinTypes[FUNCTION]->typeExpression->get_type(),
                                                    this->builtinTypes[fromTypeId]->typeExpression->get_type(),
                                                    this->builtinTypes[toTypeId]->typeExpression->get_type());
    type->prepare_members();
    constructorDefiner->set_field( TxField::make_field(constructorDecl, type) );
}



void BuiltinTypes::declare_tx_functions(TxModule* module) {
    // public _address( r : Ref ) ULong
// FIXME
//    std::vector<const TxType*> argumentTypes( { this->builtinTypes[REFERENCE]->typeExpression->get_type() } );
//    auto funcTypeDef = new TxBuiltinFieldDefNode( this->builtinLocation );
//    auto funcDecl = module->declare_field("_address", funcTypeDef, TXD_PUBLIC | TXD_BUILTIN, TXS_GLOBAL, TxIdentifier(""));
//    auto type = new TxFunctionType(nullptr, this->builtinTypes[FUNCTION]->typeExpression->get_type(), argumentTypes,
//                                   this->builtinTypes[ULONG]->typeExpression->get_type());
//    funcTypeDef->set_field( TxField::make_field(funcDecl, type) );
}



void BuiltinTypes::add_builtin_abstract(TxModule* module, TxTypeClass typeClass, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId) {
    auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, plainName, id, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT );
    LexicalContext lexCtx( module );
    definer->symbol_declaration_pass( lexCtx );
    auto type = new TxBuiltinBaseType( typeClass, definer->get_declaration(), this->builtinTypes[parentId]->typeExpression->get_type());
    type->prepare_members();
    definer->set_type( type );
    definer->symbol_resolution_pass();
    this->builtinTypes[definer->id] = definer;
}

void BuiltinTypes::add_builtin_integer(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId,
                                       int size, bool sign) {
    auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, plainName, id, TXD_PUBLIC | TXD_BUILTIN );
    LexicalContext lexCtx( module );
    definer->symbol_declaration_pass( lexCtx );
    auto type = new TxIntegerType( definer->get_declaration(), this->builtinTypes[parentId]->typeExpression->get_type(), size, sign);
    type->prepare_members();
    definer->set_type( type );
    definer->symbol_resolution_pass();
    this->builtinTypes[definer->id] = definer;
}

void BuiltinTypes::add_builtin_floating(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId, int size) {
    auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, plainName, id, TXD_PUBLIC | TXD_BUILTIN );
    LexicalContext lexCtx( module );
    definer->symbol_declaration_pass( lexCtx );
    auto type = new TxFloatingType( definer->get_declaration(), this->builtinTypes[parentId]->typeExpression->get_type(), size );
    type->prepare_members();
    definer->set_type( type );
    definer->symbol_resolution_pass();
    this->builtinTypes[definer->id] = definer;
}



TxParsingUnitNode* BuiltinTypes::createTxModuleAST() {
    // create the Any root type:
    auto anyTypeDecl = new TxTypeDeclNode( this->builtinLocation, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, "Any", nullptr,
                                           new TxAnyTypeDefNode( this->builtinLocation ) );
    this->builtinTypes[ANY] = anyTypeDecl;
    // candidate members of the Any root type:
    // public static _typeid() UInt
    // public final _address() ULong


    // create the built-in abstract base types:
    this->builtinTypes[ELEMENTARY]    = this->make_builtin_abstract( TXTC_ELEMENTARY, ELEMENTARY,    "Elementary",     "Any" );
    this->builtinTypes[SCALAR]        = this->make_builtin_abstract( TXTC_ELEMENTARY, SCALAR,        "Scalar",         "Elementary" );
    this->builtinTypes[INTEGER]       = this->make_builtin_abstract( TXTC_ELEMENTARY, INTEGER,       "Integer",        "Scalar" );
    this->builtinTypes[SIGNED]        = this->make_builtin_abstract( TXTC_ELEMENTARY, SIGNED,        "Signed",         "Integer" );
    this->builtinTypes[UNSIGNED]      = this->make_builtin_abstract( TXTC_ELEMENTARY, UNSIGNED,      "Unsigned",       "Integer" );
    this->builtinTypes[FLOATINGPOINT] = this->make_builtin_abstract( TXTC_ELEMENTARY, FLOATINGPOINT, "FloatingPoint",  "Scalar" );

    // create the built-in concrete scalar types:
    this->builtinTypes[BYTE]          = this->make_builtin_integer ( BYTE,          "Byte",     "Signed",   1, true );
    this->builtinTypes[SHORT]         = this->make_builtin_integer ( SHORT,         "Short",    "Signed",   2, true );
    this->builtinTypes[INT]           = this->make_builtin_integer ( INT,           "Int",      "Signed",   4, true );
    this->builtinTypes[LONG]          = this->make_builtin_integer ( LONG,          "Long",     "Signed",   8, true );
    this->builtinTypes[UBYTE]         = this->make_builtin_integer ( UBYTE,         "UByte",    "Unsigned", 1, false );
    this->builtinTypes[USHORT]        = this->make_builtin_integer ( USHORT,        "UShort",   "Unsigned", 2, false );
    this->builtinTypes[UINT]          = this->make_builtin_integer ( UINT,          "UInt",     "Unsigned", 4, false );
    this->builtinTypes[ULONG]         = this->make_builtin_integer ( ULONG,         "ULong",    "Unsigned", 8, false );
    this->builtinTypes[HALF]          = this->make_builtin_floating( HALF,          "Half",     "FloatingPoint", 2 );
    this->builtinTypes[FLOAT]         = this->make_builtin_floating( FLOAT,         "Float",    "FloatingPoint", 4 );
    this->builtinTypes[DOUBLE]        = this->make_builtin_floating( DOUBLE,        "Double",   "FloatingPoint", 8 );

    // create the boolean type:
    //auto boolDefConstr = make_default_constructor( "Bool", new TxBoolLitNode( this->builtinLocation, false ) );
    this->builtinTypes[BOOL] = new TxTypeDeclNode( this->builtinLocation, TXD_PUBLIC | TXD_BUILTIN, "Bool", nullptr,
            new TxBoolTypeDefNode( this->builtinLocation, new TxIdentifiedTypeNode( this->builtinLocation, "Elementary" ), { /*boolDefConstr*/ } ) );

    // create the function base type:
    this->builtinTypes[FUNCTION] = new TxTypeDeclNode( this->builtinLocation, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, "Function", nullptr,
           new TxBuiltinAbstractTypeDefNode( this->builtinLocation, FUNCTION, new TxIdentifiedTypeNode( this->builtinLocation, "Any" ), TXTC_FUNCTION, {} ) );

    // create the tuple base type:
    this->builtinTypes[TUPLE] = new TxTypeDeclNode( this->builtinLocation, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, "Tuple", nullptr,
            new TxTupleTypeDefNode( this->builtinLocation, new TxIdentifiedTypeNode( this->builtinLocation, "Any" ), {} ) );

    // create the reference base type:
    {
        auto paramNodes = new std::vector<TxDeclarationNode*>( {
                new TxTypeDeclNode(this->builtinLocation, TXD_PUBLIC | TXD_GENPARAM, "T", nullptr, new TxIdentifiedTypeNode(this->builtinLocation, "Any") )
            } );
        this->builtinTypes[REFERENCE] = new TxTypeDeclNode( this->builtinLocation, TXD_PUBLIC | TXD_BUILTIN, "Ref", paramNodes,
                new TxRefTypeDefNode( this->builtinLocation, new TxIdentifiedTypeNode( this->builtinLocation, "Any" ), {} ) );
    }

    // create the array base type:
    {
        auto lTypeNode = new TxIdentifiedTypeNode(this->builtinLocation, "UInt");
        auto paramNodes = new std::vector<TxDeclarationNode*>( {
                new TxTypeDeclNode (this->builtinLocation, TXD_PUBLIC | TXD_GENPARAM, "E", nullptr, new TxIdentifiedTypeNode(this->builtinLocation, "Any") ),
                new TxFieldDeclNode(this->builtinLocation, TXD_PUBLIC | TXD_GENPARAM, new TxFieldDefNode(this->builtinLocation, "L", lTypeNode, nullptr, false) )
            } );
        this->builtinTypes[ARRAY] = new TxTypeDeclNode( this->builtinLocation, TXD_PUBLIC | TXD_BUILTIN, "Array", paramNodes,
                new TxArrayTypeDefNode( this->builtinLocation, new TxIdentifiedTypeNode( this->builtinLocation, "Any" ), {} ) );
    }

    // create the interface base type:
    {
        // the adaptee type id virtual field member, which is abstract here but concrete in adapter subtypes:
        const TxDeclarationFlags adapteeIdFieldFlags = TXD_PUBLIC | TXD_BUILTIN | TXD_STATIC | TXD_ABSTRACT | TXD_IMPLICIT;
        auto adapteeIdFType = new TxIdentifiedTypeNode( this->builtinLocation, "UInt" );
        auto adapteeIdField = new TxFieldDefNode( this->builtinLocation, "$adTypeId", adapteeIdFType, nullptr );
        auto adapteeIdFDecl = new TxFieldDeclNode( this->builtinLocation, adapteeIdFieldFlags, adapteeIdField );

        auto ifTypeDef = new TxInterfaceTypeDefNode( this->builtinLocation, new TxIdentifiedTypeNode( this->builtinLocation, "Any" ), { adapteeIdFDecl } );
        this->builtinTypes[INTERFACE] = new TxTypeDeclNode( this->builtinLocation, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, "Interface", nullptr,
                                                            ifTypeDef, true );
    }



//    // default constructors for elementary, concrete built-ins:
//    declare_default_constructor(moduleCtx, BYTE,   new TxIntegerLitNode(this->builtinLocation, 0, true,  BYTE));
//    declare_default_constructor(moduleCtx, SHORT,  new TxIntegerLitNode(this->builtinLocation, 0, true,  SHORT));
//    declare_default_constructor(moduleCtx, INT,    new TxIntegerLitNode(this->builtinLocation, 0, true,  INT));
//    declare_default_constructor(moduleCtx, LONG,   new TxIntegerLitNode(this->builtinLocation, 0, true,  LONG));
//    declare_default_constructor(moduleCtx, UBYTE,  new TxIntegerLitNode(this->builtinLocation, 0, false, UBYTE));
//    declare_default_constructor(moduleCtx, USHORT, new TxIntegerLitNode(this->builtinLocation, 0, false, USHORT));
//    declare_default_constructor(moduleCtx, UINT,   new TxIntegerLitNode(this->builtinLocation, 0, false, UINT));
//    declare_default_constructor(moduleCtx, ULONG,  new TxIntegerLitNode(this->builtinLocation, 0, false, ULONG));
//    declare_default_constructor(moduleCtx, HALF,   new TxFloatingLitNode(this->builtinLocation, HALF));
//    declare_default_constructor(moduleCtx, FLOAT,  new TxFloatingLitNode(this->builtinLocation, FLOAT));
//    declare_default_constructor(moduleCtx, DOUBLE, new TxFloatingLitNode(this->builtinLocation, DOUBLE));
//
//    // scalar conversion-constructor functions:
//    for (auto fromTypeId : SCALAR_TYPE_IDS) {
//        for (auto toTypeId : SCALAR_TYPE_IDS) {
//            declare_conversion_constructor(fromTypeId, toTypeId);
//        }
//        declare_conversion_constructor(fromTypeId, BOOL);
//    }
//    declare_conversion_constructor(BOOL, BOOL);
//
//
//    declare_tx_functions(module);



    std::vector<TxImportNode*>* imports = nullptr;
    std::vector<TxModuleNode*>* subModules = new std::vector<TxModuleNode*>();
    std::vector<TxDeclarationNode*>* members = new std::vector<TxDeclarationNode*>();

    // register the built-in types and create modifiable specializations of them:
    for (unsigned id = 0; id < BuiltinTypeId_COUNT; id++) {
        if (this->builtinTypes[id])
            members->push_back( this->builtinTypes[id] );
        else
            std::cerr << "not yet coded builtin type id: " << id << std::endl;
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
//    auto module = this->registry.get_package().declare_module( this->registry.get_package().root_origin(), * new TxIdentifier(BUILTIN_NS), true );
//    LexicalContext moduleCtx(module);
//
//    // create the Any root type:
//    {
//        auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, "Any", ANY, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT );
//        LexicalContext lexCtx( module );
//        definer->symbol_declaration_pass( lexCtx );
//        auto type = new TxAnyType( definer->get_declaration() );
//        type->prepare_type_members();
//        definer->set_type( type );
//        definer->symbol_resolution_pass();
//        this->builtinTypes[definer->id] = definer;
//    }
//
//    // create the built-in abstract base types:
//    this->add_builtin_abstract( module, TXTC_ELEMENTARY, ELEMENTARY,    "Elementary",     ANY );
//    this->add_builtin_abstract( module, TXTC_ELEMENTARY, SCALAR,        "Scalar",         ELEMENTARY );
//    this->add_builtin_abstract( module, TXTC_ELEMENTARY, INTEGER,       "Integer",        SCALAR );
//    this->add_builtin_abstract( module, TXTC_ELEMENTARY, SIGNED,        "Signed",         INTEGER );
//    this->add_builtin_abstract( module, TXTC_ELEMENTARY, UNSIGNED,      "Unsigned",       INTEGER );
//    this->add_builtin_abstract( module, TXTC_ELEMENTARY, FLOATINGPOINT, "FloatingPoint",  SCALAR );
//
//    // create the built-in concrete scalar types:
//    this->add_builtin_integer ( module, BYTE,          "Byte",     SIGNED,   1, true );
//    this->add_builtin_integer ( module, SHORT,         "Short",    SIGNED,   2, true );
//    this->add_builtin_integer ( module, INT,           "Int",      SIGNED,   4, true );
//    this->add_builtin_integer ( module, LONG,          "Long",     SIGNED,   8, true );
//    this->add_builtin_integer ( module, UBYTE,         "UByte",    UNSIGNED, 1, false );
//    this->add_builtin_integer ( module, USHORT,        "UShort",   UNSIGNED, 2, false );
//    this->add_builtin_integer ( module, UINT,          "UInt",     UNSIGNED, 4, false );
//    this->add_builtin_integer ( module, ULONG,         "ULong",    UNSIGNED, 8, false );
//    this->add_builtin_floating( module, HALF,          "Half",     FLOATINGPOINT, 2 );
//    this->add_builtin_floating( module, FLOAT,         "Float",    FLOATINGPOINT, 4 );
//    this->add_builtin_floating( module, DOUBLE,        "Double",   FLOATINGPOINT, 8 );
//
//    //this->add_builtin_integer( module, CHAR,          "Char",    UNSIGNED, 1, false );
//    //this->add_builtin( module, STRING,        "String",  new TxArrayType(this->builtinTypes[ARRAY );
//
//    // create the boolean type:
//    {
//        auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, "Bool", BOOL, TXD_PUBLIC | TXD_BUILTIN );
//        definer->symbol_declaration_pass( moduleCtx );
//        auto type = new TxBoolType(definer->get_declaration(), this->builtinTypes[ELEMENTARY]->typeExpression->get_type() );
//        type->prepare_type_members();
//        definer->set_type( type );
//        definer->symbol_resolution_pass();
//        this->builtinTypes[definer->id] = definer;
//    }
//
//    // create the function base type:
//    {
//        auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, "Function", FUNCTION, TXD_PUBLIC | TXD_BUILTIN );
//        definer->symbol_declaration_pass( moduleCtx );
//        auto type = new TxBuiltinBaseType(TXTC_FUNCTION, definer->get_declaration(), this->builtinTypes[ANY]->typeExpression->get_type() );
//        type->prepare_type_members();
//        definer->set_type( type );
//        definer->symbol_resolution_pass();
//        this->builtinTypes[definer->id] = definer;
//    }
//
//    // create the reference base type:
//    {
//        //auto elemTypeDefiner = this->builtinTypes[ANY];
//        //typeDecl->get_symbol()->declare_type( "T", elemTypeDefiner, TXD_PUBLIC | TXD_BUILTIN | TXD_GENPARAM );
//        auto anyType = new TxIdentifiedTypeNode(this->builtinLocation, "tx.Any");
//        auto paramNodes = new std::vector<TxDeclarationNode*>( { new TxTypeDeclNode(this->builtinLocation, TXD_PUBLIC | TXD_GENPARAM,
//                                                                                    "T", nullptr, anyType) } );
//        auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, "Ref", REFERENCE, TXD_PUBLIC | TXD_BUILTIN, paramNodes );
//        definer->symbol_declaration_pass( moduleCtx );
//
//        auto type = new TxReferenceType(definer->get_declaration(), this->builtinTypes[ANY]->typeExpression->get_type());
//        definer->set_type( type );
//        definer->symbol_resolution_pass();
//        this->builtinTypes[definer->id] = definer;
//
//        type->prepare_type_members();
//    }
//
//    // create the array base type:
//    {
//        //auto elemTypeDefiner = this->builtinTypes[ANY];
//        //typeDecl->get_symbol()->declare_type( "E", elemTypeDefiner, TXD_PUBLIC | TXD_BUILTIN | TXD_GENPARAM );
//        //auto lengthDefiner = new TxBuiltinFieldDefiner();
//        //auto lengthDecl = typeDecl->get_symbol()->declare_field( "L", lengthDefiner, TXD_PUBLIC | TXD_BUILTIN | TXD_GENPARAM, TXS_INSTANCE, TxIdentifier() );
//        //lengthDefiner->set_field( TxField::make_field(lengthDecl, this->builtinTypes[UINT]->typeExpression->get_type()) );
//        auto anyType = new TxIdentifiedTypeNode(this->builtinLocation, "tx.Any");
//        auto lTypeNode = new TxTypeDeclWrapperNode( this->builtinLocation, this->builtinTypes[UINT]->get_declaration() );
//        auto lenFieldDef = new TxFieldDefNode(this->builtinLocation, "L", lTypeNode, nullptr, false);
//        auto paramNodes = new std::vector<TxDeclarationNode*>( { new TxTypeDeclNode(this->builtinLocation, TXD_PUBLIC | TXD_GENPARAM,
//                                                                                    "E", nullptr, anyType),
//                                                                 new TxFieldDeclNode(this->builtinLocation, TXD_PUBLIC | TXD_GENPARAM,
//                                                                                     lenFieldDef) } );
//        auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, "Array", ARRAY, TXD_PUBLIC | TXD_BUILTIN, paramNodes );
//        definer->symbol_declaration_pass( moduleCtx );
//
//        auto type = new TxArrayType(definer->get_declaration(), this->builtinTypes[ANY]->typeExpression->get_type());
//        definer->set_type( type );
//        definer->symbol_resolution_pass();
//        this->builtinTypes[definer->id] = definer;
//
//        type->prepare_type_members();
//    }
//
//    // create the tuple base type:
//    {
//        auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, "Tuple", TUPLE, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT );
//        definer->symbol_declaration_pass( moduleCtx );
//        auto type = new TxTupleType(definer->get_declaration(), this->builtinTypes[ANY]->typeExpression->get_type(), true );
//        type->prepare_type_members();
//        definer->set_type( type );
//        definer->symbol_resolution_pass();
//        this->builtinTypes[definer->id] = definer;
//    }
//
//    // create the interface base type:
//    {
//        auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, "Interface", INTERFACE, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT );
//        definer->symbol_declaration_pass( moduleCtx );
//        {   // declare the adaptee type id virtual field member, which is abstract here but concrete in adapter subtypes:
//            const TxType* fieldType = this->get_builtin_type(UINT);
//            auto fieldDef = new TxBuiltinFieldDefNode( this->builtinLocation );
//            fieldDef->set_context(moduleCtx);  // FIXME: refactor so that declaration pass is run on builtin nodes
//            auto fieldDecl = definer->get_declaration()->get_symbol()->declare_field(
//                    "$adTypeId", fieldDef, TXD_PUBLIC | TXD_BUILTIN | TXD_STATIC | TXD_ABSTRACT | TXD_IMPLICIT, TXS_STATIC, "");
//            fieldDef->set_field( TxField::make_field(fieldDecl, fieldType) );
//        }
//        auto type = new TxInterfaceType(definer->get_declaration(), this->builtinTypes[ANY]->typeExpression->get_type());
//        type->prepare_type_members();
//        definer->set_type( type );
//        definer->symbol_resolution_pass();
//        this->builtinTypes[definer->id] = definer;
//    }
//
////    // create the CString base array type:
////    {
////        auto definer = new TxBuiltinTypeDefiner( CSTRING, "CString" );
////        auto charBinding = TxGenericBinding::make_type_binding("E", this->builtinTypes[UBYTE]);
////        this->builtinTypes[definer->id] = definer;
////
////        std::vector<TxGenericBinding> bindings( { charBinding } );
////        TxTypeSpecialization specialization(this->builtinTypes[ARRAY]->typeExpression->get_type(), bindings);
////        std::vector<TxTypeParam> typeParams { TxTypeParam(TxTypeParam::TXB_VALUE, "L", this->builtinTypes[UINT]) };
////        definer->set_type( specialization.type->make_specialized_type(definer->get_declaration(), specialization) );
////    }


    // FIXME: remove when initializeBuiltinSymbols() no longer creates entities needing these to be pre-resolved:
    for (unsigned id = 0; id < BuiltinTypeId_COUNT; id++) {
        // verify that all built-in types are initialized:
        ASSERT(this->builtinTypes[id], "Uninitialized built-in type! id=" << id);
        this->builtinTypes[id]->typeExpression->resolve_type();
    }

    auto module = this->registry.get_package().lookup_module("tx");
    LexicalContext moduleCtx( module );

    // default constructors for elementary, concrete built-ins:
    declare_default_constructor(moduleCtx, BOOL,   new TxBoolLitNode(this->builtinLocation, false));
    declare_default_constructor(moduleCtx, BYTE,   new TxIntegerLitNode(this->builtinLocation, 0, true,  BYTE));
    declare_default_constructor(moduleCtx, SHORT,  new TxIntegerLitNode(this->builtinLocation, 0, true,  SHORT));
    declare_default_constructor(moduleCtx, INT,    new TxIntegerLitNode(this->builtinLocation, 0, true,  INT));
    declare_default_constructor(moduleCtx, LONG,   new TxIntegerLitNode(this->builtinLocation, 0, true,  LONG));
    declare_default_constructor(moduleCtx, UBYTE,  new TxIntegerLitNode(this->builtinLocation, 0, false, UBYTE));
    declare_default_constructor(moduleCtx, USHORT, new TxIntegerLitNode(this->builtinLocation, 0, false, USHORT));
    declare_default_constructor(moduleCtx, UINT,   new TxIntegerLitNode(this->builtinLocation, 0, false, UINT));
    declare_default_constructor(moduleCtx, ULONG,  new TxIntegerLitNode(this->builtinLocation, 0, false, ULONG));
    declare_default_constructor(moduleCtx, HALF,   new TxFloatingLitNode(this->builtinLocation, HALF));
    declare_default_constructor(moduleCtx, FLOAT,  new TxFloatingLitNode(this->builtinLocation, FLOAT));
    declare_default_constructor(moduleCtx, DOUBLE, new TxFloatingLitNode(this->builtinLocation, DOUBLE));

    // scalar conversion-constructor functions:
    for (auto fromTypeId : SCALAR_TYPE_IDS) {
        for (auto toTypeId : SCALAR_TYPE_IDS) {
            declare_conversion_constructor(fromTypeId, toTypeId);
        }
        declare_conversion_constructor(fromTypeId, BOOL);
    }
    declare_conversion_constructor(BOOL, BOOL);

    declare_tx_functions(module);

//    // built-in global constants:
//    auto charsType = new TxArrayType("tx.Char"); // BUILTIN_TYPES[CHAR].type);
//    TxBuiltinTypeDefiner* charsProd = new TxBuiltinTypeDefiner("CharArray", charsType);
//    auto strRec = BuiltinTypeRecord { STRING, "String", new TxReferenceType("tx.CharArray") };
//    TxBuiltinTypeDefiner* strProd = new TxBuiltinTypeDefiner(strRec);
//    builtinModule->declareType(strProd->name, TXD_PUBLIC, false, *strProd);

//    auto txCfuncModule = this->registry.get_package().declare_module( this->registry.get_package().root_origin(), TxIdentifier(BUILTIN_NS ".c"), true);
    auto txCfuncModule = this->registry.get_package().lookup_module("tx.c");
    {   // declare tx.c.puts:
//        auto implTypeName = "UByte$Ref";
//        auto ubyteRefDef = new TxBuiltinTypeDefiner();
//        auto ubyteRefDecl = txCfuncModule->declare_type(implTypeName, ubyteRefDef, TXD_PUBLIC | TXD_IMPLICIT);
//        auto charBinding = TxGenericBinding::make_type_binding("T", this->builtinTypes[UBYTE]);
//        ubyteRefDef->type = this->get_reference_type(ubyteRefDecl, charBinding);

        auto c_puts_func_type_def = new TxBuiltinFieldDefNode( this->builtinLocation );
        auto c_puts_decl = txCfuncModule->declare_field("puts", c_puts_func_type_def, TXD_PUBLIC | TXD_BUILTIN, TXS_GLOBAL, TxIdentifier(""));

        auto refTypeNode = new TxReferenceTypeNode(this->builtinLocation, nullptr, new TxIdentifiedTypeNode(this->builtinLocation, "tx.UByte"));
        LexicalContext ctx(txCfuncModule);
        refTypeNode->symbol_declaration_pass( ctx, ctx, nullptr );
        //refTypeNode->symbol_declaration_pass( ctx, ctx, TXD_PUBLIC | TXD_IMPLICIT, "puts$argtype", nullptr );
        refTypeNode->symbol_resolution_pass();
        auto ubyteRefType = refTypeNode->resolve_type();

        const TxType* returnType = this->builtinTypes[INT]->typeExpression->resolve_type();

        auto type = new TxFunctionType(nullptr, this->builtinTypes[FUNCTION]->typeExpression->resolve_type(), { ubyteRefType }, returnType);
        c_puts_func_type_def->set_field( TxField::make_field(c_puts_decl, type) );
    }
    {  // declare tx.c.abort:
        std::vector<const TxType*> argumentTypes( {} );
        auto c_abort_func_type_def = new TxBuiltinFieldDefNode( this->builtinLocation );
        auto c_abort_decl = txCfuncModule->declare_field("abort", c_abort_func_type_def, TXD_PUBLIC | TXD_BUILTIN, TXS_GLOBAL, TxIdentifier(""));
        auto type = new TxFunctionType(nullptr, this->builtinTypes[FUNCTION]->typeExpression->resolve_type(), argumentTypes);
        c_abort_func_type_def->set_field( TxField::make_field(c_abort_decl, type) );
    }
}

//void BuiltinTypes::declare_default_constructor(LexicalContext& ctx, BuiltinTypeId toTypeId, TxExpressionNode* initValueExpr) {
//    initValueExpr->symbol_declaration_pass( ctx );
//
//    // constructors for built-in elementary types are really inline conversion expressions
//    auto typeDecl = this->builtinTypes[toTypeId]->get_declaration();
//    auto constructorDefiner = new TxBuiltinFieldDefNode( this->builtinLocation );
//    auto constructorDecl = typeDecl->get_symbol()->declare_field("$init", constructorDefiner, TXD_PUBLIC | TXD_BUILTIN | TXD_CONSTRUCTOR, TXS_INSTANCEMETHOD, TxIdentifier(""));
//    auto type = new TxBuiltinDefaultConstructorType(nullptr, this->builtinTypes[FUNCTION]->typeExpression->get_type(),
//                                                    this->builtinTypes[toTypeId]->typeExpression->get_type(),
//                                                    initValueExpr);
//    type->prepare_type_members();
//    constructorDefiner->set_field( TxField::make_field(constructorDecl, type) );
//}
//
//void BuiltinTypes::declare_conversion_constructor(BuiltinTypeId fromTypeId, BuiltinTypeId toTypeId) {
//    // constructors for built-in elementary types are really inline conversion expressions
//    auto typeDecl = this->builtinTypes[toTypeId]->get_declaration();
//    auto constructorDefiner = new TxBuiltinFieldDefNode( this->builtinLocation );
//    auto constructorDecl = typeDecl->get_symbol()->declare_field("$init", constructorDefiner, TXD_PUBLIC | TXD_BUILTIN | TXD_CONSTRUCTOR, TXS_INSTANCEMETHOD, TxIdentifier(""));
//    auto type = new TxBuiltinConversionFunctionType(nullptr, this->builtinTypes[FUNCTION]->typeExpression->get_type(),
//                                                    this->builtinTypes[fromTypeId]->typeExpression->get_type(),
//                                                    this->builtinTypes[toTypeId]->typeExpression->get_type());
//    type->prepare_type_members();
//    constructorDefiner->set_field( TxField::make_field(constructorDecl, type) );
//}






BuiltinTypes::BuiltinTypes( TypeRegistry& registry )
        : registry(registry), builtinLocation( registry.get_package().root_origin().get_parse_location() ) {
}


const TxType* BuiltinTypes::get_builtin_type( const BuiltinTypeId id ) const {
    return this->builtinTypes[id]->typeExpression->get_type();
}


const TxInterfaceAdapterType* BuiltinTypes::inner_get_interface_adapter(const TxType* interfaceType, const TxType* adaptedType) {
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
    std::string adapterName = ifDecl->get_unique_name() + "$if$" + encode_type_name(adaptedType);

    if (auto existingAdapterSymbol = dynamic_cast<TxEntitySymbol*>(scope->get_member_symbol(adapterName))) {
        if (auto typeDecl = existingAdapterSymbol->get_type_decl()) {
            auto adapterType = static_cast<const TxInterfaceAdapterType*>(typeDecl->get_definer()->resolve_type());
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
        auto fieldDecl = typeDecl->get_symbol()->declare_field("$adTypeId", fieldDef, TXD_PUBLIC | TXD_STATIC | TXD_OVERRIDE | TXD_IMPLICIT, TXS_STATIC, "");
        fieldDef->set_field( TxField::make_field(fieldDecl, fieldType) );
    }

    auto adapterType = new TxInterfaceAdapterType(typeDecl, interfaceType, adaptedType);
    adapterDefiner->set_type( adapterType );
    adapterDefiner->symbol_resolution_pass();
    this->registry.add_type(adapterType);
    return adapterType;
}

