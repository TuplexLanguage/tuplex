#include "util/assert.hpp"

#include "type_registry.hpp"

#include "package.hpp"
#include "entity.hpp"
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


/** the flags that may be inherited when specializing a type */
static const TxDeclarationFlags DECL_FLAG_FILTER = TXD_STATIC | TXD_PUBLIC | TXD_PROTECTED | TXD_ABSTRACT | TXD_FINAL | TXD_IMPLICIT | TXD_EXPERRBLOCK;


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
        this->resolve_field();  // auto-resolves
    }

    virtual const TxExpressionNode* get_init_expression() const { return nullptr; }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override {
        return nullptr;
    }
};


class TxImplicitTypeDefiningNode final : public TxTypeExpressionNode {
    const TxImplicitTypeDefiningNode* originalNode;
    const TxType* baseType;

    TxImplicitTypeDefiningNode( const TxImplicitTypeDefiningNode* original )
            : TxTypeExpressionNode(original->parseLocation), originalNode(original), baseType() { }

protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext,
                                                      TxDeclarationFlags declFlags ) override { }

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

    void set_type(const TxType* type) {
        ASSERT(!this->baseType, "type already set");
        this->baseType = type;
    }

    virtual bool has_predefined_type() const override { return true; }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }
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
            throw std::logic_error("Specified a base type for TxAnyType that was not a TxAnyType: " + baseTypeSpec.type->to_string());
        return new TxAnyType(declaration, baseTypeSpec);
    }

public:
    TxAnyType(const TxTypeDeclaration* declaration) : TxType(TXTC_ANY, declaration) { }

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override {
        //ASSERT(false, "Can't contruct LLVM type for abstract type " << this->to_string());
        context.LOG.debug("LLVM type for abstract type %s is VOID", this->to_string().c_str());
        return context.get_voidT();
    }
};

/** Used for the built-in types' abstract base types. */
class TxBuiltinBaseType final : public TxType {
    TxType* make_specialized_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                  const std::vector<TxTypeSpecialization>& interfaces) const override {
        if (! dynamic_cast<const TxBuiltinBaseType*>(baseTypeSpec.type))
            throw std::logic_error("Specified a base type for TxBuiltinBaseType that was not a TxBuiltinBaseType: " + baseTypeSpec.type->to_string());
        return new TxBuiltinBaseType(baseTypeSpec.type->get_type_class(), declaration, baseTypeSpec);
    }

public:
    TxBuiltinBaseType(TxTypeClass typeClass, const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec)
        : TxType(typeClass, declaration, baseTypeSpec)  { }
    TxBuiltinBaseType(TxTypeClass typeClass, const TxTypeDeclaration* declaration, const TxType* baseType)
        : TxType(typeClass, declaration, TxTypeSpecialization(baseType))  { }

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override {
        //ASSERT(false, "Can't contruct LLVM type for abstract type " << this->to_string());
        context.LOG.debug("LLVM type for abstract type %s is VOID", this->to_string().c_str());
        return context.get_voidT();
    }
};


/** Declares the members of the Any root type. */
static void declare_any_members(const TxTypeDeclaration* anyDecl) {
    // candidates:
    // public static _typeid() UInt
    // public final _address() ULong
}


TypeRegistry::TypeRegistry(TxPackage& package)
        : package(package), builtinLocation(nullptr, 0, 0, new TxParserContext(package.driver(), TxIdentifier(""), "")) {
    this->createdTypes = new std::vector<TxType*>();
}


void TypeRegistry::declare_tx_functions(TxModule* module) {
    // public _address( r : Ref ) ULong
    std::vector<const TxType*> argumentTypes( { this->builtinTypes[REFERENCE]->get_type() } );
    auto funcTypeDef = new TxBuiltinFieldDefNode( this->builtinLocation );
    auto funcDecl = module->declare_field("_address", funcTypeDef, TXD_PUBLIC | TXD_BUILTIN, TXS_GLOBAL, TxIdentifier(""));
    auto type = new TxFunctionType(nullptr, this->builtinTypes[FUNCTION]->get_type(), argumentTypes,
                                   this->builtinTypes[ULONG]->get_type());
    funcTypeDef->set_field( new TxField(funcDecl, type) );
}


void TypeRegistry::add_builtin_abstract(TxModule* module, TxTypeClass typeClass, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId) {
    auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, plainName, id, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT );
    LexicalContext lexCtx( module );
    definer->symbol_declaration_pass( lexCtx );
    auto type = new TxBuiltinBaseType( typeClass, definer->get_declaration(), this->builtinTypes[parentId]->get_type());
    type->prepare_type_members();
    definer->set_type( type );
    definer->symbol_resolution_pass();
    this->builtinTypes[definer->id] = definer;
}

void TypeRegistry::add_builtin_integer(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId,
                                       int size, bool sign) {
    auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, plainName, id, TXD_PUBLIC | TXD_BUILTIN );
    LexicalContext lexCtx( module );
    definer->symbol_declaration_pass( lexCtx );
    auto type = new TxIntegerType( definer->get_declaration(), this->builtinTypes[parentId]->get_type(), size, sign);
    type->prepare_type_members();
    definer->set_type( type );
    definer->symbol_resolution_pass();
    this->builtinTypes[definer->id] = definer;
}

void TypeRegistry::add_builtin_floating(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId, int size) {
    auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, plainName, id, TXD_PUBLIC | TXD_BUILTIN );
    LexicalContext lexCtx( module );
    definer->symbol_declaration_pass( lexCtx );
    auto type = new TxFloatingType( definer->get_declaration(), this->builtinTypes[parentId]->get_type(), size );
    type->prepare_type_members();
    definer->set_type( type );
    definer->symbol_resolution_pass();
    this->builtinTypes[definer->id] = definer;
}

/** Initializes the built-in symbols. */
void TypeRegistry::initializeBuiltinSymbols() {
    auto module = this->package.declare_module(* new TxIdentifier(BUILTIN_NS), true);
    LexicalContext moduleCtx(module);

    // create the Any root type:
    {
        auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, "Any", ANY, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT );
        LexicalContext lexCtx( module );
        definer->symbol_declaration_pass( lexCtx );
        auto type = new TxAnyType( definer->get_declaration() );
        type->prepare_type_members();
        definer->set_type( type );
        definer->symbol_resolution_pass();
        this->builtinTypes[definer->id] = definer;
    }

    // create the built-in abstract base types:
    this->add_builtin_abstract( module, TXTC_ELEMENTARY, ELEMENTARY,    "Elementary",     ANY );
    this->add_builtin_abstract( module, TXTC_ELEMENTARY, SCALAR,        "Scalar",         ELEMENTARY );
    this->add_builtin_abstract( module, TXTC_ELEMENTARY, INTEGER,       "Integer",        SCALAR );
    this->add_builtin_abstract( module, TXTC_ELEMENTARY, SIGNED,        "Signed",         INTEGER );
    this->add_builtin_abstract( module, TXTC_ELEMENTARY, UNSIGNED,      "Unsigned",       INTEGER );
    this->add_builtin_abstract( module, TXTC_ELEMENTARY, FLOATINGPOINT, "FloatingPoint",  SCALAR );

    // create the built-in concrete scalar types:
    this->add_builtin_integer ( module, BYTE,          "Byte",     SIGNED,   1, true );
    this->add_builtin_integer ( module, SHORT,         "Short",    SIGNED,   2, true );
    this->add_builtin_integer ( module, INT,           "Int",      SIGNED,   4, true );
    this->add_builtin_integer ( module, LONG,          "Long",     SIGNED,   8, true );
    this->add_builtin_integer ( module, UBYTE,         "UByte",    UNSIGNED, 1, false );
    this->add_builtin_integer ( module, USHORT,        "UShort",   UNSIGNED, 2, false );
    this->add_builtin_integer ( module, UINT,          "UInt",     UNSIGNED, 4, false );
    this->add_builtin_integer ( module, ULONG,         "ULong",    UNSIGNED, 8, false );
    this->add_builtin_floating( module, HALF,          "Half",     FLOATINGPOINT, 2 );
    this->add_builtin_floating( module, FLOAT,         "Float",    FLOATINGPOINT, 4 );
    this->add_builtin_floating( module, DOUBLE,        "Double",   FLOATINGPOINT, 8 );

    //this->add_builtin_integer( module, CHAR,          "Char",    UNSIGNED, 1, false );
    //this->add_builtin( module, STRING,        "String",  new TxArrayType(this->builtinTypes[ARRAY );

    // create the boolean type:
    {
        auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, "Bool", BOOL, TXD_PUBLIC | TXD_BUILTIN );
        definer->symbol_declaration_pass( moduleCtx );
        auto type = new TxBoolType(definer->get_declaration(), this->builtinTypes[ELEMENTARY]->get_type() );
        type->prepare_type_members();
        definer->set_type( type );
        definer->symbol_resolution_pass();
        this->builtinTypes[definer->id] = definer;
    }

    // create the function base type:
    {
        auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, "Function", FUNCTION, TXD_PUBLIC | TXD_BUILTIN );
        definer->symbol_declaration_pass( moduleCtx );
        auto type = new TxBuiltinBaseType(TXTC_FUNCTION, definer->get_declaration(), this->builtinTypes[ANY]->get_type() );
        type->prepare_type_members();
        definer->set_type( type );
        definer->symbol_resolution_pass();
        this->builtinTypes[definer->id] = definer;
    }

    // create the reference base type:
    {
        //auto elemTypeDefiner = this->builtinTypes[ANY];
        //typeDecl->get_symbol()->declare_type( "T", elemTypeDefiner, TXD_PUBLIC | TXD_BUILTIN | TXD_GENPARAM );
        auto anyType = new TxPredefinedTypeNode(this->builtinLocation, "tx.Any");
        auto paramNodes = new std::vector<TxDeclarationNode*>( { new TxTypeDeclNode(this->builtinLocation, TXD_PUBLIC | TXD_GENPARAM,
                                                                                    "T", nullptr, anyType) } );
        auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, "Ref", REFERENCE, TXD_PUBLIC | TXD_BUILTIN, paramNodes );
        definer->symbol_declaration_pass( moduleCtx );

        auto type = new TxReferenceType(definer->get_declaration(), this->builtinTypes[ANY]->get_type());
        definer->set_type( type );
        definer->symbol_resolution_pass();
        this->builtinTypes[definer->id] = definer;

        type->prepare_type_members();
    }

    // create the array base type:
    {
        //auto elemTypeDefiner = this->builtinTypes[ANY];
        //typeDecl->get_symbol()->declare_type( "E", elemTypeDefiner, TXD_PUBLIC | TXD_BUILTIN | TXD_GENPARAM );
        //auto lengthDefiner = new TxBuiltinFieldDefiner();
        //auto lengthDecl = typeDecl->get_symbol()->declare_field( "L", lengthDefiner, TXD_PUBLIC | TXD_BUILTIN | TXD_GENPARAM, TXS_INSTANCE, TxIdentifier() );
        //lengthDefiner->set_field( new TxField(lengthDecl, this->builtinTypes[UINT]->get_type()) );
        auto anyType = new TxPredefinedTypeNode(this->builtinLocation, "tx.Any");
        auto lTypeNode = new TxTypeDeclWrapperNode( this->builtinLocation, this->builtinTypes[UINT]->get_declaration() );
        auto lenFieldDef = new TxFieldDefNode(this->builtinLocation, "L", lTypeNode, nullptr, false);
        auto paramNodes = new std::vector<TxDeclarationNode*>( { new TxTypeDeclNode(this->builtinLocation, TXD_PUBLIC | TXD_GENPARAM,
                                                                                    "E", nullptr, anyType),
                                                                 new TxFieldDeclNode(this->builtinLocation, TXD_PUBLIC | TXD_GENPARAM,
                                                                                     lenFieldDef) } );
        auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, "Array", ARRAY, TXD_PUBLIC | TXD_BUILTIN, paramNodes );
        definer->symbol_declaration_pass( moduleCtx );

        auto type = new TxArrayType(definer->get_declaration(), this->builtinTypes[ANY]->get_type());
        definer->set_type( type );
        definer->symbol_resolution_pass();
        this->builtinTypes[definer->id] = definer;

        type->prepare_type_members();
    }

    // create the tuple base type:
    {
        auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, "Tuple", TUPLE, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT );
        definer->symbol_declaration_pass( moduleCtx );
        auto type = new TxTupleType(definer->get_declaration(), this->builtinTypes[ANY]->get_type(), true );
        type->prepare_type_members();
        definer->set_type( type );
        definer->symbol_resolution_pass();
        this->builtinTypes[definer->id] = definer;
    }

    // create the interface base type:
    {
        auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, "Interface", INTERFACE, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT );
        definer->symbol_declaration_pass( moduleCtx );
        {   // declare the adaptee type id virtual field member, which is abstract here but concrete in adapter subtypes:
            const TxType* fieldType = this->get_builtin_type(UINT);
            auto fieldDef = new TxBuiltinFieldDefNode( this->builtinLocation );
            fieldDef->set_context(moduleCtx);  // FIXME: refactor so that declaration pass is run on builtin nodes
            auto fieldDecl = definer->get_declaration()->get_symbol()->declare_field(
                    "$adTypeId", fieldDef, TXD_PUBLIC | TXD_BUILTIN | TXD_STATIC | TXD_ABSTRACT | TXD_IMPLICIT, TXS_STATIC, "");
            fieldDef->set_field( new TxField(fieldDecl, fieldType) );
        }
        auto type = new TxInterfaceType(definer->get_declaration(), this->builtinTypes[ANY]->get_type());
        type->prepare_type_members();
        definer->set_type( type );
        definer->symbol_resolution_pass();
        this->builtinTypes[definer->id] = definer;
    }

//    // create the CString base array type:
//    {
//        auto definer = new TxBuiltinTypeDefiner( CSTRING, "CString" );
//        auto charBinding = TxGenericBinding::make_type_binding("E", this->builtinTypes[UBYTE]);
//        this->builtinTypes[definer->id] = definer;
//
//        std::vector<TxGenericBinding> bindings( { charBinding } );
//        TxTypeSpecialization specialization(this->builtinTypes[ARRAY]->get_type(), bindings);
//        std::vector<TxTypeParam> typeParams { TxTypeParam(TxTypeParam::TXB_VALUE, "L", this->builtinTypes[UINT]) };
//        definer->set_type( specialization.type->make_specialized_type(definer->get_declaration(), specialization) );
//    }


    // register the built-in types and create modifiable specializations of them:
    for (unsigned id = 0; id < BuiltinTypeId_COUNT; id++) {
        // verify that all built-in types are initialized:
        ASSERT(this->builtinTypes[id], "Uninitialized built-in type! id=" << id);
        auto biType = const_cast<TxType*>(this->builtinTypes[id]->get_type());
        ASSERT(id == this->staticTypes.size(), "registering built-in type in wrong order / id: " << id);
        ASSERT(biType->is_prepared(), "built-in type not prepared: " << biType);
        biType->runtimeTypeId = id;
        this->staticTypes.push_back(biType);

        this->builtinModTypes[id] = nullptr;  // prevents read from uninitialized value
        this->builtinModTypes[id] = this->get_modifiable_type(nullptr, biType);
        ASSERT(this->builtinModTypes[id], "NULL mod builtin type: " << biType);
    }

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

    declare_any_members(this->builtinTypes[ANY]->get_declaration());

    declare_tx_functions(module);

//    // built-in global constants:
//    auto charsType = new TxArrayType("tx.Char"); // BUILTIN_TYPES[CHAR].type);
//    TxBuiltinTypeDefiner* charsProd = new TxBuiltinTypeDefiner("CharArray", charsType);
//    auto strRec = BuiltinTypeRecord { STRING, "String", new TxReferenceType("tx.CharArray") };
//    TxBuiltinTypeDefiner* strProd = new TxBuiltinTypeDefiner(strRec);
//    builtinModule->declareType(strProd->name, TXD_PUBLIC, false, *strProd);

    auto txCfuncModule = this->package.declare_module(TxIdentifier(BUILTIN_NS ".c"), true);
    {   // declare tx.c.puts:
//        auto implTypeName = "UByte$Ref";
//        auto ubyteRefDef = new TxBuiltinTypeDefiner();
//        auto ubyteRefDecl = txCfuncModule->declare_type(implTypeName, ubyteRefDef, TXD_PUBLIC | TXD_IMPLICIT);
//        auto charBinding = TxGenericBinding::make_type_binding("T", this->builtinTypes[UBYTE]);
//        ubyteRefDef->type = this->get_reference_type(ubyteRefDecl, charBinding);

        auto c_puts_func_type_def = new TxBuiltinFieldDefNode( this->builtinLocation );
        auto c_puts_decl = txCfuncModule->declare_field("puts", c_puts_func_type_def, TXD_PUBLIC | TXD_BUILTIN, TXS_GLOBAL, TxIdentifier(""));

        auto refTypeNode = new TxReferenceTypeNode(this->builtinLocation, nullptr, new TxPredefinedTypeNode(this->builtinLocation, "tx.UByte"));
        LexicalContext ctx(txCfuncModule);
        refTypeNode->symbol_declaration_pass( ctx, ctx, TXD_PUBLIC | TXD_IMPLICIT, "puts$argtype", nullptr );
        refTypeNode->symbol_resolution_pass();
        auto ubyteRefType = refTypeNode->get_type();

        const TxType* returnType = this->builtinTypes[INT]->get_type();

        auto type = new TxFunctionType(nullptr, this->builtinTypes[FUNCTION]->get_type(), { ubyteRefType }, returnType);
        c_puts_func_type_def->set_field( new TxField(c_puts_decl, type) );
    }
    {  // declare tx.c.abort:
        std::vector<const TxType*> argumentTypes( {} );
        auto c_abort_func_type_def = new TxBuiltinFieldDefNode( this->builtinLocation );
        auto c_abort_decl = txCfuncModule->declare_field("abort", c_abort_func_type_def, TXD_PUBLIC | TXD_BUILTIN, TXS_GLOBAL, TxIdentifier(""));
        auto type = new TxFunctionType(nullptr, this->builtinTypes[FUNCTION]->get_type(), argumentTypes);
        c_abort_func_type_def->set_field( new TxField(c_abort_decl, type) );
    }
}

void TypeRegistry::declare_default_constructor(LexicalContext& ctx, BuiltinTypeId toTypeId, TxExpressionNode* initValueExpr) {
    initValueExpr->symbol_declaration_pass( ctx );

    // constructors for built-in elementary types are really inline conversion expressions
    auto typeDecl = this->builtinTypes[toTypeId]->get_declaration();
    auto constructorDefiner = new TxBuiltinFieldDefNode( this->builtinLocation );
    auto constructorDecl = typeDecl->get_symbol()->declare_field("$init", constructorDefiner, TXD_PUBLIC | TXD_BUILTIN | TXD_CONSTRUCTOR, TXS_INSTANCEMETHOD, TxIdentifier(""));
    auto type = new TxBuiltinDefaultConstructorType(nullptr, this->builtinTypes[FUNCTION]->get_type(),
                                                    this->builtinTypes[toTypeId]->get_type(),
                                                    initValueExpr);
    type->prepare_type_members();
    constructorDefiner->set_field( new TxField(constructorDecl, type) );
}

void TypeRegistry::declare_conversion_constructor(BuiltinTypeId fromTypeId, BuiltinTypeId toTypeId) {
    // constructors for built-in elementary types are really inline conversion expressions
    auto typeDecl = this->builtinTypes[toTypeId]->get_declaration();
    auto constructorDefiner = new TxBuiltinFieldDefNode( this->builtinLocation );
    auto constructorDecl = typeDecl->get_symbol()->declare_field("$init", constructorDefiner, TXD_PUBLIC | TXD_BUILTIN | TXD_CONSTRUCTOR, TXS_INSTANCEMETHOD, TxIdentifier(""));
    auto type = new TxBuiltinConversionFunctionType(nullptr, this->builtinTypes[FUNCTION]->get_type(),
                                                    this->builtinTypes[fromTypeId]->get_type(),
                                                    this->builtinTypes[toTypeId]->get_type());
    type->prepare_type_members();
    constructorDefiner->set_field( new TxField(constructorDecl, type) );
}


void TypeRegistry::enqueued_resolution_pass() {
    // Note: Queue can be appended to during processing.
    for (unsigned i = 0; i != this->enqueuedSpecializations.size(); i++) {
        //std::cerr << "Nof enqueued specializations: " << this->enqueuedSpecializations.size() << std::endl;
        this->enqueuedSpecializations.at(i)->symbol_resolution_pass();
    }
}


void TypeRegistry::add_type(TxType* type) {
    //std::cerr << "Adding type: " << type << std::endl;
    this->createdTypes->push_back(type);
}

void TypeRegistry::register_types() {
    auto createdTypes = this->createdTypes;
    this->createdTypes = new std::vector<TxType*>();
    for (auto type : *createdTypes) {
        if (type->is_builtin()) {
            ASSERT(type->is_prepared(), "Unprepared builtin type: " << type);
            ASSERT(type->runtimeTypeId != UINT32_MAX, "builtin type doesn't have type id set: " << type);
        }
        else {
            type->prepare_type_members();
            // Types that are distinct in instance data type, or vtable, get distinct runtime type id and vtable.
            if (type->get_type_class() == TXTC_FUNCTION)
                continue;
            if (type->is_equivalent_derivation()) {
                //std::cerr << "Not registering distinct runtime type id for equivalent derivation: " << type << std::endl;
                continue;
            }
//            // As long as we only generate actual code for six 0, don't register distinct runtime type id for reinterpreted types:
//            if (type->is_reinterpreted()) {
//                //std::cerr << "Not registering distinct runtime type id for reinterpreted type: " << type << std::endl;
//                continue;
//            }
            //if (type->get_declaration())
            //    std::cerr << "with six=" << type->get_declaration()->get_definer()->get_six() << ": registering type " << type << std::endl;
            type->runtimeTypeId = this->staticTypes.size();
        }
        this->staticTypes.push_back(type);
        //std::cerr << "Registering: " << type << " with distinct runtime type id " << type->runtimeTypeId << std::endl;
    }
    ASSERT(this->createdTypes->empty(), "'Extra' types were created while register_types() was running");
    //for (auto type : *this->createdTypes)
    //    std::cerr << "'EXTRA' CREATED TYPE: " << type << std::endl;
    delete this->createdTypes;
    this->createdTypes = createdTypes;
}


const TxType* TypeRegistry::get_builtin_type(const BuiltinTypeId id, bool mod) const {
    return mod ? this->builtinModTypes[id] : this->builtinTypes[id]->get_type();
}



TxType* TypeRegistry::make_specialized_type(const TxTypeDeclaration* declaration,
                                            const TxTypeSpecialization& specialization,
                                            const std::vector<TxTypeSpecialization>& interfaces) {
    auto newType = specialization.type->make_specialized_type(declaration, specialization, interfaces);
    this->add_type(newType);
//    printf("MADE SPECIALIZED TYPE: %2d:%02d: %s\n", newType->get_parse_location().begin.line, newType->get_parse_location().begin.column,
//            newType->to_string().c_str());
    return newType;
}



const TxType* TypeRegistry::get_modifiable_type(const TxTypeDeclaration* declaration, const TxType* type) {
    // 'modifiable' is always a distinct 'specialization' (no parameter bindings (or type extensions))
    while (type->is_empty_derivation() && !type->get_explicit_declaration())  //!type->is_explicit_nongen_declaration())
        type = type->get_base_type();
    ASSERT(!type->is_modifiable(), "Can't make a modifiable specialization of a modifiable type: " << type);
    if (type->is_builtin()) {
        ASSERT(type->get_type_id() != UINT32_MAX, "builtin type doesn't have type id set: " << type);
        if (auto mtype = this->builtinModTypes[type->get_type_id()])
            return mtype;
        // if not set, then this is run during initialization and a modifiable of a built-in type is being created
    }

    if (! declaration) {
        std::string prefix = "~";
        std::string name = prefix + type->get_declaration()->get_unique_name();
        TxScopeSymbol* scope = type->get_declaration()->get_symbol()->get_outer();
        if (auto entitySymbol = dynamic_cast<TxEntitySymbol*>(scope->get_member_symbol(name))) {
            if (auto typeDecl = entitySymbol->get_type_decl()) {
                if (auto existingType = typeDecl->get_definer()->resolve_type()) {
                    if (existingType->is_modifiable() && *existingType->get_base_type() == *type)
                        return existingType;
                    //std::cerr << "existing: " << existingType << "  new: " << type << std::endl;
                }
            }
            type->LOGGER().warning("Name collision when trying to declare implicit MOD type, preexisting symbol: %s", entitySymbol->to_string().c_str());
            name = scope->make_unique_name(name);
        }

        auto typeDefiner = type->get_declaration()->get_definer();
        auto & ctx = typeDefiner->context();
        auto modNode = new TxModifiableTypeNode(this->builtinLocation, new TxPredefinedTypeNode(this->builtinLocation, type->get_declaration()->get_unique_name()));
        TxDeclarationFlags newDeclFlags = ( type->get_decl_flags() & DECL_FLAG_FILTER ); // | TXD_IMPLICIT;
        modNode->symbol_declaration_pass( ctx, ctx, newDeclFlags, name, nullptr );
        modNode->symbol_resolution_pass();
        return modNode->get_type();
    }

    return this->make_specialized_type(declaration, TxTypeSpecialization(type, true));
}

const TxType* TypeRegistry::get_empty_specialization(const TxTypeDeclaration* declaration, const TxType* type) {
    //std::cerr << "MAKING EMPTY TYPE: " << declaration->to_string() << std::endl;
    while (type->is_empty_derivation() && !type->get_explicit_declaration())  //!type->is_explicit_nongen_declaration())
        type = type->get_base_type();
    return this->make_specialized_type(declaration, TxTypeSpecialization(type, false, true));
}



/** Collates the remaining unbound type parameters for the given specialization. */
static std::vector<TxEntityDeclaration*> get_unbound_type_parameters(const TxType* baseType, const std::vector<TxGenericBinding>* bindings) {
    std::vector<TxEntityDeclaration*> unboundParams;
    for (auto baseTypeParamDecl : baseType->type_params()) {
        auto pname = baseTypeParamDecl->get_unique_name();
        if (! std::any_of( bindings->cbegin(), bindings->cend(),
                           [&pname](const TxGenericBinding& b){ return b.param_name() == pname; } ) ) {
            unboundParams.emplace_back(baseTypeParamDecl);
        }
    }
    return unboundParams;
}


static std::string encode_type_name(const TxType* type) {
    std::stringstream boundTypeName;
    if (type->is_modifiable() && !type->get_declaration()) {
        boundTypeName << "~";
        type = type->get_base_type();
    }
    ASSERT(type->get_declaration(), "Type does not have declaration: " << type);
    boundTypeName << hashify( type->get_declaration()->get_unique_full_name() );
    return boundTypeName.str();
}


static const TxType* get_existing_type(const TxType* baseType, const std::vector<TxGenericBinding>* bindings,
                                       TxScopeSymbol* baseScope, const std::string& newBaseName) {
    if (bindings->size() == baseType->type_params().size()) {
        // if generic type specialization is equivalent to the generic base type, reuse it:
        //std::cerr << "existingBaseType    0: " << baseType << std::endl;
        bool matchOK = true;
        for (auto & binding : *bindings) {
            auto paramDecl = baseType->get_type_param_decl(binding.param_name());
            if (binding.meta_type() == MetaType::TXB_TYPE) {
                if (auto typeParamDecl = dynamic_cast<const TxTypeDeclaration*>(paramDecl)) {
                    auto bindingType = binding.type_definer().get_type();
                    auto constraintType = typeParamDecl->get_definer()->resolve_type();
                    ASSERT(constraintType, "NULL constraint type for type parameter " << typeParamDecl);
                    //baseScope->LOGGER().trace("bindingType decl:    %s", bindingType->get_declaration()->to_string().c_str());
                    //baseScope->LOGGER().trace("constraintType decl: %s", constraintType->get_declaration()->to_string().c_str());
                    if (auto bindingDecl = bindingType->get_declaration()) {
                        if (bindingDecl == constraintType->get_declaration())
                            continue;  // binding refers to "itself" (its parameter declaration)
                        if (! (bindingDecl->get_decl_flags() & TXD_GENPARAM)) {
                            // (if binding is to another type parameter it is an explicit unique type)
                            if (*constraintType == *bindingType)
                                continue;
                        }
                    }
                    else if (*constraintType == *bindingType)
                        continue;
                }
            }
            else {  // MetaType::TXB_VALUE
                // VALUE parameters don't have "defaults"
            }
            matchOK = false;
            break;
        }
        if (matchOK) {
            baseScope->LOGGER().debug("new specialization equal to the generic base type, reusing: %s", baseType->to_string().c_str());
            return baseType;
        }
    }

    // if name already exists and specialization is equal, reuse it:
    if (auto existingBaseSymbol = dynamic_cast<TxEntitySymbol*>(baseScope->get_member_symbol(newBaseName))) {
        if (auto typeDecl = existingBaseSymbol->get_type_decl()) {
            auto existingBaseType = typeDecl->get_definer()->resolve_type();
            //std::cerr << "existingBaseType    1: " << existingBaseType << std::endl;
            auto existingGenBaseType = existingBaseType->get_semantic_base_type();
            //std::cerr << "existingGenBaseType 2: " << existingGenBaseType << std::endl;
            if (existingGenBaseType->is_empty_derivation()) {
                existingGenBaseType = existingGenBaseType->get_semantic_base_type();
                //std::cerr << "existingGenBaseType 3: " << existingGenBaseType << std::endl;
            }
            if (*existingGenBaseType == *baseType) {
                bool matchOK = true;
                for (auto & binding : *bindings) {
                    if (auto existingBinding = existingBaseType->get_binding(binding.param_name())) {
                        //std::cerr << "found existing binding: " << existingBinding << std::endl;
                        if (binding.meta_type() == MetaType::TXB_TYPE) {
                            if (const TxType* membType = static_cast<const TxTypeDeclaration*>(existingBinding)->get_definer()->resolve_type()) {
                                //std::cerr << "COMPARING: " << membType << " AND " << binding.type_definer().get_type() << std::endl;
                                if (*membType == *binding.type_definer().get_type())
                                    continue;
                                //else
                                //    std::cerr << "BINDING MISMATCH: " << membType << " != " << binding.type_definer().get_type() << std::endl;
                            }
                            else
                                baseScope->LOGGER().warning("NULL type for member symbol %s", existingBinding->get_symbol()->to_string().c_str());
                        }
                        else {  // MetaType::TXB_VALUE
                            // (For now, statically constant VALUE specializations with diff. values don't share the same static type.)
                            auto existingFieldDefiner = static_cast<const TxFieldDeclaration*>(existingBinding)->get_definer();
                            uint32_t newValue = 0;  // zero means dynamically specified value
                            uint32_t existingValue = 0;
                            if (auto newConstantValueProxy = binding.value_definer().get_static_constant_proxy()) {
                                // new binding has statically constant value
                                newValue = newConstantValueProxy->get_value_UInt();
                            }
                            if (auto existingInitializer = existingFieldDefiner->get_init_expression()) {
                                if (auto existingConstantValueProxy = existingInitializer->get_static_constant_proxy()) {
                                    // existing binding has statically constant value
                                    existingValue = existingConstantValueProxy->get_value_UInt();
                                }
                            }
                            // dynamic VALUE specializations get distinct compile time types, which hold the specific VALUE expressions
                            if (newValue != 0 && newValue == existingValue)
                                continue;
                        }
                    }
                    //std::cerr << "NOT ACCEPTING PRE-EXISTING TYPE " << existingBaseType << " SINCE " << std::endl;
                    matchOK = false;
                    break;
                }
                if (matchOK) {
                    baseScope->LOGGER().debug("new specialization equal to preexisting one, reusing: %s", existingBaseType->to_string().c_str());
                    return existingBaseType;
                }
            }
            baseScope->LOGGER().debug("Found existing but mismatching type with sought name: %s", typeDecl->to_string().c_str());
        }
    }
    return nullptr;
}


/** Makes a TYPE type parameter declaration node (both for bindings and param redeclarations). */
static TxDeclarationNode* make_type_type_param_decl_node( const TxLocation& parseLoc, const std::string& paramName, TxDeclarationFlags flags,
                                                          const TxTypeDeclaration* typeDecl ) {
    auto typeExpr = new TxTypeDeclWrapperNode( parseLoc, typeDecl );
    auto declNode = new TxTypeDeclNode(parseLoc, flags | TXD_PUBLIC , paramName, nullptr, typeExpr);
    return declNode;
}

/** Makes a VALUE type parameter declaration node (both for bindings and param redeclarations). */
static TxDeclarationNode* make_value_type_param_decl_node( const TxLocation& parseLoc, const std::string& paramName, TxDeclarationFlags flags,
                                                           const TxEntityDeclaration* paramValueTypeDecl, TxExpressionNode* valueDefiner=nullptr) {
    auto paramTypeNode = new TxTypeDeclWrapperNode( parseLoc, paramValueTypeDecl );
    auto fieldDef = new TxFieldDefNode(parseLoc, paramName, paramTypeNode, valueDefiner, false);
    auto declNode = new TxFieldDeclNode(parseLoc, flags | TXD_PUBLIC | TXD_IMPLICIT, fieldDef);
    return declNode;
}


TxType* TypeRegistry::get_type_specialization(const TxTypeDeclaration* declaration, const TxType* baseType,
                                              const std::vector<TxTypeSpecialization>& interfaces,
                                              const std::vector<TxGenericBinding>* bindings, bool _mutable) {
    // Note: type specialization is never applied to a modifiable-specialization (legal only on generic base type)
    // Note: A non-parameterized type (without any declared type parameters) is not necessarily non-generic:
    //       It may have members that refer to generic parameters declared in an outer scope.
    ASSERT(!baseType->is_modifiable(), "Can't specialize a 'modifiable' base type: " << baseType);

    const TxParseOrigin* pOrigin = (declaration ? static_cast<const TxParseOrigin*>(declaration->get_definer()) : baseType);

    bool hasBindings = (bindings && !bindings->empty());

    while (baseType->is_empty_derivation() && !baseType->get_explicit_declaration())  //!baseType->is_explicit_nongen_declaration())
        baseType = baseType->get_base_type();

    if (baseType->get_decl_flags() & TXD_GENPARAM) {
        // only empty derivation allowed from generic type parameter
        if (! (!hasBindings && interfaces.empty())) { // && (!typeParams || typeParams->empty()))) {
            CERROR(pOrigin, "Can't specialize a generic type parameter: " << baseType);
            return nullptr;
        }
    }

    // TODO: pass _mutable flag to type extensions

    // Note: Binding of ref-constrained type parameters don't make the code generation more concrete,
    //       but accesses to the members still need the specialized version of them (e.g. the correct ref-target).

    if (hasBindings)        // type that binds parameters of its base type
    {
        // re-base it on new non-generic specialization of the base type:
        // (this replaces the type parameter bindings with direct declarations within the new type)

        ASSERT(declaration, "expected type that binds base type's parameters to be named (declared) but was not");
        this->package.LOGGER().debug("Re-basing non-parameterized type %s by specializing its parameterized base type %s",
                                     declaration->get_unique_full_name().c_str(), baseType->to_string().c_str());

        auto baseDecl = baseType->get_declaration();
        ASSERT(baseDecl, "base type has no declaration: " << baseType);

        std::stringstream newBaseTypeName;
        TxDeclarationFlags newDeclFlags;
        // Note: The same generic type specialization may be produced by multiple statements,
        // both within ExpErr constructs and without. Therefore the type name must distinguish between them.
        if (declaration->get_decl_flags() & TXD_EXPERRBLOCK) {
            newBaseTypeName << "$EE$" << baseDecl->get_unique_name() << "<";
            newDeclFlags = ( baseDecl->get_decl_flags() & DECL_FLAG_FILTER ) | TXD_IMPLICIT | TXD_EXPERRBLOCK;
        }
        else {
            newBaseTypeName << "$" << baseDecl->get_unique_name() << "<";
            newDeclFlags = ( baseDecl->get_decl_flags() & DECL_FLAG_FILTER ) | TXD_IMPLICIT;
        }

        // make new parameter declarations that resolve to the bindings:
        auto bindingDeclNodes = new std::vector<TxDeclarationNode*>();
        std::vector<TxGenericBinding> newBindings;
        unsigned bindCount = 0;
        for (auto & binding : *bindings) {
            if (bindCount++)  newBaseTypeName << ",";

            if (binding.meta_type() == MetaType::TXB_TYPE) {
                // we want to bypass empty, implicit derivations:
                auto btype = binding.type_definer().resolve_type();
                if (! btype)
                    return nullptr;  // specialization fails if a binding fails resolve
                auto origBType = btype;
                while (btype->is_empty_derivation()
                        && ( (! btype->get_declaration())
                             || (btype->get_declaration()->get_decl_flags() & (TXD_IMPLICIT | TXD_GENBINDING) ) ) ) {
                    // if no declaration or implicit declaration, or
                    // if binding refers to a previous binding (i.e. in surrounding scope), fold it
                    //std::cerr << "###### skipping bindingType to base type: " << btype << std::endl;
                    btype = btype->get_base_type();
                }

                newBaseTypeName << encode_type_name(btype);

                bindingDeclNodes->push_back( make_type_type_param_decl_node( binding.get_parse_location(), binding.param_name(),
                                                                             TXD_GENBINDING, btype->get_declaration() ) );
                if (btype != origBType)
                    newBindings.emplace_back( binding, btype->get_declaration()->get_definer() ); //new TxTypeWrapperNode(btype));
                else
                    newBindings.emplace_back( binding );
                package.LOGGER().trace("Re-bound base type %s parameter '%s' with %s", baseDecl->get_unique_full_name().c_str(),
                                       binding.param_name().c_str(), btype->to_string().c_str());
            }
            else {
                // implementation note: binding's value expression not necessarily 'resolved' at this point
                if (auto bindingValueProxy = binding.value_definer().get_static_constant_proxy()) {
                    uint32_t bindingValue = bindingValueProxy->get_value_UInt();
                    newBaseTypeName << bindingValue;  // statically known value
                }
                else {
                    newBaseTypeName << "$VALUE";  // dynamic value
                    // implementation note: a distinct compile time type is registered which holds this specific dynamic value expression
                }

                auto paramDecl = baseType->get_type_param_decl(binding.param_name());
                bindingDeclNodes->push_back( make_value_type_param_decl_node( binding.get_parse_location(), "$" + binding.param_name(),
                                                                              TXD_GENBINDING, paramDecl, &binding.value_definer() ) );
                newBindings.emplace_back(binding);
                package.LOGGER().trace("Re-bound base type %s parameter '%s' with %s", baseDecl->get_unique_full_name().c_str(),
                                       binding.param_name().c_str(), binding.value_definer().to_string().c_str());
            }
        }
        newBaseTypeName << ">";
        auto newBaseTypeNameStr = newBaseTypeName.str();

        // if equivalent specialized type already exists then reuse it, otherwise create new one:
        auto baseScope = baseDecl->get_symbol()->get_outer();
        const TxType* specializedBaseType = get_existing_type(baseType, &newBindings, baseScope, newBaseTypeNameStr);
        if (! specializedBaseType) {
            // If any parameter is not bound, the parameter is redeclared (inherited) as still-unbound type parameter:
            for (auto unboundParamDecl : get_unbound_type_parameters(baseType, &newBindings)) {
                package.LOGGER().note("Implicitly inheriting (redeclaring) type parameter %s in type %s",
                                      unboundParamDecl->get_unique_full_name().c_str(), newBaseTypeNameStr.c_str());
                if (auto typeDecl = dynamic_cast<const TxTypeDeclaration*>(unboundParamDecl)) {
                    bindingDeclNodes->push_back( make_type_type_param_decl_node( pOrigin->get_parse_location(), typeDecl->get_unique_name(),
                                                                                 TXD_GENPARAM, typeDecl ) );
                }
                else {
                    auto fieldDecl = static_cast<const TxFieldDeclaration*>(unboundParamDecl);
                    auto paramDecl = baseType->get_type_param_decl(fieldDecl->get_unique_name());
                    bindingDeclNodes->push_back( make_value_type_param_decl_node( pOrigin->get_parse_location(), fieldDecl->get_unique_name(),
                                                                                  TXD_GENPARAM, paramDecl ) );
                }
            }

            {   // pass on the generic base type to the new specialization via member named $GenericBase:
                auto baseTypeExpr = new TxTypeDeclWrapperNode( pOrigin->get_parse_location(), baseDecl );
                auto declNode = new TxTypeDeclNode( pOrigin->get_parse_location(), TXD_PUBLIC | TXD_IMPLICIT, "$GenericBase", nullptr, baseTypeExpr );
                bindingDeclNodes->push_back( declNode );
            }

            // process new specialization of the base type:
            //std::cerr << "specializing base " << newBaseTypeNameStr << ": " << baseType << std::endl;
            auto baseTypeExpr = dynamic_cast<TxTypeExpressionNode*>( baseDecl->get_definer() );
            ASSERT(baseTypeExpr, "baseTypeExpression not a TxTypeExpressionNode: " << baseDecl->get_definer());
            ASSERT(baseTypeExpr->context().scope() == baseScope, "Unexpected lexical scope: " << baseTypeExpr->context().scope() << " != " << baseScope);
            LexicalContext newBaseContext = LexicalContext( baseScope, declaration->get_definer()->context().exp_err_ctx(), true );

            newBaseTypeNameStr = newBaseContext.scope()->make_unique_name(newBaseTypeNameStr, true);
            auto newBaseTypeExpr = baseTypeExpr->make_ast_copy();
            newBaseTypeExpr->symbol_declaration_pass( newBaseContext, newBaseContext, newDeclFlags, newBaseTypeNameStr, bindingDeclNodes );

            // Invoking the resolution pass here can cause infinite recursion
            // (since the same source text construct may be recursively reprocessed),
            // so we enqueue this "specialization pass" for later processing.
            //std::cerr << "enqueuing specialization " << newBaseTypeNameStr << "  " << baseTypeExpr << std::endl;
            this->enqueuedSpecializations.emplace_back( newBaseTypeExpr );
            specializedBaseType = newBaseTypeExpr->resolve_type();
        }
        // TODO: else bindingDeclNodes thrown away...

        return this->make_specialized_type(declaration, TxTypeSpecialization(specializedBaseType, false, true), interfaces);
    }
    else {
        return this->make_specialized_type(declaration, TxTypeSpecialization(baseType), interfaces);
    }
}


const TxInterfaceAdapterType* TypeRegistry::inner_get_interface_adapter(const TxType* interfaceType, const TxType* adaptedType) {
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

    auto adapterDefiner = new TxImplicitTypeDeclNode( this->builtinLocation, adapterName );
    {
        auto ifDefiner = ifDecl->get_definer();
        auto & ctx = ifDefiner->context();
        adapterDefiner->symbol_declaration_pass(ctx);
    }
    //auto typeDecl = scope->declare_type(adapterName, adapterDefiner, TXD_PUBLIC | TXD_IMPLICIT);
    auto typeDecl = adapterDefiner->get_declaration();

    {   // override the adaptee type id virtual field member:
        const TxType* fieldType = this->get_builtin_type(UINT);
        auto fieldDef = new TxBuiltinFieldDefNode( this->builtinLocation );
        fieldDef->set_context( adapterDefiner->context() );  // FIXME: refactor so that declaration pass is run on builtin nodes
        auto fieldDecl = typeDecl->get_symbol()->declare_field("$adTypeId", fieldDef, TXD_PUBLIC | TXD_STATIC | TXD_OVERRIDE | TXD_IMPLICIT, TXS_STATIC, "");
        fieldDef->set_field( new TxField(fieldDecl, fieldType) );
    }

    auto adapterType = new TxInterfaceAdapterType(typeDecl, interfaceType, adaptedType);
    adapterDefiner->set_type( adapterType );
    adapterDefiner->symbol_resolution_pass();
    this->add_type(adapterType);
    return adapterType;
}

const TxType* TypeRegistry::get_interface_adapter(const TxType* interfaceType, const TxType* adaptedType) {
    auto modIf = interfaceType->is_modifiable();
    while (interfaceType->is_same_vtable_type())
        interfaceType = interfaceType->get_semantic_base_type();
    while (adaptedType->is_same_vtable_type())
        adaptedType = adaptedType->get_semantic_base_type();

    auto adapterType = this->inner_get_interface_adapter(interfaceType, adaptedType);
//    ASSERT(adapterType->adapted_type()->get_type_id() == adaptedType->get_type_id(),
//           "Mismatching type ids between adapter and adaptee: " << adapterType->adapted_type()->get_type_id() << " != " << adaptedType->get_type_id());
    if (modIf)
        return this->get_modifiable_type(nullptr, adapterType);
    else
        return adapterType;
}



// FUTURE: maybe remove the declaration argument, disallowing 'user' type names for references?
const TxReferenceType* TypeRegistry::get_reference_type(const TxTypeDeclaration* declaration, TxGenericBinding targetTypeBinding,
                                                        const TxIdentifier* dataspace) {
//    std::vector<TxGenericBinding> bindings( { TxGenericBinding::make_type_binding("T", targetTypeDefiner) } );
    std::vector<TxGenericBinding> bindings( { targetTypeBinding } );
    return static_cast<const TxReferenceType*>(this->get_type_specialization(declaration, this->builtinTypes[REFERENCE]->get_type(), {}, &bindings));
}



const TxArrayType* TypeRegistry::get_array_type(const TxTypeDeclaration* declaration,
                                                TxGenericBinding elemTypeBinding, TxGenericBinding lengthBinding) {
//    std::vector<TxGenericBinding> bindings( { TxGenericBinding::make_type_binding("E", elemTypeDefiner),
//                                              TxGenericBinding::make_value_binding("L", lengthDefiner) } );
    std::vector<TxGenericBinding> bindings( { elemTypeBinding, lengthBinding } );
    return static_cast<const TxArrayType*>(this->get_type_specialization(declaration, this->builtinTypes[ARRAY]->get_type(), {}, &bindings));
}

const TxArrayType* TypeRegistry::get_array_type(const TxTypeDeclaration* declaration, TxGenericBinding elemTypeBinding) {
//    std::vector<TxGenericBinding> bindings( { TxGenericBinding::make_type_binding("E", elemTypeDefiner) } );
    std::vector<TxGenericBinding> bindings( { elemTypeBinding } );
    return static_cast<const TxArrayType*>(this->get_type_specialization(declaration, this->builtinTypes[ARRAY]->get_type(), {}, &bindings));
}



const TxFunctionType* TypeRegistry::get_function_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes, const TxType* returnType,
                                                      bool modifiableClosure) {
    auto funcType = new TxFunctionType(declaration, this->builtinTypes[FUNCTION]->get_type(), argumentTypes, returnType, modifiableClosure);
    this->add_type(funcType);
    return funcType;
}

const TxFunctionType* TypeRegistry::get_function_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes,
                                                      bool modifiableClosure) {
    auto funcType = new TxFunctionType(declaration, this->builtinTypes[FUNCTION]->get_type(), argumentTypes, nullptr, modifiableClosure);
    this->add_type(funcType);
    return funcType;
}

const TxConstructorType* TypeRegistry::get_constructor_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes,
                                                            TxTypeDeclaration* objectTypeDecl) {
    auto type = new TxConstructorType(declaration, this->builtinTypes[FUNCTION]->get_type(), argumentTypes, objectTypeDecl);
    this->add_type(type);
    return type;
}
