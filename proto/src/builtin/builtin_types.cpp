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



class TxBuiltinTypeDefiningNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext ) override final { }

public:
    TxBuiltinTypeDefiningNode( const TxLocation& parseLocation ) : TxTypeExpressionNode( parseLocation )  { }

    /** Creates a copy of this node and all its descendants for purpose of generic specialization. */
    virtual TxImplicitTypeDefiningNode* make_ast_copy() const override final {
        ASSERT(false, "Can't make AST copy of Any definer");
        return nullptr;
    }

    virtual std::string get_auto_type_name() const override final {
        return this->get_declared_name();
    }

    virtual bool has_predefined_type() const override final { return true; }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }
};



class TxAnyTypeNode final : public TxBuiltinTypeDefiningNode {
protected:
    virtual const TxType* define_type() override {
        auto type = new TxAnyType( this->get_declaration() );
        return type;
    }

public:
    TxAnyTypeNode( const TxLocation& parseLocation ) : TxBuiltinTypeDefiningNode( parseLocation ) { }
};
///* * Declares the members of the Any root type. */
//static void declare_any_members(const TxTypeDeclaration* anyDecl) {
//    // candidates:
//    // public static _typeid() UInt
//    // public final _address() ULong
//}



BuiltinTypes::BuiltinTypes( TypeRegistry& registry )
        : registry(registry), builtinLocation( registry.get_package().root_origin().get_parse_location() ) {
}

void BuiltinTypes::declare_tx_functions(TxModule* module) {
    // public _address( r : Ref ) ULong
    std::vector<const TxType*> argumentTypes( { this->builtinTypes[REFERENCE]->get_type() } );
    auto funcTypeDef = new TxBuiltinFieldDefNode( this->builtinLocation );
    auto funcDecl = module->declare_field("_address", funcTypeDef, TXD_PUBLIC | TXD_BUILTIN, TXS_GLOBAL, TxIdentifier(""));
    auto type = new TxFunctionType(nullptr, this->builtinTypes[FUNCTION]->get_type(), argumentTypes,
                                   this->builtinTypes[ULONG]->get_type());
    funcTypeDef->set_field( new TxField(funcDecl, type) );
}


void BuiltinTypes::add_builtin_abstract(TxModule* module, TxTypeClass typeClass, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId) {
    auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, plainName, id, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT );
    LexicalContext lexCtx( module );
    definer->symbol_declaration_pass( lexCtx );
    auto type = new TxBuiltinBaseType( typeClass, definer->get_declaration(), this->builtinTypes[parentId]->get_type());
    type->prepare_type_members();
    definer->set_type( type );
    definer->symbol_resolution_pass();
    this->builtinTypes[definer->id] = definer;
}

void BuiltinTypes::add_builtin_integer(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId,
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

void BuiltinTypes::add_builtin_floating(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId, int size) {
    auto definer = new TxBuiltinTypeDeclNode( this->builtinLocation, plainName, id, TXD_PUBLIC | TXD_BUILTIN );
    LexicalContext lexCtx( module );
    definer->symbol_declaration_pass( lexCtx );
    auto type = new TxFloatingType( definer->get_declaration(), this->builtinTypes[parentId]->get_type(), size );
    type->prepare_type_members();
    definer->set_type( type );
    definer->symbol_resolution_pass();
    this->builtinTypes[definer->id] = definer;
}

//auto definer = new TxTypeDeclNode( this->builtinLocation, TXD_PUBLIC | TXD_BUILTIN | TXD_ABSTRACT, "Any", nullptr,
//                                   new TxAnyTypeNode( this->builtinLocation ) );

/** Initializes the built-in symbols. */
void BuiltinTypes::initializeBuiltinSymbols() {
    auto module = this->registry.get_package().declare_module( this->registry.get_package().root_origin(), * new TxIdentifier(BUILTIN_NS), true );
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
        auto anyType = new TxIdentifiedTypeNode(this->builtinLocation, "tx.Any");
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
        auto anyType = new TxIdentifiedTypeNode(this->builtinLocation, "tx.Any");
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
        ASSERT(id == this->registry.staticTypes.size(), "registering built-in type in wrong order / id: " << id);
        ASSERT(biType->is_prepared(), "built-in type not prepared: " << biType);
        biType->runtimeTypeId = id;
        this->registry.staticTypes.push_back(biType);

        this->builtinModTypes[id] = nullptr;  // prevents read from uninitialized value
        this->builtinModTypes[id] = this->registry.get_modifiable_type(nullptr, biType);
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

    declare_tx_functions(module);

//    // built-in global constants:
//    auto charsType = new TxArrayType("tx.Char"); // BUILTIN_TYPES[CHAR].type);
//    TxBuiltinTypeDefiner* charsProd = new TxBuiltinTypeDefiner("CharArray", charsType);
//    auto strRec = BuiltinTypeRecord { STRING, "String", new TxReferenceType("tx.CharArray") };
//    TxBuiltinTypeDefiner* strProd = new TxBuiltinTypeDefiner(strRec);
//    builtinModule->declareType(strProd->name, TXD_PUBLIC, false, *strProd);

    auto txCfuncModule = this->registry.get_package().declare_module( this->registry.get_package().root_origin(), TxIdentifier(BUILTIN_NS ".c"), true);
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

void BuiltinTypes::declare_default_constructor(LexicalContext& ctx, BuiltinTypeId toTypeId, TxExpressionNode* initValueExpr) {
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

void BuiltinTypes::declare_conversion_constructor(BuiltinTypeId fromTypeId, BuiltinTypeId toTypeId) {
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



const TxType* BuiltinTypes::get_builtin_type( const BuiltinTypeId id, bool mod ) const {
    return mod ? this->builtinModTypes[id] : this->builtinTypes[id]->get_type();
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
        fieldDef->set_context( adapterDefiner->context() );  // FIXME: refactor so that declaration pass is run on builtin nodes
        auto fieldDecl = typeDecl->get_symbol()->declare_field("$adTypeId", fieldDef, TXD_PUBLIC | TXD_STATIC | TXD_OVERRIDE | TXD_IMPLICIT, TXS_STATIC, "");
        fieldDef->set_field( new TxField(fieldDecl, fieldType) );
    }

    auto adapterType = new TxInterfaceAdapterType(typeDecl, interfaceType, adaptedType);
    adapterDefiner->set_type( adapterType );
    adapterDefiner->symbol_resolution_pass();
    this->registry.add_type(adapterType);
    return adapterType;
}

