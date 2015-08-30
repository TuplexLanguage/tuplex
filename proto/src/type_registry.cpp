#include "tx_lang_defs.hpp"
#include "type_registry.hpp"

#include "assert.hpp"
#include "package.hpp"
#include "entity.hpp"
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


/** the flags that may be inherited when specializing a type */
static const TxDeclarationFlags DECL_FLAG_FILTER = TXD_STATIC | TXD_PUBLIC | TXD_PROTECTED | TXD_ABSTRACT | TXD_FINAL | TXD_EXPERRBLOCK;


/*--- private classes providing indirection for fetching the built-in type objects ---*/

class TxBuiltinTypeDefiningNode final : public TxTypeExpressionNode {
    const TxType* baseType;
protected:
    virtual void symbol_declaration_pass_descendants(TxSpecializationIndex six, LexicalContext& defContext,
                                                     LexicalContext& lexContext, TxDeclarationFlags declFlags) override { }

    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        if (six == 0)
            return baseType;
        else
            return this->types(six).make_specialized_type(this->get_declaration(six), TxTypeSpecialization(baseType), {});
    }

public:
    TxBuiltinTypeDefiningNode() : TxTypeExpressionNode(NULL_LOC), baseType() { }

    void set_type(const TxType* type) {
        ASSERT(!this->baseType, "type already set");
        this->baseType = type;
    }

    virtual bool has_predefined_type() const override { return true; }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }
};


class TxBuiltinTypeDefiner final : public TxTypeDefiner {  // TODO: remove/replace this
public:
    const TxType* type;

    TxBuiltinTypeDefiner() : type() { }
    TxBuiltinTypeDefiner(const TxType* type) : type(type) { }

    virtual TxDriver* get_driver() const override { return &this->type->get_declaration()->get_symbol()->get_root_scope()->driver(); }
    virtual const yy::location& get_parse_location() const override { return NULL_LOC; }

    virtual const TxType* resolve_type(ResolutionContext& resCtx) override { return this->type; }
    virtual const TxType* attempt_get_type() const override { return this->type; }
    virtual const TxType* get_type() const override { return this->type; }
    virtual TxTypeDefiningNode* get_node() const override { ASSERT(false, "unexpected invocation"); return nullptr; }
};

class TxBuiltinFieldDefiner final : public TxFieldDefiner {
public:
    const TxField* field;

    TxBuiltinFieldDefiner() : field() { }
    TxBuiltinFieldDefiner(const TxField* field) : field(field) { }

    virtual TxDriver* get_driver() const override { return &this->field->get_declaration()->get_symbol()->get_root_scope()->driver(); }
    virtual const yy::location& get_parse_location() const override { return NULL_LOC; }

    virtual const TxExpressionNode* get_init_expression() const override { return nullptr; }
    virtual const TxField* resolve_field(ResolutionContext& resCtx) override { return this->field; }
    virtual const TxField* get_field() const override { return this->field; }
    virtual const TxType* get_type() const override { return this->field->get_type(); }
    virtual const TxType* attempt_get_type() const override { return this->field->get_type(); }
};


class BuiltinTypeRecord final : public TxTypeDefiner {
    //const TxType* type;
    //const TxTypeDeclaration* declaration;
    TxBuiltinTypeDefiningNode* node;
public:
    const BuiltinTypeId id;
    const std::string plainName;

    BuiltinTypeRecord(BuiltinTypeId id, std::string plainName)
        : node(new TxBuiltinTypeDefiningNode()), id(id), plainName(plainName)  {
    }

    void symbol_declaration_pass(TxModule* module, const std::vector<TxDeclarationNode*>* typeParamDeclNodes=nullptr) {
        LexicalContext ctx(module);
        this->node->symbol_declaration_pass(0, ctx, ctx, TXD_PUBLIC | TXD_BUILTIN, this->plainName, typeParamDeclNodes);
    }

    void set_type(const TxType* type) {
        this->node->set_type(type);
    }

    void symbol_resolution_pass() {
        ResolutionContext resCtx;
        this->node->symbol_resolution_pass(0, resCtx);
    }

//    void set_declaration(const TxTypeDeclaration* declaration) {
//        ASSERT(!this->declaration, "declaration already set");
//        this->declaration = declaration;
//    }
    const TxTypeDeclaration* get_declaration() const { return this->node->get_declaration(0); }

    virtual TxDriver* get_driver() const override { return this->node->get_driver(); }
    virtual const yy::location& get_parse_location() const override { return this->node->get_parse_location(); }

    virtual const TxType* resolve_type(ResolutionContext& resCtx) override { return this->get_type(); }
    virtual const TxType* attempt_get_type() const override { return this->get_type(); }
    virtual const TxType* get_type() const override { return this->node->get_type(0); }

    virtual TxTypeExpressionNode* get_node() const override { return this->node; }
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


void TypeRegistry::declare_tx_functions(TxModule* module) {
    // public _address( r : Ref ) ULong
    std::vector<const TxType*> argumentTypes( { this->builtinTypes[REFERENCE]->get_type() } );
    auto funcTypeDef = new TxBuiltinFieldDefiner();
    auto funcDecl = module->declare_field("_address", funcTypeDef, TXD_PUBLIC | TXD_BUILTIN, TXS_GLOBAL, TxIdentifier(""));
    auto type = new TxFunctionType(nullptr, this->builtinTypes[FUNCTION]->get_type(), argumentTypes,
                                   this->builtinTypes[ULONG]->get_type());
    funcTypeDef->field = new TxField(funcDecl, type);
}


void TypeRegistry::add_builtin_abstract(TxModule* module, TxTypeClass typeClass, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId) {
    auto record = new BuiltinTypeRecord( id, plainName );
    //record->set_declaration( module->declare_type(plainName, record, TXD_PUBLIC | TXD_BUILTIN) );
    record->symbol_declaration_pass(module);
    auto type = new TxBuiltinBaseType( typeClass, record->get_declaration(), this->builtinTypes[parentId]->get_type());
    type->prepare_type_members();
    record->set_type( type );
    record->symbol_resolution_pass();
    this->builtinTypes[record->id] = record;
}

void TypeRegistry::add_builtin_integer(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId,
                                       int size, bool sign) {
    auto record = new BuiltinTypeRecord( id, plainName );
    //record->set_declaration( module->declare_type(plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
    record->symbol_declaration_pass(module);
    auto type = new TxIntegerType( record->get_declaration(), this->builtinTypes[parentId]->get_type(), size, sign);
    type->prepare_type_members();
    record->set_type( type );
    record->symbol_resolution_pass();
    this->builtinTypes[record->id] = record;
}

void TypeRegistry::add_builtin_floating(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId, int size) {
    auto record = new BuiltinTypeRecord( id, plainName );
    //record->set_declaration( module->declare_type(plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
    record->symbol_declaration_pass(module);
    auto type = new TxFloatingType( record->get_declaration(), this->builtinTypes[parentId]->get_type(), size );
    type->prepare_type_members();
    record->set_type( type );
    record->symbol_resolution_pass();
    this->builtinTypes[record->id] = record;
}

/** Initializes the built-in symbols. */
void TypeRegistry::initializeBuiltinSymbols() {
    auto module = this->package.declare_module(* new TxIdentifier(BUILTIN_NS), true);

    // create the Any root type:
    {
        auto record = new BuiltinTypeRecord( ANY, "Any" );
        //record->set_declaration( module->declare_type(record->plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
        record->symbol_declaration_pass(module);
        auto type = new TxAnyType( record->get_declaration() );
        type->prepare_type_members();
        record->set_type( type );
        record->symbol_resolution_pass();
        this->builtinTypes[record->id] = record;
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
        auto record = new BuiltinTypeRecord( BOOL, "Bool" );
        //record->set_declaration( module->declare_type(record->plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
        record->symbol_declaration_pass(module);
        auto type = new TxBoolType(record->get_declaration(), this->builtinTypes[ELEMENTARY]->get_type() );
        type->prepare_type_members();
        record->set_type( type );
        record->symbol_resolution_pass();
        this->builtinTypes[record->id] = record;
    }

    // create the function base type:
    {
        auto record = new BuiltinTypeRecord( FUNCTION, "Function" );
        //record->set_declaration( module->declare_type(record->plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
        record->symbol_declaration_pass(module);
        auto type = new TxBuiltinBaseType(TXTC_FUNCTION, record->get_declaration(), this->builtinTypes[ANY]->get_type() );
        type->prepare_type_members();
        record->set_type( type );
        record->symbol_resolution_pass();
        this->builtinTypes[record->id] = record;
    }

    // create the reference base type:
    {
        auto record = new BuiltinTypeRecord( REFERENCE, "Ref" );
        //auto typeDecl = module->declare_type(record->plainName, record, TXD_PUBLIC | TXD_BUILTIN );
        //record->set_declaration( typeDecl );

        //auto elemTypeDefiner = this->builtinTypes[ANY];
        //typeDecl->get_symbol()->declare_type( "T", elemTypeDefiner, TXD_PUBLIC | TXD_BUILTIN | TXD_GENPARAM );
        auto anyType = new TxPredefinedTypeNode(NULL_LOC, "tx.Any");
        auto paramNodes = new std::vector<TxDeclarationNode*>( { new TxTypeDeclNode(NULL_LOC, TXD_PUBLIC | TXD_GENPARAM,
                                                                                    "T", nullptr, anyType) } );
        record->symbol_declaration_pass(module, paramNodes);

        auto type = new TxReferenceType(record->get_declaration(), this->builtinTypes[ANY]->get_type());
        record->set_type( type );
        record->symbol_resolution_pass();
        this->builtinTypes[record->id] = record;

        type->prepare_type_members();
    }

    // create the array base type:
    {
        auto record = new BuiltinTypeRecord( ARRAY, "Array" );
        //auto typeDecl = module->declare_type( record->plainName, record, TXD_PUBLIC | TXD_BUILTIN );
        //record->set_declaration( typeDecl );

        //auto elemTypeDefiner = this->builtinTypes[ANY];
        //typeDecl->get_symbol()->declare_type( "E", elemTypeDefiner, TXD_PUBLIC | TXD_BUILTIN | TXD_GENPARAM );

        //auto lengthDefiner = new TxBuiltinFieldDefiner();
        //auto lengthDecl = typeDecl->get_symbol()->declare_field( "L", lengthDefiner, TXD_PUBLIC | TXD_BUILTIN | TXD_GENPARAM, TXS_INSTANCE, TxIdentifier() );
        //lengthDefiner->field = new TxField(lengthDecl, this->builtinTypes[UINT]->get_type());
        auto anyType = new TxPredefinedTypeNode(NULL_LOC, "tx.Any");
        auto lenFieldDef = new TxFieldDefNode(NULL_LOC, "L", nullptr, false, this->builtinTypes[UINT]);
        auto paramNodes = new std::vector<TxDeclarationNode*>( { new TxTypeDeclNode(NULL_LOC, TXD_PUBLIC | TXD_GENPARAM,
                                                                                    "E", nullptr, anyType),
                                                                 new TxFieldDeclNode(NULL_LOC, TXD_PUBLIC | TXD_GENPARAM,
                                                                                     lenFieldDef) } );
        record->symbol_declaration_pass(module, paramNodes);

        auto type = new TxArrayType(record->get_declaration(), this->builtinTypes[ANY]->get_type());
        record->set_type( type );
        record->symbol_resolution_pass();
        this->builtinTypes[record->id] = record;

        type->prepare_type_members();
    }

    // create the tuple base type:
    {
        auto record = new BuiltinTypeRecord( TUPLE, "Tuple" );
        //record->set_declaration( module->declare_type(record->plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
        record->symbol_declaration_pass(module);
        auto type = new TxTupleType(record->get_declaration(), this->builtinTypes[ANY]->get_type(), true );
        type->prepare_type_members();
        record->set_type( type );
        record->symbol_resolution_pass();
        this->builtinTypes[record->id] = record;
    }

    // create the interface base type:
    {
        auto record = new BuiltinTypeRecord( INTERFACE, "Interface" );
        //record->set_declaration( module->declare_type(record->plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
        record->symbol_declaration_pass(module);
        {   // declare the adaptee type id virtual field member, which is abstract here but concrete in adapter subtypes:
            const TxType* fieldType = this->get_builtin_type(UINT);
            auto fieldDef = new TxBuiltinFieldDefiner();
            auto fieldDecl = record->get_declaration()->get_symbol()->declare_field(
                    "$adTypeId", fieldDef, TXD_PUBLIC | TXD_BUILTIN | TXD_STATIC | TXD_ABSTRACT | TXD_IMPLICIT, TXS_STATIC, "");
            fieldDef->field = new TxField(fieldDecl, fieldType);
        }
        auto type = new TxInterfaceType(record->get_declaration(), this->builtinTypes[ANY]->get_type());
        type->prepare_type_members();
        record->set_type( type );
        record->symbol_resolution_pass();
        this->builtinTypes[record->id] = record;
    }

//    // create the CString base array type:
//    {
//        auto record = new BuiltinTypeRecord( CSTRING, "CString" );
//        record->set_declaration( module->declare_type( record->plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
//        auto charBinding = TxGenericBinding::make_type_binding("E", this->builtinTypes[UBYTE]);
//        this->builtinTypes[record->id] = record;
//
//        std::vector<TxGenericBinding> bindings( { charBinding } );
//        TxTypeSpecialization specialization(this->builtinTypes[ARRAY]->get_type(), bindings);
//        std::vector<TxTypeParam> typeParams { TxTypeParam(TxTypeParam::TXB_VALUE, "L", this->builtinTypes[UINT]) };
//        record->set_type( specialization.type->make_specialized_type(record->get_declaration(), specialization) );
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

    // scalar conversion-constructor functions:
    for (auto fromTypeId : SCALAR_TYPE_IDS) {
        for (auto toTypeId : SCALAR_TYPE_IDS) {
            declare_conversion_constructor(fromTypeId, toTypeId);
        }
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

        auto c_puts_func_type_def = new TxBuiltinFieldDefiner();
        auto c_puts_decl = txCfuncModule->declare_field("puts", c_puts_func_type_def, TXD_PUBLIC | TXD_BUILTIN, TXS_GLOBAL, TxIdentifier(""));

        auto refTypeNode = new TxReferenceTypeNode(NULL_LOC, nullptr, new TxPredefinedTypeNode(NULL_LOC, "tx.UByte"));
        LexicalContext ctx(txCfuncModule);
        refTypeNode->symbol_declaration_pass(0, ctx, ctx, TXD_PUBLIC | TXD_IMPLICIT, "puts$argtype", nullptr);
        ResolutionContext resCtx;
        refTypeNode->symbol_resolution_pass(0, resCtx);
        auto ubyteRefType = refTypeNode->get_type(0);

        const TxType* returnType = this->builtinTypes[INT]->get_type();

        auto type = new TxFunctionType(nullptr, this->builtinTypes[FUNCTION]->get_type(), { ubyteRefType }, returnType);
        c_puts_func_type_def->field = new TxField(c_puts_decl, type);
    }
    {  // declare tx.c.abort:
        std::vector<const TxType*> argumentTypes( {} );
        auto c_abort_func_type_def = new TxBuiltinFieldDefiner();
        auto c_abort_decl = txCfuncModule->declare_field("abort", c_abort_func_type_def, TXD_PUBLIC | TXD_BUILTIN, TXS_GLOBAL, TxIdentifier(""));
        auto type = new TxFunctionType(nullptr, this->builtinTypes[FUNCTION]->get_type(), argumentTypes);
        c_abort_func_type_def->field = new TxField(c_abort_decl, type);
    }
}

void TypeRegistry::declare_conversion_constructor(BuiltinTypeId fromTypeId, BuiltinTypeId toTypeId) {
//    // global-scope converter function:
//    TxBuiltinDeclProxy* fieldDefiner = new TxBuiltinDeclProxy(this->builtinTypes[toTypeId]->plainName);
//    module->declare_field(fieldDefiner->name, fieldDefiner, TXD_PUBLIC | TXD_BUILTIN, TXS_GLOBAL, TxIdentifier(""));
//    fieldDefiner->type = new TxBuiltinConversionFunctionType(nullptr, this->builtinTypes[FUNCTION]->get_type(),
//                                                             this->builtinTypes[fromTypeId]->get_type(),
//                                                             this->builtinTypes[toTypeId]->get_type());

    // constructors for built-in elementary types are really inline conversion expressions
    auto typeDecl = this->builtinTypes[toTypeId]->get_declaration();
    auto constructorDefiner = new TxBuiltinFieldDefiner();
    auto constructorDecl = typeDecl->get_symbol()->declare_field("$init", constructorDefiner, TXD_PUBLIC | TXD_BUILTIN | TXD_CONSTRUCTOR, TXS_INSTANCEMETHOD, TxIdentifier(""));
    auto type = new TxBuiltinConversionFunctionType(nullptr, this->builtinTypes[FUNCTION]->get_type(),
                                                    this->builtinTypes[fromTypeId]->get_type(),
                                                    this->builtinTypes[toTypeId]->get_type());
    type->prepare_type_members();
    constructorDefiner->field = new TxField(constructorDecl, type);
}


void TypeRegistry::enqueued_resolution_pass() {
    ResolutionContext resCtx;
    while (! this->enqueuedSpecializations.empty()) {
        auto & enqSpec = this->enqueuedSpecializations.front();
        enqSpec.node->symbol_resolution_pass(enqSpec.six, resCtx);
        this->enqueuedSpecializations.pop();
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
            // Types that are distinct in instance data type, or vtable, get their own type id and vtable.
            if (type->get_type_class() == TXTC_FUNCTION)
                continue;
            if (type->is_equivalent_derivation()) {
                //std::cerr << "Not registering equivalent derivation: " << type << std::endl;
                continue;
            }
            type->runtimeTypeId = this->staticTypes.size();
        }
        this->staticTypes.push_back(type);
        //std::cerr << "Registering: " << type << " with id " << type->runtimeTypeId << std::endl;
    }
    ASSERT(this->createdTypes->empty(), "'Extra' types were created while register_types() was running");
    //for (auto type : *this->createdTypes)
    //    std::cerr << "'EXTRA' CREATED TYPE: " << type << std::endl;
    delete this->createdTypes;
    this->createdTypes = createdTypes;
}


const TxTypeDefiner* TypeRegistry::get_builtin_type_def(const BuiltinTypeId id) const {
    return this->builtinTypes[id];
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
    while (type->is_empty_derivation() && !type->is_explicit_nongen_declaration())
        type = type->get_base_type();
    ASSERT(!type->is_modifiable(), "Can't make a modifiable specialization of a modifiable type: " << type);
    if (type->is_builtin()) {
        ASSERT(type->get_type_id() != UINT32_MAX, "builtin type doesn't have type id set: " << type);
        if (auto mtype = this->builtinModTypes[type->get_type_id()])
            return mtype;
        // if not set, then this is run during initialization and a modifiable of a built-in type is being created
    }

    if (! declaration) {
        std::string prefix = ( type->get_declaration()->get_decl_flags() & TXD_IMPLICIT ? "~" : "~$" );
        std::string name = prefix + type->get_declaration()->get_unique_name();
        auto & ctx = type->get_declaration()->get_definer()->get_node()->context(0);
        if (auto entitySymbol = dynamic_cast<TxEntitySymbol*>(ctx.scope()->get_member_symbol(name))) {
            if (auto typeDecl = entitySymbol->get_type_decl()) {
                ResolutionContext resCtx;
                if (auto existingType = typeDecl->get_definer()->resolve_type(resCtx)) {
                    if (existingType->is_modifiable() && *existingType->get_base_type() == *type)
                        return existingType;
                }
            }
            type->LOGGER().warning("Name collision when trying to declare implicit MOD type, preexisting symbol: %s", entitySymbol->to_string().c_str());
            name = ctx.scope()->make_unique_name(name);
        }
        auto modNode = new TxModifiableTypeNode(NULL_LOC, new TxPredefinedTypeNode(NULL_LOC, type->get_declaration()->get_unique_name()));
        TxDeclarationFlags newDeclFlags = ( type->get_decl_flags() & DECL_FLAG_FILTER ) | TXD_IMPLICIT;
        modNode->symbol_declaration_pass(0, ctx, ctx, newDeclFlags, name, nullptr);
        ResolutionContext resCtx;
        modNode->symbol_resolution_pass(0, resCtx);
        return modNode->get_type(0);
    }

    return this->make_specialized_type(declaration, TxTypeSpecialization(type, true));
}

const TxType* TypeRegistry::get_empty_specialization(const TxTypeDeclaration* declaration, const TxType* type) {
    //std::cerr << "MAKING EMPTY TYPE: " << declaration->to_string() << std::endl;
    while (type->is_empty_derivation() && !type->is_explicit_nongen_declaration())
        type = type->get_base_type();
    return this->make_specialized_type(declaration, TxTypeSpecialization(type, false, true));
}



/** Collates the remaining unbound type parameters for the given specialization. * /
static std::vector<TxTypeParam> get_unbound_type_parameters(const TxTypeSpecialization& specialization,
                                                            const std::vector<TxGenericBinding>* bindings,
                                                            const std::vector<TxTypeParam>* typeParams) {
    std::vector<TxTypeParam> unboundParams;
    if (typeParams) {
        for (auto & newTypeParam : *typeParams)
            unboundParams.push_back(newTypeParam);
    }
    for (auto baseTypeParamDecl : specialization.type->type_params()) {
        auto pname = baseTypeParamDecl->get_unique_name();
        if (! std::any_of( bindings->cbegin(), bindings->cend(),
                           [&pname](const TxGenericBinding& b){ return b.param_name() == pname; } ) ) {
            unboundParams.emplace_back(baseTypeParamDecl);
        }
    }
    return unboundParams;
}
*/

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
                                       TxScopeSymbol* baseScope, const std::string& newBaseName,
                                       ResolutionContext& resCtx) {
    if (bindings->size() == baseType->type_params().size()) {
        // if generic type specialization is equivalent to the generic base type, reuse it:
        //std::cerr << "existingBaseType    0: " << baseType << std::endl;
        bool matchOK = true;
        for (auto & binding : *bindings) {
            auto & param = baseType->get_type_param(binding.param_name());
            if (binding.meta_type() == MetaType::TXB_TYPE) {
                auto bindingType = binding.type_definer().get_type();
                auto constraintType = param.get_constraint_type_definer()->resolve_type(resCtx);
                ASSERT(constraintType, "NULL constraint type for type parameter " << param);
                if (*constraintType == *bindingType)
                    continue;
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
            auto existingBaseType = typeDecl->get_definer()->resolve_type(resCtx);
            //std::cerr << "existingBaseType    1: " << existingBaseType << std::endl;
            auto existingGenBaseType = existingBaseType->get_semantic_base_type();
            //std::cerr << "existingGenBaseType 2: " << existingGenBaseType << std::endl;
            if (existingGenBaseType->is_empty_derivation()) {
                existingGenBaseType = existingGenBaseType->get_semantic_base_type();
                //std::cerr << "existingGenBaseType 3: " << existingGenBaseType << std::endl;
            }
            if (*existingGenBaseType == *baseType
                    && existingBaseType->type_params().empty()) {
                bool matchOK = true;
                for (auto & binding : *bindings) {
                    if (auto memberSymbol = dynamic_cast<TxEntitySymbol*>(existingBaseSymbol->get_member_symbol(binding.param_name()))) {
                        if (binding.meta_type() == MetaType::TXB_TYPE) {
                            if (const TxType* membType = memberSymbol->get_type_decl()->get_definer()->resolve_type(resCtx)) {
                                //std::cerr << "COMPARING: " << membType << " AND " << binding.type_definer().get_type() << std::endl;
                                if (*membType == *binding.type_definer().get_type())
                                    continue;
                                //else
                                //    std::cerr << "BINDING MISMATCH: " << membType << " != " << binding.type_definer().get_type() << std::endl;
                            }
                            else
                                baseScope->LOGGER().warning("NULL type for member symbol %s", memberSymbol->get_type_decl()->to_string().c_str());
                        }
                        else {  // MetaType::TXB_VALUE
                            //if (auto fieldDecl = memberSymbol->get_first_field_decl())
                            //    continue;  // FIXME: once Array.L properly implemented, existence of the VALUE binding is sufficient
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
        }
    }
    return nullptr;
}

TxType* TypeRegistry::get_type_specialization(const TxTypeDeclaration* declaration, const TxType* baseType,
                                              const std::vector<TxTypeSpecialization>& interfaces,
                                              const std::vector<TxGenericBinding>* bindings,
                                              const std::vector<TxTypeParam>* typeParams, bool _mutable) {
    // Note: type specialization is never applied to a modifiable-specialization (legal only on generic base type)
    // Note: A non-parameterized type (without any declared type parameters) is not necessarily non-generic:
    //       It may have members that refer to generic parameters declared in an outer scope.
    ASSERT(!baseType->is_modifiable(), "Can't specialize a 'modifiable' base type: " << baseType);

    const TxParseOrigin* pOrigin = (declaration ? static_cast<const TxParseOrigin*>(declaration->get_definer()) : baseType);

    bool hasBindings = (bindings && !bindings->empty());
    if (baseType->get_decl_flags() & TXD_GENPARAM) {
        // only empty derivation allowed from generic type parameter
        if (! (!hasBindings && interfaces.empty() && (!typeParams || typeParams->empty()))) {
            CERROR(pOrigin, "Can't derive from generic type parameter " << baseType);
            return nullptr;
        }
    }

    while (baseType->is_empty_derivation() && !baseType->is_explicit_nongen_declaration())
        baseType = baseType->get_base_type();

    // TODO: pass _mutable flag to type extensions

    // Note: Binding of ref-constrained type parameters don't make the code generation more concrete,
    //       but accesses to the members still need the specialized version of them (e.g. the correct ref-target).

    if (hasBindings)        // type that binds parameters of its base type
        //&& get_unbound_type_parameters(specialization, bindings, typeParams).empty()  // non-parameterized type
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
        ResolutionContext resCtx;
        unsigned bindCount = 0;
        for (auto & binding : *bindings) {
            if (bindCount++)  newBaseTypeName << ",";

            if (binding.meta_type() == MetaType::TXB_TYPE) {
                TxTypeDefiner* bdefiner;
                // we want to bypass empty, implicit derivations:
                auto btype = binding.type_definer().resolve_type(resCtx);
                if (! btype)
                    return nullptr;  // specialization fails if a binding fails resolve
                while (btype->is_empty_derivation() && !btype->is_explicit_nongen_declaration())
                    btype = btype->get_base_type();
                newBaseTypeName << encode_type_name(btype);
                bdefiner = new TxTypeWrapperDef(btype);

                auto & parseLoc = binding.type_definer().get_parse_location();
                auto typeExpr = new TxTypeExprWrapperNode(parseLoc, bdefiner);
                auto declNode = new TxTypeDeclNode(parseLoc, TXD_PUBLIC | TXD_GENBINDING, binding.param_name(), nullptr, typeExpr);
                bindingDeclNodes->push_back(declNode);
                package.LOGGER().trace("Re-bound base type %s parameter '%s' with %s", baseDecl->get_unique_full_name().c_str(),
                                       binding.param_name().c_str(), typeExpr->to_string().c_str());
            }
            else {
                auto & parseLoc = binding.value_definer().get_parse_location();
                auto fieldDef = new TxFieldDefNode(parseLoc, binding.param_name(), nullptr, &binding.value_definer());
                auto declNode = new TxFieldDeclNode(parseLoc, TXD_PUBLIC | TXD_GENBINDING, fieldDef);
                bindingDeclNodes->push_back(declNode);
                package.LOGGER().trace("Re-bound base type %s parameter '%s' with %s", baseDecl->get_unique_full_name().c_str(),
                                       binding.param_name().c_str(), binding.value_definer().to_string().c_str());

                newBaseTypeName << "$VALUE";
            }
        }
        newBaseTypeName << ">";
        auto newBaseTypeNameStr = newBaseTypeName.str();

        // if equivalent specialized type already exists then reuse it, otherwise create new one:
        auto baseScope = baseDecl->get_symbol()->get_outer();
        const TxType* specializedBaseType = get_existing_type(baseType, bindings, baseScope, newBaseTypeNameStr, resCtx);
        if (! specializedBaseType) {
            {   // pass on the generic base type to the new specialization via member named $GenericBase:
                auto & parseLoc = declaration->get_definer()->get_parse_location();
                auto typeExpr = new TxTypeExprWrapperNode(parseLoc, new TxTypeWrapperDef(baseType));
                auto declNode = new TxTypeDeclNode(parseLoc, TXD_PUBLIC | TXD_IMPLICIT, "$GenericBase", nullptr, typeExpr);
                bindingDeclNodes->push_back(declNode);
            }

            // process new specialization of the base type:
            auto baseTypeExpr = static_cast<TxTypeExpressionNode*>(baseDecl->get_definer()->get_node());
            TxSpecializationIndex newSix = baseTypeExpr->next_spec_index();
            LexicalContext baseContext = LexicalContext(baseTypeExpr->context(0));
            ASSERT(baseContext.scope() == baseScope, "Unexpected lexical scope: " << baseContext.scope() << " != " << baseScope);

            newBaseTypeNameStr = baseContext.scope()->make_unique_name(newBaseTypeNameStr, true);
            //std::cerr << "specializing base " << newBaseName << ": " << baseType << std::endl;
            baseTypeExpr->symbol_declaration_pass(newSix, baseContext, baseContext, newDeclFlags, newBaseTypeNameStr, bindingDeclNodes);
            // Invoking the resolution pass here can cause infinite recursion
            // (since the same source text construct may be recursively reprocessed),
            // so we enqueue this "specialization pass" for later processing.
            //baseTypeExpr->symbol_resolution_pass(newSix, resCtx);
            this->enqueuedSpecializations.emplace( EnqueuedSpecialization{ baseTypeExpr, newSix } );
            specializedBaseType = baseTypeExpr->resolve_type(newSix, resCtx);
        }

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

    auto ifDecl = interfaceType->get_declaration();
    auto scope = ifDecl->get_symbol()->get_outer();

    // we want to bypass empty, implicit derivations:
    while (adaptedType->is_empty_derivation() && !adaptedType->is_explicit_nongen_declaration())
        adaptedType = adaptedType->get_base_type();
    std::string adapterName = ifDecl->get_unique_name() + "$if$" + encode_type_name(adaptedType);

    if (auto existingAdapterSymbol = dynamic_cast<TxEntitySymbol*>(scope->get_member_symbol(adapterName))) {
        if (auto typeDecl = existingAdapterSymbol->get_type_decl()) {
            ResolutionContext resCtx;
            auto adapterType = static_cast<const TxInterfaceAdapterType*>(typeDecl->get_definer()->resolve_type(resCtx));
            std::cerr << "Getting existing interface adapter: " << adapterType << std::endl;
            return adapterType;
        }
    }

    //std::cerr << "Creating interface adapter:\n\tfrom " << adaptedType << "\n\tto   " << interfaceType << std::endl;
    auto typeDefiner = new TxBuiltinTypeDefiner();
    // TODO: combine flags from adapted and adaptee types, including TXD_EXPERRBLOCK
    auto typeDecl = scope->declare_type(adapterName, typeDefiner, TXD_PUBLIC | TXD_IMPLICIT);
    {   // override the adaptee type id virtual field member:
        const TxType* fieldType = this->get_builtin_type(UINT);
        auto fieldDef = new TxBuiltinFieldDefiner();
        auto fieldDecl = typeDecl->get_symbol()->declare_field("$adTypeId", fieldDef, TXD_PUBLIC | TXD_STATIC | TXD_OVERRIDE | TXD_IMPLICIT, TXS_STATIC, "");
        fieldDef->field = new TxField(fieldDecl, fieldType);
    }
    auto adapterType = new TxInterfaceAdapterType(typeDecl, interfaceType, adaptedType);
    typeDefiner->type = adapterType;
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
