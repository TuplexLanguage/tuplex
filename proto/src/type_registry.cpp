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

/*--- private classes providing indirection for fetching the built-in type objects ---*/

class TxBuiltinTypeDefiner final : public TxTypeDefiner {
public:
    const TxType* type;

    TxBuiltinTypeDefiner() : type() { }
    //TxBuiltinTypeDefiner(const TxType* type) : package(package), type(type) { }

    virtual TxDriver* get_driver() const override { return &this->type->get_declaration()->get_symbol()->get_root_scope()->driver(); }
    virtual const yy::location& get_parse_location() const override { return NULL_LOC; }

    virtual const TxType* resolve_type(ResolutionContext& resCtx) override { return this->type; }
    virtual const TxType* attempt_get_type() const override { return this->type; }
    virtual const TxType* get_type() const override { return this->type; }
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


class BuiltinTypeRecord : public TxTypeDefiner {
    const TxType* type;
    const TxTypeDeclaration* declaration;
public:
    const BuiltinTypeId id;
    const std::string plainName;

    BuiltinTypeRecord(BuiltinTypeId id, std::string plainName)
        : type(), declaration(), id(id), plainName(plainName)  {
    }

    void set_type(const TxType* type) {
        ASSERT(!this->type, "type already set");
        this->type = type;
    }

    void set_declaration(const TxTypeDeclaration* declaration) {
        ASSERT(!this->declaration, "declaration already set");
        this->declaration = declaration;
    }

    const TxTypeDeclaration* get_declaration() const { return this->declaration; }

    virtual TxDriver* get_driver() const override { return &this->declaration->get_symbol()->get_root_scope()->driver(); }
    virtual const yy::location& get_parse_location() const override { return NULL_LOC; }

    virtual const TxType* resolve_type(ResolutionContext& resCtx) override { return this->type; }
    virtual const TxType* attempt_get_type() const override { return this->type; }
    virtual const TxType* get_type() const override { return this->type; }
};



/** Used solely for the Any root type object. */
class TxAnyType final : public TxType {
    TxAnyType(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec)
            : TxType(TXTC_ANY, declaration, baseTypeSpec)  { }

    TxType* make_specialized_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                  const std::vector<TxTypeParam>& typeParams,
                                  std::string* errorMsg=nullptr) const override {
        ASSERT(typeParams.empty(), "can't specify type parameters for " << this);
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
                                  const std::vector<TxTypeParam>& typeParams,
                                  std::string* errorMsg=nullptr) const override {
        ASSERT(typeParams.empty(), "can't specify type parameters for " << this);
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



void TypeRegistry::add_builtin_abstract(TxModule* module, TxTypeClass typeClass, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId) {
    auto record = new BuiltinTypeRecord( id, plainName );
    record->set_declaration( module->declare_type(plainName, record, TXD_PUBLIC | TXD_BUILTIN) );
    record->set_type( new TxBuiltinBaseType( typeClass, record->get_declaration(), this->builtinTypes[parentId]->get_type() ) );
    this->builtinTypes[record->id] = record;
}

void TypeRegistry::add_builtin_integer(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId,
                                       int size, bool sign) {
    auto record = new BuiltinTypeRecord( id, plainName );
    record->set_declaration( module->declare_type(plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
    record->set_type( new TxIntegerType( record->get_declaration(), this->builtinTypes[parentId]->get_type(), size, sign) );
    this->builtinTypes[record->id] = record;
}

void TypeRegistry::add_builtin_floating(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId, int size) {
    auto record = new BuiltinTypeRecord( id, plainName );
    record->set_declaration( module->declare_type(plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
    record->set_type( new TxFloatingType( record->get_declaration(), this->builtinTypes[parentId]->get_type(), size ) );
    this->builtinTypes[record->id] = record;
}

/** Initializes the built-in symbols. */
void TypeRegistry::initializeBuiltinSymbols() {
    auto module = this->package.declare_module(* new TxIdentifier(BUILTIN_NS));

    // create the Any root type:
    {
        auto record = new BuiltinTypeRecord( ANY, "Any" );
        record->set_declaration( module->declare_type(record->plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
        record->set_type( new TxAnyType(record->get_declaration()) );
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
        record->set_declaration( module->declare_type(record->plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
        record->set_type( new TxBoolType(record->get_declaration(), this->builtinTypes[ANY]->get_type() ) );
        this->builtinTypes[record->id] = record;
    }

    // create the function base type:
    {
        auto record = new BuiltinTypeRecord( FUNCTION, "Function" );
        record->set_declaration( module->declare_type(record->plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
        record->set_type( new TxBuiltinBaseType(TXTC_FUNCTION, record->get_declaration(), this->builtinTypes[ANY]->get_type() ) );
        this->builtinTypes[record->id] = record;
    }

    // create the reference base type:
    {
        auto record = new BuiltinTypeRecord( REFERENCE, "Ref" );
        auto typeDecl = module->declare_type(record->plainName, record, TXD_PUBLIC | TXD_BUILTIN );
        record->set_declaration( typeDecl );
        record->set_type( new TxReferenceType(record->get_declaration(), this->builtinTypes[ANY]->get_type(), this->builtinTypes[ANY] ) );
        this->builtinTypes[record->id] = record;

        // create empty specialization for the constraint type (uniquely named but identical type)
        auto constraintType = this->builtinTypes[ANY]->get_type();
        auto elemTypeDefiner = new TxBuiltinTypeDefiner();
        auto elemTypeEnt = typeDecl->get_symbol()->declare_type( "T", elemTypeDefiner, TXD_PUBLIC | TXD_GENPARAM );
        elemTypeDefiner->type = constraintType->make_specialized_type(elemTypeEnt, TxTypeSpecialization(constraintType));
    }

    // create the array base type:
    {
        auto record = new BuiltinTypeRecord( ARRAY, "Array" );
        auto typeDecl = module->declare_type( record->plainName, record, TXD_PUBLIC | TXD_BUILTIN );
        record->set_declaration( typeDecl );
        record->set_type( new TxArrayType(record->get_declaration(), this->builtinTypes[ANY]->get_type(),
                                          this->builtinTypes[ANY], this->builtinTypes[UINT] ) );
        this->builtinTypes[record->id] = record;

        // create empty specialization for the constraint type (uniquely named but identical type)
        auto constraintType = this->builtinTypes[ANY]->get_type();
        auto elemTypeDefiner = new TxBuiltinTypeDefiner();
        auto elemTypeDecl = typeDecl->get_symbol()->declare_type( "E", elemTypeDefiner, TXD_PUBLIC | TXD_GENPARAM );
        elemTypeDefiner->type = constraintType->make_specialized_type(elemTypeDecl, TxTypeSpecialization(constraintType));

        auto lengthDefiner = new TxBuiltinFieldDefiner();
        auto lengthDecl = typeDecl->get_symbol()->declare_field( "L", lengthDefiner, TXD_PUBLIC | TXD_GENPARAM, TXS_INSTANCE, TxIdentifier() );
        lengthDefiner->field = new TxField(lengthDecl, this->builtinTypes[UINT]->get_type());
    }

    // create the tuple base type:
    {
        auto record = new BuiltinTypeRecord( TUPLE, "Tuple" );
        record->set_declaration( module->declare_type(record->plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
        record->set_type( new TxTupleType(record->get_declaration(), this->builtinTypes[ANY]->get_type(), true ) );
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


    // create modifiable specializations of the concrete built-in types:
    for (int id = 0; id < BuiltinTypeId_COUNT; id++) {
        // verify that all built-in types are initialized:
        ASSERT(this->builtinTypes[id], "Uninitialized built-in type! id=" << id);

        auto biType = this->builtinTypes[id]->get_type();
        TxTypeSpecialization tmpSpec(biType, true);
        std::vector<TxTypeParam> unbound;
        this->builtinModTypes[id] = biType->make_specialized_type(nullptr, tmpSpec, unbound, nullptr);

        this->register_type(const_cast<TxType*>(biType));
    }

//    // test adding static field to types:
//    for (int id = 0; id < BuiltinTypeId_COUNT; id++) {
//        const_cast<TxTypeEntity*>(this->builtinTypes[id]->get_entity())->declare_field("typeid", this->builtinTypes[USHORT],
//                (TxDeclarationFlags)(TXD_PUBLIC | TXD_STATIC | TXD_BUILTIN), TXS_STATIC, TxIdentifier());
//    }

    // scalar conversion-constructor functions:
    for (auto fromTypeId : SCALAR_TYPE_IDS) {
        for (auto toTypeId : SCALAR_TYPE_IDS) {
            declare_conversion_constructor(fromTypeId, toTypeId);
        }
    }
    declare_conversion_constructor(BOOL, BOOL);

//    // built-in global constants:
//    auto charsType = new TxArrayType("tx.Char"); // BUILTIN_TYPES[CHAR].type);
//    TxBuiltinTypeDefiner* charsProd = new TxBuiltinTypeDefiner("CharArray", charsType);
//    auto strRec = BuiltinTypeRecord { STRING, "String", new TxReferenceType("tx.CharArray") };
//    TxBuiltinTypeDefiner* strProd = new TxBuiltinTypeDefiner(strRec);
//    builtinModule->declareType(strProd->name, TXD_PUBLIC, false, *strProd);

    {
        auto txCfuncModule = this->package.declare_module(TxIdentifier(BUILTIN_NS ".c"));

        auto implTypeName = "UByte$Ref";
        auto ubyteRefDef = new TxBuiltinTypeDefiner();
        auto ubyteRefDecl = txCfuncModule->declare_type(implTypeName, ubyteRefDef, TXD_PUBLIC | TXD_IMPLICIT);
        auto charBinding = TxGenericBinding::make_type_binding("T", this->builtinTypes[UBYTE]);
        ubyteRefDef->type = this->get_reference_type(ubyteRefDecl, charBinding);

        std::vector<const TxType*> argumentTypes( { ubyteRefDef->type } );  // CString arg type
        auto c_puts_func_type_def = new TxBuiltinFieldDefiner();
        auto c_puts_decl = txCfuncModule->declare_field("puts", c_puts_func_type_def, TXD_PUBLIC | TXD_BUILTIN, TXS_GLOBAL, TxIdentifier(""));
        const TxType* returnType = this->builtinTypes[INT]->get_type();
        auto type = new TxFunctionType(nullptr, this->builtinTypes[FUNCTION]->get_type(), argumentTypes, returnType);
        c_puts_func_type_def->field = new TxField(c_puts_decl, type);
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
    constructorDefiner->field = new TxField(constructorDecl, type);
}


void TypeRegistry::register_type(TxType* type) {
    // Types that are distinct in instance data type, or vtable, get their own type id and vtable.
    if (type->is_equivalent_derivation()) {
        //std::cerr << "Not registering equivalent derivation: " << type << std::endl;
        return;
    }
    type->typeId = this->allStaticTypes.size();
    //std::cerr << "Registering: " << type << " with id " << type->typeId << std::endl;
    this->allStaticTypes.push_back(type);
}



const TxTypeDefiner* TypeRegistry::get_builtin_type_def(const BuiltinTypeId id) const {
    return this->builtinTypes[id];
}

const TxType* TypeRegistry::get_builtin_type(const BuiltinTypeId id, bool mod) const {
    return mod ? this->builtinModTypes[id] : this->builtinTypes[id]->get_type();
}


const TxType* TypeRegistry::get_modifiable_type(const TxTypeDeclaration* declaration, const TxType* type, std::string* errorMsg) {
    // 'modifiable' is always a distinct 'specialization' (no parameter bindings (or type extensions))
    ASSERT(!type->is_modifiable(), "Can't make a modifiable specialization of a modifiable type: " << type);
    if (type->is_builtin()) {
        // FUTURE: rewrite when we have the built-in type id in the type object
        for (int id = 0; id < BuiltinTypeId_COUNT; id++)
            if (this->builtinTypes[id]->get_type() == type)
                return this->builtinModTypes[id];
        ASSERT(false, "could not find built-in base type: " << type);
    }
    TxTypeSpecialization tmpSpec(type, true);
    std::vector<TxTypeParam> unbound;
    return type->make_specialized_type(declaration, tmpSpec, unbound, errorMsg);
}

/*
static const TxType* make_concrete(const TxType* type, const std::vector<TxGenericBinding>& bindings) {
    // Re-create the super types, starting with the top generic one, and continue down to this new type.
    // (Can't restrict the traversed super types based on the parameter names, since super types'
    // parameters can have been rebound to derivations of sub-types' parameters.)
    // Each recreated super type shall thus be made concrete.
    auto baseType = type->get_base_type();
    if (baseType->is_generic()) {
        // FIX ME: accumulate bindings
        auto newBaseType = make_concrete(baseType, bindings);
    }

    // FIX ME:
    // resolve bindings (tricky part?)
    // determine new type name
    // run declaration pass
    // run resolution pass
}
*/

static inline std::vector<TxTypeParam> prepare_type_parameters(const TxTypeSpecialization& specialization,
                                                               const std::vector<TxTypeParam>* typeParams,
                                                               const TxParseOrigin* pOrigin) {
    std::vector<TxTypeParam> unboundParams;
    if (typeParams)
        unboundParams = *typeParams;
    for (auto & baseTypeParam : specialization.type->type_params()) {
        const TxGenericBinding* matchedBinding = nullptr;
        for (auto & paramBinding : specialization.bindings)
            if (baseTypeParam.param_name() == paramBinding.param_name() && baseTypeParam.meta_type() == paramBinding.meta_type()) {
                matchedBinding = &paramBinding;
                break;
            }
        if (! matchedBinding) {
            if (typeParams) {
                const TxTypeParam* matchedNewParam = nullptr;
                for (auto & newParam : *typeParams) {
                    if (baseTypeParam.param_name() == newParam.param_name() && baseTypeParam.meta_type() == newParam.meta_type()) {
                        matchedNewParam = &newParam;
                        break;
                    }
                }
                if (! matchedNewParam) {
                    // Note: This can be the case for specializations that have defined the parameter (it is no longer GENPARAM)
                    CERROR(pOrigin, "Type parameter " << baseTypeParam << " of base type " << specialization.type
                           << " neither bound or redeclared in the specialization" );
                }
            }
            else {
                // if no type parameters declared at all, automatically redeclare the unbound type parameters of base type:
                unboundParams.push_back(baseTypeParam);
            }
        }
    }
    return unboundParams;
}

// FUTURE: add interfaces
const TxType* TypeRegistry::get_type_specialization(const TxTypeDeclaration* declaration, const TxTypeSpecialization& specialization,
                                                    const std::vector<TxTypeParam>* typeParams, bool _mutable) {
    // Note: type specialization is never applied to a modifiable-specialization (legal only on generic base type)
    // Note: A non-parameterized type (without any declared type parameters) is not necessarily non-generic:
    //       It may have members that refer to generic parameters declared in an outer scope.
    ASSERT(!specialization.type->is_modifiable(), "Can't specialize a 'modifiable' base type: " << specialization);
    ASSERT(!specialization.modifiable, "Can't specify 'modifiable' in a type parameter specialization: " << specialization);

    if (specialization.type->get_declaration() && (specialization.type->get_declaration()->get_decl_flags() & TXD_GENPARAM)) {
        CERROR(declaration->get_definer(), "Can't derive from generic type parameter " << specialization.type);
        return nullptr;
    }

    std::string errorMsg;  // TODO: deprecated, remove
    // TODO: pass _mutable flag to type extensions

    const TxParseOrigin* pOrigin = (declaration ? static_cast<const TxParseOrigin*>(declaration->get_definer()) : specialization.type);
    std::vector<TxTypeParam> allParams = prepare_type_parameters(specialization, typeParams, pOrigin);

    if (allParams.empty() && ! specialization.bindings.empty()  // non-parameterized type that binds parameters of its base type
        && ! specialization.type->is_builtin()) {  // (avoids source-expression-less built-in types (incl Ref<> and Array<>))
        // re-base it on new non-generic specialization of the base type:
        // (this replaces the bindings of the TxTypeSpecialization object with direct declarations within the new type)

        // TODO: How should we handle Ref? We can't resolve it's parameter in order to prevent infinite recursion.
        //       We also need to handle the dataspace use case.
        // TODO: How should we handle Array? Should user be allowed to extend it?

        ASSERT(declaration, "expected type that binds base type's parameters to be named (declared) but was not");
        this->package.LOGGER().alert("Re-basing non-parameterized type %s by specializing its parameterized base type %s",
                                     declaration->get_unique_full_name().c_str(), specialization.type->to_string().c_str());

        // before we substitute the TxTypeSpecialization, validate it:
        specialization.validate();

        auto baseDecl = specialization.type->get_declaration();
        ASSERT(baseDecl, "base type has no declaration: " << specialization.type);
        auto baseTypeExpr = static_cast<TxTypeExpressionNode*>(baseDecl->get_definer()->get_node());
        TxSpecializationIndex newSix = baseTypeExpr->next_spec_index();

        //LexicalContext lexContext = LexicalContext(declaration->get_symbol()->get_outer());
        //std::string newBaseName = lexContext.scope()->make_unique_name(declaration->get_unique_name() + "$" + baseDecl->get_unique_name());
        LexicalContext lexContext = LexicalContext(baseDecl->get_symbol()->get_outer());
        std::string newBaseName = baseDecl->get_unique_name();

        // make new parameter declarations that resolve to the bindings:
        auto typeParamDeclNodes = new std::vector<TxDeclarationNode*>();
        ResolutionContext resCtx;
        for (auto & binding : specialization.bindings) {
            if (binding.meta_type() == TxTypeParam::MetaType::TXB_TYPE) {
                auto & parseLoc = binding.type_definer().get_parse_location();
                auto typeExpr = new TxTypeExprWrapperNode(parseLoc, &binding.type_definer());
                auto declNode = new TxTypeDeclNode(parseLoc, TXD_PUBLIC | TXD_IMPLICIT, binding.param_name(), nullptr, typeExpr);
                typeParamDeclNodes->push_back(declNode);

                const TxType* btype = binding.type_definer().resolve_type(resCtx);
                while (btype->is_equivalent_derivation())
                    btype = btype->get_base_type();
                ASSERT(btype->get_declaration(), "Bound type (or equiv. parent) does not have declaration: " << btype);
                auto boundTypeName = btype->get_declaration()->get_symbol()->get_full_name().to_hash_string();
                newBaseName += "$" + boundTypeName;
                package.LOGGER().alert("Re-bound base type %s parameter '%s' with %s", baseDecl->get_unique_full_name().c_str(),
                                       binding.param_name().c_str(), typeExpr->to_string().c_str());
            }
            else {
                ASSERT(!binding.value_definer().is_context_set(newSix), "VALUE param '" << binding.param_name() << "' at "
                       << binding.value_definer().to_string() << " already processed for s-ix " << newSix);
                auto & parseLoc = binding.value_definer().get_parse_location();
                auto fieldDef = new TxFieldDefNode(parseLoc, binding.param_name(), nullptr, &binding.value_definer());
                auto declNode = new TxFieldDeclNode(parseLoc, TXD_PUBLIC | TXD_IMPLICIT, fieldDef);
                typeParamDeclNodes->push_back(declNode);

                newBaseName += "$VALUE";
                package.LOGGER().trace("Re-bound base type %s parameter '%s' with %s", baseDecl->get_unique_full_name().c_str(),
                                       binding.param_name().c_str(), binding.value_definer().to_string().c_str());
            }
        }

        {
            auto & parseLoc = declaration->get_definer()->get_parse_location();
            auto typeExpr = new TxTypeExprWrapperNode(parseLoc, new TxTypeWrapperDef(specialization.type));
            auto declNode = new TxTypeDeclNode(parseLoc, TXD_PUBLIC | TXD_IMPLICIT, "$GenericBase", nullptr, typeExpr);
            typeParamDeclNodes->push_back(declNode);
            //package.LOGGER().alert("Provided $GenericBase type: %s", baseDecl->get_unique_full_name().c_str());
        }

        newBaseName = lexContext.scope()->make_unique_name(newBaseName, true);
        // TODO: if name already exists and specialization is equal, return existing type instead of creating new one

        // process new specialization of the base type:
        baseTypeExpr->symbol_declaration_pass(newSix, lexContext, lexContext, TXD_PUBLIC | TXD_IMPLICIT, newBaseName, typeParamDeclNodes);
        baseTypeExpr->symbol_resolution_pass(newSix, resCtx);
        auto newBaseType = baseTypeExpr->resolve_type(newSix, resCtx);
        TxTypeSpecialization newSpec(newBaseType);
        auto newType = specialization.type->make_specialized_type(declaration, newSpec, allParams, &errorMsg);
        this->register_type(newType);
        return newType;
    }
    else {
        auto newType = specialization.type->make_specialized_type(declaration, specialization, allParams, &errorMsg);
        this->register_type(newType);
        return newType;
    }
}


const TxReferenceType* TypeRegistry::get_reference_type(const TxTypeDeclaration* declaration, TxGenericBinding targetTypeBinding,
                                                        const TxIdentifier* dataspace, std::string* errorMsg) {
    std::vector<TxGenericBinding> bindings( { targetTypeBinding } );
    TxTypeSpecialization specialization(this->builtinTypes[REFERENCE]->get_type(), bindings, dataspace);
    return static_cast<const TxReferenceType*>(this->get_type_specialization(declaration, specialization));

    /* if to use this approach, find way to avoid resolving the target type (causes recursion):
    // Each type gets a shared reference type declared adjacent to it.
    ResolutionContext resCtx;
    auto targetType = targetTypeBinding.type_definer().resolve_type(resCtx);
    auto targetTypeSymbol = targetType->get_declaration()->get_symbol();
    auto targetTypeScope = targetTypeSymbol->get_outer();
    std::string refTypeName = targetTypeSymbol->get_name() + ( targetType->is_modifiable() ? "$MRef" : "$Ref" );
    auto refTypeSymbol = dynamic_cast<TxEntitySymbol*>(lookup_member(targetTypeScope->get_root_scope(), targetTypeScope, refTypeName));
    const TxType* refType = ( (refTypeSymbol && refTypeSymbol->get_type_decl()) ? refTypeSymbol->get_type_decl()->get_type_definer()->resolve_type(resCtx) : nullptr);

    if (! refType) {
        // this target type has no shared ref type declared for it yet
        auto typeDefiner = new TxBuiltinTypeDefiner();
        auto commonDecl = targetTypeScope->declare_type(refTypeName, typeDefiner, TXD_PUBLIC);
        std::vector<TxGenericBinding> bindings( { targetTypeBinding } );
        TxTypeSpecialization specialization(this->builtinTypes[REFERENCE]->get_type(), bindings);
        typeDefiner->type = this->get_type_specialization(commonDecl, specialization, false, nullptr, errorMsg);
        refType = typeDefiner->type;
    }

    if (declaration) {
        // new declaration provided - create empty specialization (uniquely named but identical type)
        refType = this->get_type_specialization(declaration, TxTypeSpecialization(refType), false, nullptr, errorMsg);
    }
    // TODO: Dataspace-specific instances may need their own derivation regardless
    return static_cast<const TxReferenceType*>(refType);
    */
}

//const TxReferenceType* TypeRegistry::get_reference_type(const TxTypeDeclaration* declaration, TxEntityDefiner* targetType,
//                                                        std::string* errorMsg) {
//    return this->get_reference_type(declaration, TxTypeBinding("T", targetType) );
//}


const TxArrayType* TypeRegistry::get_array_type(const TxTypeDeclaration* declaration,
                                                TxGenericBinding elemTypeBinding, TxGenericBinding lengthBinding,
                                                std::string* errorMsg) {
    //std::vector<TxTypeBinding> bindings( { TxTypeBinding("E", elemType), TxTypeBinding("L", length) } );
    std::vector<TxGenericBinding> bindings( { elemTypeBinding, lengthBinding } );
    TxTypeSpecialization specialization(this->builtinTypes[ARRAY]->get_type(), bindings);
    return static_cast<const TxArrayType*>(this->get_type_specialization(declaration, specialization));
}

const TxArrayType* TypeRegistry::get_array_type(const TxTypeDeclaration* declaration, TxGenericBinding elemTypeBinding,
                                                std::string* errorMsg) {
    //std::vector<TxTypeBinding> bindings( { TxTypeBinding("E", elemType) } );
    std::vector<TxGenericBinding> bindings( { elemTypeBinding } );
    TxTypeSpecialization specialization(this->builtinTypes[ARRAY]->get_type(), bindings);
    return static_cast<const TxArrayType*>(this->get_type_specialization(declaration, specialization));
}


const TxFunctionType* TypeRegistry::get_function_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes, const TxType* returnType,
                                                      bool mod, std::string* errorMsg) {
    return new TxFunctionType(declaration, this->builtinTypes[FUNCTION]->get_type(), argumentTypes, returnType, mod);
}

const TxFunctionType* TypeRegistry::get_function_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes,
                                                      bool mod, std::string* errorMsg) {
    return new TxFunctionType(declaration, this->builtinTypes[FUNCTION]->get_type(), argumentTypes, nullptr, mod);
}

const TxConstructorType* TypeRegistry::get_constructor_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes,
                                                            TxTypeDeclaration* objectTypeDecl, std::string* errorMsg) {
    return new TxConstructorType(declaration, this->builtinTypes[FUNCTION]->get_type(), argumentTypes, objectTypeDecl);
}

//const TxTupleType* TypeRegistry::get_tuple_type(const TxTypeDeclaration* declaration, bool mut, std::string* errorMsg) {
//    return new TxTupleType(declaration, this->builtinTypes[TUPLE]->get_type(), mut);
//}
//
//const TxTupleType* TypeRegistry::get_tuple_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseType,
//                                                bool mut, std::string* errorMsg) {
//    return static_cast<const TxTupleType*>(this->get_type_specialization(declaration, baseType, errorMsg));
//}
