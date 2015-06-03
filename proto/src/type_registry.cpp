#include "txassert.hpp"
#include "tx_lang_defs.hpp"
#include "type_registry.hpp"
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
    //const std::string name;
    const TxType* type;

    //TxBuiltinTypeDefiner(const std::string name) : name(name), type() { }
    TxBuiltinTypeDefiner() : type() { }
    TxBuiltinTypeDefiner(const TxType* type) : type(type) { }

    virtual const TxType* resolve_type(ResolutionContext& resCtx) override { return this->type; }
    virtual const TxType* attempt_get_type() const override { return this->type; }
    virtual const TxType* get_type() const override { return this->type; }
};

class TxBuiltinFieldDefiner final : public TxFieldDefiner {
public:
    const TxField* field;

    TxBuiltinFieldDefiner() : field() { }
    TxBuiltinFieldDefiner(const TxField* field) : field(field) { }

    virtual const TxExpressionNode* get_init_expression() const override { return nullptr; }
    virtual const TxField* resolve_field(ResolutionContext& resCtx) override { return this->field; }
    virtual const TxField* get_field() const override { return this->field; }
    virtual const TxType* get_type() const override { return this->field->get_type(); }
    virtual const TxType* attempt_get_type() const override { return this->field->get_type(); }
};


/** Used solely for the Any root type object. */
class TxAnyType final : public TxType {
    TxAnyType(TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec)
            : TxType(TXTC_ANY, declaration, baseTypeSpec)  { }

    TxType* make_specialized_type(TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                  const std::vector<TxTypeParam>& typeParams,
                                  std::string* errorMsg=nullptr) const override {
        ASSERT(typeParams.empty(), "can't specify type parameters for " << this);
        if (! dynamic_cast<const TxAnyType*>(baseTypeSpec.type))
            throw std::logic_error("Specified a base type for TxAnyType that was not a TxAnyType: " + baseTypeSpec.type->to_string());
        return new TxAnyType(declaration, baseTypeSpec);
    }

public:
    TxAnyType(TxTypeDeclaration* declaration) : TxType(TXTC_ANY, declaration) { }

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override {
        //ASSERT(false, "Can't contruct LLVM type for abstract type " << this->to_string());
        context.LOG.debug("LLVM type for abstract type %s is VOID", this->to_string().c_str());
        return context.get_voidT();
    }
};

/** Used for the built-in types' abstract base types. */
class TxBuiltinBaseType final : public TxType {
    TxType* make_specialized_type(TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                  const std::vector<TxTypeParam>& typeParams,
                                  std::string* errorMsg=nullptr) const override {
        ASSERT(typeParams.empty(), "can't specify type parameters for " << this);
        if (! dynamic_cast<const TxBuiltinBaseType*>(baseTypeSpec.type))
            throw std::logic_error("Specified a base type for TxBuiltinBaseType that was not a TxBuiltinBaseType: " + baseTypeSpec.type->to_string());
        return new TxBuiltinBaseType(baseTypeSpec.type->get_type_class(), declaration, baseTypeSpec);
    }

public:
    TxBuiltinBaseType(TxTypeClass typeClass, TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec)
        : TxType(typeClass, declaration, baseTypeSpec)  { }
    TxBuiltinBaseType(TxTypeClass typeClass, TxTypeDeclaration* declaration, const TxType* baseType)
        : TxType(typeClass, declaration, TxTypeSpecialization(baseType))  { }

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override {
        //ASSERT(false, "Can't contruct LLVM type for abstract type " << this->to_string());
        context.LOG.debug("LLVM type for abstract type %s is VOID", this->to_string().c_str());
        return context.get_voidT();
    }
};



class BuiltinTypeRecord : public TxTypeDefiner {
    const TxType * type;
    TxTypeDeclaration* declaration;
public:
    const BuiltinTypeId id;
    const std::string plainName;

    BuiltinTypeRecord(BuiltinTypeId id, std::string plainName)
        : type(), declaration(), id(id), plainName(plainName)  {
    }

    TxTypeDeclaration* get_declaration() const { return this->declaration; }
    void set_declaration(TxTypeDeclaration* declaration) {
        ASSERT(!this->declaration, "declaration already set");
        this->declaration = declaration;
    }

    //virtual const TxExpressionNode* get_init_expression() const override { return nullptr; }
    virtual const TxType* resolve_type(ResolutionContext& resCtx) override { return this->type; }
    virtual const TxType* attempt_get_type() const override { return this->type; }
    virtual const TxType* get_type() const override { return this->type; }
    void set_type(const TxType* type) {
        ASSERT(!this->type, "type already set");
        this->type = type;
    }
};


//static const BuiltinTypeRecord BUILTIN_COMPLEX_TYPES[] {
//    { STRING,     "String",    new TxArrayType(BUILTIN_TYPES[CHAR].type) },
//};

//static const TxBuiltinTypeProducer FOREIGN_FUNCS[] {
//    // TO DO: make these members of mutable "io" objects
//    TxBuiltinTypeProducer("printf", new TxFunctionType(std::vector<const TxType*>(foobar)))
//};


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

        auto charBinding = TxGenericBinding::make_type_binding("T", this->builtinTypes[UBYTE]);
        const TxType* cStringType = this->get_reference_type(nullptr, charBinding);
        //const TxTypeProxy* cStringTypeDef = new TxBuiltinTypeDefiner("", cStringType);

        std::vector<const TxType*> argumentTypes( { cStringType } );
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
    if (type->is_pure_specialization())
        return;
    type->typeId = this->allStaticTypes.size();
    this->allStaticTypes.push_back(type);
}



const TxType* TypeRegistry::get_builtin_type(const BuiltinTypeId id, bool mod) const {
    return mod ? this->builtinModTypes[id] : this->builtinTypes[id]->get_type();
}


const TxType* TypeRegistry::get_modifiable_type(TxTypeDeclaration* declaration, const TxType* type, std::string* errorMsg) {
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

const TxType* TypeRegistry::get_type_specialization(TxTypeDeclaration* declaration, const TxTypeSpecialization& specialization,
                                                    bool _mutable, const std::vector<TxTypeParam>* typeParams,
                                                    std::string* errorMsg) {
    // FUTURE: Should sequences of pure type specializations be collapsed into single pure specializations
    // (except for individual type specializations that are named (have an entity))?
    // Note: type specialization is never applied to a modifiable-specialization (legal only on generic base type)
    ASSERT(!specialization.type->is_modifiable(), "Can't specialize a 'modifiable' base type: " << specialization);
    ASSERT(!specialization.modifiable, "Can't specify 'modifiable' in a type parameter specialization: " << specialization);

    if (specialization.type->get_symbol() && (specialization.type->get_declaration()->get_decl_flags() & TXD_GENPARAM)) {
        if (errorMsg)
            errorMsg->append("Can't derive from generic type parameter " + specialization.type->to_string());
        return nullptr;
    }

    std::vector<TxTypeParam> unboundParams;
    if (typeParams)
        unboundParams = *typeParams;
    std::vector<TxGenericBinding> newBindings = specialization.bindings;
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
                for (auto & newParam : *typeParams)
                    if (baseTypeParam.param_name() == newParam.param_name() && baseTypeParam.meta_type() == newParam.meta_type()) {
                        matchedNewParam = &newParam;
                        break;
                    }
                if (matchedNewParam) {
                    // effectively redeclared
                    //newBindings.push_back(TxTypeBinding(baseTypeParam.param_name(), *matchedNewParam));
                }
                else {
                    if (errorMsg)
                        errorMsg->append("Type parameter " + baseTypeParam.to_string()
                                         + " neither bound or redeclared in specialization of " + specialization.type->to_string());
                    return nullptr;
                }
            }
            else {
                // automatically redeclare unbound type parameters:
                unboundParams.push_back(baseTypeParam);
                //newBindings.push_back(TxTypeBinding(baseTypeParam.param_name(), baseTypeParam));
            }
        }
    }

    TxTypeSpecialization newSpec(specialization.type, newBindings, specialization.dataspace);
    auto newType = specialization.type->make_specialized_type(declaration, newSpec, unboundParams, errorMsg);
    this->register_type(newType);
    return newType;
}


const TxReferenceType* TypeRegistry::get_reference_type(TxTypeDeclaration* declaration, TxGenericBinding targetTypeBinding,
                                                        const TxIdentifier* dataspace, std::string* errorMsg) {
    std::vector<TxGenericBinding> bindings( { targetTypeBinding } );
    TxTypeSpecialization specialization(this->builtinTypes[REFERENCE]->get_type(), bindings, dataspace);
    return static_cast<const TxReferenceType*>(this->get_type_specialization(declaration, specialization, false, nullptr, errorMsg));
}

//const TxReferenceType* TypeRegistry::get_reference_type(TxTypeDeclaration* declaration, TxEntityDefiner* targetType,
//                                                        std::string* errorMsg) {
//    return this->get_reference_type(declaration, TxTypeBinding("T", targetType) );
//}


const TxArrayType* TypeRegistry::get_array_type(TxTypeDeclaration* declaration,
                                                TxGenericBinding elemTypeBinding, TxGenericBinding lengthBinding,
                                                std::string* errorMsg) {
    //std::vector<TxTypeBinding> bindings( { TxTypeBinding("E", elemType), TxTypeBinding("L", length) } );
    std::vector<TxGenericBinding> bindings( { elemTypeBinding, lengthBinding } );
    TxTypeSpecialization specialization(this->builtinTypes[ARRAY]->get_type(), bindings);
    return static_cast<const TxArrayType*>(this->get_type_specialization(declaration, specialization, false, nullptr, errorMsg));
}

const TxArrayType* TypeRegistry::get_array_type(TxTypeDeclaration* declaration, TxGenericBinding elemTypeBinding,
                                                std::string* errorMsg) {
    //std::vector<TxTypeBinding> bindings( { TxTypeBinding("E", elemType) } );
    std::vector<TxGenericBinding> bindings( { elemTypeBinding } );
    TxTypeSpecialization specialization(this->builtinTypes[ARRAY]->get_type(), bindings);
    return static_cast<const TxArrayType*>(this->get_type_specialization(declaration, specialization, false, nullptr, errorMsg));
}


const TxFunctionType* TypeRegistry::get_function_type(TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes, const TxType* returnType,
                                                      bool mod, std::string* errorMsg) {
    return new TxFunctionType(declaration, this->builtinTypes[FUNCTION]->get_type(), argumentTypes, returnType, mod);
}

const TxFunctionType* TypeRegistry::get_function_type(TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes,
                                                      bool mod, std::string* errorMsg) {
    return new TxFunctionType(declaration, this->builtinTypes[FUNCTION]->get_type(), argumentTypes, nullptr, mod);
}

const TxConstructorType* TypeRegistry::get_constructor_type(TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes,
                                                            TxTypeDeclaration* objectTypeDecl, std::string* errorMsg) {
    return new TxConstructorType(declaration, this->builtinTypes[FUNCTION]->get_type(), argumentTypes, objectTypeDecl);
}

//const TxTupleType* TypeRegistry::get_tuple_type(TxTypeDeclaration* declaration, bool mut, std::string* errorMsg) {
//    return new TxTupleType(declaration, this->builtinTypes[TUPLE]->get_type(), mut);
//}
//
//const TxTupleType* TypeRegistry::get_tuple_type(TxTypeDeclaration* declaration, const TxTypeSpecialization& baseType,
//                                                bool mut, std::string* errorMsg) {
//    return static_cast<const TxTupleType*>(this->get_type_specialization(declaration, baseType, errorMsg));
//}
