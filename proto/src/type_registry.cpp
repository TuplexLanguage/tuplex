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

class TxBuiltinTypeProxy final : public TxFieldDefiner {
public:
    const std::string name;
    const TxType* type;

    TxBuiltinTypeProxy(const std::string name) : name(name), type() { }
    TxBuiltinTypeProxy(const std::string name, const TxType* type) : name(name), type(type) { }

    virtual const TxExpressionNode* get_init_expression() const override { return nullptr; }
    virtual const TxType* resolve_type(ResolutionContext& resCtx) override { return this->type; }
    virtual const TxType* attempt_get_type() const override { return this->type; }
    virtual const TxType* get_type() const override { return this->type; }
};


/** Used solely for the Any root type object. */
class TxAnyType final : public TxType {
    TxAnyType(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec) : TxType(entity, baseTypeSpec)  { }

    TxType* make_specialized_type(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                                  const std::vector<TxTypeParam>& typeParams,
                                  std::string* errorMsg=nullptr) const override {
        ASSERT(typeParams.empty(), "can't specify type parameters for " << this);
        if (! dynamic_cast<const TxAnyType*>(baseTypeSpec.type))
            throw std::logic_error("Specified a base type for TxAnyType that was not a TxAnyType: " + baseTypeSpec.type->to_string());
        return new TxAnyType(entity, baseTypeSpec);
    }

public:
    TxAnyType(TxTypeEntity* entity) : TxType(entity) { }

    virtual TxTypeClass get_type_class() const override { return TXTC_ANY; }

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override {
        //ASSERT(false, "Can't contruct LLVM type for abstract type " << this->to_string());
        context.LOG.debug("LLVM type for abstract type %s is VOID", this->to_string().c_str());
        return context.get_voidT();
    }
};

/** Used for the built-in types' abstract base types. */
class TxBuiltinBaseType final : public TxType {
    const TxTypeClass typeClass;

    TxType* make_specialized_type(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                                  const std::vector<TxTypeParam>& typeParams,
                                  std::string* errorMsg=nullptr) const override {
        ASSERT(typeParams.empty(), "can't specify type parameters for " << this);
        if (! dynamic_cast<const TxBuiltinBaseType*>(baseTypeSpec.type))
            throw std::logic_error("Specified a base type for TxBuiltinBaseType that was not a TxBuiltinBaseType: " + baseTypeSpec.type->to_string());
        return new TxBuiltinBaseType(entity, baseTypeSpec.type->get_type_class(), baseTypeSpec);
    }

public:
    TxBuiltinBaseType(TxTypeEntity* entity, TxTypeClass typeClass, const TxTypeSpecialization& baseTypeSpec)
        : TxType(entity, baseTypeSpec), typeClass(typeClass)  { }
    TxBuiltinBaseType(TxTypeEntity* entity, TxTypeClass typeClass, const TxType* baseType)
        : TxType(entity, TxTypeSpecialization(baseType)), typeClass(typeClass)  { }

    virtual TxTypeClass get_type_class() const override { return this->typeClass; }

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override {
        //ASSERT(false, "Can't contruct LLVM type for abstract type " << this->to_string());
        context.LOG.debug("LLVM type for abstract type %s is VOID", this->to_string().c_str());
        return context.get_voidT();
    }
};



class BuiltinTypeRecord : public TxFieldDefiner {
    const TxType * type;
    TxTypeEntity * entity;
public:
    const BuiltinTypeId id;
    const std::string plainName;

    BuiltinTypeRecord(BuiltinTypeId id, std::string plainName)
        : type(), entity(), id(id), plainName(plainName)  {
    }

    TxTypeEntity* get_entity() const { return this->entity; }
    void set_entity(TxTypeEntity* entity) {
        ASSERT(!this->entity, "entity already set");
        this->entity = entity;
    }

    virtual const TxExpressionNode* get_init_expression() const override { return nullptr; }
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
    record->set_entity( module->declare_type(plainName, record, TXD_PUBLIC | TXD_BUILTIN) );
    record->set_type( new TxBuiltinBaseType( record->get_entity(), typeClass, this->builtinTypes[parentId]->get_type() ) );
    this->builtinTypes[record->id] = record;
}

void TypeRegistry::add_builtin_integer(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId,
                                       int size, bool sign) {
    auto record = new BuiltinTypeRecord( id, plainName );
    record->set_entity( module->declare_type(plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
    record->set_type( new TxIntegerType( record->get_entity(), this->builtinTypes[parentId]->get_type(), size, sign) );
    this->builtinTypes[record->id] = record;
}

void TypeRegistry::add_builtin_floating(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId, int size) {
    auto record = new BuiltinTypeRecord( id, plainName );
    record->set_entity( module->declare_type(plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
    record->set_type( new TxFloatingType( record->get_entity(), this->builtinTypes[parentId]->get_type(), size ) );
    this->builtinTypes[record->id] = record;
}

/** Initializes the built-in symbols. */
void TypeRegistry::initializeBuiltinSymbols() {
    auto module = this->package.declare_module(* new TxIdentifier(BUILTIN_NS));

    // create the Any root type:
    {
        auto record = new BuiltinTypeRecord( ANY, "Any" );
        record->set_entity( module->declare_type(record->plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
        record->set_type( new TxAnyType(record->get_entity()) );
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
        record->set_entity( module->declare_type(record->plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
        record->set_type( new TxBoolType(record->get_entity(), this->builtinTypes[ANY]->get_type() ) );
        this->builtinTypes[record->id] = record;
    }

    // create the function base type:
    {
        auto record = new BuiltinTypeRecord( FUNCTION, "Function" );
        record->set_entity( module->declare_type(record->plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
        record->set_type( new TxBuiltinBaseType(record->get_entity(), TXTC_FUNCTION, this->builtinTypes[ANY]->get_type() ) );
        this->builtinTypes[record->id] = record;
    }

    // create the reference base type:
    {
        auto record = new BuiltinTypeRecord( REFERENCE, "Ref" );
        auto typeEntity = module->declare_type(record->plainName, record, TXD_PUBLIC | TXD_BUILTIN );
        record->set_entity( typeEntity );
        record->set_type( new TxReferenceType(record->get_entity(), this->builtinTypes[ANY]->get_type(), this->builtinTypes[ANY] ) );
        this->builtinTypes[record->id] = record;

        // create empty specialization for the constraint type (uniquely named but identical type)
        auto constraintType = this->builtinTypes[ANY]->get_type();
        TxBuiltinTypeProxy* elemTypeDefiner = new TxBuiltinTypeProxy(this->builtinTypes[ANY]->plainName);
        auto elemTypeEnt = typeEntity->declare_type( "T", elemTypeDefiner, TXD_PUBLIC | TXD_GENPARAM );
        elemTypeDefiner->type = constraintType->make_specialized_type(elemTypeEnt, TxTypeSpecialization(constraintType));
    }

    // create the array base type:
    {
        auto record = new BuiltinTypeRecord( ARRAY, "Array" );
        auto typeEntity = module->declare_type( record->plainName, record, TXD_PUBLIC | TXD_BUILTIN );
        record->set_entity( typeEntity );
        record->set_type( new TxArrayType(record->get_entity(), this->builtinTypes[ANY]->get_type(),
                                          this->builtinTypes[ANY], this->builtinTypes[UINT] ) );
        this->builtinTypes[record->id] = record;

        // create empty specialization for the constraint type (uniquely named but identical type)
        auto constraintType = this->builtinTypes[ANY]->get_type();
        TxBuiltinTypeProxy* elemTypeDefiner = new TxBuiltinTypeProxy(this->builtinTypes[ANY]->plainName);
        auto elemTypeEnt = typeEntity->declare_type( "E", elemTypeDefiner, TXD_PUBLIC | TXD_GENPARAM );
        elemTypeDefiner->type = constraintType->make_specialized_type(elemTypeEnt, TxTypeSpecialization(constraintType));

        typeEntity->declare_field( "L", this->builtinTypes[UINT], TXD_PUBLIC | TXD_GENPARAM, TXS_INSTANCE, TxIdentifier() );
    }

    // create the tuple base type:
    {
        auto record = new BuiltinTypeRecord( TUPLE, "Tuple" );
        record->set_entity( module->declare_type(record->plainName, record, TXD_PUBLIC | TXD_BUILTIN ) );
        record->set_type( new TxTupleType(record->get_entity(), this->builtinTypes[ANY]->get_type(), true ) );
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

    // scalar conversion constructor function types:
    for (auto fromTypeId : SCALAR_TYPE_IDS) {
        for (auto toTypeId : SCALAR_TYPE_IDS) {
            TxBuiltinTypeProxy* fieldDefiner = new TxBuiltinTypeProxy(this->builtinTypes[toTypeId]->plainName);
            module->declare_field(fieldDefiner->name, fieldDefiner, TXD_PUBLIC | TXD_BUILTIN, TXS_GLOBAL, TxIdentifier(""));
            fieldDefiner->type = new TxBuiltinConversionFunctionType(nullptr, this->builtinTypes[FUNCTION]->get_type(),
                                                                     this->builtinTypes[fromTypeId]->get_type(),
                                                                     this->builtinTypes[toTypeId]->get_type());
        }
    }

//    // built-in global constants:
//    // FUTURE: implement TRUE and FALSE as enumeration values and/or parser tokens
//    //TxTypeDefiner* BoolTypeDef = new TxBuiltinTypeProxy(this->builtinTypes[BOOLEAN]->get_type());
//    module->declare_field("FALSE", this->builtinTypes[BOOLEAN], TXD_PUBLIC | TXD_BUILTIN, TXS_GLOBAL, TxIdentifier(""));
//    module->declare_field("TRUE",  this->builtinTypes[BOOLEAN], TXD_PUBLIC | TXD_BUILTIN, TXS_GLOBAL, TxIdentifier(""));

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
        auto c_puts_func_type_def = new TxBuiltinTypeProxy("puts");
        txCfuncModule->declare_field("puts", c_puts_func_type_def, TXD_PUBLIC | TXD_BUILTIN, TXS_GLOBAL, TxIdentifier(""));
        const TxType* returnType = this->builtinTypes[INT]->get_type();
        c_puts_func_type_def->type = new TxFunctionType(nullptr, this->builtinTypes[FUNCTION]->get_type(), argumentTypes, returnType);
    }
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


const TxType* TypeRegistry::get_modifiable_type(TxTypeEntity* newEntity, const TxType* type, std::string* errorMsg) {
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
    return type->make_specialized_type(newEntity, tmpSpec, unbound, errorMsg);
}

const TxType* TypeRegistry::get_type_specialization(TxTypeEntity* newEntity, const TxTypeSpecialization& specialization,
                                                    bool _mutable, const std::vector<TxTypeParam>* typeParams,
                                                    std::string* errorMsg) {
    // FUTURE: Should sequences of pure type specializations be collapsed into single pure specializations
    // (except for individual type specializations that are named (have an entity))?
    // Note: type specialization is never applied to a modifiable-specialization (legal only on generic base type)
    ASSERT(!specialization.type->is_modifiable(), "Can't specialize a 'modifiable' base type: " << specialization);
    ASSERT(!specialization.modifiable, "Can't specify 'modifiable' in a type parameter specialization: " << specialization);

    if (specialization.type->entity() && (specialization.type->entity()->get_decl_flags() & TXD_GENPARAM)) {
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
    auto newType = specialization.type->make_specialized_type(newEntity, newSpec, unboundParams, errorMsg);
    this->register_type(newType);
    return newType;
}


const TxReferenceType* TypeRegistry::get_reference_type(TxTypeEntity* newEntity, TxGenericBinding targetTypeBinding,
                                                        const TxIdentifier* dataspace, std::string* errorMsg) {
    std::vector<TxGenericBinding> bindings( { targetTypeBinding } );
    TxTypeSpecialization specialization(this->builtinTypes[REFERENCE]->get_type(), bindings, dataspace);
    return static_cast<const TxReferenceType*>(this->get_type_specialization(newEntity, specialization, false, nullptr, errorMsg));
}

//const TxReferenceType* TypeRegistry::get_reference_type(TxTypeEntity* newEntity, TxEntityDefiner* targetType,
//                                                        std::string* errorMsg) {
//    return this->get_reference_type(newEntity, TxTypeBinding("T", targetType) );
//}


const TxArrayType* TypeRegistry::get_array_type(TxTypeEntity* newEntity,
                                                TxGenericBinding elemTypeBinding, TxGenericBinding lengthBinding,
                                                std::string* errorMsg) {
    //std::vector<TxTypeBinding> bindings( { TxTypeBinding("E", elemType), TxTypeBinding("L", length) } );
    std::vector<TxGenericBinding> bindings( { elemTypeBinding, lengthBinding } );
    TxTypeSpecialization specialization(this->builtinTypes[ARRAY]->get_type(), bindings);
    return static_cast<const TxArrayType*>(this->get_type_specialization(newEntity, specialization, false, nullptr, errorMsg));
}

const TxArrayType* TypeRegistry::get_array_type(TxTypeEntity* newEntity, TxGenericBinding elemTypeBinding,
                                                std::string* errorMsg) {
    //std::vector<TxTypeBinding> bindings( { TxTypeBinding("E", elemType) } );
    std::vector<TxGenericBinding> bindings( { elemTypeBinding } );
    TxTypeSpecialization specialization(this->builtinTypes[ARRAY]->get_type(), bindings);
    return static_cast<const TxArrayType*>(this->get_type_specialization(newEntity, specialization, false, nullptr, errorMsg));
}


const TxFunctionType* TypeRegistry::get_function_type(TxTypeEntity* newEntity, const std::vector<const TxType*>& argumentTypes, const TxType* returnType,
                                                      bool mod, std::string* errorMsg) {
    return new TxFunctionType(newEntity, this->builtinTypes[FUNCTION]->get_type(), argumentTypes, returnType, mod);
}

const TxFunctionType* TypeRegistry::get_function_type(TxTypeEntity* newEntity, const std::vector<const TxType*>& argumentTypes,
                                                      bool mod, std::string* errorMsg) {
    return new TxFunctionType(newEntity, this->builtinTypes[FUNCTION]->get_type(), argumentTypes, nullptr, mod);
}

//const TxTupleType* TypeRegistry::get_tuple_type(TxTypeEntity* newEntity, bool mut, std::string* errorMsg) {
//    return new TxTupleType(newEntity, this->builtinTypes[TUPLE]->get_type(), mut);
//}
//
//const TxTupleType* TypeRegistry::get_tuple_type(TxTypeEntity* newEntity, const TxTypeSpecialization& baseType,
//                                                bool mut, std::string* errorMsg) {
//    return static_cast<const TxTupleType*>(this->get_type_specialization(newEntity, baseType, errorMsg));
//}
