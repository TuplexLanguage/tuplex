#include "txassert.hpp"
#include "tx_lang_defs.hpp"
#include "type_registry.hpp"
#include "package.hpp"
#include "entity.hpp"


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

class TxBuiltinTypeProxy : public TxTypeProxy {
public:
    const std::string name;
    TxType const * const type;

    TxBuiltinTypeProxy(const std::string name, const TxType* type) : name(name), type(type) { }

    const TxType* get_type() const { return this->type; }
};


/** Used solely for the Any root type object. */
class TxAnyType : public TxType {
    TxAnyType(const TxTypeSpecialization& baseTypeSpec) : TxType(baseTypeSpec)  { }

    // (can only be specialized once: Any -> mod Any)
    TxType* make_specialized_type(const TxTypeSpecialization& baseTypeSpec,
            const std::vector<TxTypeParam>& typeParams,
            std::string* errorMsg=nullptr) const override {
        if (! dynamic_cast<const TxAnyType*>(baseTypeSpec.type))
            throw std::logic_error("Specified a base type for TxAnyType that was not a TxAnyType: " + baseTypeSpec.type->to_string());
        ASSERT(typeParams.empty(), "typeParams not empty");
        ASSERT(baseTypeSpec.modifiable, "not a modifiable specialization");
        return new TxAnyType(baseTypeSpec);
    }

public:
    TxAnyType() : TxType() { }

    long size() const { throw std::logic_error("Can't get size of abstract type " + this->to_string()); }
};

/** Used for the built-in types' base type objects. */
class TxBuiltinBaseType : public TxType {
    // (can only be specialized once -> mod)
    TxType* make_specialized_type(const TxTypeSpecialization& baseTypeSpec,
            const std::vector<TxTypeParam>& typeParams,
            std::string* errorMsg=nullptr) const override {
        if (! dynamic_cast<const TxBuiltinBaseType*>(baseTypeSpec.type))
            throw std::logic_error("Specified a base type for TxBuiltinBaseType that was not a TxBuiltinBaseType: " + baseTypeSpec.type->to_string());
        ASSERT(typeParams.empty(), "typeParams not empty");
        ASSERT(baseTypeSpec.modifiable, "not a modifiable specialization");
        return new TxBuiltinBaseType(baseTypeSpec);
    }
//    TxType* make_specialized_type(const TxTypeSpecialization& baseTypeSpec,
//                                  const std::vector<TxTypeParam>& typeParams) const {
//        throw std::logic_error("Can't specialize type " + this->to_string());
//    }

public:
    TxBuiltinBaseType(const TxTypeSpecialization& baseTypeSpec) : TxType(baseTypeSpec)  { }
    TxBuiltinBaseType(const TxType* baseType) : TxType(TxTypeSpecialization(baseType))  { }

    long size() const { throw std::logic_error("Can't get size of abstract type " + this->to_string()); }
};



class BuiltinTypeRecord : public TxTypeProxy {
public:
    const BuiltinTypeId id;
    const std::string plainName;
    TxType * const type;
    TxTypeEntity * entity;

    BuiltinTypeRecord(BuiltinTypeId id, std::string plainName, TxType* type)
        : id(id), plainName(plainName), type(type), entity()  {
    }

    const TxType* get_type() const { return this->type; }
};


//static const BuiltinTypeRecord BUILTIN_COMPLEX_TYPES[] {
//    { STRING,     "String",    new TxArrayType(BUILTIN_TYPES[CHAR].type) },
//};

//static const TxBuiltinTypeProducer FOREIGN_FUNCS[] {
//    // TO DO: make these members of mutable "io" objects
//    TxBuiltinTypeProducer("printf", new TxFunctionType(std::vector<const TxType*>(foobar)))
//};

void TypeRegistry::add_builtin(TxModule* module, BuiltinTypeRecord* record) {
    this->builtinTypes[record->id] = record;
    record->entity = module->declare_type(record->plainName, record, TXD_PUBLIC);
    record->type->set_entity(record->entity);
}

/** Initializes the built-in symbols. */
void TypeRegistry::initializeBuiltinSymbols() {
    auto module = this->package.declare_module(* new TxIdentifier(BUILTIN_NS));
//    int ix = 0;
//    for (auto & record : BUILTIN_TYPES) {
//        ASSERT(record.id==ix, "Error in built-in type list, id out of order for: " << record.plainName); ix++;
//        builtinModule->declareType(&record, record.plainName, TXD_PUBLIC, false);
//    }
    this->add_builtin( module, new BuiltinTypeRecord( ANY,      "Any",      new TxAnyType() ) );
    this->add_builtin( module, new BuiltinTypeRecord( ELEMENTARY, "Elementary", new TxBuiltinBaseType(this->builtinTypes[ANY]->type) ) );
    this->add_builtin( module, new BuiltinTypeRecord( SCALAR,   "Scalar",   new TxBuiltinBaseType(this->builtinTypes[ELEMENTARY]->type) ) );
    this->add_builtin( module, new BuiltinTypeRecord( INTEGER,  "Integer",  new TxBuiltinBaseType(this->builtinTypes[SCALAR]->type) ) );

    this->add_builtin( module, new BuiltinTypeRecord( SIGNED,   "Signed",   new TxBuiltinBaseType(this->builtinTypes[INTEGER]->type) ) );
    this->add_builtin( module, new BuiltinTypeRecord( BYTE,     "Byte",     new TxIntegerType(this->builtinTypes[SIGNED]->type, 1, true) ) );
    this->add_builtin( module, new BuiltinTypeRecord( SHORT,    "Short",    new TxIntegerType(this->builtinTypes[SIGNED]->type, 2, true) ) );
    this->add_builtin( module, new BuiltinTypeRecord( INT,      "Int",      new TxIntegerType(this->builtinTypes[SIGNED]->type, 4, true) ) );
    this->add_builtin( module, new BuiltinTypeRecord( LONG,     "Long",     new TxIntegerType(this->builtinTypes[SIGNED]->type, 8, true) ) );

    this->add_builtin( module, new BuiltinTypeRecord( UNSIGNED, "Unsigned", new TxBuiltinBaseType(this->builtinTypes[INTEGER]->type) ) );
    this->add_builtin( module, new BuiltinTypeRecord( UBYTE,    "UByte",    new TxIntegerType(this->builtinTypes[UNSIGNED]->type, 1, false) ) );
    this->add_builtin( module, new BuiltinTypeRecord( USHORT,   "UShort",   new TxIntegerType(this->builtinTypes[UNSIGNED]->type, 2, false) ) );
    this->add_builtin( module, new BuiltinTypeRecord( UINT,     "UInt",     new TxIntegerType(this->builtinTypes[UNSIGNED]->type, 4, false) ) );
    this->add_builtin( module, new BuiltinTypeRecord( ULONG,    "ULong",    new TxIntegerType(this->builtinTypes[UNSIGNED]->type, 8, false) ) );
    //this->add_builtin( module, new BuiltinTypeRecord( CHAR,     "Char",    new TxIntegerType(this->builtinTypes[UNSIGNED]->type, 1, false) ) );

    this->add_builtin( module, new BuiltinTypeRecord( FLOATINGPOINT, "FloatingPoint", new TxBuiltinBaseType(this->builtinTypes[SCALAR]->type) ) );
    this->add_builtin( module, new BuiltinTypeRecord( HALF,     "Half",     new TxFloatingType(this->builtinTypes[FLOATINGPOINT]->type, 2) ) );
    this->add_builtin( module, new BuiltinTypeRecord( FLOAT,    "Float",    new TxFloatingType(this->builtinTypes[FLOATINGPOINT]->type, 4) ) );
    this->add_builtin( module, new BuiltinTypeRecord( DOUBLE,   "Double",   new TxFloatingType(this->builtinTypes[FLOATINGPOINT]->type, 8) ) );

    this->add_builtin( module, new BuiltinTypeRecord( BOOLEAN,  "Boolean",  new TxIntegerType(this->builtinTypes[UNSIGNED]->type, 1, false) ) );

    this->add_builtin( module, new BuiltinTypeRecord( REFERENCE,"Ref",      new TxReferenceType(this->builtinTypes[ANY]->type) ) );
    this->add_builtin( module, new BuiltinTypeRecord( ARRAY,    "Array",    new TxArrayType(this->builtinTypes[ANY]->type) ) );
    //this->add_builtin( module, new BuiltinTypeRecord( STRING,   "String",  new TxArrayType(this->builtinTypes[ARRAY]->type) ) );

    this->add_builtin( module, new BuiltinTypeRecord( FUNCTION, "Function", new TxBuiltinBaseType(this->builtinTypes[ANY]->type) ) );
    this->add_builtin( module, new BuiltinTypeRecord( TUPLE,    "Tuple",    new TxBuiltinBaseType(this->builtinTypes[ANY]->type) ) );

    // create modifiable specializations of the concrete built-in types:
    for (int id = 0; id < BuiltinTypeId_COUNT; id++) {
        // verify that all built-in types are initialized:
        ASSERT(this->builtinTypes[id], "Uninitialized built-in type! id=" << id);

        this->builtinModTypes[id] = this->get_type_specialization(TxTypeSpecialization(this->builtinTypes[id]->type, true));
    }

    // test adding static field to types:
    for (int id = 0; id < BuiltinTypeId_COUNT; id++) {
        this->builtinTypes[id]->entity->declare_field("typeid", this->builtinTypes[USHORT],
                (TxDeclarationFlags)(TXD_PUBLIC | TXD_STATIC), TXS_STATIC, TxIdentifier());
    }

    // scalar conversion constructor function types:
    for (auto fromTypeId : SCALAR_TYPE_IDS) {
        for (auto toTypeId : SCALAR_TYPE_IDS) {
            const TxType* constructorType = new TxBuiltinConversionFunctionType(this->builtinTypes[FUNCTION]->type,
                                                                                this->builtinTypes[fromTypeId]->type,
                                                                                this->builtinTypes[toTypeId]->type);
            TxBuiltinTypeProxy* prod = new TxBuiltinTypeProxy(this->builtinTypes[toTypeId]->plainName, constructorType);
            module->declare_field(prod->name, prod, TXD_PUBLIC, TXS_STATIC, TxIdentifier(""));
        }
    }

    // built-in global constants:
    // FUTURE: implement TRUE and FALSE as enumeration values and/or parser tokens
    module->declare_field("FALSE", this->builtinTypes[BOOLEAN]->type, TXD_PUBLIC, TXS_GLOBAL, TxIdentifier(""));
    module->declare_field("TRUE",  this->builtinTypes[BOOLEAN]->type, TXD_PUBLIC, TXS_GLOBAL, TxIdentifier(""));

//    auto charsType = new TxArrayType("tx.Char"); // BUILTIN_TYPES[CHAR].type);
//    TxBuiltinTypeDefiner* charsProd = new TxBuiltinTypeDefiner("CharArray", charsType);
//    auto strRec = BuiltinTypeRecord { STRING, "String", new TxReferenceType("tx.CharArray") };
//    TxBuiltinTypeDefiner* strProd = new TxBuiltinTypeDefiner(strRec);
//    builtinModule->declareType(strProd->name, TXD_PUBLIC, false, *strProd);

    {
        auto txCfuncModule = this->package.declare_module(TxIdentifier(BUILTIN_NS ".c"));

        const TxType* charType = this->get_builtin_type(UBYTE);
        const TxType* cStringType = this->get_reference_type(charType);
        //const TxTypeProxy* cStringTypeDef = new TxBuiltinTypeDefiner("", cStringType);

        std::vector<const TxType*> argumentTypes( { cStringType } );
        const TxType* returnType = this->builtinTypes[INT]->type;
        const TxType* c_puts_func_type = new TxFunctionType(this->builtinTypes[FUNCTION]->type, argumentTypes, returnType);
        auto c_puts_func_type_def = new TxBuiltinTypeProxy("puts", c_puts_func_type);
        txCfuncModule->declare_field("puts", c_puts_func_type_def, TXD_PUBLIC, TXS_GLOBAL, TxIdentifier(""));
    }
}

const TxType* TypeRegistry::get_builtin_type(const BuiltinTypeId id, bool mod) const {
    return mod ? this->builtinModTypes[id] : this->builtinTypes[id]->type;
}


const TxType* TypeRegistry::get_type_specialization(const TxTypeSpecialization& specialization, std::string* errorMsg) {
    // TODO: ensure sequences of pure type specializations are collapsed into single pure specializations
    // TODO: ensure type specialization is never applied to a modifiable-specialization (legal only on generic base type)
    // TODO: ensure modifiable specialization is always a distinct specialization (no parameter bindings (or type extensions))
    std::vector<TxTypeParam> unbound;
    for (auto & p : specialization.type->typeParams) {
        const TxTypeBinding* match = nullptr;
        for (auto & b : specialization.bindings)
            if (p.param_name() == b.param_name()) {
                match = &b;
                break;
            }
        if (! match)
            unbound.push_back(p);
    }
    if (specialization.type->typeParams.size() - unbound.size() != specialization.bindings.size()) {
        for (auto & b : specialization.bindings)  // attempt to give specific error message
            if (! specialization.type->has_type_param(b.param_name()))
                throw std::logic_error("'" + b.param_name() + "' is not an unbound type parameter name of " + specialization.type->to_string());
        if (errorMsg)  errorMsg->append("Invalid type parameter bindings for " + specialization.type->to_string());
        return nullptr;
        //throw std::logic_error("Invalid type parameter bindings for " + specialization.type->to_string());
    }

    // redeclare unbound type parameters:
    std::vector<TxTypeBinding> tmpBindings = specialization.bindings;
    for (auto & p : unbound) {
        tmpBindings.push_back(TxTypeBinding(p.param_name(), p));
//        if (p.meta_type() == TxTypeParam::TXB_TYPE) {
//            TxTypeProxy* typeProxy = make_identified_type_proxy(p.param_name());
//            tmpBindings.push_back(TxTypeBinding(p.param_name(), typeProxy));
//        }
//        else {
//            TxConstantProxy* valueProxy = make_identified_field_proxy(p.param_name());
//            tmpBindings.push_back(TxTypeBinding(p.param_name(), valueProxy));
//        }
    }
    TxTypeSpecialization tmpSpec(specialization.type, tmpBindings, specialization.modifiable);

    try {
        return specialization.type->make_specialized_type(tmpSpec, unbound, errorMsg);
    }
    catch (const std::logic_error& e) {
        if (errorMsg) {
            errorMsg->append(e.what());
            return nullptr;
        }
        else
            throw e;
    }
}


const TxReferenceType* TypeRegistry::get_reference_type(const TxTypeProxy* targetType, bool mod, std::string* errorMsg) {
    std::vector<TxTypeBinding> bindings( { TxTypeBinding("T", targetType) } );
    TxTypeSpecialization specialization(this->builtinTypes[REFERENCE]->type, bindings, mod);
    return static_cast<const TxReferenceType*>(this->get_type_specialization(specialization, errorMsg));
}

const TxArrayType* TypeRegistry::get_array_type(const TxTypeProxy* elemType, const TxConstantProxy* length,
                                                bool mod, std::string* errorMsg) {
    std::vector<TxTypeBinding> bindings( { TxTypeBinding("E", elemType), TxTypeBinding("L", length) } );
    TxTypeSpecialization specialization(this->builtinTypes[ARRAY]->type, bindings, mod);
    return static_cast<const TxArrayType*>(this->get_type_specialization(specialization, errorMsg));
}

const TxArrayType* TypeRegistry::get_array_type(const TxTypeProxy* elemType, bool mod, std::string* errorMsg) {
    std::vector<TxTypeBinding> bindings( { TxTypeBinding("E", elemType) } );
    TxTypeSpecialization specialization(this->builtinTypes[ARRAY]->type, bindings, mod);
    return static_cast<const TxArrayType*>(this->get_type_specialization(specialization, errorMsg));
}

const TxFunctionType* TypeRegistry::get_function_type(const std::vector<const TxType*>& argumentTypes, const TxType* returnType,
                                                      bool mod, std::string* errorMsg) {
    return new TxFunctionType(this->builtinTypes[FUNCTION]->type, argumentTypes, returnType, mod);
}

const TxFunctionType* TypeRegistry::get_function_type(const std::vector<const TxType*>& argumentTypes,
                                                      bool mod, std::string* errorMsg) {
    return new TxFunctionType(this->builtinTypes[FUNCTION]->type, argumentTypes, nullptr, mod);
}

const TxTupleType* TypeRegistry::get_tuple_type(TxTypeEntity* entity, bool mod, std::string* errorMsg) {
    return new TxTupleType(this->builtinTypes[TUPLE]->type, entity, mod);
}
