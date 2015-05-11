#include "txassert.hpp"

#include "tx_lang_defs.hpp"
#include "scope.hpp"
#include "package.hpp"
#include "entity.hpp"


/*=== TxScope implementation ===*/

/*--- lexical scope tracking ---*/

TxSymbolScope::TxSymbolScope(TxSymbolScope* parent, const std::string& name)
        : LOG(Logger::get("SYMBOLTABLE")), name(name), outer(parent)  {
    if (parent) {
        ASSERT(!name.empty() && name.find_first_of('.') == std::string::npos, "Non-plain name specified for non-root scope: '" << name << "'");
        this->fullName = TxIdentifier(this->outer->get_full_name(), this->name);
    }
    else {
        ASSERT(name.empty(), "Non-empty name specified for parent-less root scope: " << name);
        this->fullName = TxIdentifier();
    }
}


TxSymbolScope* TxSymbolScope::get_root_scope() {
    return const_cast<TxSymbolScope*>(static_cast<const TxSymbolScope *>(this)->get_root_scope());
}
const TxSymbolScope* TxSymbolScope::get_root_scope() const {
    if (! this->has_outer())
        return dynamic_cast<const TxSymbolScope*>(this);
    return this->get_outer()->get_root_scope();
}



std::string TxSymbolScope::get_unique_name(const std::string& baseName) const {
    // ensures name is unique under parent scope
    // (don't append numeral if plain name provided and first occurrence)
    int counter = 0;
    while (true) {
        std::string uniqueName = (baseName.length() > 1 && counter == 0) ? baseName : baseName + std::to_string(counter);
        if (! this->has_symbol(uniqueName))
            return uniqueName;
        counter++;
    }
}


TxSymbolScope* TxSymbolScope::create_code_block_scope(const std::string& plainName) {
    std::string baseName = plainName + '$';
    std::string uniqueName = this->get_unique_name(baseName);
    TxSymbolScope* scope = new TxSymbolScope(this, uniqueName);
    bool success = this->declare_symbol(scope);
    ASSERT(success, "failed to insert duplicate subscope name '" << baseName << "." << uniqueName << "'");
    this->LOGGER().trace("-->            %s", scope->get_full_name().to_string().c_str());
    return scope;
}


/*--- symbol map implementation ---*/

TxSymbolScope* TxSymbolScope::add_symbol(TxSymbolScope* symbol) {
    ASSERT(symbol->outer==this, "Mismatching symbol parent reference! " << symbol);
    ASSERT((this->outer==NULL && symbol->get_full_name().is_plain()) || symbol->get_full_name().parent()==this->get_full_name(),
            "Symbol qualifier doesn't match parent scope! " << symbol);
    auto result = this->symbols.emplace(symbol->get_name(), symbol);
    if (! result.second) {
        // symbol has previously been declared, replace it and return original symbol
        auto prev = result.first->second;
        result.first->second = symbol;
        return prev;
    }
    else {
        this->symbolNames.push_back(symbol->get_name());
        return nullptr;
    }
}

bool TxSymbolScope::has_symbol(const std::string& name) const {
    return this->symbols.count(name);
}

const TxSymbolScope* TxSymbolScope::get_symbol(const std::string& name) const {
    return this->symbols.count(name) ? this->symbols.at(name) : nullptr;
}

//std::vector<const TxIdentifier*> TxSymbolScope::get_symbol_full_names() const {
//    std::vector<const TxIdentifier*> symNames;
//    for(auto & sym : this->symbols)
//        symNames.push_back(&sym.second->get_full_name());
//    return symNames;
//}


/*--- symbol table handling ---*/

static inline bool is_stack_field(const TxSymbolScope* entity) {
    if (auto fieldEnt = dynamic_cast<const TxFieldEntity*>(entity))
        if (fieldEnt->get_storage() == TXS_STACK)
            return true;
    return false;
}

bool TxSymbolScope::declare_symbol(TxSymbolScope* symbol) {
    if (this->has_symbol(symbol->get_name()))
        return false;
    this->add_symbol(symbol);
    return true;
}


TxDistinctEntity* TxSymbolScope::overload_entity(TxDistinctEntity* entity, TxSymbolScope* prevSymbol) {
    TxDistinctEntity* specificEntity = nullptr;
    // Note: We don't guard against all illegal collisions here (since type-dependent and types not yet known).
    if (is_stack_field(entity)) {
        // this->LOGGER().error("Can't overload local fields: %s", entity->to_string().c_str());
    }
    else if (auto firstEntity = dynamic_cast<TxDistinctEntity*>(prevSymbol)) {
        // second entity with same qualified name, symbol is hereby overloaded
        if (is_stack_field(firstEntity))
            this->LOGGER().error("Can't overload local fields: %s", entity->to_string().c_str());
        auto groupEntity = new TxOverloadedEntity(firstEntity);
        std::string uniqueName(entity->get_name() + "$1");
        specificEntity = entity->make_copy(uniqueName);
        if (groupEntity->add(specificEntity))
            this->add_symbol(groupEntity);
        else {
            // overload failed
            delete groupEntity; delete specificEntity;
            specificEntity = nullptr;
        }
    }
    else if (auto groupEntity = dynamic_cast<TxOverloadedEntity*>(prevSymbol)) {
        // symbol already overloaded, add this entity
        std::string uniqueName(groupEntity->get_name() + "$" + std::to_string(groupEntity->count()));
        specificEntity = entity->make_copy(uniqueName);
        if (! groupEntity->add(specificEntity)) {
            // overload failed
            delete specificEntity;
            specificEntity = nullptr;
        }
    }

    if (specificEntity) {
        // also register overloaded entity in symbol table under unique name:
        auto success = this->declare_symbol(specificEntity);
        ASSERT(success, "Internal error, assigned internal name for overloaded entity is not unique: " << specificEntity->get_full_name());
        if (! entity->get_full_name().begins_with(BUILTIN_NS))
            this->LOGGER().info("    Overloaded %-32s %s", entity->get_full_name().to_string().c_str(), specificEntity->to_string().c_str());
        return specificEntity;
    }
    else {
        this->LOGGER().error("Symbol name already exists and can't be overloaded: %s", entity->to_string().c_str());
        return nullptr;
    }
}

TxDistinctEntity* TxSymbolScope::declare_entity(TxDistinctEntity* entity) {
    // TODO: guard against using reserved keywords (including "tx")

// TODO: disabled to prevent error conditions when symbol pass is only partially completed;
//        move this check to symbol table verification pass
//    {   // shadowing a name from outer scope or parent type shall produce a warning:
//        std::vector<const TxSymbolScope*> path;
//        auto shadowed = this->lookup_symbol(path, entity->get_name());
//        if (shadowed && shadowed->get_full_name() != entity->get_full_name()) {
//                //&& !(is_stack_field(entity) && is_stack_field(shadowed))) {  // (skip warn when stack var shadows another stack var)
//            if (! this->get_full_name().begins_with(TxIdentifier(BUILTIN_NS)))
//                this->LOGGER().warning("%-40s (in %-40s) shadows %s", entity->to_string().c_str(), this->to_string().c_str(), shadowed->to_string().c_str());
//        }
//    }

    if (auto prevSymbol = this->get_symbol(entity->get_name())) {
        // symbol has previously been declared, handle overloading
        return overload_entity(entity, prevSymbol);
    }
    else {
        auto success = this->declare_symbol(entity);
        if (success) {
            // register possible main() function:
            if (auto field = dynamic_cast<TxFieldEntity*>(entity)) {
                if (entity->get_name() == "main") {
                    // TODO: check that public and static function of correct signature: static mod main(args) Int
                    auto package = dynamic_cast<TxPackage*>(this->get_root_scope());
                    ASSERT(package, "root scope is not a TxPackage");
                    package->registerMainFunc(field);
                }
            }
            this->LOGGER().trace("    Defined    %-32s %s", entity->get_full_name().to_string().c_str(), entity->to_string().c_str());
            return entity;
        }
        else
            this->LOGGER().error("Failed to define symbol %s", entity->to_string().c_str());
    }
    delete entity;
    return nullptr;
}

TxTypeEntity* TxSymbolScope::declare_type(const std::string& plainName, TxTypeDefiner* entityDefiner,
                                          TxDeclarationFlags declFlags) {
    auto entity = new TxTypeEntity(this, plainName, entityDefiner, declFlags);
    return dynamic_cast<TxTypeEntity*>(this->declare_entity(entity));
}

TxFieldEntity* TxSymbolScope::declare_field(const std::string& plainName, TxFieldDefiner* entityDefiner,
                                            TxDeclarationFlags declFlags, TxFieldStorage storage,
                                            const TxIdentifier& dataspace) {
    auto entity = new TxFieldEntity(this, plainName, entityDefiner, declFlags, storage, dataspace);
    return dynamic_cast<TxFieldEntity*>(this->declare_entity(entity));
}

TxAliasEntity* TxSymbolScope::declare_alias(const std::string& plainName, TxDeclarationFlags declFlags, TxDistinctEntity* aliasedEntity) {
    auto entity = new TxAliasEntity(this, plainName, declFlags, aliasedEntity);
    // TODO: guard against using reserved keywords (including "tx")
    auto success = this->declare_symbol(entity);
    if (success) {
        this->LOGGER().trace("    Defined    %-32s %s", entity->get_full_name().to_string().c_str(), entity->to_string().c_str());
        return entity;
    }
    this->LOGGER().error("Failed to define symbol %s", entity->to_string().c_str());
    delete entity;
    return nullptr;
}



/*--- symbol table lookup ---*/

TxSymbolScope* TxSymbolScope::start_lookup_symbol(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) {
    ASSERT(path.empty(), "Non-empty symbol path vector provided to resolve_symbol() of " << this);
    path.push_back(this);  // starting point of search - the first segment's so-called vantage scope
    auto symbol = this->lookup_symbol(path, ident);
    // TODO: implement visibility check
    return symbol;
}


TxSymbolScope* TxSymbolScope::lookup_symbol(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) {
    if (auto symbol = this->lookup_member(path, ident)) {  // FIXME: use a path copy, in case null is returned
//        std::cout << ident << " => ";
//        for (auto s : path)  std::cout << s->get_full_name() << " . ";
//        std::cout << std::endl;
        ASSERT(ident.segment_count()==path.size()-1, "Erroneous lookup path length: ident " << ident << " length != " << path.size());
        ASSERT(symbol == path.back(), "Returned entity != last entity in path: " << *symbol << " != " << *path.back());
        return symbol;
    }
    else if (this->has_outer())
        return this->get_outer()->lookup_symbol(path, ident);
    return nullptr;
}

TxSymbolScope* TxSymbolScope::lookup_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) {
    auto memberName = ident.segment(0);
    //std::cout << "Looking up member " << memberName << " in " << this << std::endl;
    if (auto member = this->get_symbol(memberName)) {
        path.push_back(member);
        if (ident.is_plain())
            return member;
        else
            return member->lookup_member(path, TxIdentifier(ident, 1));
    }
    return nullptr;
}



TxTypeEntity* TxSymbolScope::lookup_type(ResolutionContext& resCtx, std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) {
    auto symbol = this->start_lookup_symbol(path, ident);
    if (! symbol)
        return nullptr;
    else if (auto typeEnt = dynamic_cast<TxTypeEntity*>(symbol))
        return typeEnt;
    else if (auto overloaded = dynamic_cast<TxOverloadedEntity*>(symbol))
        if (auto typeEnt = overloaded->get_type_declaration())
            return typeEnt;
    //std::string msg = "Symbol " + ident.to_string() + " referenced from " + this->to_string() + " is not a Type: " + symbol->to_string();
    //this->LOGGER().error("%s", msg.c_str());
    return nullptr;
}

TxFieldEntity* TxSymbolScope::lookup_field(ResolutionContext& resCtx, std::vector<TxSymbolScope*>& path, const TxIdentifier& ident,
                                           const std::vector<const TxType*>* typeParameters) {
    TxSymbolScope* symbol = this->start_lookup_symbol(path, ident);
    if (! symbol)
        return nullptr;
    TxFieldEntity* field = this->resolve_field_lookup(resCtx, symbol, typeParameters);
    if (field && path.back() != field)
        path[path.size()-1] = field;
    return field;
}

TxFieldEntity* TxSymbolScope::resolve_field_lookup(ResolutionContext& resCtx, TxSymbolScope* symbol,
                                                   const std::vector<const TxType*>* typeParameters) {
    if (auto fieldEnt = dynamic_cast<TxFieldEntity*>(symbol)) {
        // if (typeParameters)  TODO: if type parameters specified, verify that they match
        return fieldEnt;
    }
    else if (auto overloadedEnt = dynamic_cast<const TxOverloadedEntity*>(symbol)) {
        if (typeParameters) {
            std::vector<TxFieldEntity*> matches;
            for (auto fieldCandidateI = overloadedEnt->fields_cbegin();
                      fieldCandidateI != overloadedEnt->fields_cend(); fieldCandidateI++) {
                auto fieldCandidateType = (*fieldCandidateI)->resolve_symbol_type(resCtx);
                if (auto candidateFuncType = dynamic_cast<const TxFunctionType*>(fieldCandidateType)) {
                    this->LOGGER().trace("Candidate function: %s", candidateFuncType->to_string().c_str());
                    if (candidateFuncType->argumentTypes.size() == typeParameters->size()) {
                        auto typeParamI = typeParameters->cbegin();
                        for (auto argDef : candidateFuncType->argumentTypes) {
                            if (! argDef->auto_converts_from(**typeParamI)) {
                                this->LOGGER().trace("Argument mismatch: %s  can't convert to  %s", (*typeParamI)->to_string(true).c_str(), argDef->to_string(true).c_str());
                                goto NEXT_CANDIDATE;
                            }
                            typeParamI++;
                        }
                        matches.push_back(*fieldCandidateI);
                    }
                }
                //else
                //    std::cerr << "Callee of function call expression is not a function type: " << fieldCandidateType << std::endl;
                NEXT_CANDIDATE:
                ;
            }
            if (! matches.empty()) {
                // TODO: get best match instead of first match
                return matches.front();
            }
            this->LOGGER().warning("Type parameters do not match any candidate of %s", symbol->to_string().c_str());
            return nullptr;
        }
        else if (overloadedEnt->field_count() == 1) {
            return *overloadedEnt->fields_cbegin();
        }
        else {
            this->LOGGER().warning("%s must be matched using type parameters", symbol->to_string().c_str());
            return nullptr;
        }
    }
    //this->LOGGER().warning("Symbol %s referenced from %s is not a field: %s",
    //                       ident.to_string().c_str(), this->to_string().c_str(), symbol->to_string().c_str());
    return nullptr;
}


bool TxSymbolScope::symbol_validation_pass(ResolutionContext& resCtx) {
//    if (!this->fullName.begins_with(BUILTIN_NS))
//        this->LOGGER().debug("Validating symbol %s", this->fullName.to_string().c_str());
    bool valid = this->validate_symbol(resCtx);
    if (! valid)
        this->LOGGER().debug("Failed symbol validity test: %s", this->fullName.to_string().c_str());
    for (auto entry : this->symbols)
        valid &= entry.second->symbol_validation_pass(resCtx);
    return valid;
}

bool TxSymbolScope::validate_symbol(ResolutionContext& resCtx) {
    return true;
}

void TxSymbolScope::dump_symbols() const {
    const TxIdentifier builtinNamespace(BUILTIN_NS);
    std::vector<const TxModule*> subModules;
    for (auto & symName : this->symbolNames) {
        auto symbol = this->symbols.at(symName);
        if (auto submod = dynamic_cast<const TxModule*>(symbol))
            subModules.push_back(submod);
        else if (true) {//this->get_full_name() != builtinNamespace) {
            try {
                if (auto ent = dynamic_cast<const TxFieldEntity*>(symbol)) {
                    std::string typestr; // = (type && type->entity()) ? type->entity()->get_full_name().to_string() : "nulltype/Void";
                    if (const TxType* type = ent->get_type())
                        if (type->entity())
                            typestr = type->entity()->get_full_name().to_string();
                        else
                            typestr = type->to_string();
                    else
                        typestr = "nulltype/Void";
                    printf("%-64s %s\n", symbol->to_string().c_str(), typestr.c_str());
                }
                else if (auto ent = dynamic_cast<const TxTypeEntity*>(symbol)) {
                    const TxType* type = ent->get_type();
                    std::string typestr = type ? type->to_string() : "nulltype/Void";
                    printf("%-64s %s\n", symbol->to_string().c_str(), typestr.c_str());
                }
                else
                    printf("%s\n", symbol->to_string().c_str());
//                else if (dynamic_cast<const TxOverloadedEntity*>(symbol))
//                    printf("<overloaded>     %s\n", symbol->get_full_name().to_string().c_str());
//                else
//                    printf("<scope>          %s\n", symbol->to_string().c_str());
            }
            catch (std::logic_error& e) {
                printf(">>> Caught logic_error while printing symbol '%s': %s\n", this->get_full_name().to_string().c_str(), e.what());
            }
            symbol->dump_symbols();
        }
    }
    if (! subModules.empty()) {
        //printf("--- submodules ---\n");
        for (auto mod : subModules)
            mod->dump_symbols();
            //printf("<module>       %s\n", mod->to_string().c_str());
    }
}
