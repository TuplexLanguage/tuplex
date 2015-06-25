#include "tx_lang_defs.hpp"
#include "assert.hpp"
#include "package.hpp"
#include "entity.hpp"
#include "declaration.hpp"
#include "symbol.hpp"


/*=== TxScopeSymbol implementation ===*/

Logger& TxScopeSymbol::LOG = Logger::get("SYMBOL");

/*--- lexical scope tracking ---*/

TxScopeSymbol::TxScopeSymbol(TxScopeSymbol* parent, const std::string& name)
        : name(name), outer(parent)  {
    if (parent) {
        ASSERT(!name.empty() && name.find_first_of('.') == std::string::npos, "Non-plain name specified for non-root scope: '" << name << "'");
        this->fullName = TxIdentifier(this->outer->get_full_name(), this->name);
        this->root = parent->get_root_scope();
    }
    else {
        ASSERT(name.empty(), "Non-empty name specified for parent-less root scope: " << name);
        this->fullName = TxIdentifier();
        this->root = (TxPackage*)this;
    }
}


std::string TxScopeSymbol::make_unique_name(const std::string& baseName, bool suppressZeroSuffix) const {
    int counter = 0;
    while (true) {
        std::string uniqueName = (counter == 0 && suppressZeroSuffix && !baseName.empty()) ? baseName : baseName + std::to_string(counter);
        if (! this->has_symbol(uniqueName))
            return uniqueName;
        counter++;
    }
}


TxScopeSymbol* TxScopeSymbol::create_code_block_scope(const std::string& plainName) {
    std::string baseName = plainName + '$';
    std::string uniqueName = this->make_unique_name(baseName);
    TxScopeSymbol* scope = new TxScopeSymbol(this, uniqueName);
    bool success = this->declare_symbol(scope);
    ASSERT(success, "failed to insert duplicate subscope name '" << baseName << "." << uniqueName << "'");
    this->LOGGER().trace("-->            %s", scope->get_full_name().to_string().c_str());
    return scope;
}


/*--- symbol map implementation ---*/

TxScopeSymbol* TxScopeSymbol::add_symbol(TxScopeSymbol* symbol) {
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

bool TxScopeSymbol::has_symbol(const std::string& name) const {
    return this->symbols.count(name);
}

const TxScopeSymbol* TxScopeSymbol::get_symbol(const std::string& name) const {
    return this->symbols.count(name) ? this->symbols.at(name) : nullptr;
}

//std::vector<const TxIdentifier*> TxScopeSymbol::get_symbol_full_names() const {
//    std::vector<const TxIdentifier*> symNames;
//    for(auto & sym : this->symbols)
//        symNames.push_back(&sym.second->get_full_name());
//    return symNames;
//}


/*--- symbol table handling ---*/

//static inline bool is_stack_field(const TxScopeSymbol* entity) {
//    if (auto fieldEnt = dynamic_cast<const TxFieldEntity*>(entity))
//        if (fieldEnt->get_storage() == TXS_STACK)
//            return true;
//    return false;
//}

bool TxScopeSymbol::declare_symbol(TxScopeSymbol* symbol) {
    if (this->has_symbol(symbol->get_name()))
        return false;
    this->add_symbol(symbol);
    return true;
}

/*
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
            this->LOGGER().debug("    Overloaded %-32s %s", entity->get_full_name().to_string().c_str(), specificEntity->to_string().c_str());
        return specificEntity;
    }
    else {
        this->LOGGER().error("Symbol name already exists and can't be overloaded: %s", entity->to_string().c_str());
        return nullptr;
    }
}
*/

TxEntitySymbol* TxScopeSymbol::declare_entity(const std::string& plainName, TxEntityDefiner* definer) {
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

    TxEntitySymbol* entitySymbol;
    if (auto symbol = this->get_symbol(plainName)) {
        entitySymbol = dynamic_cast<TxEntitySymbol*>(symbol);
        if (! entitySymbol) {
            CERROR(definer, "Failed to declare entity symbol, can't overload entities and non-entities under same symbol: " << symbol);
            return nullptr;
        }
    }
    else {
        entitySymbol = new TxEntitySymbol(this, plainName);
        this->declare_symbol(entitySymbol);
        this->LOGGER().trace("    Declared   %-32s %s", entitySymbol->get_full_name().to_string().c_str(), entitySymbol->to_string().c_str());

        // register possible main() function:
        if (plainName == "main") {
            // TODO: check that public and static function of correct signature: static mod main(args) Int
            auto package = dynamic_cast<TxPackage*>(this->get_root_scope());
            ASSERT(package, "root scope is not a TxPackage");
            package->registerMainFunc(entitySymbol);
        }
    }
    return entitySymbol;
}

TxTypeDeclaration* TxScopeSymbol::declare_type(const std::string& plainName, TxTypeDefiner* typeDefiner,
                                               TxDeclarationFlags declFlags) {
    if (TxEntitySymbol* entitySymbol = this->declare_entity(plainName, typeDefiner)) {
        auto typeDeclaration = new TxTypeDeclaration(entitySymbol, declFlags, typeDefiner);
        if (entitySymbol->add_type(typeDeclaration))
            return typeDeclaration;
    }
    return nullptr;
}

TxFieldDeclaration* TxScopeSymbol::declare_field(const std::string& plainName, TxFieldDefiner* fieldDefiner,
                                                 TxDeclarationFlags declFlags, TxFieldStorage storage,
                                                 const TxIdentifier& dataspace) {
    if (TxEntitySymbol* entitySymbol = this->declare_entity(plainName, fieldDefiner)) {
        auto fieldDeclaration = new TxFieldDeclaration(entitySymbol, declFlags, fieldDefiner, storage, dataspace);
        if (entitySymbol->add_field(fieldDeclaration))
            return fieldDeclaration;
    }
    return nullptr;
}

TxAliasSymbol* TxScopeSymbol::declare_alias(const std::string& plainName, TxDeclarationFlags declFlags, TxEntityDeclaration* aliasedDeclaration) {
    if (auto prev_symbol = this->get_symbol(plainName)) {
        CERROR(&this->get_root_scope()->driver(), "Failed to declare alias symbol, can't overload alias with other declarations under same symbol " << prev_symbol);
        return nullptr;
    }
    auto symbol = new TxAliasSymbol(this, plainName, declFlags, aliasedDeclaration);
    this->declare_symbol(symbol);
    this->LOGGER().trace("    Declared   %-32s %s", symbol->get_full_name().to_string().c_str(), symbol->to_string().c_str());
    return symbol;
}



bool TxScopeSymbol::symbol_validation_pass() const {
//    if (!this->fullName.begins_with(BUILTIN_NS))
//        this->LOGGER().debug("Validating symbol %s", this->fullName.to_string().c_str());
    bool valid = this->validate_symbol();
    if (! valid)
        this->LOGGER().debug("Failed symbol validity test: %s", this->fullName.to_string().c_str());
    for (auto entry : this->symbols)
        valid &= entry.second->symbol_validation_pass();
    return valid;
}

bool TxScopeSymbol::validate_symbol() const {
    return true;
}

void TxScopeSymbol::dump_symbols() const {
    const TxIdentifier builtinNamespace(BUILTIN_NS);
    std::vector<const TxModule*> subModules;
    for (auto & symName : this->symbolNames) {
        auto symbol = this->symbols.at(symName);
        if (auto submod = dynamic_cast<const TxModule*>(symbol))
            subModules.push_back(submod);
        else if (this->get_full_name() != builtinNamespace) {
            try {
                printf("%-11s %-48s %s\n", symbol->declaration_string().c_str(), symbol->get_full_name().to_string().c_str(),
                       symbol->description_string().c_str());
                /*
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
                */
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



/*=== TxAliasSymbol implementation ===*/

TxScopeSymbol* TxAliasSymbol::get_aliased_symbol() const {
    return this->aliasedDeclaration->get_symbol();
}

std::string TxAliasSymbol::description_string() const {
    return "alias " + this->aliasedDeclaration->get_unique_full_name();
};



/*=== TxEntitySymbol implementation ===*/

TxEntityDeclaration* TxEntitySymbol::get_distinct_decl() const {
    ASSERT(!this->is_overloaded(), "Can't get 'distinct' declaration of an overloaded entity: " << this->to_string());
    if (this->typeDeclaration)
        return this->typeDeclaration;
    else
        return this->get_first_field_decl();
}

bool TxEntitySymbol::add_type(TxTypeDeclaration* typeDeclaration) {
    if (this->typeDeclaration) {
        CERROR(typeDeclaration->get_definer(), "Can't overload several type declarations under the same name: " << this->get_full_name());
        return false;
    }
    this->typeDeclaration = typeDeclaration;
    return true;
}

bool TxEntitySymbol::add_field(TxFieldDeclaration* fieldDeclaration) {
    this->fieldDeclarations.push_back(fieldDeclaration);
    return true;
}

TxScopeSymbol* TxEntitySymbol::get_member_symbol(const std::string& name) {
    // overrides in order to handle instance members
    //std::cout << "In '" << this->get_full_name() << "': get_member_symbol(" << name << ")" << std::endl;
    // (if this symbol is a type, static member lookup of the type takes precedence if overloaded)
    if (this->get_type_decl())
        return this->TxScopeSymbol::get_member_symbol(name);
    else if (! this->is_overloaded()) {
        // this symbol represents a distinct field; look up its instance members
        if (auto type = this->get_first_field_decl()->get_definer()->attempt_get_type()) {
            if (auto member = type->lookup_instance_member(name))
                return member;
        }
        else
            this->LOGGER().warning("Type not resolved of %s", this->to_string().c_str());
    }
    return nullptr;
}

bool TxEntitySymbol::validate_symbol() const {
    bool valid = true;

    if (this->typeDeclaration) {
        valid &= this->typeDeclaration->validate();
    }

    for (auto fieldDeclI = this->fields_cbegin(); fieldDeclI != this->fields_cend(); fieldDeclI++) {
        valid &= (*fieldDeclI)->validate();

        // check that only fields of function type are overloaded
        auto type = (*fieldDeclI)->get_definer()->get_type();
        if (this->field_count() > 1 && ! dynamic_cast<const TxFunctionType*>(type)) {
            CERROR((*fieldDeclI)->get_definer(), "Illegal overload of symbol " << (*fieldDeclI) << " with type "
                   << (type ? type->to_string().c_str() : "NULL"));
            valid = false;
        }
        // TODO: check that no two signatures are exactly equal
    }

    return valid;
}

void TxEntitySymbol::dump_symbols() const {
    TxScopeSymbol::dump_symbols();
    if (this->is_overloaded()) {
        for (auto fieldDecl : this->fieldDeclarations) {
            printf("%-11s %-48s FIELD  %s\n", ::to_string(fieldDecl->get_decl_flags()).c_str(),
                   fieldDecl->get_unique_full_name().c_str(),
                   fieldDecl->get_definer()->get_type()->to_string().c_str());
        }
    }
}

std::string TxEntitySymbol::declaration_string() const {
    if (this->is_overloaded())
        return "";
    else if (this->typeDeclaration)
        return ::to_string(this->typeDeclaration->get_decl_flags());
    else if (this->field_count())
        return ::to_string(this->get_first_field_decl()->get_decl_flags());
    else  // declaration not yet assigned to this entity symbol
        return "";
}

std::string TxEntitySymbol::description_string() const {
    if (this->is_overloaded())
        return "<overloaded>";
    else if (this->typeDeclaration)
        if (auto type = this->typeDeclaration->get_definer()->attempt_get_type()) {
            if (type->is_empty_derivation())
                return "TYPE      = " + type->get_base_data_type()->to_string(true);
            else
                return "TYPE      " + type->to_string(true, true);
        }
        else
            return "TYPE      <undef>";
    else if (this->field_count()) {
        auto field = this->get_first_field_decl()->get_definer()->get_field();
        auto storageIx = (field->get_decl_flags() & TXD_CONSTRUCTOR ? -1 : field->get_storage_index());
        std::string storageIxString = ( storageIx >= 0 ? std::string("[") + std::to_string(storageIx) + "] " : std::string("    ") );
        if (auto type = this->get_first_field_decl()->get_definer()->attempt_get_type())
            return "FIELD " + storageIxString + type->to_string(true);
        else
            return "FIELD " + storageIxString + "<undef type>";
    }
    else  // declaration not yet assigned to this entity symbol
        return "<undef entity>";
}



/*=== symbol table lookup functions ===*/

static TxScopeSymbol* search_symbol(TxScopeSymbol* vantageScope, const TxIdentifier& ident);

TxScopeSymbol* TxAliasSymbol::resolve_generic(TxScopeSymbol* vantageScope, TxScopeSymbol* scope) {
    LOGGER().alert("Substituting alias %s with %s", this->get_full_name().to_string().c_str(),
                   this->get_aliased_symbol()->to_string().c_str());
    return this->get_aliased_symbol()->resolve_generic(vantageScope, scope);
}

TxScopeSymbol* TxEntitySymbol::resolve_generic(TxScopeSymbol* vantageScope, TxScopeSymbol* scope) {
    if (this->is_overloaded())
        return this;
    if (this->get_distinct_decl()->get_decl_flags() & TXD_GENPARAM) {
        std::string bindingName = this->get_full_name().to_string();
        std::replace(bindingName.begin(), bindingName.end(), '.', '#');
        this->LOGGER().debug("Trying to resolve generic parameter %s = %s from %s", this->get_full_name().to_string().c_str(), bindingName.c_str(), scope->get_full_name().to_string().c_str());
        if (auto boundSym = search_symbol(scope, bindingName)) {
            // #-ified symbol is bound
            this->LOGGER().alert("Substituting generic parameter %s with %s", this->get_full_name().to_string().c_str(), boundSym->to_string().c_str());
            return boundSym->resolve_generic(vantageScope, scope);
        }
        else {
            // #-ified symbol is unbound
            // unbound symbols are not resolved against, unless they're defined by an outer or parent scope -
            // meaning they're type parameters pertaining to the current lexical context
            if (scope->get_full_name().begins_with(this->get_outer()->get_full_name()))
                this->LOGGER().debug("Scope (%s) of unbound generic parameter %s encompasses scope %s (so OK)",
                                     this->get_outer()->get_full_name().to_string().c_str(),
                                     bindingName.c_str(), scope->get_full_name().to_string().c_str());
            else
                this->LOGGER().warning("Scope (%s) of unbound generic parameter %s DOESN'T encompass scope %s",
                                       this->get_outer()->get_full_name().to_string().c_str(),
                                       bindingName.c_str(), scope->get_full_name().to_string().c_str());
        }
    }
    return this;
}


static TxScopeSymbol* inner_lookup_member(TxScopeSymbol* vantageScope, TxScopeSymbol* scope, const TxIdentifier& ident) {
    //std::cout << "From '" << scope->get_full_name() << "': lookup_member(" << ident << ")" << std::endl;
    if (auto member = scope->get_member_symbol(ident.segment(0))) {
        // if the identified member is a type parameter/alias, attempt to resolve it by substituting it for its binding:
        member = member->resolve_generic(vantageScope, scope);

        if (ident.is_plain())
            return member;
        else
            return inner_lookup_member(vantageScope, member, TxIdentifier(ident, 1));
    }
    return nullptr;
}

static TxScopeSymbol* search_symbol(TxScopeSymbol* vantageScope, const TxIdentifier& ident) {
    for (auto scope = vantageScope; scope; scope = scope->get_outer()) {
        if (auto symbol = inner_lookup_member(vantageScope, scope, ident))
            return symbol;
        if (dynamic_cast<TxModule*>(scope))
            // if member lookup within a module fails, skip parent modules and do global lookup via root namespace (package)
            return inner_lookup_member(vantageScope, scope->get_root_scope(), ident);
    }
    return nullptr;
}


TxScopeSymbol* lookup_member(TxScopeSymbol* vantageScope, TxScopeSymbol* scope, const TxIdentifier& ident) {
    auto symbol = inner_lookup_member(vantageScope, scope, ident);
    // TODO: implement visibility check
    return symbol;
}

TxScopeSymbol* lookup_symbol(TxScopeSymbol* vantageScope, const TxIdentifier& ident) {
    auto symbol = search_symbol(vantageScope, ident);
    // TODO: implement visibility check
    return symbol;
}

TxTypeDeclaration* lookup_type(TxScopeSymbol* vantageScope, const TxIdentifier& ident) {
    if (auto entitySymbol = dynamic_cast<TxEntitySymbol*>(lookup_symbol(vantageScope, ident)))
        return entitySymbol->get_type_decl();
    return nullptr;
}

TxFieldDeclaration* lookup_field(TxScopeSymbol* vantageScope, const TxIdentifier& ident,
                                 const std::vector<const TxType*>* typeParameters) {
    ResolutionContext resCtx;
    return resolve_field_lookup(resCtx, lookup_symbol(vantageScope, ident), typeParameters);
}



static bool arg_type_matches(const TxType *expectedType, const TxType* providedType) {
    // mimics behavior of inner_validate_wrap_convert()   FUTURE: merge code
    if (providedType == expectedType)
        return true;
    if (expectedType->auto_converts_from(*providedType))
        return true;
    if (auto refType = dynamic_cast<const TxReferenceType*>(expectedType)) {
        auto refTargetType = refType->target_type();
        if (refTargetType && providedType->is_a(*refTargetType)) {
            if (! refTargetType->is_modifiable()) {
                // originalExpr will be auto-wrapped with a reference-to node
                return true;
            }
//            else {
//                if (!originalType->is_modifiable())
//                    LOGGER().debug("Cannot convert reference with non-mod-target to one with mod target: %s -> %s",
//                                   originalType->to_string().c_str(), requiredType->to_string().c_str());
//                else
//                    LOGGER().debug("Cannot implicitly convert to reference with modifiable target: %s -> %s",
//                                   originalType->to_string().c_str(), requiredType->to_string().c_str());
//            }
        }
    }
//    LOGGER().debug("Can't auto-convert value\n\tFrom: %80s\n\tTo:   %80s",
//                   originalType->to_string().c_str(), requiredType->to_string().c_str());
    return false;
}


TxFieldDeclaration* resolve_field_lookup(ResolutionContext& resCtx, TxScopeSymbol* symbol,
                                         const std::vector<const TxType*>* typeParameters) {
    if (auto entitySymbol = dynamic_cast<const TxEntitySymbol*>(symbol)) {
        if (entitySymbol->field_count() == 1) {
            return *entitySymbol->fields_cbegin();
        }
        if (typeParameters) {
            std::vector<TxFieldDeclaration*> matches;
            for (auto fieldCandidateI = entitySymbol->fields_cbegin();
                      fieldCandidateI != entitySymbol->fields_cend(); fieldCandidateI++) {
                auto fieldCandidate = (*fieldCandidateI)->get_definer()->resolve_field(resCtx);
                auto fieldCandidateType = fieldCandidate->get_type();
                if (auto candidateFuncType = dynamic_cast<const TxFunctionType*>(fieldCandidateType)) {
                    symbol->LOGGER().debug("Candidate function: %s", candidateFuncType->to_string().c_str());
                    if (candidateFuncType->argumentTypes.size() == typeParameters->size()) {
                        auto typeParamI = typeParameters->cbegin();
                        for (auto argDef : candidateFuncType->argumentTypes) {
                            if (! arg_type_matches(argDef, *typeParamI)) {
                                symbol->LOGGER().debug("Argument mismatch, can't convert\n\tFrom: %80s\n\tTo:   %80s",
                                                       (*typeParamI)->to_string(true).c_str(), argDef->to_string(true).c_str());
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
            if (entitySymbol->field_count()) {
                symbol->LOGGER().warning("Type parameters do not match any candidate of %s", symbol->to_string().c_str());
                return nullptr;
            }
        }
        else if (entitySymbol->field_count() > 1) {
            symbol->LOGGER().warning("%s must be matched using type parameters", symbol->to_string().c_str());
            return nullptr;
        }
    }
    // name is unknown, or a type
    if (symbol)
        symbol->LOGGER().warning("%s is not a field", symbol->to_string().c_str());
    return nullptr;
}
