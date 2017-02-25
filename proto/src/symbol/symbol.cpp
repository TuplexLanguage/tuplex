#include "util/assert.hpp"

#include "tx_lang_defs.hpp"
#include "package.hpp"
#include "entity.hpp"
#include "declaration.hpp"
#include "symbol.hpp"

#include "type.hpp"
#include "ast_declbase.hpp"


/*=== TxScopeSymbol implementation ===*/

Logger& TxScopeSymbol::LOG = Logger::get("SYMBOL");

/*--- lexical scope tracking ---*/

TxScopeSymbol::TxScopeSymbol(TxScopeSymbol* parent, const std::string& name)
        : name(name), outer(parent)  {
    if (parent) {
        ASSERT(!name.empty() && name.find_first_of('.') == std::string::npos, "Non-plain name specified for non-root scope: '" << name << "'");
        this->fullName = TxIdentifier(this->outer->get_full_name(), this->name);
        this->root = parent->get_root_scope();
        //this->inExpErrBlock = parent->inExpErrBlock;
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


TxScopeSymbol* TxScopeSymbol::create_code_block_scope( const TxParseOrigin& origin, const std::string& plainName ) {
    std::string baseName = plainName + '$';
    std::string uniqueName = this->make_unique_name(baseName);
    TxScopeSymbol* scope = new TxScopeSymbol(this, uniqueName);
    //scope->inExpErrBlock |= isExpErrBlock;
    bool success = this->declare_symbol(origin, scope);
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

bool TxScopeSymbol::declare_symbol( const TxParseOrigin& origin, TxScopeSymbol* symbol ) {
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

TxEntitySymbol* TxScopeSymbol::declare_entity(const std::string& plainName, TxNode* definingNode) {
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
            CERROR(definingNode, "Failed to declare entity symbol, can't overload entities and non-entities under same symbol: " << symbol);
            return nullptr;
        }
    }
    else {
        entitySymbol = new TxEntitySymbol(this, plainName);
        this->declare_symbol( *definingNode, entitySymbol );
        this->LOGGER().trace("    Declared   %s", entitySymbol->to_string().c_str());

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


static inline bool is_internal_name( const std::string& name ) {
    return ( name.find_first_of('$') != std::string::npos );
}


TxTypeDeclaration* TxScopeSymbol::declare_type(const std::string& plainName, TxTypeDefiningNode* typeDefiner,
                                               TxDeclarationFlags declFlags) {
    ASSERT(!is_internal_name(plainName) || (declFlags & (TXD_IMPLICIT | TXD_CONSTRUCTOR)),
           "Mismatch between name format and IMPLICIT flag for type declaration " << plainName);

    if (TxEntitySymbol* entitySymbol = this->declare_entity(plainName, typeDefiner)) {
        auto typeDeclaration = new TxTypeDeclaration(entitySymbol, declFlags, typeDefiner);
        if (entitySymbol->add_type(typeDeclaration))
            return typeDeclaration;
    }
    return nullptr;
}

TxFieldDeclaration* TxScopeSymbol::declare_field(const std::string& plainName, TxFieldDefiningNode* fieldDefiner,
                                                 TxDeclarationFlags declFlags, TxFieldStorage storage,
                                                 const TxIdentifier& dataspace) {
    ASSERT(!is_internal_name(plainName) || (declFlags & (TXD_IMPLICIT | TXD_CONSTRUCTOR)),
           "Mismatch between name format and IMPLICIT flag for field declaration " << plainName);

    if (TxEntitySymbol* entitySymbol = this->declare_entity(plainName, fieldDefiner)) {
        auto fieldDeclaration = new TxFieldDeclaration(entitySymbol, declFlags, fieldDefiner, storage, dataspace);
        if (entitySymbol->add_field(fieldDeclaration))
            return fieldDeclaration;
    }
    return nullptr;
}



void TxScopeSymbol::dump_symbols() const {
    const TxIdentifier builtinNamespace(BUILTIN_NS);
    std::vector<const TxModule*> subModules;
    for (auto & symName : this->symbolNames) {
        auto symbol = this->symbols.at(symName);
        if (auto submod = dynamic_cast<const TxModule*>(symbol))
            subModules.push_back(submod);
        else if (this->get_full_name() != builtinNamespace || this->get_root_scope()->driver().get_options().dump_tx_symbols) {
            printf("%-14s %-48s %s\n", symbol->declaration_string().c_str(), symbol->get_full_name().to_string().c_str(),
                   symbol->description_string().c_str());
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

static TxScopeSymbol* search_symbol(TxScopeSymbol* vantageScope, const TxIdentifier& ident);

static const TxEntityDeclaration* get_symbols_declaration(TxEntitySymbol* entitySymbol) {
    TxEntityDeclaration* decl = entitySymbol->get_type_decl();
    if (! decl) {
        if (! entitySymbol->is_overloaded())
            decl = entitySymbol->get_first_field_decl();
        else  // overloaded field, can't match
            return nullptr;
    }
    return decl;
}

TxScopeSymbol* TxEntitySymbol::get_member_symbol(const std::string& name) {
    // overrides in order to handle instance members
    // (if this symbol is a type, static member lookup of the type takes precedence if overloaded)

    if (name.find_first_of('#') != std::string::npos) {
        // sought name is a hashified, fully qualified name (e.g. my#SType#E)
        auto fullName = dehashify(name);
        if (auto hashedSym = search_symbol(this, TxIdentifier(fullName))) {  // FUTURE: review if this may breach visibility
            if (auto hashedEntSym = dynamic_cast<TxEntitySymbol*>(hashedSym)) {
                if (auto hashedDecl = get_symbols_declaration(hashedEntSym)) {
                    if (hashedDecl->get_decl_flags() & TXD_GENPARAM) {
                        // symbol distinctly (non-overloaded) refers to a GENPARAM
                        // Resolves e.g:  my#SType#E = my.SType.E  to binding  -P---- ---B--  my.$SType<tx#~Float,tx#~Double>.E
                        if (auto thisDecl = get_symbols_declaration(this)) {
                            if (auto thisType = thisDecl->get_definer()->get_type()) {
                                if (auto bindingDecl = thisType->lookup_param_binding(hashedDecl)) {
                                    this->LOGGER().debug("Resolved %-16s = %-16s to binding\t%s", name.c_str(),
                                                         hashedSym->get_full_name().to_string().c_str(), bindingDecl->to_string().c_str());
                                    return bindingDecl->get_symbol();
                                }
                            }
                        }
                    }
                }
            }
            return hashedSym;
        }
    }

    if (this->get_type_decl())
        return this->TxScopeSymbol::get_member_symbol(name);
    else if (! this->is_overloaded()) {
        // this symbol represents a distinct field; look up its instance members
        if (auto type = this->get_first_field_decl()->get_definer()->get_type()) {
            if (auto member = type->lookup_inherited_instance_member(name))
                return member;
        }
        else
            this->LOGGER().warning("Type not resolved of %s", this->to_string().c_str());
    }
    return nullptr;
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
        return "-overloaded-";
    else if (this->typeDeclaration)  // non-overloaded type name
        if (auto type = this->typeDeclaration->get_definer()->attempt_get_type()) {
            if (auto typeExpr = dynamic_cast<TxTypeExpressionNode*>( this->typeDeclaration->get_definer() ))
                return "\tTYPE      ( " + typeExpr->get_auto_type_name() + " )  " + type->to_string(false, true);
            else
                return "\tTYPE      NOT A TYPE EXPRESSION NODE!  " + type->to_string(false, true);
        }
        else
            return "\tTYPE      -undef-";
    else if (this->field_count()) {  // non-overloaded field name
        if (auto field = this->get_first_field_decl()->get_definer()->get_field()) {
            if (auto type = field->get_type()) {
                int storageIx = -1;
                if (! (field->get_decl_flags() & (TXD_CONSTRUCTOR | TXD_GENBINDING)))
                    storageIx = field->get_decl_storage_index();
                std::string storageIxString = ( storageIx >= 0 ? std::string("[") + std::to_string(storageIx) + "] " : std::string("    ") );
                return "\tFIELD " + storageIxString + type->to_string(true);
            }
        }
        return "\tFIELD     -undef-";
    }
    else  // declaration not yet assigned to this entity symbol
        return "-undef entity-";
}



/*=== symbol table lookup functions ===*/

static TxScopeSymbol* search_symbol(TxScopeSymbol* vantageScope, const TxIdentifier& ident);

//TxScopeSymbol* TxEntitySymbol::resolve_generic(TxScopeSymbol* vantageScope, TxScopeSymbol* scope) {
//    if (this->is_overloaded())
//        return this;
//    if (this->get_distinct_decl()->get_decl_flags() & TXD_GENPARAM) {
//        std::string bindingName = this->get_full_name().to_string();
//        std::replace(bindingName.begin(), bindingName.end(), '.', '#');
//        this->LOGGER().debug("Trying to resolve generic parameter %s = %s from %s", this->get_full_name().to_string().c_str(), bindingName.c_str(), scope->get_full_name().to_string().c_str());
//        if (auto boundSym = search_symbol(scope, bindingName)) {
//            // #-ified symbol is bound
//            this->LOGGER().note("Substituting generic parameter %s with %s", this->get_full_name().to_string().c_str(), boundSym->to_string().c_str());
//            return boundSym->resolve_generic(vantageScope, scope);
//        }
//        else {
//            // #-ified symbol is unbound
//            // unbound symbols are not resolved against, unless they're defined by an outer or parent scope -
//            // meaning they're type parameters pertaining to the current lexical context
//            if (scope->get_full_name().begins_with(this->get_outer()->get_full_name()))
//                this->LOGGER().debug("Scope (%s) of unbound generic parameter %s encompasses scope %s (so OK)",
//                                     this->get_outer()->get_full_name().to_string().c_str(),
//                                     bindingName.c_str(), scope->get_full_name().to_string().c_str());
//            else
//                this->LOGGER().warning("Scope (%s) of unbound generic parameter %s DOESN'T encompass scope %s",
//                                       this->get_outer()->get_full_name().to_string().c_str(),
//                                       bindingName.c_str(), scope->get_full_name().to_string().c_str());
//        }
//    }
//    return this;
//}


static TxScopeSymbol* inner_lookup_member(TxScopeSymbol* scope, const TxIdentifier& ident) {
    //std::cout << "From '" << scope->get_full_name() << "': lookup_member(" << ident << ")" << std::endl;
    if (auto member = scope->get_member_symbol(ident.segment(0))) {
        if (ident.is_plain())
            return member;
        else
            return inner_lookup_member(member, TxIdentifier(ident, 1));
    }
    return nullptr;
}

static TxScopeSymbol* search_symbol(TxScopeSymbol* vantageScope, const TxIdentifier& ident) {
    for (auto scope = vantageScope; scope; scope = scope->get_outer()) {
        if (auto symbol = inner_lookup_member(scope, ident))
            return symbol;
        if (dynamic_cast<TxModule*>(scope))
            // if member lookup within a module fails, skip parent modules and do global lookup via root namespace (package)
            return inner_lookup_member(scope->get_root_scope(), ident);
    }
    return nullptr;
}


TxScopeSymbol* lookup_member(TxScopeSymbol* vantageScope, TxScopeSymbol* scope, const TxIdentifier& ident) {
    auto symbol = inner_lookup_member(scope, ident);
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

TxFieldDeclaration* lookup_field( TxScopeSymbol* vantageScope, const TxIdentifier& ident ) {
    if (auto entitySymbol = dynamic_cast<const TxEntitySymbol*>( lookup_symbol( vantageScope, ident ) )) {
        if (entitySymbol->field_count() == 1)
            return entitySymbol->get_first_field_decl();
        if (entitySymbol->field_count() > 1)
            entitySymbol->LOGGER().note("%s must be matched using type parameters", entitySymbol->to_string().c_str());
    }
    return nullptr;
}
