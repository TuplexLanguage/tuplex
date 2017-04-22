#include "util/assert.hpp"

#include "util/util.hpp"

#include "tx_lang_defs.hpp"
#include "package.hpp"
#include "entity.hpp"
#include "declaration.hpp"
#include "symbol.hpp"

#include "type.hpp"
#include "ast_declbase.hpp"


/*=== TxScopeSymbol implementation ===*/

Logger& TxScopeSymbol::_LOG = Logger::get("SYMBOL");

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
    (void)success;   // suppresses unused variable warning in release mode
    this->LOGGER()->trace("-->            %s", scope->get_full_name().str().c_str());
    return scope;
}


/*--- symbol map implementation ---*/

void TxScopeSymbol::add_symbol(TxScopeSymbol* symbol) {
    ASSERT(symbol->outer==this, "Mismatching symbol parent reference! " << symbol);
    ASSERT((this->outer==NULL && symbol->get_full_name().is_plain()) || symbol->get_full_name().parent()==this->get_full_name(),
            "Symbol qualifier doesn't match parent scope! " << symbol);
    auto result = this->symbols.emplace(symbol->get_name(), symbol);
    ASSERT(result.second, "Failed to insert new symbol (previously inserted?): " << symbol);
    this->declOrderNames.push_back( symbol->get_name() );
    this->alphaOrderNames.insert( symbol->get_name() );
}

bool TxScopeSymbol::has_symbol(const std::string& name) const {
    return this->symbols.count(name);
}

const TxScopeSymbol* TxScopeSymbol::get_symbol(const std::string& name) const {
    return this->symbols.count(name) ? this->symbols.at(name) : nullptr;
}


/*--- symbol table handling ---*/

bool TxScopeSymbol::declare_symbol( const TxParseOrigin& origin, TxScopeSymbol* symbol ) {
    if (this->has_symbol(symbol->get_name()))
        return false;
    this->add_symbol(symbol);
    return true;
}

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
//                this->LOGGER()->warning("%-40s (in %-40s) shadows %s", entity->to_string().c_str(), this->to_string().c_str(), shadowed->to_string().c_str());
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
        this->LOGGER()->trace("    Declared   %s", entitySymbol->str().c_str());

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


const TxTypeDeclaration* TxScopeSymbol::declare_type( const std::string& plainName, TxTypeDefiningNode* typeDefiner,
                                                      TxDeclarationFlags declFlags ) {
    ASSERT(!is_internal_name(plainName) || (declFlags & (TXD_IMPLICIT | TXD_CONSTRUCTOR | TXD_INITIALIZER)),
           "Mismatch between name format and IMPLICIT flag for type declaration " << plainName);

    if (TxEntitySymbol* entitySymbol = this->declare_entity(plainName, typeDefiner)) {
        auto typeDeclaration = new TxTypeDeclaration(entitySymbol, declFlags, typeDefiner);
        if (entitySymbol->add_type(typeDeclaration))
            return typeDeclaration;
    }
    return nullptr;
}

const TxFieldDeclaration* TxScopeSymbol::declare_field( const std::string& plainName, TxFieldDefiningNode* fieldDefiner,
                                                        TxDeclarationFlags declFlags, TxFieldStorage storage,
                                                        const TxIdentifier& dataspace ) {
    ASSERT(!is_internal_name(plainName) || (declFlags & (TXD_IMPLICIT | TXD_CONSTRUCTOR | TXD_INITIALIZER)),
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
    for (auto & symName : this->declOrderNames) {
        auto symbol = this->symbols.at(symName);
        if (auto submod = dynamic_cast<const TxModule*>(symbol))
            subModules.push_back(submod);
        else if (this->get_full_name() != builtinNamespace || this->get_root_scope()->driver().get_options().dump_tx_symbols) {
            printf("%s %s\n", symbol->declaration_string().c_str(), symbol->description_string().c_str());
            symbol->dump_symbols();
        }
    }
    if (! subModules.empty()) {
        for (auto mod : subModules)
            mod->dump_symbols();
    }
}

std::string TxScopeSymbol::description_string() const {
    return "                           " + this->get_full_name().str();
}



/*=== TxEntitySymbol implementation ===*/

const TxEntityDeclaration* TxEntitySymbol::get_distinct_decl() const {
    ASSERT(!this->is_overloaded(), "Can't get 'distinct' declaration of an overloaded entity: " << this->str());
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

    if (!( typeDeclaration->get_decl_flags() & ( TXD_GENPARAM | TXD_GENBINDING ) )
            && this->get_name() != "$Self" && this->get_name() != "$GenericBase" && !begins_with( this->get_name(), "$Ftype" )) {
        auto definer = typeDeclaration->get_definer();
        if (TxEntitySymbol* entitySymbol = this->declare_entity( "$Self", definer )) {
            auto selfDeclaration = new TxTypeDeclaration( entitySymbol, TXD_IMPLICIT, definer );
            entitySymbol->add_type( selfDeclaration );
        }
    }

    return true;
}

bool TxEntitySymbol::add_field(TxFieldDeclaration* fieldDeclaration) {
    this->fieldDeclarations.push_back(fieldDeclaration);
    return true;
}

static TxScopeSymbol* search_symbol(TxScopeSymbol* vantageScope, const TxIdentifier& ident);

static const TxEntityDeclaration* get_symbols_declaration(TxEntitySymbol* entitySymbol) {
    const TxEntityDeclaration* decl = entitySymbol->get_type_decl();
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
                                    this->LOGGER()->debug("Resolved %-16s = %-16s to binding\t%s", name.c_str(),
                                                         hashedSym->get_full_name().str().c_str(), bindingDecl->str().c_str());
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
            this->LOGGER()->warning("Type not resolved of %s", this->str().c_str());
    }
    return nullptr;
}



static std::string field_description( const TxFieldDeclaration* fieldDecl ) {
    if (auto field = fieldDecl->get_definer()->attempt_get_field()) {
        if (auto type = field->get_type()) {
            char buf[512];

            int storageIx = -1;
            if (! ( field->get_decl_flags() & ( TXD_CONSTRUCTOR | TXD_INITIALIZER ) ))
                storageIx = field->get_decl_storage_index();
            if (storageIx >= 0)
                snprintf( buf, 512, "FIELD [%2d]  %-48s : %s",
                          storageIx,
                          fieldDecl->get_unique_full_name().c_str(),
                          type->str().c_str() );
            else
                snprintf( buf, 512, "FIELD       %-48s : %s",
                          fieldDecl->get_unique_full_name().c_str(),
                          type->str().c_str() );
            return std::string( buf );
        }
    }
    return "FIELD       -undef-";
}

void TxEntitySymbol::dump_symbols() const {
    TxScopeSymbol::dump_symbols();
    if (this->is_overloaded()) {
        for (auto fieldDecl : this->fieldDeclarations) {
            printf("%s %s\n", to_string( fieldDecl->get_decl_flags() ).c_str(), field_description( fieldDecl ).c_str() );
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
        return "   overloaded symbol       " + this->get_full_name().str();
    else if (this->typeDeclaration)  // non-overloaded type name
        if (auto type = this->typeDeclaration->get_definer()->attempt_get_type()) {
            if (type->get_declaration() == this->typeDeclaration)
                return "TYPE        " + type->str( false );
            else {
                auto name = this->typeDeclaration->get_unique_full_name();
                if (name.size() < 48)
                    name.resize(48, ' ');
                return "TYPE ALIAS  " + name + " = " + type->str();
            }
        }
        else
            return "TYPE        -undef-";
    else if (this->field_count()) {  // non-overloaded field name
        return field_description( this->get_first_field_decl() );
    }
    else  // declaration not yet assigned to this entity symbol
        return "-undef entity-";
}



/*=== symbol table lookup functions ===*/

static TxScopeSymbol* search_symbol(TxScopeSymbol* vantageScope, const TxIdentifier& ident);


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

const TxTypeDeclaration* lookup_type(TxScopeSymbol* vantageScope, const TxIdentifier& ident) {
    if (auto entitySymbol = dynamic_cast<TxEntitySymbol*>(lookup_symbol(vantageScope, ident)))
        return entitySymbol->get_type_decl();
    return nullptr;
}

const TxFieldDeclaration* lookup_field( TxScopeSymbol* vantageScope, const TxIdentifier& ident ) {
    if (auto entitySymbol = dynamic_cast<const TxEntitySymbol*>( lookup_symbol( vantageScope, ident ) )) {
        if (entitySymbol->field_count() == 1)
            return entitySymbol->get_first_field_decl();
        if (entitySymbol->field_count() > 1)
            entitySymbol->LOGGER()->note("%s must be matched using type parameters", entitySymbol->str().c_str());
    }
    return nullptr;
}
