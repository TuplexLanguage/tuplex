#include "util/assert.hpp"

#include "util/util.hpp"

#include "tx_lang_defs.hpp"

#include "symbol.hpp"
#include "package.hpp"
#include "declaration.hpp"
#include "entity.hpp"
#include "driver.hpp"
#include "qual_type.hpp"
#include "type_base.hpp"


Logger& TxScopeSymbol::_LOG = Logger::get( "SYMBOL" );

/*--- lexical scope tracking ---*/

TxScopeSymbol::TxScopeSymbol( TxScopeSymbol* parent, const std::string& name )
        : name( name ), outer( parent ) {
    if ( parent ) {
        ASSERT( !name.empty() && name.find_first_of( '.' ) == std::string::npos, "Non-plain name specified for non-root scope: '" << name << "'" );
        this->fullName = TxIdentifier( this->outer->get_full_name(), this->name );
        this->root = parent->get_root_scope();
    }
    else {
        ASSERT( name.empty(), "Non-empty name specified for parent-less root scope: " << name );
        this->fullName = TxIdentifier();
        this->root = (TxPackage*) this;
    }
}

std::string TxScopeSymbol::make_unique_name( const std::string& baseName, bool suppressZeroSuffix ) const {
    unsigned counter = 0;
    std::string uniqueName = ( suppressZeroSuffix && !baseName.empty() ) ? baseName : baseName + std::to_string( counter );
    if ( !this->has_symbol( uniqueName ) )
        return uniqueName;
    while ( true ) {
        counter++;
        std::string uniqueName = baseName + std::to_string( counter );
        if ( !this->has_symbol( uniqueName ) )
            return uniqueName;
    }
}

TxScopeSymbol* TxScopeSymbol::create_code_block_scope( const TxParseOrigin& origin, const std::string& plainName ) {
    std::string uniqueName = this->make_unique_name( plainName + '$' );
    TxScopeSymbol* scope = new TxScopeSymbol( this, uniqueName );
    this->declare_symbol( origin, scope );
    return scope;
}

/*--- symbol map implementation ---*/

void TxScopeSymbol::add_symbol( TxScopeSymbol* symbol ) {
    ASSERT( symbol->outer == this, "Mismatching symbol parent reference! " << symbol );
    ASSERT( (this->outer==NULL && symbol->get_full_name().is_plain()) || symbol->get_full_name().parent()==this->get_full_name(),
            "Symbol qualifier doesn't match parent scope! " << symbol );
    auto result = this->symbols.emplace( symbol->get_name(), symbol );
    if ( !result.second ) {
        THROW_LOGIC( "Failed to insert new symbol (previously inserted?): " << symbol );
    }
    this->declOrderNames.push_back( symbol->get_name() );
}

bool TxScopeSymbol::has_symbol( const std::string& name ) const {
    return this->symbols.count( name );
}

const TxScopeSymbol* TxScopeSymbol::get_symbol( const std::string& name ) const {
    return this->symbols.count( name ) ? this->symbols.at( name ) : nullptr;
}

/*--- symbol table handling ---*/

void TxScopeSymbol::declare_symbol( const TxParseOrigin& origin, TxScopeSymbol* symbol ) {
    this->add_symbol( symbol );
}

TxEntitySymbol* TxScopeSymbol::declare_entity( const std::string& plainName, const TxNode* declarer, TxNode* definingNode ) {
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
    if ( auto symbol = this->get_symbol( plainName ) ) {
        entitySymbol = dynamic_cast<TxEntitySymbol*>( symbol );
        if ( !entitySymbol ) {
            CERR_THROWDECL( declarer, "Failed to declare entity symbol, "
                            << "can't overload entities and non-entities under same symbol: " << symbol );
        }
    }
    else {
        entitySymbol = new TxEntitySymbol( this, plainName );
        this->declare_symbol( *definingNode, entitySymbol );
        LOG_TRACE( LOGGER(), "    Declared   " << entitySymbol->str().c_str() );
    }
    return entitySymbol;
}

static inline bool is_internal_name( const std::string& name ) {
    return ( name.find_first_of( '$' ) != std::string::npos );
}

const TxTypeDeclaration* TxScopeSymbol::declare_type( const std::string& plainName, const TxNode* declarer,
                                                      TxTypeCreatingNode* typeDefiner, TxDeclarationFlags declFlags ) {
    ASSERT( !is_internal_name( plainName ) || ( declFlags & ( TXD_IMPLICIT | TXD_CONSTRUCTOR | TXD_INITIALIZER ) ),
            "Mismatch between name format and IMPLICIT flag for type declaration " << plainName );

    TxEntitySymbol* entitySymbol = this->declare_entity( plainName, declarer, typeDefiner );
    auto typeDeclaration = new TxTypeDeclaration( entitySymbol, declFlags, typeDefiner );
    entitySymbol->add_type( declarer, typeDeclaration );
    return typeDeclaration;
}

const TxFieldDeclaration* TxScopeSymbol::declare_field( const std::string& plainName, const TxNode* declarer,
                                                        TxFieldDefiningNode* fieldDefiner, TxDeclarationFlags declFlags,
                                                        TxFieldStorage storage, const TxIdentifier& dataspace ) {
//    ASSERT( !is_internal_name( plainName ) || ( declFlags & ( TXD_IMPLICIT | TXD_CONSTRUCTOR | TXD_INITIALIZER ) ),
//            "Mismatch between name format and IMPLICIT flag for field declaration " << plainName );

    TxEntitySymbol* entitySymbol = this->declare_entity( plainName, declarer, fieldDefiner );
    auto fieldDeclaration = new TxFieldDeclaration( entitySymbol, declFlags, fieldDefiner, storage, dataspace );
    entitySymbol->add_field( declarer, fieldDeclaration );
    return fieldDeclaration;
}

void TxScopeSymbol::dump_symbols() const {
    if ( this->get_full_name().begins_with( BUILTIN_NS ) && !this->get_root_scope()->driver().get_options().dump_tx_symbols ) {
        return;
    }
    std::vector<const TxModule*> subModules;
    for ( auto & symName : this->declOrderNames ) {
        auto symbol = this->symbols.at( symName );
        if ( auto submod = dynamic_cast<const TxModule*>( symbol ) ) {
            subModules.push_back( submod );
        }
        else {
            printf( "%s %s\n", symbol->declaration_string().c_str(), symbol->description_string().c_str() );
            symbol->dump_symbols();
        }
    }
    for ( auto mod : subModules )
        mod->dump_symbols();
}

std::string TxScopeSymbol::description_string() const {
    return "                            " + this->get_full_name().str();
}

/*=== TxEntitySymbol implementation ===*/

const TxEntityDeclaration* TxEntitySymbol::get_distinct_decl() const {
    ASSERT( !this->is_overloaded(), "Can't get 'distinct' declaration of an overloaded entity: " << this->str() );
    if ( this->typeDeclaration )
        return this->typeDeclaration;
    else
        return this->get_first_field_decl();
}

void TxEntitySymbol::add_type( const TxNode* declarer, const TxTypeDeclaration* typeDeclaration ) {
    if ( this->typeDeclaration || !this->fieldDeclarations.empty() ) {
        CERR_THROWDECL( declarer, "Can't overload several type declarations under the same name: " << this->get_full_name() );
    }
    this->typeDeclaration = typeDeclaration;

    // declare implicit 'Self' type:
    if ( !( typeDeclaration->get_decl_flags() & ( TXD_GENPARAM | TXD_GENBINDING ) )
         && this->get_name() != "Self" && this->get_name() != "Super"
         && this->get_name() != "$GenericBase" && !begins_with( this->get_name(), "$Ftype" ) ) {
        auto definer = typeDeclaration->get_definer();
        // TODO: Hackish to reuse declarer here:
        TxEntitySymbol* entitySymbol = this->declare_entity( "Self", declarer, definer );
        auto selfDeclaration = new TxTypeDeclaration( entitySymbol, TXD_PUBLIC | TXD_IMPLICIT, definer );
        entitySymbol->add_type( declarer, selfDeclaration );
    }
}

void TxEntitySymbol::add_field( const TxNode* declarer, const TxFieldDeclaration* fieldDeclaration ) {
    if ( this->typeDeclaration ) {
        CERR_THROWDECL( declarer, "Can't overload both type and field declarations under the same name: " << this->get_full_name() );
    }
    this->fieldDeclarations.push_back( fieldDeclaration );
}

void TxEntitySymbol::add_type_specialization( const TxTypeDeclaration* typeDeclaration ) {
    this->typeSpecDeclarations.push_back( typeDeclaration );
}

static std::string field_description( const TxFieldDeclaration* fieldDecl ) {
    const unsigned bufsize = 256;
    char buf[bufsize];

    if ( auto field = fieldDecl->get_definer()->attempt_field() ) {
        auto qtype = field->qtype();

        if ( !( field->get_decl_flags() & ( TXD_CONSTRUCTOR | TXD_INITIALIZER ) ) ) {
            if ( auto outerEntity = dynamic_cast<TxEntitySymbol*>( field->get_symbol()->get_outer() ) ) {
                if ( auto typeDecl = outerEntity->get_type_decl() ) {
                    if ( auto outerType = typeDecl->get_definer()->qtype() ) {  // assumes already resolved
                        char storageType = ' ';
                        int storageIx = -1;
                        switch ( field->get_storage() ) {
                        case TXS_STATIC:
                            storageType = 's';
                            storageIx = outerType->get_static_fields().get_field_index( field->get_unique_name() );
                            break;
                        case TXS_VIRTUAL:
                            case TXS_INSTANCEMETHOD:
                            storageType = 'v';
                            storageIx = outerType->get_virtual_fields().get_field_index( field->get_unique_name() );
                            break;
                        case TXS_INSTANCE:
                            storageType = 'i';
                            storageIx = outerType->get_instance_fields().get_field_index( field->get_unique_name() );
                            break;
                        default:
                            //ASSERT(false, "Only fields of static/virtual/instancemethod/instance storage classes have a storage index: " << *this);
                            break;
                        }
                        if ( storageIx >= 0 ) {
                            snprintf( buf, bufsize, "FIELD [%c%2d]  %-48s : %s",
                                      storageType, storageIx, fieldDecl->get_unique_full_name().c_str(), qtype.str().c_str() );
                            return std::string( buf );
                        }
                    }
                }
            }
        }

        snprintf( buf, bufsize, "FIELD        %-48s : %s", fieldDecl->get_unique_full_name().c_str(), qtype.str().c_str() );
        return std::string( buf );
    }
    snprintf( buf, bufsize, "FIELD        %-48s : -unresolved-", fieldDecl->get_unique_full_name().c_str() );
    return std::string( buf );
}

void TxEntitySymbol::dump_symbols() const {
    TxScopeSymbol::dump_symbols();
    if ( this->is_overloaded() ) {
        for ( auto fieldDecl : this->fieldDeclarations ) {
            printf( "%s %s\n", to_string( fieldDecl->get_decl_flags() ).c_str(), field_description( fieldDecl ).c_str() );
        }
    }
}

std::string TxEntitySymbol::declaration_string() const {
    if ( this->is_overloaded() )
        return "";
    else if ( this->typeDeclaration )
        return ::to_string( this->typeDeclaration->get_decl_flags() );
    else if ( this->field_count() )
        return ::to_string( this->get_first_field_decl()->get_decl_flags() );
    else
        // declaration not yet assigned to this entity symbol
        return "";
}

std::string TxEntitySymbol::description_string() const {
    if ( this->is_overloaded() )
        return "   overloaded symbol        " + this->get_full_name().str();
    else if ( this->typeDeclaration ) {  // non-overloaded type name
        if ( auto qtype = this->typeDeclaration->get_definer()->attempt_qtype() ) {
            if ( qtype->get_declaration() == this->typeDeclaration ) {
                const TxActualType *sembasetype = nullptr;
                try {
                    sembasetype = qtype->get_semantic_base_type();
                }
                catch ( const resolution_error& err ) { }
                if ( sembasetype ) {
                    auto name = qtype.str();
                    if ( name.size() < 48 )
                        name.resize( 48, ' ' );
                    return "TYPE         " + name + " : " + sembasetype->str();
                }
                else
                    return "TYPE         " + qtype.str( false );
            }
            else {
                auto name = this->typeDeclaration->get_unique_full_name();
                if ( name.size() < 48 )
                    name.resize( 48, ' ' );
                return "TYPE ALIAS   " + name + " = " + qtype.str();
            }
        }
        else
            return "TYPE         " + this->get_full_name().str() + " : -unresolved-";
    }
    else if ( this->field_count() ) {  // non-overloaded field name (we check for overload above)
        return field_description( this->get_first_field_decl() );
    }
    else
        // declaration not assigned to this entity symbol
        return "-undef entity-";
}
