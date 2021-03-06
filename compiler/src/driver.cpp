#include <string>
#include <symbol/symbol_lookup.hpp>

#include "tinydir/tinydir.h"

#include "util/assert.hpp"
#include "util/files_env.hpp"

#include "driver.hpp"

#include "builtin/builtin_types.hpp"
#include "llvm_generator.hpp"

#include "tx_lang_defs.hpp"
#include "TuplexConfig.h"

#include "parser_if.hpp"

#include "parsercontext.hpp"
#include "ast/ast_modbase.hpp"
#include "ast/ast_declpass.hpp"
#include "ast/ast_entitydecls.hpp"
#include "ast/ast_wrappers.hpp"


const char* CORE_TX_SOURCE_STR =
        // #include "lang.tx"
        R"=====(
module tx;

/** Returns TRUE if v has tid among its supertypes / -interfaces. */
isa( valueRef : &Any, tid : UInt )->Bool :
    return isa( _typeid( valueRef ), tid );

isa( valueTid : UInt, tid : UInt )->Bool :
    supers : &Array{UInt} = _supertypes( valueTid );
    lower := ~ 0UI;
    upper := ~ supers.L;
    while lower != upper :
        pos := ( lower + upper ) / 2;
        st := supers[pos];
        if st == tid:
            return TRUE;
        else if st > tid:
            upper = pos;
        else:
            lower = pos + 1;
    return FALSE;

builtin type Function derives Any :
    override equals( other : &Any ) -> Bool :
        if other is o : &Self :
            return self^ == o^;
        return FALSE;
)=====";


static TxSourceBuffer load_file( const std::string& filePath );

std::string get_version_string() {
    size_t len = 128;
    char buf[len];
    snprintf( buf, len, "%s version %d.%d", "Tuplex", Tuplex_VERSION_MAJOR, Tuplex_VERSION_MINOR );
    return std::string( buf );
}


TxDriver::TxDriver( const TxOptions& options )
        : _LOG( Logger::get( "DRIVER" )), options( options ),
          builtinParserContext(), llvmContext( new llvm::LLVMContext()) {
}

TxDriver::~TxDriver() = default;


int TxDriver::compile( const std::vector<std::string>& startSourceFiles, const std::string& outputFileName ) {
    ASSERT( this->parsedSourceFiles.empty(), "Can only run driver instance once" );

    if ( startSourceFiles.empty()) {
        this->_LOG.fatal( "No source specified." );
        return 1;
    }
    this->firstSourceFilename = startSourceFiles.front();

    if ( options.moduleSearchPaths.empty())
        this->_LOG.config( "Module search path is empty" );
    else
        for ( const auto& pathItem : options.moduleSearchPaths )
            this->_LOG.config( "Module search path item: '%s'", pathItem.c_str());

    /*--- prepare the root package and built-in types ---*/

    this->builtinParserContext = new TxParserContext( *this, TxIdentifier( "" ), get_builtins_file_name(),
                                                      TxSourceBuffer( "" ), TxParserContext::BUILTINS );

    this->builtinTypes = new BuiltinTypes( this->builtinParserContext );
    this->package = this->builtinTypes->get_root_package();

    // ONLY used for constant evaluation before code generation pass:
    this->genContext = new LlvmGenerationContext( *this->package, *this->llvmContext );

    /*--- prepare the parse units ---*/

    {  // initialize the built-in ASTs
        this->builtinParserContext->parsingUnit = this->builtinTypes->createTxModuleAST();
        this->parsedASTs.push_back( this->builtinParserContext );
    }

    {  // parse the intrinsic source:
        std::string thisSrcFileName( __FILE__ );
        auto slashPos = thisSrcFileName.find_last_of( '/' );
        if ( slashPos != std::string::npos )
            thisSrcFileName = thisSrcFileName.substr( slashPos+1, thisSrcFileName.size() );
        auto* parserContext = new TxParserContext( *this, TxIdentifier( "" ), thisSrcFileName,
                                                   TxSourceBuffer( CORE_TX_SOURCE_STR ), TxParserContext::BUILTINS );
        int ret = parse( parserContext );
        if ( ret ) {
            _LOG.fatal( "Exiting due to unrecovered syntax error" );
            return ret;
        }
        ASSERT( parserContext->parsingUnit, "parsingUnit not set by parser" );
        this->parsedASTs.push_back( parserContext );
    }

    /*--- enqueue initial sources ---*/

    if ( !this->options.homePath.empty()) {
        // add the tx namespace sources
        std::string homePath( this->options.homePath );
        if ( file_status( homePath ) == 2 ) {
            // path item exists and is a directory
            this->_LOG.config( "Including Tuplex home path '%s'", homePath.c_str());
            if ( !is_path_separator( homePath.back()))
                homePath.push_back( get_path_separator());
            homePath.append( BUILTIN_NS );
            this->add_all_in_dir( BUILTIN_NS, homePath, true );
        }
        else {
            _LOG.error( "Home path / tx namespace source path is not a valid directory: '%s'", homePath.c_str() );
        }
    }

    for ( const auto& startFile : startSourceFiles )
        this->sourceFileQueue.emplace_back( TxIdentifier(), startFile );

    /*--- parse all source files (during parsing, files are added to the queue as they are imported) ---*/

    TxParserContext::ParseInputSourceSet iss = TxParserContext::TX_SOURCES;
    while ( !this->sourceFileQueue.empty()) {
        TxIdentifier moduleName = this->sourceFileQueue.front().first;  // note, may be empty

        if ( iss == TxParserContext::TX_SOURCES ) {
            if ( !moduleName.begins_with( BUILTIN_NS ))  // if first user source file
                iss = TxParserContext::FIRST_USER_SOURCE;
        }
        else if ( iss == TxParserContext::FIRST_USER_SOURCE ) {
            iss = TxParserContext::REST_USER_SOURCES;
        }

        std::string nextFilePath = this->sourceFileQueue.front().second;
        if ( !this->parsedSourceFiles.count( nextFilePath )) {  // if not already parsed
            TxSourceBuffer srcBuffer = load_file( nextFilePath );
            if ( srcBuffer.source != nullptr ) {
                _LOG.info( "+ Loaded source file '%s'", nextFilePath.c_str());
                auto* parserContext = new TxParserContext( *this, moduleName, nextFilePath, srcBuffer, iss );
                int ret = parse( parserContext );
                if ( ret ) {
                    if ( ret < 0 )  // input file / stream error
                        _LOG.fatal( "Exiting due to input file / stream error" );
                    else if ( ret == 1 )  // syntax error
                        _LOG.fatal( "Exiting due to unrecovered syntax error" );
                    else
                        // ret == 2, out of memory
                        _LOG.fatal( "Exiting due to out of memory" );
                    return ret;
                }
                ASSERT( parserContext->parsingUnit, "parsingUnit not set by parser" );
                this->parsedASTs.push_back( parserContext );
                this->parsedSourceFiles.emplace( nextFilePath, parserContext->parsingUnit );
            }
            else {
                _LOG.fatal( "Exiting due to input file / stream error" );
                return -1;
            }
        }
        this->sourceFileQueue.pop_front();
    }

    if ( error_count )
        _LOG.error( "- Grammar parse encountered %d errors", error_count );
    else
        _LOG.info( "+ Grammar parse OK" );
    if ( this->options.only_parse )
        return error_count;

    /*--- perform declaration pass ---*/

    auto prev_error_count = error_count;
    for ( auto parserContext : this->parsedASTs ) {
        // by processing root node here we avoid root checking in visitor implementation
        parserContext->parsingUnit->node_declaration_pass( this->package );
        run_declaration_pass( parserContext->parsingUnit->module, parserContext->parsingUnit, "module" );
    }

    // So that builtins are resolved first and in the proper order  FUTURE - maybe add to queue instead?
    this->builtinTypes->resolveBuiltinSymbols();

    this->package->prepare_modules();  // (prepares the declared imports)

    if ( error_count != prev_error_count ) {
        _LOG.error( "- Declaration pass encountered %d errors", error_count - prev_error_count );
        prev_error_count = error_count;
    }
    else
        _LOG.info( "+ Declaration pass OK" );


    /*--- type creation / integration / resolution passes ---*/
    if ( this->options.compile_all_source )
        this->compile_lexical();
    else
        this->compile_reachable();


    /*--- type preparation / layout pass ---*/

    {
        prev_error_count = error_count;
        this->package->registry().prepare_types();

        for ( auto parserContext : this->parsedASTs ) {
            parserContext->finalize_expected_error_clauses();
        }

        if ( error_count != prev_error_count ) {
            _LOG.error( "- Type preparation pass encountered %d errors", error_count - prev_error_count );
        }
        else
            _LOG.info( "+ Type preparation pass OK" );
    }

    _LOG.info( "Number of AST nodes created: %u", TxNode::nodes_created_count() );

    if ( this->options.dump_ast ) {
        auto visitorFunc = []( const TxNode* node, const AstCursor& cursor, const std::string& role, void* ctx ) {
            char buf[256];
            snprintf( buf, 256, "%*s%s", cursor.depth * 2, "", role.c_str());
            printf( "%-50s %s\n", buf, node->str().c_str());
        };
        AstVisitor visitor = { visitorFunc, nullptr, TXP_NIL };

        for ( auto parserContext : this->parsedASTs ) {
            std::cout << "AST DUMP " << parserContext << ":" << std::endl;
            parserContext->parsingUnit->visit_ast( visitor, AstCursor( nullptr ), "", nullptr );
            std::cout << "END AST DUMP " << parserContext << std::endl;
        }
    }

    if ( this->options.dump_symbol_table ) {
        std::cout << "SYMBOL TABLE DUMP:\n";
        std::cout << "Declaration flags legend: " << (TxDeclarationFlags) 0xFFFF << std::endl;
        std::cout << "Public  pRotected  Virtual  eXternc  Abstract  Final  Override    "
                  << "Built-in  Implicit  Genparam  genBinding  Constructor  Initializer  Expected-error"
                  << std::endl;
        this->package->dump_symbols();
        std::cout << "END SYMBOL TABLE DUMP\n";
    }

    if ( this->options.dump_types ) {
        std::cout << "TYPES DUMP:\n";
        std::cout << "Declaration flags legend: " << (TxDeclarationFlags) 0xFFFF << std::endl;
        std::cout << "Public  pRotected  Virtual  eXternc  Abstract  Final  Override    "
                  << "Built-in  Implicit  Genparam  genBinding  Constructor  Initializer  Expected-error"
                  << std::endl;
        this->package->registry().dump_types();
        std::cout << "END TYPES DUMP\n";
    }

    if ( error_count )
        return 2;

    /*--- generate LLVM code ---*/

    if ( !this->options.no_codegen ) {
        int ret = this->llvm_compile( outputFileName );
        if ( ret )
            return 3;
    }

    return 0;
}

void TxDriver::compile_lexical() {
    auto prev_error_count = error_count;

    /*--- perform type definition pass - instantiation and integration ---*/

    for ( auto parserContext : this->parsedASTs ) {
        run_type_pass( parserContext->parsingUnit->module, "module" );
    }

    auto& reinterpretedASTs = this->package->registry().get_enqueued_specializations();
    unsigned specCount = reinterpretedASTs.size();

    for ( unsigned specIx = 0; specIx < specCount; specIx++ ) {
        auto node = reinterpretedASTs.at( specIx );
        run_type_pass( node, "reinterpretation" );
    }

    if ( this->package->registry().get_unintegrated_type_count() )
        this->package->registry().integrate_types();

    if ( error_count != prev_error_count ) {
        _LOG.error( "- Type definition pass encountered %d errors", error_count - prev_error_count );
        prev_error_count = error_count;
    }
    else
        _LOG.info( "+ Type definition pass OK" );


    /*--- perform resolution pass ---*/

    for ( auto parserContext : this->parsedASTs ) {
        run_resolution_pass( parserContext->parsingUnit->module, "module" );
    }
    for ( unsigned specIx = 0; specIx < specCount; specIx++ ) {
        auto node = reinterpretedASTs.at( specIx );
        run_resolution_pass( node, "reinterpretation" );
    }

    if ( error_count != prev_error_count ) {
        _LOG.error( "- Resolution pass encountered %d errors", error_count - prev_error_count );
        prev_error_count = error_count;
    }
    else {
        _LOG.info( "+ Resolution pass OK" );
    }


    /*--- perform type definition and full resolution passes on trailing types ---*/

    while ( specCount != reinterpretedASTs.size()) {
        unsigned nextSpecIx = specCount;
        specCount = reinterpretedASTs.size();

        for ( unsigned specIx = nextSpecIx; specIx < specCount; specIx++ ) {
            auto node = reinterpretedASTs.at( specIx );
            run_type_pass( node, "reinterpretation" );
        }

        if ( this->package->registry().get_unintegrated_type_count() )
            this->package->registry().integrate_types();

        for ( unsigned specIx = nextSpecIx; specIx < specCount; specIx++ ) {
            auto node = reinterpretedASTs.at( specIx );
            run_resolution_pass( node, "reinterpretation" );
        }
    }
    if ( this->package->registry().get_unintegrated_type_count() )
        this->package->registry().integrate_types( true );

    if ( error_count != prev_error_count ) {
        _LOG.error( "- Deferred resolution pass encountered %d errors", error_count - prev_error_count );
        prev_error_count = error_count;
    }
    else {
        _LOG.info( "+ Deferred resolution pass OK" );
    }


    /*--- perform verification pass ---*/

    for ( auto parserContext : this->parsedASTs ) {
        run_verification_pass( parserContext->parsingUnit->module, "module" );
    }
    for ( auto node : this->package->registry().get_enqueued_specializations()) {
        run_verification_pass( node, "reinterpretation" );
    }
    this->package->determine_main_func();
    if ( error_count != prev_error_count ) {
        _LOG.error( "- Verification pass encountered %d errors", error_count - prev_error_count );
        prev_error_count = error_count;
    }
    else {
        _LOG.info( "+ Verfication pass OK" );
    }
}

void TxDriver::compile_reachable() {
    // add main() and other possible external-linkage entities, this is the starting point for the reachable graph
    for ( auto decl : this->package->get_extlink_declarations() ) {
        this->add_reachable( decl->get_definer() );
    }
    {  // add built-ins referenced by hardcoded code-gen
        auto symbol = dynamic_cast<TxEntitySymbol*>( search_symbol( this->package, "tx.c.memcmp" ) );
        for ( auto fi = symbol->fields_cbegin(); fi != symbol->fields_cend(); fi++ )
            this->add_reachable( (*fi)->get_definer() );

        symbol = dynamic_cast<TxEntitySymbol*>( search_symbol( this->package, "tx.panic" ) );
        for ( auto fi = symbol->fields_cbegin(); fi != symbol->fields_cend(); fi++ )
            this->add_reachable( (*fi)->get_definer() );

        symbol = dynamic_cast<TxEntitySymbol*>( search_symbol( this->package, "tx.isa" ) );
        for ( auto fi = symbol->fields_cbegin(); fi != symbol->fields_cend(); fi++ )
            this->add_reachable( (*fi)->get_definer() );
    }
    {  // add built-ins that may have only been referenced implicitly, i.e. not by name
        auto symbol = dynamic_cast<TxEntitySymbol*>( search_symbol( this->package, "tx.Interface" ) );
        this->add_reachable( symbol->get_type_decl()->get_definer() );

        symbol = dynamic_cast<TxEntitySymbol*>( search_symbol( this->package, "tx.Tuple" ) );
        this->add_reachable( symbol->get_type_decl()->get_definer() );
    }
    {  // add ExpErr clauses
        for ( auto parserContext : this->parsedASTs ) {
            for ( auto n : parserContext->get_exp_error_nodes() ) {
                this->add_reachable( n );
            }
        }
    }

    /*--- resolve by traversing the reachable graph ---*/
    {
        auto prev_error_count = error_count;
        auto& reinterpretedASTs = this->package->registry().get_enqueued_specializations();
        unsigned specIx = 0;

        for ( unsigned i = 0; i < this->reachableASTsQueue.size(); i++ ) {
            TxDeclarationNode* reachedNode = reachableASTsQueue[i];

            run_type_pass( reachedNode, "reachable" );

            if ( this->package->registry().get_unintegrated_type_count() )
                this->package->registry().integrate_types();

            run_resolution_pass( reachedNode, "reachable" );

            for ( ; specIx < reinterpretedASTs.size(); specIx++ ) {
                auto specNode = reinterpretedASTs.at( specIx );
                this->add_reachable( specNode );
            }
        }
        if ( this->package->registry().get_unintegrated_type_count() )
            this->package->registry().integrate_types( true );

        if ( error_count != prev_error_count )
            _LOG.error( "- Resolution pass encountered %d errors", error_count - prev_error_count );
        else
            _LOG.info( "+ Resolution pass OK" );
    }

    /*--- perform verification pass ---*/
    {
        auto prev_error_count = error_count;
        for ( unsigned i = 0; i < this->reachableASTsQueue.size(); i++ ) {
            TxDeclarationNode* reachedNode = reachableASTsQueue[i];
            run_verification_pass( reachedNode, "reachable" );
        }
        this->package->determine_main_func();
        if ( error_count != prev_error_count )
            _LOG.error( "- Verification pass encountered %d errors", error_count - prev_error_count );
        else
            _LOG.info( "+ Verfication pass OK" );
    }
}

void TxDriver::add_reachable( TxNode* node ) {
    //unsigned indent = 0;
    for ( TxNode* n = node; n != nullptr; n = const_cast<TxNode*>( n->parent() ) ) {
        auto ret = this->reachableASTs.insert( n->get_node_id() );
        if ( !ret.second )
            return;
        else {
            //std::cerr << std::string( indent, ' ' ) << "Adding reachable: " << n << std::endl;  indent += 2;
            if ( auto dn = dynamic_cast<TxDeclarationNode*>( n ) ) {
                if ( dynamic_cast<const TxExpErrDeclNode*>( dn->parent() ) ) {
                    continue;
                }
                else if ( dynamic_cast<const TxModuleNode*>( dn->parent() ) ) {
                    // module-level declaration - add to queue
                    this->reachableASTsQueue.push_back( dn );
                    return;
                }
                else if ( dynamic_cast<TxModule*>( dn->get_declaration()->get_symbol()->get_outer() ) ) {
                    // module-level declaration - add to queue
                    ASSERT ( dynamic_cast<const TxInternalRootNode*>(dn->parent()),
                             "Quazi-orphan top level decl node for " << dn->get_declaration()->get_unique_full_name()
                             << ", decl node's parent is " << dn->parent() );
                    this->reachableASTsQueue.push_back( dn );
                    return;
                }
            }
        }
    }
    //ASSERT( false, "Orphan top level decl node for " << node );
}

bool TxDriver::add_import( const TxIdentifier& moduleName ) {
    if ( this->package->lookup_module( moduleName )) {
        this->_LOG.debug( "Skipping import of previously imported module: %s", moduleName.str().c_str());
        return true;
    }
    if ( moduleName.begins_with( BUILTIN_NS )) {  // so we won't search for built-in modules' sources
        this->_LOG.debug( "Skipping import of built-in namespace: %s", moduleName.str().c_str());
        return true;
    }
    // TODO: guard against or handle circular imports
    const std::string moduleFileName = moduleName.str() + ".tx";
    for ( auto& pathItem : this->options.moduleSearchPaths ) {
        if ( file_status( pathItem ) == 2 ) {
            // path item exists and is a directory

            // if a file name exists that exactly matches the module name, pick it
            // (the file is assumed to contain the whole module it if it's named 'module.name.tx')
            const std::string moduleFilePath( pathItem + get_path_separator() + moduleFileName );
            if ( file_status( moduleFilePath ) == 1 ) {
                this->add_source_file( moduleName, moduleFilePath );
                return true;
            }

            // if module dir exists, pick all .tx files in it
            std::string moduleDirPath = pathItem;
            for ( auto si = moduleName.segments_cbegin(); si != moduleName.segments_cend(); si++ ) {
                moduleDirPath += get_path_separator();
                moduleDirPath += *si;
            }
            if ( file_status( moduleDirPath ) == 2 ) {
                this->add_all_in_dir( moduleName, moduleDirPath, false );
                return true;
            }
        }
    }
    //this->LOG.error("Could not find source for module: %s", moduleName.to_string().c_str());
    return false;
}

int TxDriver::add_all_in_dir( const TxIdentifier& moduleName, const std::string& dirPath, bool recurseSubDirs ) {
    int addCount = 0;
    tinydir_dir dir;
    tinydir_open( &dir, dirPath.c_str());
    while ( dir.has_next ) {
        tinydir_file file;
        tinydir_readfile( &dir, &file );
        if ( file.is_dir ) {
            if ( recurseSubDirs ) {
                if ( !strchr( file.name, '.' )) {
                    TxIdentifier submodName( moduleName, file.name );
                    this->add_all_in_dir( submodName, file.path, true );
                }
            }
        }
        else if ( !strcmp( file.extension, "tx" )) {
            this->add_source_file( moduleName, file.path );
            addCount++;
        }
        tinydir_next( &dir );
    }
    tinydir_close( &dir );
    this->_LOG.debug( "Added %d source files to compilation from directory '%s'", addCount, dirPath.c_str());
    return addCount;
}

void TxDriver::add_source_file( const TxIdentifier& moduleName, const std::string& filePath ) {
    this->_LOG.debug( "Adding source file to compilation: '%s'", filePath.c_str());
    // TODO: verify that the source file actually contains the specified module
    this->sourceFileQueue.emplace_back( std::pair<TxIdentifier, std::string>( moduleName, filePath ));
}

int TxDriver::llvm_compile( const std::string& outputFilename ) {
    this->genContext->init_codegen();

    // initialize debug info generation:
    for ( auto parserContext : this->parsedASTs ) {
        parserContext->init_debug();
    }

    try {
        this->genContext->generate_runtime_type_info();
    }
    catch ( const codecheck_error& err ) {
        LOG_DEBUG(( &_LOG ), "Caught code check error generating runtime type info: " << err );
        return 1;
    }

    this->genContext->declare_builtin_code();

    int codegen_errors = 0;

    if ( this->options.compile_all_source ) {
        // generate the code for the program in lexical order:
        for ( auto parserContext : this->parsedASTs ) {
            codegen_errors += this->genContext->generate_code( parserContext->parsingUnit );
        }

        // generate the code for the type specializations that are defined by reinterpreted source:
        for ( auto specNode : this->package->registry().get_enqueued_specializations()) {
            _LOG.debug( "Generating code for enqueued specialization: %s", specNode->get_declaration()->str().c_str());
            codegen_errors += this->genContext->generate_code( specNode );
        }
    }
    else {
        // generate the code for the program by traversing the reachable graph:
        for ( auto reachableASTNode : this->reachableASTsQueue ) {
            try {
                reachableASTNode->code_gen( *this->genContext );
            }
            catch ( const codecheck_error& err ) {
                _LOG.warning( "Caught code check error in reachable node %s: %s", reachableASTNode->str().c_str(), err.what());
                codegen_errors++;
            }
        }
    }

    if ( codegen_errors ) {
        _LOG.error( "- LLVM code generation encountered %d errors", codegen_errors );
        return codegen_errors;
    }

    this->genContext->generate_builtin_code();
    this->genContext->generate_runtime_vtables();

    bool mainGenerated = false;
    if ( auto funcDecl = this->package->get_main_func()) {
        auto funcField = funcDecl->get_definer()->field();
        this->genContext->generate_main( funcDecl->get_unique_full_name(), funcField->qtype().type());
        mainGenerated = true;
        LOG_DEBUG( &_LOG, "Generated program entry for user main method " << funcDecl );
    }

    this->genContext->finalize_codegen();

    _LOG.info( "+ LLVM code generated (not yet written)" );

    if ( this->options.strip_debug )
        llvm::StripDebugInfo( this->genContext->llvmModule());

    if ( this->options.dump_ir )
        this->genContext->print_IR();

    int retCode = 0;
    if ( this->options.run_verifier ) {
        retCode = this->genContext->verify_code();
        if ( !retCode )
            _LOG.info( "+ LLVM code verification OK" );
        else
            return retCode;
    }

    if ( this->options.run_jit ) {
        if ( !mainGenerated )
            this->_LOG.error( "Can't run program, no main() method found." );
        else {
            this->genContext->run_code();
        }
    }

    if ( !this->options.no_bc_output ) {
        retCode = this->genContext->write_bitcode( outputFilename );
        _LOG.info( "+ Wrote bitcode file '%s'", outputFilename.c_str());
    }

    return retCode;
}


static Logger& _LOG = Logger::get( "DRIVER" );

const size_t chunk_size = 4000;  // FiXME: test this with small chunk size

/// consolidates chunks into a contiguous buffer
static char* consolidate( std::vector<char*>& bufChunks, char* lastChunk,
                          size_t fileSize, size_t lastReadLen) {
    auto buffer = (char*) malloc( sizeof( char ) * ( fileSize + 1 ));
    if ( !buffer ) {
        _LOG.fatal( "memory allocation failed" );
        for ( auto ch : bufChunks )
            free( ch );
        free( lastChunk );
        return nullptr;
    }
    auto dest = buffer;
    for ( auto ch : bufChunks ) {
        memcpy( dest, ch, chunk_size );
        free( ch );
        dest += chunk_size;
    }
    memcpy( dest, lastChunk, lastReadLen );
    free( lastChunk );
    return buffer;
}

static TxSourceBuffer load_file( FILE* file ) {
    char* buffer;
    std::vector<char*> bufChunks;
    size_t fileSize = 0;
    do {
        auto chunk = (char*) malloc( chunk_size + 1 );
        if ( !chunk ) {
            _LOG.fatal( "memory allocation failed" );
            for ( auto ch : bufChunks )
                free( ch );
            return TxSourceBuffer( nullptr );
        }
        // read the next chunk of the file:
        size_t readLen = fread( chunk, 1, chunk_size, file );
        fileSize += readLen;
        if ( feof( file )) {
            if ( bufChunks.empty()) {
                //_LOG.debug( "Loaded source file in single chunk");
                buffer = chunk;
            }
            else {
                _LOG.debug( "Loaded source file in %d chunks", bufChunks.size() + 1);
                buffer = consolidate( bufChunks, chunk, fileSize, readLen );
                if ( !buffer )
                    return TxSourceBuffer( nullptr );
            }
            buffer[fileSize] = '\0';  // append null terminator
            break;
        }
        else if ( readLen != chunk_size ) {
            _LOG.fatal( "File reading error, read %ld bytes, errno=%d", readLen, errno);
            for ( auto ch : bufChunks )
                free( ch );
            free( chunk );
            return TxSourceBuffer( nullptr );
        }
        bufChunks.push_back( chunk );
    } while ( true );

    return TxSourceBuffer( buffer );
}

static TxSourceBuffer load_file( const std::string& filePath ) {
    if ( filePath.empty() || filePath == "-" ) {
        return load_file( stdin );
    }
    else {
        if ( file_status( filePath ) != 1 ) {
            _LOG.fatal( "Source file name '%s' is not found or not a file.", filePath.c_str());
            return TxSourceBuffer( nullptr );
        }
        FILE* file = fopen( filePath.c_str(), "rb" );  // open in 'binary' i.e. without translations
        if ( !file ) {
            _LOG.fatal( "Could not open source file '%s': %s", filePath.c_str(), strerror(errno));
            return TxSourceBuffer( nullptr );
        }
        TxSourceBuffer srcBuffer = load_file( file );
        fclose( file );
        return srcBuffer;
    }
}
