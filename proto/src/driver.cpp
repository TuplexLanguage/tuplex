#include <cstdlib>
#include <string>

#include "tinydir/tinydir.h"

#include "util/util.hpp"
#include "util/assert.hpp"
#include "util/files_env.hpp"

#include "driver.hpp"

#include "builtin/builtin_types.hpp"
#include "llvm_generator.hpp"

#include "tx_lang_defs.hpp"
#include "parser.hpp"

extern FILE * yyin;
extern void yyrestart ( FILE *input_file );
extern int yy_flex_debug;
typedef struct yy_buffer_state *YY_BUFFER_STATE;
YY_BUFFER_STATE yy_scan_string( const char *yy_str );
void yy_delete_buffer (YY_BUFFER_STATE b );


const char *CORE_TX_SOURCE_STR =
    // #include "lang.tx"
    R"=====(
module tx;

/** Returns TRUE if v has tid among its supertypes / -interfaces. */
isa( valueRef : &Any, tid : UInt )->Bool {
    return isa( _typeid( valueRef ), tid );
}

isa( valueTid : UInt, tid : UInt )->Bool {
    supers : &Array<UInt> = _supertypes( valueTid );
    lower := ~ 0UI;
    upper := ~ supers.L;
    while lower != upper {
        pos := ( lower + upper ) / 2;
        st := supers[pos];
        if st == tid:
            return TRUE;
        else if st > tid:
            upper = pos;
        else:
            lower = pos + 1;
    }
    return FALSE;
}

builtin type Function derives Any
{
    override equals( other : &Any ) -> Bool {
        if other is o : &Self {
            return self^ == o^;
        }
        return FALSE;
    }
}
    )====="
;


TxDriver::TxDriver( const TxOptions& options )
        : _LOG( Logger::get( "DRIVER" ) ), options( options ),
          builtinParserContext( new TxParserContext( *this, TxIdentifier( "" ), "", TxParserContext::BUILTINS ) ),
          llvmContext( new llvm::LLVMContext() )
{
}

TxDriver::~TxDriver() {
}

int TxDriver::scan_begin( const std::string &filePath ) {
    if ( filePath.empty() || filePath == "-" )
        yyin = stdin;
    else if ( !( yyin = fopen( filePath.c_str(), "r" ) ) ) {
        _LOG.error( "Could not open source file '%s': %s", filePath.c_str(), strerror( errno ) );
        return 1;
    }
    yyrestart( yyin );
    yy_flex_debug = this->options.debug_lexer;
    _LOG.info( "+ Opened file for parsing: '%s'", filePath.c_str() );
    return 0;
}

void TxDriver::scan_end() {
    fclose( yyin );
}

int TxDriver::parse( TxParserContext& parserContext ) {
    int ret = scan_begin( *parserContext.current_input_filepath() );
    if ( ret ) {
        return ret;
    }

    //if (this->options.only_scan)  // currently unsupported
    //    return test_scanner();

    yy::TxParser parser( &parserContext );
    parser.set_debug_level( this->options.debug_parser );
    ret = parser.parse();

    scan_end();
    return ret;
}

int TxDriver::compile( const std::vector<std::string>& startSourceFiles, const std::string& outputFileName ) {
    ASSERT( this->parsedSourceFiles.empty(), "Can only run driver instance once" );

    if ( startSourceFiles.empty() ) {
        this->_LOG.fatal( "No source specified." );
        return 1;
    }

    this->package = make_root_package( this->builtinParserContext );

    // ONLY used for constant evaluation before code generation pass:
    this->genContext = new LlvmGenerationContext( *this->package, *this->llvmContext );

    if ( options.sourceSearchPaths.empty() )
        this->_LOG.config( "Source search path is empty" );
    else
        for ( auto pathItem : options.sourceSearchPaths )
            this->_LOG.config( "Source search path item: '%s'", pathItem.c_str() );

    /*--- prepare the parse units ---*/

    {  // initialize the built-in ASTs
        TxParserContext* parserContext = this->builtinParserContext;
        parserContext->parsingUnit = this->package->builtins().createTxModuleAST();
        this->parsedASTs.push_back( parserContext );
    }

    {  // parse the built-in source:
        TxParserContext* parserContext = new TxParserContext( *this, TxIdentifier( "" ), "", TxParserContext::BUILTINS );
        auto memBuffer = yy_scan_string( CORE_TX_SOURCE_STR );
        yy_flex_debug = this->options.debug_lexer;
        yy::TxParser parser( parserContext );
        int ret = parser.parse();
        yy_delete_buffer( memBuffer );
        if ( ret ) {
            _LOG.fatal( "Exiting due to unrecovered syntax error" );
            return ret;
        }
        ASSERT( parserContext->parsingUnit, "parsingUnit not set by parser" );
        this->parsedASTs.push_back( parserContext );
    }

    if ( !this->options.txPath.empty() ) {
        // add the tx namespace sources
        std::string txPath( this->options.txPath );
        this->_LOG.config( "Including tx namespace source path '%s'", txPath.c_str() );
        if ( !is_path_separator( txPath.back() ) )
            txPath.push_back( get_path_separator() );
        txPath.append( BUILTIN_NS );
        this->add_all_in_dir( BUILTIN_NS, txPath, true );
    }

    for ( auto startFile : startSourceFiles )
        this->sourceFileQueue.push_back( std::pair<TxIdentifier, std::string>( TxIdentifier(), startFile ) );

    /*--- parse all source filed (during parsing, files are added to the queue as the are imported) ---*/

    TxParserContext::ParseInputSourceSet pfs = TxParserContext::TX_SOURCES;
    while ( !this->sourceFileQueue.empty() ) {
        TxIdentifier moduleName = this->sourceFileQueue.front().first;  // note, may be empty

        if ( pfs == TxParserContext::TX_SOURCES ) {
            if ( !moduleName.begins_with( BUILTIN_NS ) )  // if first user source processed
                pfs = TxParserContext::FIRST_USER_SOURCE;
        }
        else if ( pfs == TxParserContext::FIRST_USER_SOURCE ) {
            pfs = TxParserContext::REST_USER_SOURCES;
        }

        std::string nextFilePath = this->sourceFileQueue.front().second;
        if ( !this->parsedSourceFiles.count( nextFilePath ) ) {  // if not already parsed
            TxParserContext* parserContext = new TxParserContext( *this, moduleName, nextFilePath, pfs );
            int ret = this->parse( *parserContext );
            if ( ret ) {
                if ( ret == 1 )  // syntax error
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
        this->sourceFileQueue.pop_front();
    }

    if ( error_count )
        _LOG.error( "- Grammar parse encountered %d errors", error_count );
    else
        _LOG.info( "+ Grammar parse OK" );
    if ( this->options.only_parse )
        return error_count;
    int prev_error_count = error_count;

    /*--- perform declaration pass ---*/

    for ( auto parserContext : this->parsedASTs ) {
        // by processing root node here we avoid root checking in visitor implementation
        parserContext->parsingUnit->set_context( this->package );
        run_declaration_pass( parserContext->parsingUnit->module, parserContext->parsingUnit, "module" );
    }

    this->package->builtins().resolveBuiltinSymbols();  // TODO: review, maybe remove

    this->package->prepare_modules();  // (prepares the declared imports)

    if ( error_count != prev_error_count ) {
        _LOG.error( "- Declaration pass encountered %d errors", error_count - prev_error_count );
        prev_error_count = error_count;
    }
    else
        _LOG.info( "+ Declaration pass OK" );


    /*--- perform type definition pass - instantiation and integration ---*/

    for ( auto parserContext : this->parsedASTs ) {
        run_type_pass( parserContext->parsingUnit->module, "module" );
    }

    auto & reinterpretedASTs = this->package->registry().get_enqueued_specializations();
    unsigned nextSpecIx = 0;
    unsigned specCount = reinterpretedASTs.size();

    for ( unsigned specIx = nextSpecIx; specIx < specCount; specIx++ ) {
        auto node = reinterpretedASTs.at( specIx );
        run_type_pass( node, "reinterpretation" );
    }

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
    for ( unsigned specIx = nextSpecIx; specIx < specCount; specIx++ ) {
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

    while ( specCount != reinterpretedASTs.size() ) {
        nextSpecIx = specCount;
        specCount = reinterpretedASTs.size();

        for ( unsigned specIx = nextSpecIx; specIx < specCount; specIx++ ) {
            auto node = reinterpretedASTs.at( specIx );
            run_type_pass( node, "reinterpretation" );
        }

        this->package->registry().integrate_types();

        for ( unsigned specIx = nextSpecIx; specIx < specCount; specIx++ ) {
            auto node = reinterpretedASTs.at( specIx );
            run_resolution_pass( node, "reinterpretation" );
        }
    }
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
    for ( auto node : this->package->registry().get_enqueued_specializations() ) {
        run_verification_pass( node, "reinterpretation" );
    }

    if ( error_count != prev_error_count ) {
        _LOG.error( "- Verification pass encountered %d errors", error_count - prev_error_count );
        prev_error_count = error_count;
    }
    else {
        _LOG.info( "+ Verfication pass OK" );
    }


    /*--- type preparation / layout pass ---*/

    {
        this->package->registry().prepare_types();

        for ( auto parserContext : this->parsedASTs ) {
            parserContext->finalize_expected_error_clauses();
        }

        if ( error_count == prev_error_count ) {
            _LOG.info( "+ Type preparation pass OK" );
            prev_error_count = error_count;
        }
        else
            _LOG.error( "- Type preparation pass encountered %d errors", error_count - prev_error_count );
    }

    _LOG.info( "Number of AST nodes created: %u", TxNode::nextNodeId );

    if ( this->options.dump_ast ) {
        auto visitorFunc = []( const TxNode* node, const AstCursor& parent, const std::string& role, void* ctx ) {
            char buf[256];
            snprintf( buf, 256, "%*s%s", parent.depth*2, "", role.c_str() );
            printf( "%-50s %s\n", buf, node->str().c_str() );
            //std::cout << std::string( parent.depth*2, ' ' ) << role << " " << node->to_string() << std::endl;
            };
        AstVisitor visitor = { visitorFunc, nullptr };

        for ( auto parserContext : this->parsedASTs ) {
            std::cout << "AST DUMP " << parserContext << ":" << std::endl;
            parserContext->parsingUnit->visit_ast( visitor, nullptr );
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
        this->package->registry().dump_types();
        std::cout << "END TYPES DUMP\n";
    }

    if ( error_count )
        return 2;

    /*--- generate LLVM code ---*/

    int ret = this->llvm_compile( outputFileName );
    if ( ret )
        return 3;

    return 0;
}

bool TxDriver::add_import( const TxIdentifier& moduleName ) {
    if ( this->package->lookup_module( moduleName ) ) {
        this->_LOG.debug( "Skipping import of previously imported module: %s", moduleName.str().c_str() );
        return true;
    }
    if ( moduleName.begins_with( BUILTIN_NS ) ) {  // so we won't search for built-in modules' sources
        this->_LOG.debug( "Skipping import of built-in namespace: %s", moduleName.str().c_str() );
        return true;
    }
    // TODO: guard against or handle circular imports
    const std::string moduleFileName = moduleName.str() + ".tx";
    for ( auto pathItem : this->options.sourceSearchPaths ) {
        if ( file_status( pathItem ) == 2 ) {
            // path item exists and is a directory

            // if a file name exists that exactly matches the module name, pick it
            // (the file is assumed to contain the whole module it if it's named 'module.name.tx')
            std::string moduleFilePath = pathItem + get_path_separator() + moduleFileName;
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

int TxDriver::add_all_in_dir( const TxIdentifier& moduleName, const std::string &dirPath, bool recurseSubDirs ) {
    int addCount = 0;
    tinydir_dir dir;
    tinydir_open( &dir, dirPath.c_str() );
    while ( dir.has_next ) {
        tinydir_file file;
        tinydir_readfile( &dir, &file );
        if ( file.is_dir ) {
            if ( recurseSubDirs ) {
                if ( !strchr( file.name, '.' ) ) {
                    TxIdentifier submodName( moduleName, file.name );
                    this->add_all_in_dir( submodName, file.path, true );
                }
            }
        }
        else if ( !strcmp( file.extension, "tx" ) ) {
            this->add_source_file( moduleName, file.path );
            addCount++;
        }
        tinydir_next( &dir );
    }
    tinydir_close( &dir );
    this->_LOG.debug( "Added %d source files to compilation from directory '%s'", addCount, dirPath.c_str() );
    return addCount;
}

void TxDriver::add_source_file( const TxIdentifier& moduleName, const std::string &filePath ) {
    this->_LOG.debug( "Adding source file to compilation: '%s'", filePath.c_str() );
    // TODO: verify that the source file actually contains the specified module
    this->sourceFileQueue.push_back( std::pair<TxIdentifier, std::string>( moduleName, filePath ) );
}

int TxDriver::llvm_compile( const std::string& outputFileName ) {
    try {
        this->genContext->generate_runtime_type_info();
    }
    catch ( const codecheck_error& err ) {
        LOG_DEBUG( (&_LOG), "Caught code check error generating runtime type info: " << err );
        return 1;
    }

    this->genContext->declare_builtin_code();

    int codegen_errors = 0;

    // generate the code for the program in lexical order:
    for ( auto parserContext : this->parsedASTs ) {
        codegen_errors += this->genContext->generate_code( parserContext->parsingUnit );
    }

    // generate the code for the type specializations that are defined by reinterpreted source:
    for ( auto specNode : this->package->registry().get_enqueued_specializations() ) {
        _LOG.debug( "Generating code for enqueued specialization: %s", specNode->get_declaration()->str().c_str() );
        codegen_errors += this->genContext->generate_code( specNode );
    }

    if ( codegen_errors ) {
        _LOG.error( "- LLVM code generation encountered %d errors", codegen_errors );
        return codegen_errors;
    }

    this->genContext->generate_builtin_code();
    this->genContext->generate_runtime_vtables();

    bool mainGenerated = false;
    if ( auto funcDecl = this->package->getMainFunc() ) {
        auto funcField = funcDecl->get_definer()->resolve_field();
        if ( funcField->qtype()->get_type_class() == TXTC_FUNCTION ) {
            auto retType = static_cast<const TxFunctionType*>(funcField->qtype().type())->return_type();
            if ( retType->get_type_class() != TXTC_VOID
                 && !retType->is_a( *this->package->registry().get_builtin_type( TXBT_INTEGER ) ) )
                this->_LOG.error( "main() method had invalid return type: %s", retType->str().c_str() );
            else if ( ( mainGenerated = this->genContext->generate_main( funcDecl->get_unique_full_name(), funcField->qtype().type() ) ) )
                this->_LOG.debug( "Created program entry for user method %s", funcDecl->get_unique_full_name().c_str() );
        }
    }
    _LOG.info( "+ LLVM code generated (not yet written)" );

    this->genContext->initialize_target();

    if ( this->options.dump_ir )
        this->genContext->print_IR();

    int retCode = 0;
    if ( this->options.run_verifier ) {
        retCode = this->genContext->verify_code();
        if ( !retCode )
            _LOG.info( "+ LLVM code verification OK" );
        //else
        //    return retCode;
    }

    if ( this->options.run_jit ) {
        if ( !mainGenerated )
            this->_LOG.error( "Can't run program, no main() method found." );
        else {
            this->genContext->run_code();
        }
    }

    if ( !this->options.no_bc_output ) {
        retCode = this->genContext->write_bitcode( outputFileName );
        _LOG.info( "+ Wrote bitcode file '%s'", outputFileName.c_str() );
    }

    return retCode;
}
