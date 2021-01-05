#include <cstdio>
#include <cstring>

#include "util/logging.hpp"
#include "util/files_env.hpp"

#include "driver.hpp"

static Logger& LOG = Logger::get( "MAIN" );

int main( int argc, const char **argv )
          {
//    Logger::set_global_threshold(Level::ALL);
//    for (int lvl=Level::NONE; lvl < Level::ALL; lvl++)
//        LOG.log(Level(lvl), LEVEL_NAMES[lvl]);
//    Logger::set_global_threshold(Level::INFO);

    TxOptions options;
    std::vector<std::string> startSourceFiles;
    std::string outputFileName;
    bool separateJobs = false;

#ifdef DEVMODE
    // development mode default options:
    options.run_verifier = true;
    options.run_jit = true;
    options.no_bc_output = true;
    //options.compile_all_source = true;
#endif

    bool explicit_jit = false;
    bool explicit_bc = false;

    options.txPath = ".";

    // FUTURE: revamp; and pass digested options string in options struct
    for ( int a = 1; a < argc; a++ ) {
        if ( argv[a][0] == '-' && argv[a][1] ) {
            if ( !strcmp( argv[a], "-h" ) || !strcmp( argv[a], "-help" ) ) {
                printf( "Usage: %s [ <option> | <source file> ]*\n", argv[0] );
                printf( "  %-22s %s\n", "-h  | -help", "Print command line help and exit" );
                printf( "  %-22s %s\n", "-version", "Print version and exit" );
                printf( "  %-22s %s\n", "-v  | -verbose", "Verbose logging" );
                printf( "  %-22s %s\n", "-vv | -veryverbose", "Very verbose logging" );
                printf( "  %-22s %s\n", "-quiet", "Quiet, only log notes, alerts, warnings and errors" );
                printf( "  %-22s %s\n", "-vquiet", "Very quiet, only log warnings and errors" );
                printf( "  %-22s %s\n", "-nocol", "Disable color encoding in console output" );
                printf( "  %-22s %s\n", "-da", "Dump AST" );
                printf( "  %-22s %s\n", "-ds", "Dump symbol table" );
                printf( "  %-22s %s\n", "-dsx", "Dump full symbol table including built-in symbols" );
                printf( "  %-22s %s\n", "-dt", "Dump types" );
                printf( "  %-22s %s\n", "-di", "Dump intermediate representation (LLVM IR)" );
                printf( "  %-22s %s\n", "-dl", "Print debugging output from token scanner" );
                printf( "  %-22s %s\n", "-dy", "Print debugging output from grammar parser" );
                printf( "  %-22s %s\n", "-nodbg", "Strip debugging information from generated code" );
                printf( "  %-22s %s\n", "-dbg", "Include debugging information in generated code" );
                printf( "  %-22s %s\n", "-nover", "Disable verifying generated code after successful compilation (default if release build)" );
                printf( "  %-22s %s\n", "-ver", "Run generated code verifier after successful compilation" );
                printf( "  %-22s %s\n", "-nojit", "Disable running program in JIT mode after successful compilation (default if release build)" );
                printf( "  %-22s %s\n", "-jit", "Run program in JIT mode after successful compilation" );
                printf( "  %-22s %s\n", "-jo | -jitoptions", "Remaining options are passed to program run in JIT mode" );
                printf( "  %-22s %s\n", "-nobc", "Don't output bitcode (and if also running in JIT mode, exit with program's return code)" );
                printf( "  %-22s %s\n", "-bc", "Output bitcode file (default if release build)" );
                printf( "  %-22s %s\n", "-onlyparse", "Stop after grammar parse" );
                printf( "  %-22s %s\n", "-compileall", "Compile all source, rather than just the reachable graph" );
                printf( "  %-22s %s\n", "-sepjobs", "Compile each command line source file as a separate compilation job" );
                printf( "  %-22s %s\n", "-cnoassert", "Suppress code generation for assert statements" );
                // unofficial option  printf( "  %-22s %s\n", "-allowtx", "Permit source code to declare within the tx namespace" );
                printf( "  %-22s %s\n", "-notx", "Exclude the tx namespace source code (basic built-in definitions will still exist)" );
                printf( "  %-22s %s\n", "-tx <path>", "Location of the tx directory containing the tx namespace source code (default is .)" );
                printf( "  %-22s %s\n", "-o  | -output <file>", "Explicitly specify LLVM bitcode output file name" );
                printf( "  %-22s %s\n", "-sp <pathlist>", "Set source files search paths (overrides TUPLEX_PATH environment variable)" );
                printf( "  %-22s %s\n", "-sourcepath <pathlist>", "Set source files search paths (overrides TUPLEX_PATH environment variable)" );
                return 0;
            }
            else if ( !strcmp( argv[a], "-version" ) || !strcmp( argv[a], "--version" ) ) {
                printf( "%s\n", get_version_string().c_str() );
                return 0;
            }
            else if ( !strcmp( argv[a], "-v" ) || !strcmp( argv[a], "-verbose" ) )
                Logger::set_global_threshold( DEBUG );
            else if ( !strcmp( argv[a], "-vv" ) || !strcmp( argv[a], "-veryverbose" ) )
                Logger::set_global_threshold( ALL );
            else if ( !strcmp( argv[a], "-quiet" ) )
                Logger::set_global_threshold( NOTE );
            else if ( !strcmp( argv[a], "-vquiet" ) )
                Logger::set_global_threshold( WARN );
            else if ( !strcmp( argv[a], "-nocol" ) )
                Logger::set_colors_enabled( false );
            else if ( !strcmp( argv[a], "-da" ) )
                options.dump_ast = true;
            else if ( !strcmp( argv[a], "-ds" ) )
                options.dump_symbol_table = true;
            else if ( !strcmp( argv[a], "-dsx" ) )
                options.dump_symbol_table = options.dump_tx_symbols = true;
            else if ( !strcmp( argv[a], "-dt" ) )
                options.dump_types = true;
            else if ( !strcmp( argv[a], "-di" ) )
                options.dump_ir = true;
            else if ( !strcmp( argv[a], "-dl" ) )
                options.debug_scanner = true;
            else if ( !strcmp( argv[a], "-dy" ) )
                options.debug_parser = true;
            else if ( !strcmp( argv[a], "-dbg" ) )
                options.strip_debug = false;
            else if ( !strcmp( argv[a], "-nodbg" ) )
                options.strip_debug = true;
            else if ( !strcmp( argv[a], "-ver" ) )
                options.run_verifier = true;
            else if ( !strcmp( argv[a], "-nover" ) )
                options.run_verifier = false;
            else if ( !strcmp( argv[a], "-nojit" ) )
                options.run_jit = false;
            else if ( !strcmp( argv[a], "-jit" ) )
                explicit_jit = true;
            else if ( !strcmp( argv[a], "-nobc" ) )
                options.no_bc_output = true;
            else if ( !strcmp( argv[a], "-bc" ) )
                explicit_bc = true;
            else if ( !strcmp( argv[a], "-onlyparse" ) )
                options.only_parse = true;
            else if ( !strcmp( argv[a], "-compileall" ) )
                options.compile_all_source = true;
            else if ( !strcmp( argv[a], "-sepjobs" ) )
                separateJobs = true;
            else if ( !strcmp( argv[a], "-cnoassert" ) )
                options.suppress_asserts = true;
            else if ( !strcmp( argv[a], "-allowtx" ) )
                options.allow_tx = true;
            else if ( !strcmp( argv[a], "-notx" ) )
                options.txPath = "";
            else if ( !strcmp( argv[a], "-tx" ) ) {
                if ( ++a >= argc ) {
                    LOG.error( "Invalid command options, specified %s without subsequent argument", argv[a - 1] );
                    return 1;  // exits
                }
                options.txPath = argv[a];
            }
            else if ( !strcmp( argv[a], "-sp" ) || !strcmp( argv[a], "-sourcepath" ) ) {
                if ( ++a >= argc ) {
                    LOG.error( "Invalid command options, specified %s without subsequent argument", argv[a - 1] );
                    return 1;  // exits
                }
                options.sourceSearchPaths = get_path_list( argv[a] );
            }
            else if ( !strcmp( argv[a], "-o" ) || !strcmp( argv[a], "-output" ) ) {
                if ( ++a >= argc ) {
                    LOG.error( "Invalid command options, specified %s without subsequent argument", argv[a - 1] );
                    return 1;  // exits
                }
                outputFileName = argv[a];
            }
            else if ( !strcmp( argv[a], "-jo" ) || !strcmp( argv[a], "-jitoptions" ) ) {
                // remaining options are passed to jit'ed program
                options.jit_argc = argc - a - 1;
                options.jit_argv = &argv[a+1];
                break;
            }
            else {
                LOG.error( "No such option '%s' (use -h or -help to print command line usage)", argv[a] );
                return 1;  // exits
            }
        }
        else {
            startSourceFiles.emplace_back( argv[a] );
        }
    }

    if ( options.dump_ast && !options.compile_all_source ) {
        LOG.alert("Dumping AST currently requires all source to be compiled - enabling compileall option.");
        options.compile_all_source = true;
    }
    if ( explicit_jit )
        options.run_jit = true;
    if ( explicit_bc )
        options.no_bc_output = false;
    if ( options.run_jit && !options.no_bc_output ) {
        if ( explicit_jit && explicit_bc ) {
            LOG.warning("Specified both JIT and BC generation, this is unstable: supressing BC generation.");
            options.no_bc_output = true;
        }
        else if ( explicit_jit )
            options.no_bc_output = true;
        else
            options.run_jit = false;
    }

    if ( options.allow_tx )
        LOG.warning("Compiler set to allow declaration in and extension of built-in namespace (tx) from user code." );

    if ( startSourceFiles.empty() ) {
        startSourceFiles.emplace_back( "-" );  // this will read source from stdin
        // (will also write output to stdout unless an output filename has been specified)
    }

    if ( options.sourceSearchPaths.empty() )
        options.sourceSearchPaths = get_path_list( get_environment_variable( "TUPLEX_PATH" ) );
    if ( options.sourceSearchPaths.empty() )
        options.sourceSearchPaths.emplace_back("." );  // if no search paths provided, the current directory is searched

    if ( separateJobs ) {
        // TODO: by default strip directory of outputFileName (write it to current directory unless output dir specified)
        if ( !outputFileName.empty() && outputFileName != "-" )
            LOG.info( "Since compiling as separate jobs, specified output file name '%s' will be used as path prefix", outputFileName.c_str() );
        int ret = 0;
        for ( auto & sourceFile : startSourceFiles ) {
            std::string tmpOutputFileName;
            if ( outputFileName != "-" ) {
                tmpOutputFileName = outputFileName + startSourceFiles.front();
                if ( tmpOutputFileName.length() >= 3 && tmpOutputFileName.substr( tmpOutputFileName.length() - 3 ) == ".tx" )
                    tmpOutputFileName.replace( tmpOutputFileName.length() - 2, 2, "bc" );
                else
                    tmpOutputFileName.append( ".bc" );
            }
            else
                tmpOutputFileName = outputFileName;

            TxDriver driver( options );
            ret = driver.compile( { sourceFile }, tmpOutputFileName );
            if ( ret )
                LOG.error( "Completed compilation job '%s' with return code %d", sourceFile.c_str(), ret );
            else
                LOG.info( "Completed compilation job '%s' with return code %d", sourceFile.c_str(), ret );
        }
        return ret;
    }
    else {
        if ( outputFileName.empty() ) {
            outputFileName = startSourceFiles.front();
            if ( outputFileName != "-" ) {   // = if not piping from stdin to stdout
                // Use first source file's base name as output file's base name.
                // Strip the directory of the file name to write to current directory.
                outputFileName = get_file_name( outputFileName );
                if ( outputFileName.length() >= 3 && outputFileName.substr( outputFileName.length() - 3 ) == ".tx" )
                    outputFileName.replace( outputFileName.length() - 2, 2, "bc" );
                else
                    outputFileName.append( ".bc" );
            }
        }
        else if ( outputFileName != "-" && file_status( outputFileName ) == 2 ) {
            // outputFileName is specified, exists, and is a directory
            if ( outputFileName.back() != get_path_separator() )
                outputFileName.push_back( get_path_separator() );
            outputFileName.append( get_file_name( startSourceFiles.front() ) );
            if ( outputFileName.length() >= 3 && outputFileName.substr( outputFileName.length() - 3 ) == ".tx" )
                outputFileName.replace( outputFileName.length() - 2, 2, "bc" );
            else
                outputFileName.append( ".bc" );
        }

        TxDriver driver( options );
        int ret = driver.compile( startSourceFiles, outputFileName );
        return ret;
    }
}
