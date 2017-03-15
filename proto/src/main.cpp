#include <stdio.h>
#include <string.h>
#include <iostream>

#include "util/logging.hpp"
#include "util/files_env.hpp"

#include "driver.hpp"

#include "TuplexConfig.h"


static Logger& LOG = Logger::get("MAIN");

int main(int argc, char **argv)
{
//    Logger::set_global_threshold(Level::ALL);
//    for (int lvl=Level::NONE; lvl < Level::ALL; lvl++)
//        LOG.log(Level(lvl), LEVEL_NAMES[lvl]);
//    Logger::set_global_threshold(Level::INFO);

    TxOptions options;
    std::vector<std::string> startSourceFiles;
    std::string outputFileName;
    bool separateJobs = false;

#ifndef NDEBUG
    // development mode default options:
    options.run_verifier = true;
    options.run_jit = true;
    options.no_bc_output = true;
#endif

    for (int a = 1; a < argc; a++) {
        if (argv[a][0] == '-' && argv[a][1]) {
            if (! strcmp(argv[a], "-version")) {
                printf("%s version %d.%d", argv[0], Tuplex_VERSION_MAJOR, Tuplex_VERSION_MINOR);
                return 0;
            }
            else if (! strcmp(argv[a], "-h") || ! strcmp(argv[a], "-help")) {
                printf("Usage: %s [ <option> | <source file> ]*\n", argv[0]);
                printf("  %-22s %s\n", "-h  | -help", "Print command line help and exit");
                printf("  %-22s %s\n", "-version", "Print version and exit");
                printf("  %-22s %s\n", "-v  | -verbose", "Verbose logging");
                printf("  %-22s %s\n", "-vv | -veryverbose", "Very verbose logging");
                printf("  %-22s %s\n", "-quiet", "Quiet, only log notes, alerts, warnings and errors");
                printf("  %-22s %s\n", "-vquiet", "Very quiet, only log warnings and errors");
                printf("  %-22s %s\n", "-nocol", "Disable color encoding in console output");
                printf("  %-22s %s\n", "-da", "Dump AST");
                printf("  %-22s %s\n", "-ds", "Dump symbol table");
                printf("  %-22s %s\n", "-dsx", "Dump full symbol table including built-in symbols");
                printf("  %-22s %s\n", "-di", "Dump intermediate representation (LLVM IR)");
                printf("  %-22s %s\n", "-dl", "Print debugging output from lexer (token scanner)");
                printf("  %-22s %s\n", "-dy", "Print debugging output from grammar parser");
                printf("  %-22s %s\n", "-nover", "Disable verifying generated code after successful compilation (default if release build)");
                printf("  %-22s %s\n", "-ver", "Run generated code verifier after successful compilation");
                printf("  %-22s %s\n", "-nojit", "Disable running program in JIT mode after successful compilation (default if release build)");
                printf("  %-22s %s\n", "-jit", "Run program in JIT mode after successful compilation");
                printf("  %-22s %s\n", "-nobc", "Don't output bitcode (and if also running in JIT mode, exit with program's return code)");
                printf("  %-22s %s\n", "-bc", "Output bitcode file (default if release build)");
                printf("  %-22s %s\n", "-onlyparse", "Stop after grammar parse");
                printf("  %-22s %s\n", "-sepjobs", "Compile each command line source file as a separate compilation job");
                printf("  %-22s %s\n", "-cnoassert", "Suppress code generation for assert statements");
                printf("  %-22s %s\n", "-allowtx", "Permit source code to declare within the tx namespace");
                printf("  %-22s %s\n", "-o  | -output <file>", "Explicitly specify LLVM bitcode output file name");
                printf("  %-22s %s\n", "-sp <pathlist>", "Set source files search paths (overrides TUPLEX_PATH environment variable)");
                printf("  %-22s %s\n", "-sourcepath <pathlist>", "Set source files search paths (overrides TUPLEX_PATH environment variable)");
                return 0;
            }
            else if (! strcmp(argv[a], "-v") || ! strcmp(argv[a], "-verbose"))
                Logger::set_global_threshold(DEBUG);
            else if (! strcmp(argv[a], "-vv") || ! strcmp(argv[a], "-veryverbose"))
                Logger::set_global_threshold(ALL);
            else if (! strcmp(argv[a], "-quiet"))
                Logger::set_global_threshold(NOTE);
            else if (! strcmp(argv[a], "-vquiet"))
                Logger::set_global_threshold(WARN);
            else if (! strcmp(argv[a], "-nocol"))
                Logger::set_colors_enabled(false);
            else if (! strcmp(argv[a], "-da"))
                options.dump_ast = true;
            else if (! strcmp(argv[a], "-ds"))
                options.dump_symbol_table = true;
            else if (! strcmp(argv[a], "-dsx"))
                options.dump_symbol_table = options.dump_tx_symbols = true;
            else if (! strcmp(argv[a], "-di"))
                options.dump_ir = true;
            else if (! strcmp(argv[a], "-dl"))
                options.debug_lexer = true;
            else if (! strcmp(argv[a], "-dy"))
                options.debug_parser = true;
            else if (! strcmp(argv[a], "-nover"))
                options.run_verifier = false;
            else if (! strcmp(argv[a], "-nojit"))
                options.run_jit = false;
            else if (! strcmp(argv[a], "-jit"))
                options.run_jit = true;
            else if (! strcmp(argv[a], "-nobc"))
                options.no_bc_output = true;
            else if (! strcmp(argv[a], "-bc"))
                options.no_bc_output = false;
            else if (! strcmp(argv[a], "-onlyparse"))
                options.only_parse = true;
            else if (! strcmp(argv[a], "-sepjobs"))
                separateJobs = true;
            else if (! strcmp(argv[a], "-cnoassert"))
                options.suppress_asserts = true;
            else if (! strcmp(argv[a], "-allowtx"))
                options.allow_tx = true;
            else if (! strcmp(argv[a], "-sp") || ! strcmp(argv[a], "-sourcepath")) {
                if (++a >= argc) {
                    LOG.error("Invalid command options, specified %s without subsequent argument", argv[a-1]);
                    return 1;  // exits
                }
                options.sourceSearchPaths = get_path_list(argv[a]);
            }
            else if (! strcmp(argv[a], "-o") || ! strcmp(argv[a], "-output")) {
                if (++a >= argc) {
                    LOG.error("Invalid command options, specified %s without subsequent argument", argv[a-1]);
                    return 1;  // exits
                }
                outputFileName = argv[a];
            }
            else {
                LOG.error("No such option '%s' (use -h or -help to print command line usage)", argv[a]);
                return 1;  // exits
            }
        }
        else {
            startSourceFiles.push_back(argv[a]);
        }
    }

    if (startSourceFiles.empty()) {
        startSourceFiles.push_back("-");  // this will read source from stdin
        // (will also write output to stdout unless an output filename has been specified)
    }

    if (options.sourceSearchPaths.empty())
        options.sourceSearchPaths = get_path_list(get_environment_variable("TUPLEX_PATH"));
        if (options.sourceSearchPaths.empty())
            options.sourceSearchPaths.push_back(".");  // if no search paths provided, the current directory is searched

    if (separateJobs) {
        if (! outputFileName.empty() && outputFileName != "-")
            LOG.info("Since compiling as separate jobs, specified output file name '%s' will be used as path prefix", outputFileName.c_str());
        int ret = 0;
        for (auto & sourceFile : startSourceFiles) {
            std::string tmpOutputFileName;
            if (outputFileName != "-") {
                tmpOutputFileName = outputFileName + startSourceFiles.front();
                if (tmpOutputFileName.length() >= 3 && tmpOutputFileName.substr(tmpOutputFileName.length()-3) == ".tx")
                    tmpOutputFileName.replace(tmpOutputFileName.length()-2, 2, "bc");
                else
                    tmpOutputFileName.append(".bc");
            }
            else
                tmpOutputFileName = outputFileName;

            TxDriver driver(options);
            ret = driver.compile( { sourceFile }, tmpOutputFileName );
            if (ret)
                LOG.error("Completed compilation job '%s' with return code %d", sourceFile.c_str(), ret);
            else
                LOG.info("Completed compilation job '%s' with return code %d", sourceFile.c_str(), ret);
        }
        return ret;
    }
    else {
        if (outputFileName.empty()) {
            outputFileName = startSourceFiles.front();
            if (outputFileName != "-") {
                if (outputFileName.length() >= 3 && outputFileName.substr(outputFileName.length()-3) == ".tx")
                    outputFileName.replace(outputFileName.length()-2, 2, "bc");
                else
                    outputFileName.append(".bc");
            }
        }

        TxDriver driver(options);
        int ret = driver.compile(startSourceFiles, outputFileName);
        return ret;
    }
}
