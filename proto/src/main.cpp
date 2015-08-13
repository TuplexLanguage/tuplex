#include <stdio.h>
#include <string.h>
#include <iostream>

#include "driver.hpp"
#include "logging.hpp"
#include "files_env.hpp"

#include "TuplexConfig.h"


static Logger& LOG = Logger::get("MAIN");

int main(int argc, char **argv)
{
//    Logger::set_global_threshold(Level::ALL);
//    for (int lvl=Level::NONE; lvl < Level::ALL; lvl++)
//        LOG.log(Level(lvl), LEVEL_NAMES[lvl]);
//    Logger::set_global_threshold(Level::INFO);

    TxOptions options;

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
                printf("  %-22s %s\n", "-quiet", "Quiet, only log warnings and errors");
                printf("  %-22s %s\n", "-nocol", "Disable color encoding in console output");
                printf("  %-22s %s\n", "-ds", "Dump symbol table");
                printf("  %-22s %s\n", "-di", "Dump intermediate representation (LLVM IR)");
                printf("  %-22s %s\n", "-dl", "Print debugging output from lexer (token scanner)");
                printf("  %-22s %s\n", "-dy", "Print debugging output from grammar parser");
                printf("  %-22s %s\n", "-nover", "Disable verifying generated code after successful compilation");
                printf("  %-22s %s\n", "-nojit", "Disable running program in JIT mode after successful compilation");
                printf("  %-22s %s\n", "-jit", "Run program in JIT mode after successful compilation");
                printf("  %-22s %s\n", "-nobc", "Don't output bitcode (and if also running in JIT mode, exit with program's return code)");
                printf("  %-22s %s\n", "-onlyparse", "Stop after grammar parse");
                printf("  %-22s %s\n", "-cnoassert", "Suppress code generation for assert statements");
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
                Logger::set_global_threshold(WARN);
            else if (! strcmp(argv[a], "-nocol"))
                Logger::set_colors_enabled(false);
            else if (! strcmp(argv[a], "-ds"))
                options.dump_symbol_table = true;
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
            else if (! strcmp(argv[a], "-onlyparse"))
                options.only_parse = true;
            else if (! strcmp(argv[a], "-cnoassert"))
                options.suppress_asserts = true;
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
                options.outputFileName = argv[a];
            }
            else {
                LOG.error("Unknown option '%s'", argv[a]);
                return 1;  // exits
            }
        }
        else {
            options.startSourceFiles.push_back(argv[a]);
        }
    }

    if (options.outputFileName.empty()) {
        if (options.startSourceFiles.empty() || options.startSourceFiles.front() == "-")
            options.outputFileName = "-";  // this will write to stdout
        else {
            options.outputFileName = options.startSourceFiles.front();
            if (options.outputFileName.length() >= 3 && options.outputFileName.substr(options.outputFileName.length()-3) == ".tx")
                options.outputFileName.replace(options.outputFileName.length()-2, 2, "bc");
            else
                options.outputFileName.append(".bc");
        }
    }

    if (options.startSourceFiles.empty()) {
        options.startSourceFiles.push_back("-");  // this will read source from stdin
    }

    if (options.sourceSearchPaths.empty())
        options.sourceSearchPaths = get_path_list(get_environment_variable("TUPLEX_PATH"));
        if (options.sourceSearchPaths.empty())
            options.sourceSearchPaths.push_back(".");  // if no search paths provided, the current directory is searched

    TxDriver driver(options);
    int ret = driver.compile();
    return ret;
}
