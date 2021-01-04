#pragma once

#include <string>
#include <vector>


/** Represents Tuplex compilation run-time options. */
class TxOptions {
public:
    bool only_parse = false;
    bool compile_all_source = false;
    bool debug_scanner = false;
    bool debug_parser = false;
    bool dump_ast = false;
    bool dump_symbol_table = false;
    bool dump_tx_symbols = false;
    bool dump_types = false;
    bool dump_ir = false;
    bool strip_debug = false;
    bool run_verifier = false;
    bool run_jit = false;
    bool no_bc_output = false;
    bool suppress_asserts = false;
    bool allow_tx = false;
    std::string txPath;
    std::vector<std::string> sourceSearchPaths;
    int jit_argc = 0;
    const char** jit_argv = nullptr;
};
