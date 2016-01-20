#include <cstdlib>
#include <string>

#include "driver.hpp"
#include "assert.hpp"
#include "files_env.hpp"
#include "tinydir/tinydir.h"

#include "tx_lang_defs.hpp"
#include "llvm_generator.hpp"

#include "parser.hpp"


extern FILE * yyin;
extern int yy_flex_debug;



TxDriver::TxDriver(const TxOptions& options)
        : LOG(Logger::get("DRIVER")), options(options)  {
    if (options.sourceSearchPaths.empty())
        this->LOG.config("Tuplex source search path is empty");
    else for (auto pathItem : options.sourceSearchPaths)
        this->LOG.config("Tuplex source search path item: '%s'", pathItem.c_str());

    this->package = new TxPackage(*this);
}

TxDriver::~TxDriver() {
    delete this->package;
    // FUTURE: free the symbol tables and the ASTs
}


int TxDriver::scan_begin(const std::string &filePath) {
    // FUTURE: make parser not save *pointer* to filename, necessitating this leaky snippet:
    this->currentInputFilename = new std::string(filePath);
    yy_flex_debug = this->options.debug_lexer;
    if (filePath.empty() || filePath == "-")
        yyin = stdin;
    else if (!(yyin = fopen(filePath.c_str(), "r"))) {
        LOG.error("Could not open source file '%s': %s", filePath.c_str(), strerror(errno));
        return 1;
    }
    LOG.info("+ Opened file for parsing: '%s'", filePath.c_str());
    return 0;
}

void TxDriver::scan_end() {
    fclose(yyin);
}


int TxDriver::parse(const std::string &filePath) {
    int ret = scan_begin(filePath);
    if (ret) {
        return ret;
    }

    //if (this->options.only_scan)  // currently unsupported
    //    return test_scanner();

    yy::TxParser parser(*this);
    parser.set_debug_level(this->options.debug_parser);
    ret = parser.parse();

    scan_end();
    return ret;
}


int TxDriver::compile(const std::vector<std::string>& startSourceFiles, const std::string& outputFileName) {
    ASSERT(this->parsedSourceFiles.empty(), "Can only run driver instance once");
    if (startSourceFiles.empty()) {
        this->LOG.fatal("No source specified.");
        return 1;
    }
    for (auto startFile : startSourceFiles)
        this->sourceFileQueue.push_back( std::pair<TxIdentifier,std::string>(TxIdentifier(), startFile) );

    /*--- parse all source filed (during parsing, files are added to the queue as the are imported) ---*/

    while (! this->sourceFileQueue.empty()) {
        std::string nextFilePath = this->sourceFileQueue.front().second;
        if (! this->parsedSourceFiles.count(nextFilePath)) {
            int ret = this->parse(nextFilePath);
            if (ret) {
                if (ret == 1)  // syntax error
                    LOG.fatal("Exiting due to unrecovered syntax error");
                else  // ret == 2, out of memory
                    LOG.fatal("Exiting due to out of memory");
                return ret;
            }
            ASSERT(this->parsingUnit, "parsingUnit not set by parser");
            this->parsedASTs.push_back(this->parsingUnit);
            this->parsedSourceFiles.emplace(nextFilePath, this->parsingUnit);
            this->parsingUnit = nullptr;
        }
        this->sourceFileQueue.pop_front();
    }

    if (error_count)
        LOG.error("- Grammar parse completed, %d errors", error_count);
    else
        LOG.info("+ Grammar parse OK");
    if (this->options.only_parse)
        return error_count;
    int prev_error_count = error_count;


    /*--- perform declaration pass ---*/

    for (auto parsedAST : this->parsedASTs)
        parsedAST->symbol_declaration_pass(this->package);

    this->package->prepare_modules();  // (prepares the declared imports)

    if (error_count != prev_error_count)
        LOG.error("- Declaration pass completed, %d errors", error_count-prev_error_count);
    else
        LOG.info("+ Declaration pass OK");

    /*--- perform resolution pass ---*/

    for (auto parsedAST : this->parsedASTs)
        parsedAST->symbol_resolution_pass();

    this->package->types().enqueued_resolution_pass();

    this->package->types().register_types();

    bool symValid = this->package->symbol_validation_pass();

    if (symValid && error_count == prev_error_count)
        LOG.info("+ Resolution pass OK");

    if (this->options.dump_symbol_table) {
        std::cout << "SYMBOL TABLE DUMP:\n";
        this->package->dump_symbols();
        std::cout << "END SYMBOL TABLE DUMP\n";
    }

    if (! (symValid && error_count == prev_error_count)) {
        if (error_count == prev_error_count)
            LOG.warning("- Resolution pass completed with 0 errors, but symbol validation failed");
        else
            LOG.error("- Resolution pass completed, %d errors", error_count-prev_error_count);
    }

    if (error_count)
        return 2;


    /*--- generate LLVM code ---*/

    int ret = this->llvm_compile(outputFileName);
    if (ret)
        return 3;

    return 0;
}


bool TxDriver::validate_module_name(const TxIdentifier& moduleName) {
    if (moduleName.to_string() == LOCAL_NS) {
        if (! this->parsedSourceFiles.empty()) {
            this->cerror("Only the first source file may have unspecified module name (implicit module " + std::string(LOCAL_NS) + ")");
            return false;
        }
    }
    auto res = moduleName.begins_with(this->sourceFileQueue.front().first);
    if (! res)
        this->cerror("Source contains module '" + moduleName.to_string() + "', not '"
                    + this->sourceFileQueue.front().first.to_string() + "' as expected.");
    return res;
}


inline bool begins_with(const std::string& str, const std::string& tail) {
    return str.length() >= tail.length() && ! str.compare(0, tail.length(), tail);
}
inline bool ends_with(const std::string& str, const std::string& tail) {
    return str.length() >= tail.length() && ! str.compare(str.length()-tail.length(), tail.length(), tail);
}

bool TxDriver::add_import(const TxIdentifier& moduleName) {
    if (this->package->lookup_module(moduleName)) {  // so we won't search for built-in modules' sources
        this->LOG.debug("Skipping import of previously imported module: %s", moduleName.to_string().c_str());
        return true;
    }
    // TODO: guard against or handle circular imports
    const std::string moduleFileName = moduleName.to_string() + ".tx";
    for (auto pathItem : this->options.sourceSearchPaths) {
        if (file_status(pathItem) == 2) {
            // path item exists and is a directory

            // if a file name exists that exactly matches the module name, pick it
            // (the file is assumed to contain the whole module it if it's named 'module.name.tx')
            std::string moduleFilePath = pathItem + get_path_separator() + moduleFileName;
            if (file_status(moduleFilePath) == 1) {
                this->add_source_file(moduleName, moduleFilePath);
                return true;
            }

            // if module dir exists, pick all .tx files in it
            std::string moduleDirPath = pathItem;
            for (auto si = moduleName.segments_cbegin(); si != moduleName.segments_cend(); si++) {
                moduleDirPath += get_path_separator();
                moduleDirPath += *si;
            }
            if (file_status(moduleDirPath) == 2) {
                this->add_all_in_dir(moduleName, moduleDirPath);
                return true;
            }
        }
    }
    //this->LOG.error("Could not find source for module: %s", moduleName.to_string().c_str());
    return false;
}

int TxDriver::add_all_in_dir(const TxIdentifier& moduleName, const std::string &dirPath) {
    int addCount = 0;
    tinydir_dir dir;
    tinydir_open(&dir, dirPath.c_str());
    while (dir.has_next) {
        tinydir_file file;
        tinydir_readfile(&dir, &file);
        if (! file.is_dir && !strcmp(file.extension, "tx")) {
            this->add_source_file(moduleName, file.path);
            addCount++;
        }
        tinydir_next(&dir);
    }
    tinydir_close(&dir);
    this->LOG.debug("Added %d source files to compilation from directory '%s'", addCount, dirPath.c_str());
    return addCount;
}

void TxDriver::add_source_file(const TxIdentifier& moduleName, const std::string &filePath) {
    this->LOG.debug("Adding source file to compilation: '%s'", filePath.c_str());
    // TODO: verify that the source file actually contains the specified module
    this->sourceFileQueue.push_back( std::pair<TxIdentifier,std::string>(moduleName, filePath) );
}


std::string* TxDriver::current_input_filepath() {
    return this->currentInputFilename;
}


int TxDriver::llvm_compile(const std::string& outputFileName) {
    LlvmGenerationContext genContext(*this->package);

    for (auto parsedAST : this->parsedASTs) {
        genContext.generate_code(*parsedAST);
    }

    genContext.generate_runtime_data();

    bool mainGenerated = false;
    if (auto funcDecl = this->package->getMainFunc()) {
        auto funcField = funcDecl->get_definer()->resolve_field();
        if (auto funcType = dynamic_cast<const TxFunctionType*>(funcField->get_type())) {
            if ( funcType->returnType && ! funcType->returnType->is_a( *this->package->types().get_builtin_type(INTEGER) ) )
                this->LOG.error("main() method had invalid return type: %s", funcType->returnType->to_string().c_str());
            else if ((mainGenerated = genContext.generate_main(funcDecl->get_unique_full_name(), funcType)))
                this->LOG.debug("Created program entry for user method %s", funcDecl->get_unique_full_name().c_str());
        }
    }
    LOG.info("+ LLVM code generated (not yet written)");

    if (this->options.dump_ir)
        genContext.print_IR();

    int retCode = 0;
    if (this->options.run_verifier) {
        retCode = genContext.verify_code();
        if (retCode)
            return retCode;
        else
            LOG.info("+ LLVM code verification OK");
    }

    if (this->options.run_jit) {
        if (! mainGenerated)
            this->LOG.error("Can't run program, no main() method found.");
        else {
            genContext.run_code();
        }
    }

    if (! this->options.no_bc_output) {
        retCode = genContext.write_bitcode(outputFileName);
        LOG.info("+ Wrote bitcode file '%s'", outputFileName.c_str());
    }

    return retCode;
}



static void format_location_message(char *buf, size_t bufSize, const yy::location& parseLocation, char const *msg) {
    auto filename = parseLocation.begin.filename ? parseLocation.begin.filename->c_str() : "";
    if (parseLocation.begin.line == parseLocation.end.line) {
        int lcol = (parseLocation.end.column > parseLocation.begin.column) ? parseLocation.end.column : parseLocation.end.column;
        snprintf(buf, bufSize, "%s %2d.%2d-%2d: %s", filename,
                 parseLocation.begin.line, parseLocation.begin.column, lcol, msg);
    }
    else
        snprintf(buf, bufSize, "%s %2d.%2d-%2d.%2d: %s", filename,
                 parseLocation.begin.line, parseLocation.begin.column,
                 parseLocation.end.line, parseLocation.end.column, msg);
}


static Logger& CLOG = Logger::get("COMPILER");

void TxDriver::emit_comp_error(const yy::location& loc, const std::string& msg) {
    char buf[512];
    format_location_message(buf, 512, loc, msg.c_str());
    CLOG.error("%s", buf);
}

void TxDriver::emit_comp_warning(const yy::location& loc, const std::string& msg) {
    char buf[512];
    format_location_message(buf, 512, loc, msg.c_str());
    CLOG.warning("%s", buf);
}


void TxDriver::emit_comp_error(char const *msg) {
    if (this->exp_err) {
        this->exp_err_count++;
        CLOG.info("EXPECTED CERROR: %s", msg);
    }
    else {
        this->error_count++;
        CLOG.error("%s", msg);
    }
}

void TxDriver::emit_comp_warning(char const *msg) {
    this->warning_count++;
    CLOG.warning("%s", msg);
}


void TxDriver::begin_exp_err(const yy::location& loc) {
    if (this->exp_err) {
        this->cerror(loc, "Nested EXPECTED ERROR blocks not supported");
        return;
    }
    //puts("EXPERR {");
    this->exp_err = true;
    this->exp_err_count = 0;
}

int TxDriver::end_exp_err(const yy::location& loc) {
    if (!this->exp_err) {
        this->cerror(loc, "EXPECTED ERROR block end doesn't match a corresponding begin");
        return 0;
    }
    //puts("} EXPERR");
    this->exp_err = false;
    return this->exp_err_count;
}

bool TxDriver::is_exp_err() {
    return this->exp_err;
}


int  TxDriver::get_error_count() {
    return this->error_count;
}

int  TxDriver::get_warning_count() {
    return this->warning_count;
}


void TxDriver::cerror(const yy::location& loc, char const *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    char buf[512];
    vsnprintf(buf, 512, fmt, ap);
    va_end(ap);
    this->cerror(loc, std::string(buf));
}

void TxDriver::cerror(const yy::location& loc, const std::string& msg) {
    char buf[512];
    format_location_message(buf, 512, loc, msg.c_str());
    this->emit_comp_error(buf);
}

void TxDriver::cerror(const std::string& msg)
{
    this->cerror(yy::location(NULL, 0, 0), "%s", msg.c_str());
}

void TxDriver::cwarning(const yy::location& loc, char const *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    char buf[512];
    vsnprintf(buf, 512, fmt, ap);
    va_end(ap);
    this->cwarning(loc, std::string(buf));
}

void TxDriver::cwarning(const yy::location& loc, const std::string& msg) {
    char buf[512];
    format_location_message(buf, 512, loc, msg.c_str());
    this->emit_comp_warning(buf);
}

void TxDriver::cwarning(const std::string& msg) {
    this->cwarning(yy::location(NULL, 0, 0), "%s", msg.c_str());
}
