#include <cstdlib>
#include <string>

#include "driver.hpp"
#include "txassert.hpp"
#include "files_env.hpp"
#include "tinydir/tinydir.h"

#include "tx_lang_defs.hpp"
#include "llvm_generator.hpp"

#include "parser.hpp"


extern FILE * yyin;
extern int yy_flex_debug;



TxDriver::TxDriver(const TxOptions& options)
        : LOG(Logger::get("DRIVER")), options(options), package(new TxPackage(*this))  {
    if (options.sourceSearchPaths.empty())
        this->LOG.config("Tuplex source search path is empty");
    else for (auto pathItem : options.sourceSearchPaths)
        this->LOG.config("Tuplex source search path item: '%s'", pathItem.c_str());
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


int TxDriver::compile() {
    ASSERT(this->parsedSourceFiles.empty(), "Can only run driver instance once");
    if (options.startSourceFiles.empty()) {
        this->LOG.fatal("No source specified.");
        return 1;
    }
    for (auto startFile : options.startSourceFiles)
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


    /*--- perform symbol table pass ---*/

    for (auto parsedAST : this->parsedASTs)
        parsedAST->symbol_declaration_pass(this->package);

    this->package->prepare_modules();

    // Currently the symbol_validation_pass must be run before the symbol_resolution_pass
    // (symbol_resolution_pass does not traverse references, which can lead to encountering
    //  unresolved types), this order might be desirable to change in future.
    ResolutionContext resCtx;
    bool symValid = this->package->symbol_validation_pass(resCtx);
    for (auto parsedAST : this->parsedASTs)
        parsedAST->symbol_resolution_pass(resCtx);
    if (symValid && error_count == prev_error_count)
        LOG.info("+ Symbol table pass OK");

    if (this->options.dump_symbol_table) {
        std::cout << "SYMBOL TABLE DUMP:\n";
        this->package->dump_symbols();
        std::cout << "END SYMBOL TABLE DUMP\n";
    }

    if (! (symValid && error_count == prev_error_count)) {
        LOG.error("- Symbol table pass completed, %d errors", error_count-prev_error_count);
        if (! symValid)
            LOG.error("- Symbol table pass validation failed");
        return 1;
    }


    /*--- perform semantic pass ---*/

    for (auto parsedAST : this->parsedASTs) {
        parsedAST->semantic_pass();
    }
    if (error_count > prev_error_count)
        LOG.error("- Semantic pass completed, %d errors", error_count-prev_error_count);
    else
        LOG.info("+ Semantic pass OK");
    if (error_count)
        return error_count;


    /*--- generate LLVM code ---*/

    int ret = this->llvm_compile();

    return ret;
}


bool TxDriver::validate_module_name(const TxIdentifier& moduleName) {
    if (moduleName.to_string() == LOCAL_NS) {
        if (! this->parsedSourceFiles.empty()) {
            this->cerror("Only the first source file may have unspecified module name (implicit module " + std::string(LOCAL_NS) + ")");
            return false;
        }
    }
    else
        ASSERT(moduleName.to_string().find_first_of('$') == std::string::npos, "Explicit module name can't contain '$': " << moduleName);
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


int TxDriver::llvm_compile() {
    LlvmGenerationContext genContext(*this->package);

    for (auto parsedAST : this->parsedASTs) {
        genContext.generate_code(*parsedAST);
    }

    genContext.generate_runtime_data();

    bool mainGenerated = false;
    if (auto funcField = this->package->getMainFunc()) {
        if (auto funcType = dynamic_cast<const TxFunctionType*>(funcField->get_type())) {
            if ( funcType->returnType && ! funcType->returnType->is_a( *this->package->types().get_builtin_type(INTEGER) ) )
                this->LOG.error("main() method had invalid return type: %s", funcType->returnType->to_string().c_str());
            else if ((mainGenerated = genContext.generate_main(funcField->get_full_name().to_string(), funcType)))
                this->LOG.debug("Created program entry for user method %s", funcField->get_full_name().to_string().c_str());
        }
    }
    if (! mainGenerated)
        this->LOG.warning("No main() method found.");
    LOG.info("+ LLVM code generated");

    if (this->options.dump_ir)
        genContext.print_IR();

    bool verError = genContext.verify_code();
    if (! verError)
        LOG.info("+ LLVM code verification OK");
    if (verError)
        return 1;

    if (mainGenerated && this->options.run_jit) {
        genContext.run_code();
    }

    genContext.write_bitcode(options.outputFileName);
    LOG.info("+ Wrote bitcode file '%s'", options.outputFileName.c_str());

    return 0;
}



static void format_location_message(char *buf, size_t bufSize, const yy::location& parseLocation, char const *msg) {
    auto filename = parseLocation.begin.filename ? parseLocation.begin.filename->c_str() : "";
    if (parseLocation.begin.line == parseLocation.end.line) {
        int lcol = (parseLocation.end.column > parseLocation.begin.column) ? parseLocation.end.column-1 : parseLocation.end.column;
        snprintf(buf, bufSize, "%s %2d.%2d-%2d: %s", filename,
                 parseLocation.begin.line, parseLocation.begin.column, lcol, msg);
    }
    else
        snprintf(buf, bufSize, "%s %2d.%2d-%2d.%2d: %s", filename,
                 parseLocation.begin.line, parseLocation.begin.column,
                 parseLocation.end.line, parseLocation.end.column-1, msg);
}


static Logger* PLOG; // = Logger::get("PARSER");

void TxDriver::emit_comp_error(const yy::location& loc, const std::string& msg) {
    char buf[512];
    format_location_message(buf, 512, loc, msg.c_str());
    if (! PLOG)
        PLOG = &Logger::get("PARSER");
    PLOG->error("%s", buf);
}

void TxDriver::emit_comp_warning(const yy::location& loc, const std::string& msg) {
    char buf[512];
    format_location_message(buf, 512, loc, msg.c_str());
    if (! PLOG)
        PLOG = &Logger::get("PARSER");
    PLOG->warning("%s", buf);
}


void TxDriver::emit_comp_error(char const *msg) {
    error_count++;
    if (! PLOG)
        PLOG = &Logger::get("PARSER");
    PLOG->error("%s", msg);
}

void TxDriver::emit_comp_warning(char const *msg) {
    warning_count++;
    if (! PLOG)
        PLOG = &Logger::get("PARSER");
    PLOG->warning("%s", msg);
}


void TxDriver::cerror(const yy::location& loc, char const *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    char buf[512];
    vsnprintf(buf, 512, fmt, ap);
    va_end(ap);
    cerror(loc, std::string(buf));
}

void TxDriver::cerror(const yy::location& loc, const std::string& msg) {
    char buf[512];
    format_location_message(buf, 512, loc, msg.c_str());
    emit_comp_error(buf);
}

void TxDriver::cerror(const std::string& msg)
{
    cerror(yy::location(NULL, 0, 0), "%s", msg.c_str());
}

void TxDriver::cwarning(const yy::location& loc, char const *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    char buf[512];
    vsnprintf(buf, 512, fmt, ap);
    va_end(ap);
    cwarning(loc, std::string(buf));
}

void TxDriver::cwarning(const yy::location& loc, const std::string& msg) {
    char buf[512];
    format_location_message(buf, 512, loc, msg.c_str());
    emit_comp_warning(buf);
}

void TxDriver::cwarning(const std::string& msg)
{
    cwarning(yy::location(NULL, 0, 0), "%s", msg.c_str());
}
