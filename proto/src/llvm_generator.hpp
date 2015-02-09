#pragma once

#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/Support/raw_ostream.h>


#include "ast.hpp"

class CompoundStatementScope {
public:
    llvm::BasicBlock* continueBlock;
    llvm::BasicBlock* breakBlock;

    CompoundStatementScope(llvm::BasicBlock* continueBlock, llvm::BasicBlock* breakBlock)
        : continueBlock(continueBlock), breakBlock(breakBlock)  { }
};

class GenScope {
public:
    llvm::IRBuilder<> *builder = nullptr;
    std::stack<CompoundStatementScope*> compStmtStack;

    GenScope(llvm::IRBuilder<> *builder) : builder(builder) { }
};


/** An instance of this class represents a Tuplex to LLVM IR compilation context,
 * and has a 1-1 relationship to an LLVM Module.
 */
class LlvmGenerationContext {
    llvm::Function* entryFunction = nullptr;
    std::map<const std::string, llvm::Value*> llvmSymbolTable;
    std::map<const TxType*, llvm::Type* const> llvmTypeMapping;

    const TxType* lookup_builtin(const std::string& name);
    void initialize_builtin_types();
    llvm::Function* add_main_function(llvm::Module *mod, const std::string userMain, bool hasIntReturnValue);

public:
    Logger& LOG = Logger::get("LLVMGEN");

    const TxPackage& tuplexPackage;

	llvm::LLVMContext& llvmContext;
    llvm::Module& llvmModule;

    LlvmGenerationContext(const TxPackage& tuplexPackage)
        : tuplexPackage(tuplexPackage),
          llvmContext( llvm::getGlobalContext() ),
          llvmModule( * new llvm::Module("top", this->llvmContext) )
        {
    		this->initialize_builtin_types();
        }

    void dump() const {
        this->llvmModule.dump();
    }


    llvm::Type* get_llvm_type(const TxType* txType);


    // simple symbol table for llvm values:
    void register_llvm_value(const std::string& identifier, llvm::Value* val);
    llvm::Value* lookup_llvm_value(const std::string& identifier) const;


    /** Generate the LLVM code for the provided parsing unit. */
    void generate_code(const TxParsingUnitNode& topParseNode);

    /** Create the top level function to call as program entry.
     * (This is the built-in main, which calls the user main function.)  */
    bool generate_main(const std::string& userMainIdent, const TxFunctionType* mainFuncType);

    /** Verfies the generated LLVM code.
     * Should only be used for debugging, may mess with LLVM's state.
     * @return false if ok, true if error
     */
    bool verify_code();

    void print_IR();

    void write_bitcode(const std::string& filepath);

    void run_code();

    /*
    std::map<std::string, Value*>& locals() { return blocks.top()->locals; }
    BasicBlock *currentBlock() { return blocks.top()->block; }
    void pushBlock(BasicBlock *block) { blocks.push(new CodeGenBlock()); blocks.top()->block = block; }
    void popBlock() { CodeGenBlock *top = blocks.top(); blocks.pop(); delete top; }
  */
};



/*--- adapters so that LLVM Type and Value can be output to printf, cout & cerr ---*/

inline std::string to_string(const llvm::Type& type) {
    std::string tmp_str;
    llvm::raw_string_ostream rso(tmp_str);
    type.print(rso);
    return rso.str();
}
inline std::string to_string(const llvm::Type* type) { return type ? to_string(*type) : "NULL"; }

inline std::string to_string(const llvm::Value& value) {
    std::string tmp_str;
    llvm::raw_string_ostream rso(tmp_str);
    value.print(rso);
    return rso.str();
}
inline std::string to_string(const llvm::Value* value) { return value ? to_string(*value) : "NULL"; }

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<< (std::basic_ostream<charT, traits> &lhs, const llvm::Type& type) {
    std::string tmp_str;
    llvm::raw_string_ostream rso(tmp_str);
    type.print(rso);
    return lhs << rso.str();
}

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<< (std::basic_ostream<charT, traits> &lhs, const llvm::Value& value) {
    std::string tmp_str;
    llvm::raw_string_ostream rso(tmp_str);
    value.print(rso);
    return lhs << rso.str();
}

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<< (std::basic_ostream<charT, traits> &lhs, const llvm::Type* type) {
    std::string tmp_str;
    llvm::raw_string_ostream rso(tmp_str);
    if (! type)
        return lhs << "NULL";
    type->print(rso);
    return lhs << rso.str();
}

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<< (std::basic_ostream<charT, traits> &lhs, const llvm::Value* value) {
    std::string tmp_str;
    llvm::raw_string_ostream rso(tmp_str);
    if (! value)
        return lhs << "NULL";
    value->print(rso);
    return lhs << rso.str();
}
