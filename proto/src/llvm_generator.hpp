#pragma once

#include <stack>

#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/Support/raw_ostream.h>


#include "ast.hpp"
#include "tx_logging.hpp"


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
    Logger& _LOGGER = Logger::get("LLVMGEN");

    llvm::Function* entryFunction = nullptr;
    std::map<const std::string, llvm::Value*> llvmSymbolTable;
    std::map<const TxActualType*, llvm::Type*> llvmTypeMapping;
    std::map<uint32_t, llvm::StructType*> llvmVTableTypeMapping;

    // some common, basic types:
    llvm::Type* voidT;
    llvm::Type* voidPtrT;
    llvm::Type* voidRefT;

    const TxType* lookup_builtin(BuiltinTypeId id);
    void initialize_basic_llvm_types();
    void initialize_meta_type_data();
    void initialize_builtin_functions();
    void initialize_external_functions();
    void initialize_builtins();
    llvm::Function* gen_static_init_function();
    llvm::Function* gen_main_function(const std::string userMain, bool hasIntReturnValue);

public:
    TxPackage& tuplexPackage;

	llvm::LLVMContext& llvmContext;
    llvm::Module llvmModule;
    llvm::DataLayout dataLayout;

    LlvmGenerationContext(TxPackage& tuplexPackage)
        : tuplexPackage(tuplexPackage),
          llvmContext(llvm::getGlobalContext()),
          llvmModule("top", this->llvmContext),
          dataLayout(&llvmModule)
        {
            this->voidT = llvm::Type::getVoidTy(this->llvmContext);
            this->voidPtrT = llvm::Type::getInt8PtrTy(this->llvmContext);
            this->voidRefT = TxReferenceType::make_ref_llvm_type(*this, llvm::Type::getInt8Ty(this->llvmContext));

            this->initialize_builtins();
        }


    inline llvm::Type* get_voidT() const     { return this->voidT; }
    inline llvm::Type* get_voidPtrT() const  { return this->voidPtrT; }
    inline llvm::Type* get_voidRefT() const  { return this->voidRefT; }

    llvm::Type* get_llvm_type(const TxType* txType);
    llvm::Type* get_llvm_type(const TxActualType* txType);

    llvm::StructType* get_llvm_vtable_type(const TxActualType* txType) const;

    // simple symbol table for llvm values:
    void register_llvm_value(const std::string& identifier, llvm::Value* val);
    llvm::Value* lookup_llvm_value(const std::string& identifier) const;


    void generate_function_declaration( const TxLambdaExprNode* lambdaNode );

    /** Generate the LLVM code for the provided AST, which must be in global/static scope. */
    void generate_code( const TxNode* staticScopeNode );

    void generate_runtime_data();

    /** Create the top level function to call as program entry.
     * (This is the built-in main, which calls the user main function.)  */
    bool generate_main(const std::string& userMainIdent, const TxType* mainFuncType);


    // "intrinsics":
    llvm::Value* gen_malloc(GenScope* scope, llvm::Type* objT);

    llvm::Value* gen_get_vtable(GenScope* scope, const TxActualType* statDeclType, llvm::Value* typeIdV) const;
    llvm::Value* gen_get_vtable(GenScope* scope, const TxActualType* statDeclType) const;


    /** Verfies the generated LLVM code.
     * Should only be used for debugging, may mess with LLVM's state.
     * @return 0 upon success
     */
    int verify_code();

    /** Print the LLVM IR in a human-readable format to stdout */
    void print_IR();

    /** Print the LLVM IR as binary bitcode to a file.
     * @return 0 upon success */
    int write_bitcode(const std::string& filepath);

    /** Returns the program's return code. */
    int run_code();


    inline Logger* LOGGER() const { return &this->_LOGGER; }
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
