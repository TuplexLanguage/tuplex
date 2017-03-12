#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/Interpreter.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Support/TargetSelect.h>

#include "llvm_generator.hpp"


/* Executes the AST by running the main function */
int LlvmGenerationContext::run_code() {
    this->LOGGER()->info("Running code...");
    llvm::InitializeNativeTarget();
    bool forceInterpreter = false;  // true - run interpreter; false - JIT
    std::string errorString;
    llvm::ExecutionEngine* ee = llvm::ExecutionEngine::create(&this->llvmModule, forceInterpreter, &errorString);
    if (! ee) {
        this->LOGGER()->error("Failed to create LLVM ExecutionEngine with error message: %s", errorString.c_str());
        return -1;
    }
    std::vector<llvm::GenericValue> noargs;
    llvm::GenericValue v = ee->runFunction(this->entryFunction, noargs);
    int64_t retVal = v.IntVal.getSExtValue();
    if (forceInterpreter)
        this->LOGGER()->info("Code was run in interpreter mode, return value: %d", retVal);
    else
        this->LOGGER()->info("Code was run in JIT mode, return value: %d", retVal);
    return (int) retVal;
}
