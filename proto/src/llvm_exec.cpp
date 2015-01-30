#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/Interpreter.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Support/TargetSelect.h>

#include "llvm_generator.hpp"


/* Executes the AST by running the main function */
void LlvmGenerationContext::run_code() {
    this->LOG.info("Running code...");
    llvm::InitializeNativeTarget();
    bool forceInterpreter = false;  // true - run interpreter; false - JIT
    std::string errorString;
    llvm::ExecutionEngine* ee = llvm::ExecutionEngine::create(&this->llvmModule, forceInterpreter, &errorString);
    if (! ee) {
        this->LOG.error("Failed to create LLVM ExecutionEngine with error message: %s", errorString.c_str());
        return;
    }
    std::vector<llvm::GenericValue> noargs;
    llvm::GenericValue v = ee->runFunction(this->entryFunction, noargs);
    if (forceInterpreter)
        this->LOG.info("Code was run in interpreter mode, return value: %d", v.IntVal.getSExtValue());
    else
        this->LOG.info("Code was run in JIT mode, return value: %d", v.IntVal.getSExtValue());
}
