#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/Interpreter.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/Support/TargetSelect.h>

#include "llvm_generator.hpp"
#include "driver.hpp"
#include "symbol/package.hpp"

using namespace llvm;

/* Executes the AST by running the main function */
int LlvmGenerationContext::run_code() {
    this->LOGGER()->info( "Running code..." );
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();

    // Note: The Interpreter doesn't seem to cope with the data types and constant instances of them.
    EngineKind::Kind kind = EngineKind::Kind::JIT;  // JIT or Interpreter
    std::string errorString;
    EngineBuilder engBuilder( std::move( this->llvmModulePtr ) );
    engBuilder.setErrorStr( &errorString );
    engBuilder.setEngineKind( kind );
    ExecutionEngine* ee = engBuilder.create();
    if ( !ee ) {
        this->LOGGER()->error( "Failed to create LLVM ExecutionEngine with error message: %s", errorString.c_str() );
        return -1;
    }

    std::vector<GenericValue> stdargs( 2 );
    stdargs[0].IntVal = APInt( 32, this->tuplexPackage.driver().get_options().jit_argc );
    stdargs[1].PointerVal = this->tuplexPackage.driver().get_options().jit_argv;
    GenericValue v = ee->runFunction( this->entryFunction, stdargs );
    int64_t retVal = v.IntVal.getSExtValue();

    if ( kind == EngineKind::Kind::Interpreter )
        this->LOGGER()->info( "Code was run in interpreter mode, return value: %d", retVal );
    else
        this->LOGGER()->info( "Code was run in JIT mode, return value: %d", retVal );
    return (int) retVal;
}
