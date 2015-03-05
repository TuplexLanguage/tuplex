#include <iostream>
#include <stack>
#include <typeinfo>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>

#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FileSystem.h>

#include "txassert.hpp"
#include "tx_lang_defs.hpp"
#include "llvm_generator.hpp"


/** Add main function so can be fully compiled
 * define i32 @main(i32 %argc, i8 **%argv)
 */
llvm::Function* LlvmGenerationContext::add_main_function(llvm::Module *mod, const std::string userMain, bool hasIntReturnValue) {
    //define i32 @main(i32 %argc, i8 **%argv)
    llvm::Function *main_func = llvm::cast<llvm::Function>(mod->getOrInsertFunction("main",
            llvm::IntegerType::getInt32Ty(mod->getContext()),
            llvm::IntegerType::getInt32Ty(mod->getContext()),
            llvm::PointerType::getUnqual(llvm::PointerType::getUnqual(llvm::IntegerType::getInt8Ty(mod->getContext()))), NULL));
    {
        llvm::Function::arg_iterator args = main_func->arg_begin();
        llvm::Value *arg_0 = args++;
        arg_0->setName("argc");
        llvm::Value *arg_1 = args++;
        arg_1->setName("argv");
    }
    //main.0:
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(mod->getContext(), "main.0", main_func);

    // TODO: initialize statics / runtime environment

    //call i64 @userMain()
    auto func = mod->getFunction(userMain);
    if (func) {
        llvm::CallInst *user_main_call = llvm::CallInst::Create(func, "", bb);
        user_main_call->setTailCall(false);
        auto i32Type = llvm::Type::getInt32Ty(mod->getContext());
        if (hasIntReturnValue) {
            // truncate return value to i32
            llvm::CastInst* truncVal = llvm::CastInst::CreateIntegerCast(user_main_call, i32Type, true, "", bb);
            llvm::ReturnInst::Create(mod->getContext(), truncVal, bb);
        }
        else {
            llvm::ReturnInst::Create(mod->getContext(), llvm::ConstantInt::get(i32Type, 0, true), bb);
        }
    }
    else {
        this->LOG.error("LLVM function not found for name: %s", userMain.c_str());
        llvm::ReturnInst::Create(mod->getContext(), llvm::ConstantInt::get(mod->getContext(), llvm::APInt(32, 0, true)), bb);
    }

    return main_func;
}


/* Compile the AST into a module */
void LlvmGenerationContext::generate_code(const TxParsingUnitNode& topParseNode)
{
    // emit bytecode for the program
    topParseNode.code_gen(*this, nullptr);  // (global scope has no block)
}

bool LlvmGenerationContext::generate_main(const std::string& userMainIdent, const TxFunctionType* mainFuncType) {
    this->entryFunction = add_main_function(&this->llvmModule, userMainIdent, mainFuncType->returnType);
    return this->entryFunction;
}

bool LlvmGenerationContext::verify_code() {
    //this->LOG.info("Verifying LLVM code...");;
    std::string errInfo;
    llvm::raw_string_ostream ostr(errInfo);
    bool ret = llvm::verifyModule(this->llvmModule, &ostr);
    if (ret)
        this->LOG.error("LLVM code verification failed: %s", errInfo.c_str());
    return ret;
}

void LlvmGenerationContext::print_IR() {
    // TODO: support writing to a .ll file
    this->LOG.info("Printing LLVM bytecode...");
    llvm::PrintModulePass printPass(llvm::outs());
    llvm::ModulePassManager pm;
    pm.addPass(printPass);
    pm.run(&this->llvmModule);
    std::cout << std::endl;
}

void LlvmGenerationContext::write_bitcode(const std::string& filepath) {
    this->LOG.info("Writing LLVM bitcode file '%s'", filepath.c_str());
    std::string errInfo;
    llvm::raw_fd_ostream ostream(filepath.c_str(), errInfo, llvm::sys::fs::F_RW);
    if (errInfo.empty())
        llvm::WriteBitcodeToFile(&this->llvmModule, ostream);
    else
        this->LOG.error("Failed to open bitcode output file for writing: %s", errInfo.c_str());
}


void LlvmGenerationContext::register_llvm_value(const std::string& identifier, llvm::Value* val) {
    if (identifier.compare(0, strlen(BUILTIN_NS), BUILTIN_NS) != 0)
        this->LOG.debug("Registering LLVM value %s : %s", identifier.c_str(), to_string(val->getType()).c_str());
    //this->LOG.debug("Registering LLVM value %s: %s : %s", identifier.c_str(), to_string(val).c_str(), to_string(val->getType()).c_str());
    this->llvmSymbolTable.emplace(identifier, val);
}

llvm::Value* LlvmGenerationContext::lookup_llvm_value(const std::string& identifier) const {
    try {
        llvm::Value* val = this->llvmSymbolTable.at(identifier);
        this->LOG.debug("Looked up LLVM value %s", identifier.c_str());
        return val;
    }
    catch (const std::out_of_range& oor) {
        this->LOG.warning("Unknown LLVM value identifier %s", identifier.c_str());
        return nullptr;
    }
}


const TxType* LlvmGenerationContext::lookup_builtin(BuiltinTypeId id) {
    return this->tuplexPackage.types().get_builtin_type(id);
}

void LlvmGenerationContext::initialize_builtin_types() {
    /*
	this->llvmTypeMapping.emplace(lookup_builtin(ANY),    llvm::Type::getVoidTy(this->llvmContext));
	this->llvmTypeMapping.emplace(lookup_builtin(BYTE),   llvm::Type::getInt8Ty(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin(SHORT),  llvm::Type::getInt16Ty(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin(INT),    llvm::Type::getInt32Ty(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin(LONG),   llvm::Type::getInt64Ty(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin(UBYTE),  llvm::Type::getInt8Ty(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin(USHORT), llvm::Type::getInt16Ty(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin(UINT),   llvm::Type::getInt32Ty(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin(ULONG),  llvm::Type::getInt64Ty(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin(HALF),   llvm::Type::getHalfTy(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin(FLOAT),  llvm::Type::getFloatTy(this->llvmContext));
	this->llvmTypeMapping.emplace(lookup_builtin(DOUBLE), llvm::Type::getDoubleTy(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin(BOOLEAN), llvm::Type::getInt1Ty(this->llvmContext));
    */
    //this->llvmTypeMapping.emplace(lookupBuiltin(CHAR),   llvm::Type::getInt8Ty(this->llvmContext));
    //this->llvmTypeMapping.emplace(lookupBuiltin(STRING), llvm::Type::getInt8PtrTy(this->llvmContext));
//	for (auto pair : this->llvmTypeMapping)
//	    std::cout << "LLVM type mapping: " << pair.first << " -> " << pair.second << std::endl;

//    // test adding static field to types:
//    for (int id = 0; id < BuiltinTypeId_COUNT; id++) {
//        auto txType = this->tuplexPackage.types().get_builtin_type((BuiltinTypeId)id);
//        auto name = txType->entity()->get_full_name().to_string() + ".typeid";
//        auto value = llvm::ConstantInt::get(llvm::Type::getInt16Ty(this->llvmContext), id, false);
//        llvm::Value* member = new llvm::GlobalVariable(this->llvmModule, value->getType(), true,
//                                                       llvm::GlobalValue::ExternalLinkage, value, name);
//        this->register_llvm_value(name, member);
//    }

    // initialize external functions:
    std::vector<llvm::Type*> c_puts_args( { llvm::Type::getInt8PtrTy(this->llvmContext) } );
    llvm::FunctionType* c_puts_func_type = llvm::FunctionType::get(
      /*Result=*/llvm::Type::getInt32Ty(this->llvmContext),
      /*Params=*/c_puts_args,
      /*isVarArg=*/false);

    llvm::Function* c_puts_func = llvm::Function::Create(
      /*Type=*/c_puts_func_type,
      /*Linkage=*/llvm::GlobalValue::ExternalLinkage, // (external, no body)
      /*Name=*/"puts",
      &this->llvmModule);
    c_puts_func->setCallingConv(llvm::CallingConv::C);

    this->register_llvm_value("tx.c.puts", c_puts_func);

// varargs example:
//    llvm::ArrayRef<llvm::Type*> FuncTy_7_args;
//    llvm::FunctionType* FuncTy_7 = llvm::FunctionType::get(
//      /*Result=*/llvm::Type::getInt32Ty(this->llvmContext),
//      /*Params=*/FuncTy_7_args,
//      /*isVarArg=*/true);
//
//    llvm::Function* func_foo = llvm::Function::Create(
//      /*Type=*/FuncTy_7,
//      /*Linkage=*/llvm::GlobalValue::ExternalLinkage, // (external, no body)
//      /*Name=*/"foo",
//      &this->llvmModule);
//     func_foo->setCallingConv(llvm::CallingConv::C);
}



llvm::Type* LlvmGenerationContext::get_llvm_type(const TxType* txType) {
    ASSERT(txType, "NULL txType provided to getLlvmType()");
    if (txType->is_virtual_specialization())
        // same data type as base type
        return this->get_llvm_type(txType->get_base_type());

// we currently do map abstract types (e.g. reference targets)
//    if (! txType->is_concrete()) {
//        this->LOG.error("Can't map a non-concrete type to an LLVM type: %s", txType->to_string().c_str());
//        return nullptr;
//    }

    auto iter = this->llvmTypeMapping.find(txType);
    if (iter != this->llvmTypeMapping.end()) {
        return iter->second;
    }
	llvm::Type* llvmType = txType->make_llvm_type(*this);
	if (llvmType)
	    this->llvmTypeMapping.emplace(txType, llvmType);
	else
		this->LOG.error("No LLVM type mapping for type: %s", txType->to_string().c_str());
	return llvmType;
}
