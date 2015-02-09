#include <iostream>
#include <stack>
#include <typeinfo>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>

#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/PassManager.h>
#include <llvm/Assembly/PrintModulePass.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Support/raw_ostream.h>

#include "txassert.hpp"
#include "llvm_generator.hpp"
//#include "type_registry.hpp"


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
    std::string errMsg;
    bool ret = llvm::verifyModule(this->llvmModule, llvm::PrintMessageAction, &errMsg);
    if (ret)
        this->LOG.error("LLVM code verification failed: %s", errMsg.c_str());
    return ret;
}

void LlvmGenerationContext::print_IR() {
    // TODO: support writing to a .ll file
    // Print the LLVM IR in a human-readable format to stdout
    this->LOG.info("Printing LLVM bytecode...");
    llvm::PassManager pm;
    pm.add(llvm::createPrintModulePass(&llvm::outs()));
    pm.run(this->llvmModule);
    std::cout << std::endl;
}

void LlvmGenerationContext::write_bitcode(const std::string& filepath) {
    this->LOG.info("Writing LLVM bitcode file '%s'", filepath.c_str());
    llvm::PassManager pm;
    std::string errcode;
    llvm::raw_fd_ostream ostream(filepath.c_str(), errcode, llvm::sys::fs::F_Binary);
    pm.add(llvm::createBitcodeWriterPass(ostream));
    pm.run(this->llvmModule);

}


void LlvmGenerationContext::register_llvm_value(const std::string& identifier, llvm::Value* val) {
    //this->LOG.debug("Registering LLVM value %s: %s : %s", identifier.c_str(), to_string(val).c_str(), to_string(val->getType()).c_str());
    this->LOG.debug("Registering LLVM value %s : %s", identifier.c_str(), to_string(val->getType()).c_str());
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


const TxType* LlvmGenerationContext::lookup_builtin(const std::string& name) {
    static TxIdentifier nsIdent(BUILTIN_NS);
    return this->tuplexPackage.resolve_type(TxIdentifier(nsIdent, name))->get_type();
}

void LlvmGenerationContext::initialize_builtin_types() {
    TxIdentifier nsIdent(BUILTIN_NS);
    TxIdentifier nullRefScope("");
	this->llvmTypeMapping.emplace(lookup_builtin("Any"),    llvm::Type::getVoidTy(this->llvmContext));
	this->llvmTypeMapping.emplace(lookup_builtin("Byte"),   llvm::Type::getInt8Ty(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin("Short"),  llvm::Type::getInt16Ty(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin("Int"),    llvm::Type::getInt32Ty(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin("Long"),   llvm::Type::getInt64Ty(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin("UByte"),  llvm::Type::getInt8Ty(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin("UShort"), llvm::Type::getInt16Ty(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin("UInt"),   llvm::Type::getInt32Ty(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin("ULong"),  llvm::Type::getInt64Ty(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin("Half"),   llvm::Type::getHalfTy(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin("Float"),  llvm::Type::getFloatTy(this->llvmContext));
	this->llvmTypeMapping.emplace(lookup_builtin("Double"), llvm::Type::getDoubleTy(this->llvmContext));
    this->llvmTypeMapping.emplace(lookup_builtin("Boolean"), llvm::Type::getInt1Ty(this->llvmContext));
    //this->llvmTypeMapping.emplace(lookupBuiltin("Char"),   llvm::Type::getInt8Ty(this->llvmContext));
    //this->llvmTypeMapping.emplace(lookupBuiltin("String"), llvm::Type::getInt8PtrTy(this->llvmContext));
//	for (auto pair : this->llvmTypeMapping)
//	    std::cout << "LLVM type mapping: " << pair.first << " -> " << pair.second << std::endl;

    // initialize global constants:
    auto falseValue = llvm::ConstantInt::getFalse(this->llvmContext);
    auto  trueValue = llvm::ConstantInt::getTrue(this->llvmContext);
    llvm::Value* falseField = new llvm::GlobalVariable(this->llvmModule, falseValue->getType(), true,
                                                       llvm::GlobalValue::ExternalLinkage, falseValue, "tx.FALSE");
    llvm::Value*  trueField = new llvm::GlobalVariable(this->llvmModule, trueValue->getType(), true,
                                                       llvm::GlobalValue::ExternalLinkage, trueValue, "tx.TRUE");
    this->register_llvm_value("tx.FALSE", falseField);
    this->register_llvm_value("tx.TRUE", trueField);

    // test adding static field to types:
    for (int id = 0; id < BuiltinTypeId_COUNT; id++) {
        auto txType = this->tuplexPackage.types().get_builtin_type((BuiltinTypeId)id);
        auto name = txType->entity()->get_full_name().to_string() + ".typeid";
        auto value = llvm::ConstantInt::get(llvm::Type::getInt16Ty(this->llvmContext), id, false);
        llvm::Value* member = new llvm::GlobalVariable(this->llvmModule, value->getType(), true,
                                                       llvm::GlobalValue::ExternalLinkage, value, name);
        this->register_llvm_value(name, member);
    }

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



//static llvm::Type* make_box(llvm::Type* type) {
//    ASSERT(type, "NULL type");
//    //context.LOG.debug("Boxing type %s", to_string(type).c_str());
//    std::vector<llvm::Type*> llvmMemberTypes { llvm::Type::getInt32Ty(type->getContext()), type };
//    return llvm::StructType::get(type->getContext(), llvmMemberTypes);
//}


class LLVMTypeMapper : public TxTypeVisitor {
	LlvmGenerationContext& context;

public:
	llvm::Type* result = nullptr;

	LLVMTypeMapper(LlvmGenerationContext& context) : context(context) { }

    virtual void visit(const TxType& txType) {
    	this->result = nullptr;
    }

	virtual void visit(const TxScalarType& txType)  {
        context.LOG.error("Internal error, missing LLVM type mapping for scalar type '%s'", txType.to_string().c_str());
        this->result = nullptr;
	}

    virtual void visit(const TxReferenceType& txType)  {
        if (txType.is_generic())
            throw std::logic_error("Generic references currently not supported: " + txType.to_string());
        llvm::Type* targetType = this->context.get_llvm_type(txType.target_type()->get_type());
        if (targetType) {
            this->result = llvm::PointerType::get(targetType, 0);
            context.LOG.debug("Mapping reference type %s", txType.to_string().c_str());
        }
        else
            context.LOG.error("No LLVM type mapping for reference target type: %s", txType.target_type()->get_type()->to_string().c_str());
    }

    virtual void visit(const TxArrayType& txType)  {
        if (auto e = txType.resolve_param_type("E")) {
            if (llvm::Type* elemType = this->context.get_llvm_type(e->get_type())) {
                long arrayLen;
                if (auto len = txType.resolve_param_value("L")) {
                    // concrete array (specific length)
                    arrayLen = len->get_int_value();
                }
                else {
                    //throw std::logic_error("Generic arrays with unspecified length can't be directly mapped: " + txType.to_string());
                    // Generic arrays with unspecified length are currently mapped as zero length,
                    // so they can be referenced from e.g. references.
                    arrayLen = 0;
                }
                std::vector<llvm::Type*> llvmMemberTypes {
                    llvm::Type::getInt32Ty(this->context.llvmContext),
                    llvm::ArrayType::get(elemType, arrayLen)
                };
                this->result = llvm::StructType::get(this->context.llvmContext, llvmMemberTypes);
                context.LOG.debug("Mapping array type %s", txType.to_string().c_str());
                return;
            }
            else
                context.LOG.error("No LLVM type mapping for array element type: %s", e->get_type()->to_string().c_str());
        }
        throw std::logic_error("Generic arrays with unspecified element type can't be directly mapped: " + txType.to_string());
    }

	virtual void visit(const TxFunctionType& txType)  {
		std::vector<llvm::Type*> llvmArgTypes;
	    for (auto argTxType : txType.argumentTypes) {
			llvmArgTypes.push_back(this->context.get_llvm_type(argTxType));
			context.LOG.debug("Mapping arg type %s to %s", argTxType->to_string().c_str(), to_string(llvmArgTypes.back()).c_str());
		}
	    llvm::Type* llvmRetType = txType.returnType
	                              ? this->context.get_llvm_type(txType.returnType)
	                              : llvm::Type::getVoidTy(this->context.llvmContext);
		llvm::FunctionType *ftype = llvm::FunctionType::get(llvmRetType, llvmArgTypes, false);
	    this->result = ftype;
	}

    virtual void visit(const TxTupleType& txType)  {
        auto entity = txType.entity();
        if (! entity) {
            context.LOG.error("No entity for tx type %s - can't perform LLVM type mapping", txType.to_string().c_str());
            return;
        }
        context.LOG.debug("Mapping tuple type %s... (entity %s)", txType.to_string().c_str(), entity->to_string().c_str());
        std::vector<llvm::Type*> llvmMemberTypes;
        for (auto memberTxType : entity->get_instance_field_types()) {
            llvmMemberTypes.push_back(this->context.get_llvm_type(memberTxType));
            context.LOG.debug("Mapping member type %s to %s", memberTxType->to_string().c_str(), to_string(llvmMemberTypes.back()).c_str());
        }
        // note: create() might be better for "named" struct types?
        llvm::StructType* stype = llvm::StructType::get(this->context.llvmContext, llvmMemberTypes);
        this->result = stype;
    }


	static llvm::Type* mapLlvmType(LlvmGenerationContext& context, const TxType* txType) {
		LLVMTypeMapper mapper(context);
		txType->accept(mapper);
		return mapper.result;
	}
};


llvm::Type* LlvmGenerationContext::get_llvm_type(const TxType* txType) {
    ASSERT(txType, "NULL txType provided to getLlvmType()");
    if (txType->is_virtual_specialization())
        // same data type as base type
        return get_llvm_type(txType->get_base_type());

    auto iter = this->llvmTypeMapping.find(txType);
    if (iter != this->llvmTypeMapping.end()) {
        return iter->second;
    }
	llvm::Type* result = LLVMTypeMapper::mapLlvmType(*this, txType);
	if (! result)
		this->LOG.error("No LLVM type mapping for type: %s", txType->to_string().c_str());
	return result;
}

//const llvm::IntegerType& getLlvmIntegerType(const TuplexParsingContext& context) const {
//	switch(size) {
//	case 1:
//		return llvm::Type::getInt8Ty(context.llvmContext);
//	case 2:
//		return llvm::Type::getInt16Ty(context.llvmContext);
//	case 4:
//		return llvm::Type::getInt32Ty(context.llvmContext);
//	case 8:
//		return llvm::Type::getInt64Ty(context.llvmContext);
//	default:
//		fprintf(stderr, "Unsupported Integer type width: %d\n", this->size);
//		return llvm::Type::getVoidTy(context.llvmContext);
//	}
//}
//
//virtual const llvm::Type& getLlvmFloatingType(const TuplexParsingContext& context) const {
//	switch(size) {
//	case 2:
//		return llvm::Type::getHalfTy(context.llvmContext);
//	case 4:
//		return llvm::Type::getFloatTy(context.llvmContext);
//	case 8:
//		return llvm::Type::getDoubleTy(context.llvmContext);
//	default:
//		fprintf(stderr, "Unsupported Floating type width: %d\n", this->size);
//		return llvm::Type::getVoidTy(context.llvmContext);
//	}
//}
