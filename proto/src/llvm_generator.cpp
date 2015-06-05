#include <iostream>
#include <stack>
#include <unordered_map>
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
#include "util.hpp"

#include "tx_lang_defs.hpp"
#include "llvm_generator.hpp"


using namespace llvm;


// currently not used, but has working runtime initialization logic, including malloc:
Function* LlvmGenerationContext::gen_static_init_function() {
    auto typeCountA = this->lookup_llvm_value("tx.runtime.TYPE_COUNT");
    ASSERT(typeCountA, "tx.runtime.TYPE_COUNT not found");
    auto metaTypesA = this->lookup_llvm_value("tx.runtime.META_TYPES");
    ASSERT(metaTypesA, "tx.runtime.META_TYPES not found");

    auto int8T     = Type::getInt8Ty(this->llvmContext);
    auto int32T    = Type::getInt32Ty(this->llvmContext);
    auto int8PtrT  = Type::getInt8PtrTy(this->llvmContext);
    //auto int32PtrT = Type::getInt32PtrTy(this->llvmContext);
    auto int8PtrArrPtrT  = PointerType::getUnqual(ArrayType::get(int8PtrT, 0));
    auto constZeroV = ConstantInt::get(int32T, 0);
    auto constOneV  = ConstantInt::get(int32T, 1);
    auto mallocParameterType = int32T;

    Function *init_func = cast<Function>(this->llvmModule.getOrInsertFunction("tx.runtime.thread_init",
                                                                              int32T,
                                                                              NULL));
    BasicBlock *entryBlock = BasicBlock::Create(this->llvmModule.getContext(), "entry", init_func);
    IRBuilder<> builder( entryBlock );
    auto typeCountV = builder.CreateLoad(typeCountA, "TYPE_COUNT");

    Value* vtablePtrArr;
    {
        GlobalVariable* vtablePtrArrPtr = new GlobalVariable(this->llvmModule, int8PtrArrPtrT, false, GlobalValue::ExternalLinkage,
                                                             ConstantPointerNull::get(int8PtrArrPtrT),
                                                             "tx.runtime.VTABLES");
        this->register_llvm_value(vtablePtrArrPtr->getName(), vtablePtrArrPtr);
        auto int8PtrSizeV = ConstantExpr::getTruncOrBitCast(ConstantExpr::getSizeOf(int8PtrT), int32T);
        auto vtablePtrAllocI = CallInst::CreateMalloc(builder.GetInsertBlock(), mallocParameterType,
                                                      int8PtrT, int8PtrSizeV, typeCountV, nullptr, "");
        builder.GetInsertBlock()->getInstList().push_back(vtablePtrAllocI);
        vtablePtrArr = builder.CreatePointerCast(vtablePtrAllocI, int8PtrArrPtrT, "vtPtrArr");
        builder.CreateStore(vtablePtrArr, vtablePtrArrPtr);
    }

    // loop through meta type array and initialize every type's thread-local static data:
    BasicBlock* condBlock = BasicBlock::Create(this->llvmContext, "while_cond", init_func);
    BasicBlock* loopBlock = BasicBlock::Create(this->llvmContext, "while_loop", init_func);
    BasicBlock* postBlock = BasicBlock::Create(this->llvmContext, "while_post", init_func);

    auto indexA = builder.CreateAlloca(int32T, nullptr, "index");
    builder.CreateStore(constZeroV, indexA);
    builder.CreateBr(condBlock);  // branch from end of preceding block to condition-block

    builder.SetInsertPoint(condBlock);
    auto indexV = builder.CreateLoad(indexA);
    {
        auto condV = builder.CreateICmpNE(indexV, typeCountV);
        builder.CreateCondBr(condV, loopBlock, postBlock);
    }

    builder.SetInsertPoint(loopBlock);
    {
        Value* mtSizeIxs[] = { constZeroV, indexV, ConstantInt::get(int32T, 1) };
        auto allocSizeA = builder.CreateInBoundsGEP(metaTypesA, mtSizeIxs);
        auto allocSizeV = builder.CreateLoad(allocSizeA, "size");
        auto vtableI = CallInst::CreateMalloc(builder.GetInsertBlock(), mallocParameterType,
                                              int8T, constOneV, allocSizeV, nullptr, "vtable");
        builder.GetInsertBlock()->getInstList().push_back(vtableI);
        builder.CreateMemSet(vtableI, ConstantInt::get(int8T, 0), allocSizeV, 0);  // FIX ME: init the vtable

        {   // store the vtable pointer:
            Value* vtIxs[] = { constZeroV, indexV };
            auto vtPtrA = builder.CreateInBoundsGEP(vtablePtrArr, vtIxs);
            builder.CreateStore(vtableI, vtPtrA);
        }

        // bump the index:
        auto newIndexV = builder.CreateAdd(indexV, constOneV, "newindex");
        builder.CreateStore(newIndexV, indexA);
        builder.CreateBr(condBlock);  // branch from end of loop body to condition-block
    }

    builder.SetInsertPoint(postBlock);
    {
        //Value* retVal = builder.CreatePtrToInt(vtablePtrArr, int32T);
        builder.CreateRet(typeCountV);
    }

    return init_func;
}


/** Add main function so can be fully compiled
 * define i32 @main(i32 %argc, i8 **%argv)
 */
Function* LlvmGenerationContext::gen_main_function(const std::string userMain, bool hasIntReturnValue) {
    //define i32 @main(i32 %argc, i8 **%argv)
    Function *main_func = cast<Function>(this->llvmModule.getOrInsertFunction("main",
            IntegerType::getInt32Ty(this->llvmModule.getContext()),
            IntegerType::getInt32Ty(this->llvmModule.getContext()),
            PointerType::getUnqual(PointerType::getUnqual(IntegerType::getInt8Ty(this->llvmModule.getContext()))), NULL));
    {
        Function::arg_iterator args = main_func->arg_begin();
        Value *arg_0 = args++;
        arg_0->setName("argc");
        Value *arg_1 = args++;
        arg_1->setName("argv");
    }
    BasicBlock *bb = BasicBlock::Create(this->llvmModule.getContext(), "entry", main_func);

//    // initialize statics / runtime environment
//    Function *initFunc = this->gen_static_init_function();
//    CallInst *initCall = CallInst::Create(initFunc, "", bb);
//    initCall->setTailCall(false);

    //call i32 user main()
    auto userMainFName = userMain + "$func";
    auto func = this->llvmModule.getFunction(userMainFName);
    if (func) {
        auto nullClosureRefV = Constant::getNullValue(this->get_voidRefT());
        Value* args[] = { nullClosureRefV };
        CallInst *user_main_call = CallInst::Create(func, args, "", bb);
        user_main_call->setTailCall(false);
        user_main_call->setIsNoInline();
        auto int32T = Type::getInt32Ty(this->llvmModule.getContext());
        if (hasIntReturnValue) {
            // truncate return value to i32
            CastInst* truncVal = CastInst::CreateIntegerCast(user_main_call, int32T, true, "", bb);
            ReturnInst::Create(this->llvmModule.getContext(), truncVal, bb);
        }
        else {
            ReturnInst::Create(this->llvmModule.getContext(), ConstantInt::get(int32T, 0, true), bb);
        }
    }
    else {
        this->LOG.error("LLVM function not found for name: %s", userMain.c_str());
        ReturnInst::Create(this->llvmModule.getContext(), ConstantInt::get(this->llvmModule.getContext(), APInt(32, 0, true)), bb);
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
    this->entryFunction = this->gen_main_function(userMainIdent, mainFuncType->returnType);
    return this->entryFunction;
}

bool LlvmGenerationContext::verify_code() {
    //this->LOG.info("Verifying LLVM code...");;
    std::string errInfo;
    raw_string_ostream ostr(errInfo);
    bool ret = verifyModule(this->llvmModule, &ostr);
    if (ret)
        this->LOG.error("LLVM code verification failed: %s", errInfo.c_str());
    return ret;
}

void LlvmGenerationContext::print_IR() {
    // TODO: support writing to a .ll file
    this->LOG.info("Printing LLVM bytecode...");
    PrintModulePass printPass(outs());
    ModulePassManager pm;
    pm.addPass(printPass);
    pm.run(&this->llvmModule);
    std::cout << std::endl;
}

void LlvmGenerationContext::write_bitcode(const std::string& filepath) {
    this->LOG.info("Writing LLVM bitcode file '%s'", filepath.c_str());
    std::string errInfo;
    raw_fd_ostream ostream(filepath.c_str(), errInfo, sys::fs::F_RW);
    if (errInfo.empty())
        WriteBitcodeToFile(&this->llvmModule, ostream);
    else
        this->LOG.error("Failed to open bitcode output file for writing: %s", errInfo.c_str());
}


void LlvmGenerationContext::register_llvm_value(const std::string& identifier, Value* val) {
    if (identifier.compare(0, strlen(BUILTIN_NS), BUILTIN_NS) != 0)
        this->LOG.debug("Registering LLVM value %s : %s", identifier.c_str(), to_string(val->getType()).c_str());
    this->llvmSymbolTable.emplace(identifier, val);
}

Value* LlvmGenerationContext::lookup_llvm_value(const std::string& identifier) const {
    try {
        Value* val = this->llvmSymbolTable.at(identifier);
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


//static Function* gen_dummy_type_user_init_func(LlvmGenerationContext& context) {
//    std::string funcName("$dummy.$tuinit");
//    auto voidT = context.get_voidT();
//    std::vector<Type*> typeInitFuncArgTypes {
//        Type::getInt8PtrTy(context.llvmContext)  // void* to type's data
//    };
//    FunctionType *typeInitFuncType = FunctionType::get(voidType, typeInitFuncArgTypes, false);
//    Function *initFunc = cast<Function>(context.llvmModule.getOrInsertFunction(funcName, typeInitFuncType));
//    auto eb = BasicBlock::Create(context.llvmContext, "entry", initFunc);
//    ReturnInst::Create(context.llvmContext, eb);
//    return initFunc;
//}

void LlvmGenerationContext::initialize_meta_type_data() {
    /* This is a possible future C declaration equivalent of the constructed runtime type data:

    typedef void (*TypeInitializerF)(void* memory);

    typedef struct {
        uint32_t typeId;
        void* vtable;
        uint32_t size;
        TypeInitializerF initializer;
    } MetaType;

    extern uint32_t TYPE_COUNT;
    extern MetaType META_TYPES[];
    */

    // define the MetaType LLVM data type:
    auto int32T = Type::getInt32Ty(this->llvmContext);
//    std::vector<Type*> typeInitFuncArgTypes {
//        this->get_voidPtrT()  // void* to type's data
//    };
//    FunctionType *typeInitFuncT = FunctionType::get(this->get_voidT(), typeInitFuncArgTypes, false);
//    auto dummyUserInitF = gen_dummy_type_user_init_func(*this);

    auto emptyStructPtrT = PointerType::getUnqual(StructType::get(this->llvmContext));
    std::vector<Type*> memberTypes {
        int32T,  // type id
        emptyStructPtrT,  // vtable pointer
        //int32T,  // data structure size
        //typeInitFuncT,  // initialization function
    };
    StructType* metaType = StructType::get(this->llvmContext, memberTypes);

    // create static meta type data:
    uint32_t typeCount = 0;
    std::vector<Constant*> metaTypes;
    for (auto txType = this->tuplexPackage.types().types_cbegin(); txType != this->tuplexPackage.types().types_cend(); txType++) {
//        auto utinitF = (*txType)->get_type_user_init_func(*this);
//        if (! utinitF->getEntryBlock().getTerminator()) {
//            // inserting default void return instruction for entry block of function
//            ReturnInst::Create(this->llvmContext, &utinitF->getEntryBlock());
//        }
        auto vtableT = (*txType)->make_vtable_type(*this);
        if (!vtableT)
            continue;
        this->llvmVTableTypeMapping.emplace((*txType)->get_type_id(), vtableT);
        std::string vtableName((*txType)->get_declaration()->get_unique_full_name() + "$vtable");
        GlobalVariable* vtableV = new GlobalVariable(this->llvmModule, vtableT, true, GlobalValue::ExternalLinkage,
                                                     nullptr, vtableName);
        this->register_llvm_value(vtableV->getName(), vtableV);

        auto typeId = typeCount;
        std::vector<Constant*> members {
            ConstantInt::get(int32T, typeId),
            ConstantExpr::getBitCast(vtableV, emptyStructPtrT),
            // dummyUserInitF  // utinitF
        };
        metaTypes.push_back(ConstantStruct::get(metaType, members));

        // register the constant typeId values for later inclusion in the initialization code:
        std::string typeIdName((*txType)->get_declaration()->get_unique_full_name() + ".$typeid");
        this->register_llvm_value(typeIdName, ConstantInt::get(int32T, typeId));

        typeCount++;
    }
    auto mtArrayType = ArrayType::get(metaType, typeCount);
    auto mtArrayInit = ConstantArray::get(mtArrayType, metaTypes);

    Value* typeCountV = new GlobalVariable(this->llvmModule, int32T, true, GlobalValue::ExternalLinkage,
                                           ConstantInt::get(int32T, typeCount), "tx.runtime.TYPE_COUNT");
    Value* metaTypesV = new GlobalVariable(this->llvmModule, mtArrayType, true, GlobalValue::ExternalLinkage,
                                           mtArrayInit, "tx.runtime.META_TYPES");
    this->register_llvm_value(typeCountV->getName(), typeCountV);
    this->register_llvm_value(metaTypesV->getName(), metaTypesV);
}


void LlvmGenerationContext::initialize_external_functions() {
    // declare external C puts():
    std::vector<Type*> c_puts_args( { Type::getInt8PtrTy(this->llvmContext) } );
    FunctionType* c_puts_func_type = FunctionType::get(
      /*Result=*/Type::getInt32Ty(this->llvmContext),
      /*Params=*/c_puts_args,
      /*isVarArg=*/false);

    Function* c_putsF = Function::Create(
      /*Type=*/c_puts_func_type,
      /*Linkage=*/GlobalValue::ExternalLinkage, // (external, no body)
      /*Name=*/"puts",
      &this->llvmModule);
    c_putsF->setCallingConv(CallingConv::C);

    //this->register_llvm_value("tx.c.puts", c_puts_func);

    // create adapter function:
    auto cstrRefT = TxReferenceType::make_ref_llvm_type(*this, Type::getInt8Ty(this->llvmContext));
    Function *t_putsF = cast<Function>(this->llvmModule.getOrInsertFunction("tx.c.puts$func", this->get_voidT(), this->get_voidRefT(), cstrRefT, NULL));
    BasicBlock *bb = BasicBlock::Create(this->llvmModule.getContext(), "entry", t_putsF);
    IRBuilder<> builder(bb);
    GenScope scope(&builder);
    Function::arg_iterator args = t_putsF->arg_begin();
    args++;  // the implicit closure pointer (null)
    Value *arg_1 = args++;
    arg_1->setName("cstr");
    Value* ptrV = gen_get_ref_pointer(*this, &scope, arg_1);
    CallInst *cPutsCall = builder.CreateCall(c_putsF, ptrV);
    cPutsCall->setTailCall(false);
    ReturnInst::Create(this->llvmModule.getContext(), bb);

    // store lambda object:
    auto nullClosureRefV = Constant::getNullValue(this->get_voidRefT());
    std::vector<Type*> lambdaMemberTypes {
        t_putsF->getType(),   // function pointer
        this->get_voidRefT()  // null closure object pointer
    };
    auto lambdaT = StructType::get(this->llvmContext, lambdaMemberTypes);
    auto lambdaV = ConstantStruct::get(lambdaT, t_putsF, nullClosureRefV, NULL);
    auto lambdaA = new GlobalVariable(this->llvmModule, lambdaT, true, GlobalValue::InternalLinkage, lambdaV, "tx.c.puts");
    this->register_llvm_value("tx.c.puts", lambdaA);

// varargs example:
//    ArrayRef<Type*> FuncTy_7_args;
//    FunctionType* FuncTy_7 = FunctionType::get(
//      /*Result=*/Type::getInt32Ty(this->llvmContext),
//      /*Params=*/FuncTy_7_args,
//      /*isVarArg=*/true);
//
//    Function* func_foo = Function::Create(
//      /*Type=*/FuncTy_7,
//      /*Linkage=*/GlobalValue::ExternalLinkage, // (external, no body)
//      /*Name=*/"foo",
//      &this->llvmModule);
//     func_foo->setCallingConv(CallingConv::C);
}


void LlvmGenerationContext::initialize_builtins() {
    this->initialize_meta_type_data();
    this->initialize_external_functions();
}


///** Looks up a field declaration. If the symbol is overloaded, returns its first field declaration. */
//static const TxField* lookup_field_simple(TxScopeSymbol* scope, ResolutionContext& resCtx, const TxIdentifier& ident) {
//    TxScopeSymbol* symbol = lookup_symbol(scope, ident);
//    if (auto entSym = dynamic_cast<TxEntitySymbol*>(symbol))
//        return entSym->get_first_field_decl()->get_field_definer()->get_field();
//    return nullptr;
//}

void LlvmGenerationContext::generate_runtime_data() {
    for (auto txType = this->tuplexPackage.types().types_cbegin(); txType != this->tuplexPackage.types().types_cend(); txType++) {
        if (auto entity = (*txType)->get_symbol()) {
            std::string vtableName(entity->get_full_name().to_string() + "$vtable");
            if (auto vtableV = dyn_cast<GlobalVariable>(this->lookup_llvm_value(vtableName))) {
                std::vector<Constant*> initMembers;
                auto instanceMethods = (*txType)->get_instance_methods();
                auto virtualFields   = (*txType)->get_virtual_fields();
                initMembers.resize(instanceMethods.get_field_count() + virtualFields.get_field_count());
                for (auto & field : instanceMethods.fieldMap) {
                    auto actualFieldEnt = instanceMethods.fields.at(field.second);
                    //std::cout << "inserting instance method: " << field.first << " at ix " << field.second << ": " << actualFieldEnt << std::endl;
                    auto funcName = actualFieldEnt->get_declaration()->get_unique_full_name() + "$func";
                    auto llvmField = cast<Constant>(this->lookup_llvm_value(funcName));
                    auto ix = field.second;
                    initMembers[ix] = llvmField;
                }
                for (auto & field : virtualFields.fieldMap) {
                    auto actualFieldEnt = virtualFields.fields.at(field.second);
                    //std::cout << "inserting virtual field: " << field.first << " at ix " << field.second << ": " << actualFieldEnt << std::endl;
                    auto fieldName = actualFieldEnt->get_declaration()->get_unique_full_name();
                    auto llvmField = cast<Constant>(this->lookup_llvm_value(fieldName));
                    auto ix = field.second + instanceMethods.get_field_count();
                    initMembers[ix] = llvmField;
                }
                Constant* initializer = ConstantStruct::getAnon(this->llvmContext, initMembers);
                vtableV->setInitializer(initializer);
            }
            else
                this->LOG.error("No vtable found for %s", vtableName.c_str());
        }
    }
}


llvm::Value* LlvmGenerationContext::gen_malloc(GenScope* scope, llvm::Type* objT) {
    auto int32T = Type::getInt32Ty(this->llvmContext);
    auto mallocParameterType = int32T;
    auto objSizeV = ConstantExpr::getTruncOrBitCast(ConstantExpr::getSizeOf(objT), int32T);
    auto objCountV = ConstantInt::get(int32T, 1);
    auto objAllocI = CallInst::CreateMalloc(scope->builder->GetInsertBlock(), mallocParameterType,
                                            objT, objSizeV, objCountV, nullptr, "");
    scope->builder->GetInsertBlock()->getInstList().push_back(objAllocI);
    return objAllocI;
}


Value* LlvmGenerationContext::gen_get_vtable(GenScope* scope, const TxType* statDeclType, Value* runtimeBaseTypeIdV) const {
    Value* metaTypesV = this->lookup_llvm_value("tx.runtime.META_TYPES");
    Value* ixs[] = { ConstantInt::get(Type::getInt32Ty(this->llvmContext), 0),
                     runtimeBaseTypeIdV,
                     ConstantInt::get(Type::getInt32Ty(this->llvmContext), 1) };
    Value* vtablePtrA;
    if (!scope)
        vtablePtrA = GetElementPtrInst::CreateInBounds(metaTypesV, ixs);
    else
        vtablePtrA = scope->builder->CreateInBoundsGEP(metaTypesV, ixs);

    // cast vtable type according to statically declared type (may be parent type of actual type):
    if (auto vtableT = this->get_llvm_vtable_type(statDeclType)) {
        Type* vtablePtrT = PointerType::getUnqual(vtableT);
        if (!scope) {
            auto vtablePtr = new LoadInst(vtablePtrA);
            return CastInst::CreatePointerCast(vtablePtr, vtablePtrT, "vtableptr");
        }
        else {
            auto vtablePtr = scope->builder->CreateLoad(vtablePtrA);
            return scope->builder->CreatePointerCast(vtablePtr, vtablePtrT, "vtableptr");
        }
    }
    return nullptr;
}

Value* LlvmGenerationContext::gen_get_vtable(GenScope* scope, const TxType* statDeclType) const {
    return gen_get_vtable(scope, statDeclType, ConstantInt::get(Type::getInt32Ty(this->llvmContext), statDeclType->get_type_id()));
}


StructType* LlvmGenerationContext::get_llvm_vtable_type(const TxType* txType) const {
    auto iter = this->llvmVTableTypeMapping.find(txType->get_type_id());
    if (iter != this->llvmVTableTypeMapping.end())
        return iter->second;
    return nullptr;
}


Type* LlvmGenerationContext::get_llvm_type(const TxType* txType) {
    ASSERT(txType, "NULL txType provided to getLlvmType()");
    if (txType->is_virtual_specialization())
        // same data type as base type
        return this->get_llvm_type(txType->get_base_type());

    // note: we do map abstract types (e.g. reference targets)

    auto iter = this->llvmTypeMapping.find(txType);
    if (iter != this->llvmTypeMapping.end()) {
        return iter->second;
    }
	Type* llvmType = txType->make_llvm_type(*this);
	if (llvmType)
	    this->llvmTypeMapping.emplace(txType, llvmType);
	else
		this->LOG.error("No LLVM type mapping for type: %s", txType->to_string().c_str());
	return llvmType;
}
