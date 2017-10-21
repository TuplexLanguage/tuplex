#pragma once

#include <stack>
#include <memory.h>

#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/Support/raw_ostream.h>

#include "tx_logging.hpp"

#include "symbol/type.hpp"

class TxParsingUnitNode;
class TxTypeDeclNode;

class CompoundStatementScope {
public:
    llvm::BasicBlock* continueBlock;
    llvm::BasicBlock* breakBlock;

    CompoundStatementScope( llvm::BasicBlock* continueBlock, llvm::BasicBlock* breakBlock )
            : continueBlock( continueBlock ), breakBlock( breakBlock ) {
    }
};

class GenScope {
    //llvm::IRBuilderBase::InsertPoint allocaInsertionPoint;
    llvm::Instruction* lastAllocaInstr = nullptr;
    llvm::BasicBlock* entryBlock;
    llvm::BasicBlock* currentBlock;

public:
    llvm::IRBuilder<> * const builder;
    std::stack<CompoundStatementScope*> compStmtStack;

    GenScope( llvm::IRBuilder<> *builder )
            : entryBlock( builder->GetInsertBlock() ), currentBlock(), builder( builder ) {
    }

    void use_alloca_insertion_point();
    void use_current_insertion_point();
};

/** An instance of this class represents a Tuplex to LLVM IR compilation context,
 * and has a 1-1 relationship to an LLVM Module.
 */
class LlvmGenerationContext {
    Logger& _LOGGER = Logger::get( "LLVMGEN" );

    llvm::Function* entryFunction = nullptr;
    std::map<const std::string, llvm::Value*> llvmSymbolTable;
    std::map<const TxActualType*, llvm::Type*> llvmTypeMapping;
    std::map<const TxActualType*, llvm::GlobalVariable*> llvmVTables;
    std::map<const TxActualType*, llvm::StructType*> llvmVTableTypes;

    /** table of byte array constants (also used for cstrings) to share identical instances */
    std::map<const std::vector<uint8_t>, llvm::Constant*> byteArrayTable;
    /** table of String object constants to share identical instances */
    std::map<const std::vector<uint8_t>, llvm::Constant*> stringObjTable;

    std::unique_ptr<llvm::Module> llvmModulePtr;

    // some common, basic types:
    llvm::Type* voidPtrT;
    llvm::Type* closureRefT;
    llvm::Type* i32T;
    llvm::PointerType* superTypesPtrT;

    // simple symbol table for 'internal' llvm values (not in the normal AST symbol table):
    void register_llvm_value( const std::string& identifier, llvm::Value* val );
    llvm::Value* lookup_llvm_value( const std::string& identifier ) const;

    llvm::StructType* get_vtable_meta_type( const TxActualType* acttype );
    llvm::GlobalVariable* get_vtable( const TxActualType* acttype );

    llvm::Value* gen_get_type_info( GenScope* scope, const TxActualType* staticType, llvm::Value* runtimeBaseTypeIdV, unsigned fieldIndex );

    llvm::Function* gen_main_function( const std::string userMain, bool hasIntReturnValue );
    void gen_get_supertypes_array_function();
    void gen_array_any_equals_function();
    void gen_array_elementary_equals_function();

public:
    TxPackage& tuplexPackage;
    llvm::LLVMContext& llvmContext;

    LlvmGenerationContext( TxPackage& tuplexPackage, llvm::LLVMContext& llvmContext )
            : llvmModulePtr( new llvm::Module("top", llvmContext ) ),
              tuplexPackage( tuplexPackage ),
              llvmContext( llvmContext )
    {
        this->voidPtrT = llvm::Type::getInt8PtrTy( this->llvmContext );
        this->closureRefT = TxReferenceTypeClassHandler::make_ref_llvm_type( *this, llvm::Type::getInt8Ty( this->llvmContext ), "ClosRef" );
        this->i32T = llvm::Type::getInt32Ty( this->llvmContext );
        this->superTypesPtrT = llvm::PointerType::getUnqual( llvm::StructType::get( i32T, i32T, llvm::ArrayType::get( i32T, 0 ), NULL ) );
    }

    inline llvm::Module& llvmModule() const {
        return *this->llvmModulePtr;
    }

    inline llvm::Type* get_voidPtrT() const {
        return this->voidPtrT;
    }
    inline llvm::Type* get_closureRefT() const {
        return this->closureRefT;
    }

    llvm::Type* get_llvm_type( const TxQualType& qtype ) {
        return this->get_llvm_type( qtype.type() );
    }
    llvm::Type* get_llvm_type( const TxActualType* txType );

    /** Allocates global storage for constant strings, a single shared instance for each unique value. */
    llvm::Constant* gen_const_cstring_address( const std::string& value );
    /** Allocates global storage for constant byte arrays, a single shared instance for each unique value. */
    llvm::Constant* gen_const_byte_array_address( const std::vector<uint8_t>& array );
    /** Allocates global storage for constant String objects, a single shared instance for each unique value. */
    llvm::Constant* gen_const_string_obj_address( llvm::StructType* stringObjT, llvm::Constant* arrayTIdC, const std::vector<uint8_t>& array );


    // "intrinsics":
    /** Generates a malloc call that allocates storage for the specified LLVM type. */
    llvm::Value* gen_malloc( GenScope* scope, llvm::Type* objT );
    /** Generates a malloc call that allocates storage for the specified number of bytes. */
    llvm::Value* gen_malloc( GenScope* scope, llvm::Value* sizeV );

    /** Generates a dynamic call to lval.equals(rval) and returns the returned value (a boolean, i1). */
    llvm::Value* gen_equals_invocation( GenScope* scope, llvm::Value* lvalA, llvm::Value* lvalTypeIdV,
                                        llvm::Value* rvalA, llvm::Value* rvalTypeIdV );

    llvm::Value* gen_isa( GenScope* scope, llvm::Value* refV, llvm::Value* typeIdV );

    void gen_panic_call( GenScope* scope, const std::string& message );
    void gen_panic_call( GenScope* scope, const std::string& message, llvm::Value* ulongValV );

    llvm::Value* gen_get_vtable( GenScope* scope, const TxActualType* statDeclType, llvm::Value* typeIdV );

    /** Generates code that gets the instance/element size for a given type id value.
     * NOTE: For arrays this returns the instance size of their element type.
     * @return an i32 value */
    llvm::Value* gen_get_element_size( GenScope* scope, const TxActualType* staticType, llvm::Value* typeIdV );

    /** Generates code that gets the element type id for a given Array type id.
     * Only valid to call for array types.
     * @return an i32 value */
    llvm::Value* gen_get_element_type_id( GenScope* scope, const TxActualType* staticType, llvm::Value* typeIdV );

    /** Generates code that gets the type class id for a given type id.
     * @return an i8 value */
    llvm::Value* gen_get_type_class( GenScope* scope, const TxActualType* staticType, llvm::Value* typeIdV );

    /** Generates code that gets the super-types array for a given type id.
     * @return a pointer to a Tuplex array of Int: { i32, i32, [0 x i32] }* */
    llvm::Value* gen_get_supertypes_array( GenScope* scope, const TxActualType* staticType, llvm::Value* typeIdV );

    llvm::Value* gen_get_supertypes_array_ref( GenScope* scope, const TxActualType* staticType, llvm::Value* runtimeBaseTypeIdV,
                                               llvm::Constant* arrayTypeIdC );

    /** Generate the LLVM code for the provided AST, which must be in global/static scope. */
    int generate_code( const TxParsingUnitNode* staticScopeNode );
    int generate_code( const TxTypeDeclNode* staticScopeNode );

    void generate_runtime_type_info();

    void declare_builtin_code();
    void generate_builtin_code();

    void generate_runtime_vtables();

    /** Create the top level function to call as program entry.
     * (This is the built-in main, which calls the user main function.)  */
    bool generate_main( const std::string& userMainIdent, const TxActualType* mainFuncType );

    void initialize_target();

    /** Verfies the generated LLVM code.
     * Should only be used for debugging, may mess with LLVM's state.
     * @return 0 upon success
     */
    int verify_code();

    /** Print the LLVM IR in a human-readable format to stdout */
    void print_IR();

    /** Print the LLVM IR as binary bitcode to a file.
     * @return 0 upon success */
    int write_bitcode( const std::string& filepath );

    /** Returns the program's return code. */
    int run_code();

    inline Logger* LOGGER() const {
        return &this->_LOGGER;
    }
};

/*--- adapters so that LLVM Type and Value can be output to printf, cout & cerr ---*/

inline std::string to_string( const llvm::Type& type ) {
    std::string tmp_str;
    llvm::raw_string_ostream rso( tmp_str );
    type.print( rso );
    return rso.str();
}
inline std::string to_string( const llvm::Type* type ) {
    return type ? to_string( *type ) : "NULL";
}

inline std::string to_string( const llvm::Value& value ) {
    std::string tmp_str;
    llvm::raw_string_ostream rso( tmp_str );
    value.print( rso );
    return rso.str();
}
inline std::string to_string( const llvm::Value* value ) {
    return value ? to_string( *value ) : "NULL";
}

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<<( std::basic_ostream<charT, traits> &lhs, const llvm::Type& type ) {
    std::string tmp_str;
    llvm::raw_string_ostream rso( tmp_str );
    type.print( rso );
    return lhs << rso.str();
}

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<<( std::basic_ostream<charT, traits> &lhs, const llvm::Value& value ) {
    std::string tmp_str;
    llvm::raw_string_ostream rso( tmp_str );
    value.print( rso );
    return lhs << rso.str();
}

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<<( std::basic_ostream<charT, traits> &lhs, const llvm::Type* type ) {
    std::string tmp_str;
    llvm::raw_string_ostream rso( tmp_str );
    if ( !type )
        return lhs << "NULL";
    type->print( rso );
    return lhs << rso.str();
}

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<<( std::basic_ostream<charT, traits> &lhs, const llvm::Value* value ) {
    std::string tmp_str;
    llvm::raw_string_ostream rso( tmp_str );
    if ( !value )
        return lhs << "NULL";
    value->print( rso );
    return lhs << rso.str();
}
