#pragma once

#include <typeinfo>
#include <vector>
#include <stack>
#include <map>

#include "txassert.hpp"

#include "tx_operations.hpp"
#include "identifier.hpp"
#include "package.hpp"
#include "module.hpp"
#include "entity.hpp"

#include "parser_driver.hpp"
#include "location.hh"


/* forward declarations pertaining to LLVM code generation */
class LlvmGenerationContext;
class GenScope;
namespace llvm {
    class Value;
}


class TxNode : public Printable {
private:
    LexicalContext lexContext;

protected:
    void set_context(const LexicalContext& context) {
        ASSERT(!this->is_context_set(), "lexicalContext already initialized in " << this->to_string());
        this->lexContext = context;
    }
    /** Sets the lexical context of this node to be equal to that of the provided node. */
    void set_context(const TxNode* node) {
        this->set_context(node->context());
    }
    /** Sets the lexical context of this node to the current context of the module. */
    void set_context(TxSymbolScope* lexContext) {
        this->set_context(LexicalContext(lexContext));
    }
    inline bool is_context_set() const { return this->lexContext.scope(); }

//    inline const TypeRegistry& types() const {
//        return this->context().get_package()->types();
//    }
    inline TypeRegistry& types() const {
        return const_cast<TxPackage*>(this->context().package())->types();  // hackish... review type creation approach
    }

public:
    const yy::location parseLocation;

    TxNode(const yy::location& parseLocation) : lexContext(), parseLocation(parseLocation) { }
    //virtual ~TxNode() = default;
    virtual ~TxNode() {
        std::cout << "Running destructor of " << *this << std::endl;
    }

    inline const LexicalContext& context() const {
        ASSERT(this->is_context_set(), "lexicalContext not initialized in " << this->to_string());
        return this->lexContext;
    }
    inline LexicalContext& context() {
        return const_cast<LexicalContext&>(static_cast<const TxNode *>(this)->context());
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;

    virtual std::string to_string() const {
        char buf[256];
        snprintf(buf, 256, "%-28s at %s", typeid(*this).name(), this->parse_loc_string().c_str());
        return std::string(buf);
    }

    std::string parse_loc_string() const {
        char buf[128];
        if (parseLocation.begin.line == parseLocation.end.line)
            snprintf(buf, 128, "line %d (columns %d-%d)", parseLocation.begin.line, parseLocation.begin.column, parseLocation.end.column);
        else
            snprintf(buf, 128, "lines:columns %d:%d-%d:%d", parseLocation.begin.line, parseLocation.begin.column, parseLocation.end.line, parseLocation.end.column);
        return std::string(buf);
    }
};


class TxIdentifierNode : public TxNode {
public:
    enum IdentifierClass { UNSPECIFIED, MODULE_ID, IMPORT_ID, TYPE_ID, FIELD_ID, DATASPACE_ID };
    const IdentifierClass idClass;
    const TxIdentifier& ident;

    TxIdentifierNode(const yy::location& parseLocation, const TxIdentifier* ident, IdentifierClass identifierClass=UNSPECIFIED)
        : TxNode(parseLocation), idClass(identifierClass), ident(*ident)  { }

    virtual std::string to_string() const {
        return "'" + this->ident.to_string() + "'";
    }
};



class TxImportNode : public TxNode {
public:
    const TxIdentifierNode* identNode;

    TxImportNode(const yy::location& parseLocation, const TxIdentifierNode* identifier)
        : TxNode(parseLocation), identNode(identifier)  { }

    virtual void symbol_table_pass(TxModule* module) {
        this->set_context(module);
        if (! identNode->ident.is_qualified())
            parser_error(this->parseLocation, "can't import unqualified identifier '%s'", identNode->ident.to_string().c_str());
        module->register_import(identNode->ident);
    }
};


class TxDeclarationNode : public TxNode {  // either type or field
public:
    const TxDeclarationFlags declFlags;

    TxDeclarationNode(const yy::location& parseLocation, const TxDeclarationFlags declFlags)
        : TxNode(parseLocation), declFlags(declFlags) { }

    virtual void symbol_table_pass(LexicalContext& lexContext) = 0;

    virtual void semantic_pass() = 0;
};


class TxModuleNode : public TxNode {
    const TxIdentifierNode* identNode;
    std::vector<TxImportNode*>* imports;
    std::vector<TxDeclarationNode*>* members;
    std::vector<TxModuleNode*>* subModules;
    TxModule* module;  // set in symbol table pass
public:
    TxModuleNode(const yy::location& parseLocation, const TxIdentifierNode* identifier,
                 std::vector<TxImportNode*>* imports, std::vector<TxDeclarationNode*>* members,
                 std::vector<TxModuleNode*>* subModules)
        : TxNode(parseLocation), identNode(identifier), imports(imports), members(members), subModules(subModules), module()  {
        ASSERT(identifier, "NULL identifier");  // (sanity check on parser)
    }

    virtual void symbol_table_pass(TxModule* parent) {
        this->set_context(parent);
        this->module = parent->declare_module(identNode->ident);
        if (this->imports) {
            for (auto elem : *this->imports)
                elem->symbol_table_pass(this->module);
        }
        if (this->members) {
            LexicalContext lexContext(this->module);
            for (auto elem : *this->members)
                elem->symbol_table_pass(lexContext);
        }
        if (this->subModules) {
            for (auto mod : *this->subModules)
                mod->symbol_table_pass(this->module);
        }
    }

    virtual void semantic_pass() {
        if (this->members)
            for (auto elem : *this->members) {
                elem->semantic_pass();
            }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};


/** Represents a parsing unit, i.e. a given source text input (e.g. source file). */
class TxParsingUnitNode : public TxNode {
    std::vector<TxModuleNode*> modules;
public:

    TxParsingUnitNode(const yy::location& parseLocation) : TxNode(parseLocation) { }

    void add_module(TxModuleNode* module) {
        this->modules.push_back(module);
    }

    virtual void symbol_table_pass(TxPackage* package) {
        for (auto mod : this->modules)
            mod->symbol_table_pass(package);
    }

    virtual void semantic_pass() {
        for (auto mod : this->modules)
            mod->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};



class TxStatementNode : public TxNode {
public:
    TxStatementNode(const yy::location& parseLocation) : TxNode(parseLocation) { }
    virtual void symbol_table_pass(LexicalContext& lexContext) = 0;
    virtual void semantic_pass() = 0;
};


class TxTypeExpressionNode : public TxNode, public TxTypeProxy {
    mutable TxType const * cachedType = nullptr;
    TxTypeEntity* declaredEntity = nullptr;  // null until initialized in symbol table pass

    TxTypeEntity* declare_type(LexicalContext& lexContext, const std::string& implicitTypeName, TxDeclarationFlags declFlags) {
        this->declaredEntity = lexContext.scope()->declare_type(implicitTypeName, this, declFlags);
        return this->declaredEntity;
    }

    const std::vector<TxTypeParam>* makeTypeParams(const std::vector<TxDeclarationNode*>* typeParamDecls);

protected:
    const std::vector<TxTypeParam>* declTypeParams = nullptr;    // null unless set in symbol table pass
    virtual void symbol_table_pass_descendants(LexicalContext& lexContext, TxDeclarationFlags declFlags) = 0;
    virtual const TxType* define_type(std::string* errorMsg=nullptr) const = 0;

public:
    TxTypeExpressionNode(const yy::location& parseLocation) : TxNode(parseLocation)  { }

    /** Returns the type entity declared by this type expression node, or NULL if it did not declare a new type entity. */
    virtual TxTypeEntity* symbol_table_pass(LexicalContext& lexContext, const std::string& typeName, TxDeclarationFlags declFlags,
                                            const std::vector<TxDeclarationNode*>* typeParamDecls = nullptr) {
        // Note: This scheme with the purpose of naming every type construct within a type expression with an entity
        // might be removed in future. It may prevent things like value assignment (unnamed types mismatching the
        // auto-generated implicit types).
        // Type entities are however needed for:
        //  - adding members (since members are namespace symbols) - only done in explicit type extensions
        //  - using fields in type expressions (which refers to the field's type) - not terribly important
        // Implicitly declared types should have the same visibility as the type/field they are for.
        this->set_context(lexContext);
        if (typeParamDecls)
            this->declTypeParams = this->makeTypeParams(typeParamDecls);
        auto newTypeEnt = this->declare_type(lexContext, typeName, declFlags);
        LexicalContext typeExtensionCtx(newTypeEnt);
        this->symbol_table_pass_descendants(typeExtensionCtx, declFlags);
        return newTypeEnt;
    }

    /** Gets the type entity representing the declaration of this type expression. */
    virtual TxTypeEntity* get_entity() const { return this->declaredEntity; }

    inline virtual const TxType* get_type() const final {
        ASSERT(this->is_context_set(), "Can't call get_type() before symbol table pass has completed: "  << this);
        if (! cachedType) {
            std::string errorMsg;
            this->cachedType = this->define_type(&errorMsg);
            if (! cachedType) {
                if (! errorMsg.empty())
                    parser_error(this->parseLocation, "%s", errorMsg.c_str());
                return nullptr;
            }
            if (this->declaredEntity)
                ASSERT(this->cachedType->entity()==this->declaredEntity,
                        "entity " << this->cachedType->entity() << " (of type " << this->cachedType
                        << ") is not same as declared entity " << this->declaredEntity
                        << " (of node " << *this << ")");
        }
        return cachedType;
    }

    virtual void semantic_pass() = 0;
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};

class TxFieldDefNode;

class TxExpressionNode : public TxNode, public TxTypeProxy, public TxConstantProxy {
    mutable TxType const * cachedType = nullptr;
protected:
    const std::vector<const TxType*>* appliedFuncArgTypes = nullptr; // injected by expression context if applicable

    /** Defines/obtains the type (as specific as can be known) of the value this expression produces. */
    virtual const TxType* define_type(std::string* errorMsg=nullptr) const = 0;

public:
    const TxFieldDefNode* fieldDefNode = nullptr; // injected by field definition if known and applicable

    TxExpressionNode(const yy::location& parseLocation) : TxNode(parseLocation) { }

    virtual void symbol_table_pass(LexicalContext& lexContext) = 0;
    virtual void semantic_pass() = 0;

    /** Returns the type (as specific as can be known) of the value this expression produces. */
    virtual const TxType* get_type() const override final {
        if (! cachedType) {
            std::string errorMsg;
            this->cachedType = this->define_type(&errorMsg);
            if (! cachedType) {
                if (! errorMsg.empty())
                    parser_error(this->parseLocation, "%s", errorMsg.c_str());
                return nullptr;
            }
        }
        return cachedType;
    }

    ///** Returns true if this expression represents a specific storage location (e.g. a field). */
    //virtual bool has_address() const { return false; }

    /** Returns true if this expression is a constant expression that can be evaluated at compile time. */
    virtual bool is_statically_constant() const { return false; }

    virtual long get_int_value() const {
        throw std::logic_error("Getting constant int value not supported for expression node " + this->to_string());
    }

    virtual bool hasAppliedFuncArgTypes()  { return this->appliedFuncArgTypes; }
    virtual void set_applied_func_arg_types(std::vector<const TxType*>* appliedFuncArgTypes) {
        this->appliedFuncArgTypes = appliedFuncArgTypes;
    }

    ///** Returns true if this expression is a lambda expression (function definition). */
    //virtual bool isLambda(const TuplexContext& context) const { return false; }
};


TxExpressionNode* wrapConversion(TxSymbolScope* scope, TxExpressionNode* originalExpr, const TxType* requiredType,
                                 bool _explicit=false);


class TxFieldDefNode : public TxNode, public TxTypeProxy {
    mutable TxType const * cachedType = nullptr;

    bool modifiable;  // true if field name explicitly declared modifiable
    TxDeclarationFlags declFlags = TXD_NONE;

protected:
    virtual const TxType* define_type(std::string* errorMsg=nullptr) const {
        const TxType* fieldType;
        if (this->typeExpression)
            fieldType = this->typeExpression->get_type();
        else {
            fieldType = this->initExpression->get_type();
            if (fieldType) {
                if (this->modifiable) {
                    if (! fieldType->is_modifiable())
                        fieldType = this->types().get_modifiable_type(fieldType, errorMsg);
                }
                else if (fieldType->is_modifiable())
                    // if initialization expression is modifiable type, and modifiable not explicitly specified,
                    // lose modifiable attribute (modifiability must be explicit)
                    fieldType = fieldType->get_base_type_spec().type;
            }
        }
        // note: does not ensure implicit type declaration for field's type if it is not an explicit type
        return fieldType;
    }

public:
    const std::string ident;
    TxTypeExpressionNode* typeExpression;
    TxExpressionNode* initExpression;
    TxFieldEntity* declaredEntity;  // null until initialized in symbol table pass

    TxFieldDefNode(const yy::location& parseLocation, const std::string& identifier,
                   TxTypeExpressionNode* typeExpression, TxExpressionNode* initExpression)
            : TxNode(parseLocation), modifiable(false), ident(identifier), declaredEntity() {
        ASSERT(!identifier.empty(), "Empty identifier string");
        this->typeExpression = typeExpression;
        this->initExpression = initExpression;
    }
    TxFieldDefNode(const yy::location& parseLocation, const std::string& identifier,
                   TxExpressionNode* initExpression, bool modifiable=false)
            : TxNode(parseLocation), modifiable(modifiable), ident(identifier), declaredEntity() {
        ASSERT(!identifier.empty(), "Empty identifier string");
        this->typeExpression = nullptr;
        this->initExpression = initExpression;
    }

    void symbol_table_pass_local_field(LexicalContext& lexContext, bool create_local_scope) {
        if (create_local_scope)
            lexContext.scope(lexContext.scope()->create_code_block_scope());
        this->symbol_table_pass_decl_field(lexContext, TXD_NONE, TXS_STACK, TxIdentifier(""));
    }

    void symbol_table_pass_decl_field(LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                      TxFieldStorage storage, const TxIdentifier& dataspace) {
        this->declFlags = declFlags;
        this->declaredEntity = lexContext.scope()->declare_field(this->ident, this, declFlags, storage, dataspace);
        this->symbol_table_pass(lexContext);
    }

    void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        if (this->typeExpression)
            this->typeExpression->symbol_table_pass(lexContext, this->ident + "$type", this->declFlags & (TXD_PUBLIC | TXD_PROTECTED));
        if (this->initExpression) {
            this->initExpression->fieldDefNode = this;
            this->initExpression->symbol_table_pass(lexContext);
        }
    };

    TxFieldEntity* get_entity() const {
        ASSERT(this->declaredEntity, "Declared field entity not initialized");
        return this->declaredEntity;
    }

    virtual const TxType* get_type() const override final {
        if (! cachedType) {
            std::string errorMsg;
            this->cachedType = this->define_type(&errorMsg);
            if (! cachedType) {
                if (! errorMsg.empty())
                    parser_error(this->parseLocation, "%s", errorMsg.c_str());
                return nullptr;
            }
        }
        return cachedType;
    }

    void semantic_pass() {
        if (this->typeExpression)
            this->typeExpression->semantic_pass();
        if (this->initExpression) {
            this->initExpression->semantic_pass();
            if (this->typeExpression) {
                this->initExpression = wrapConversion(this->context().scope(), this->initExpression, this->typeExpression->get_type());
            }
            if (this->get_entity()->is_statically_constant())
                if (! this->initExpression->is_statically_constant())
                    parser_error(this->parseLocation, "Non-constant initializer for constant global/static field.");
        }
        if (auto type = this->get_type())
            if (! type->is_concrete())
                parser_error(this->parseLocation, "Field type %s is not concrete (size potentially unknown).", type->to_string().c_str());
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;

    virtual std::string to_string() const {
        char buf[256];
        snprintf(buf, 256, "%-28s '%s' at %s", typeid(*this).name(), this->ident.c_str(), this->parse_loc_string().c_str());
        return std::string(buf);
    }
};

/** Non-local field declaration */
class TxFieldDeclNode : public TxDeclarationNode {
    const bool isMethod = false;
public:
    TxFieldDefNode* field;

    TxFieldDeclNode(const yy::location& parseLocation, const TxDeclarationFlags declFlags, TxFieldDefNode* field,
                    bool isMethod=false)
            : TxDeclarationNode(parseLocation, declFlags), isMethod(isMethod), field(field) { }

    virtual void symbol_table_pass(LexicalContext& lexContext);

    virtual void semantic_pass() {
        this->field->semantic_pass();
        if (auto type = this->field->get_type())
            if ((this->field->get_entity()->get_storage() == TXS_GLOBAL) && type->is_modifiable())
                parser_error(this->parseLocation, "Global fields may not be modifiable: %s", field->ident.c_str());
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};


/** Non-local type declaration */
class TxTypeDeclNode : public TxDeclarationNode {
    /** If the type expression is a simple type identifier, wraps the type expression in a type alias declaration node. */
    void wrap_alias();

public:
    const std::string typeName;
    const std::vector<TxDeclarationNode*>* typeParamDecls;
    TxTypeExpressionNode* typeExpression;

    TxTypeDeclNode(const yy::location& parseLocation, const TxDeclarationFlags declFlags,
                   const std::string typeName, const std::vector<TxDeclarationNode*>* typeParamDecls,
                   TxTypeExpressionNode* typeExpression)
        : TxDeclarationNode(parseLocation, declFlags), //declaredEntity(),
          typeName(typeName), typeParamDecls(typeParamDecls), typeExpression(typeExpression) {
    }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        // type declaration is performed by the type expression node, unless this is a decl of an alias for a predef type:
        this->wrap_alias();
        auto newTypeEntity = this->typeExpression->symbol_table_pass(lexContext, this->typeName, this->declFlags, this->typeParamDecls);
        if (newTypeEntity) {
            // declare type parameters, if any, within type definition's scope:
            LexicalContext typeCtx(newTypeEntity);
            if (this->typeParamDecls) {
                for (auto paramDecl : *this->typeParamDecls) {
                    paramDecl->symbol_table_pass(typeCtx);
                }
            }
        }
//        if (this->typeParams) {
//            for (auto & param : *this->typeParams) {
//                if (param.meta_type() == TxTypeParam::TXB_VALUE) {
//                    newTypeEntity->declare_field(param.param_name(), param.get_base_type_definer(), TXD_PUBLIC, TXS_STATIC, TxIdentifier());
//                }
//                else {  // TXB_TYPE
//                    const TxTypeProxy* baseTypeDefiner = param.has_base_type_definer()
//                                                         ? param.get_base_type_definer()
//                                                         : this->types().get_builtin_type(ANY);
//                    newTypeEntity->declare_type(param.param_name(), baseTypeDefiner, TXD_PUBLIC);
//                }
//            }
//        }
        //parser_error(this->parseLocation, "Declared type parameters but type definition does not describe a generic type.");
    }

    TxTypeEntity* get_entity() const {
        ASSERT(this->typeExpression->get_entity(), "Declared type entity not initialized");
        return this->typeExpression->get_entity();
    }

    virtual void semantic_pass() {
        this->typeExpression->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};


class TxAssigneeNode : public TxNode {
public:
    TxAssigneeNode(const yy::location& parseLocation) : TxNode(parseLocation) { }
    virtual void symbol_table_pass(LexicalContext& lexContext) = 0;
    virtual void semantic_pass() = 0;

    /** Gets the type of this assignee. */
    virtual const TxType* get_type() const = 0;
};
