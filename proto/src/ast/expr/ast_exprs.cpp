#include "ast_exprs.hpp"
#include "ast_lambda_node.hpp"
#include "ast_array.hpp"
#include "ast_ref.hpp"
#include "ast/ast_util.hpp"
#include "ast_field.hpp"


const TxQualType* TxConstructorCalleeExprNode::define_type() {
    ASSERT( this->appliedFuncArgs, "appliedFuncArgTypes of TxConstructorCalleeExprNode not initialized" );
    {
        auto allocType = this->objectExpr->resolve_type();
        // find the constructor (note, constructors aren't inherited)
        auto instanceBaseType = allocType->type()->get_instance_base_type();
        auto constrMember = lookup_member( this->context().scope(), instanceBaseType->get_declaration()->get_symbol(), CONSTR_IDENT );
        if ( auto constructorSymbol = dynamic_cast<TxEntitySymbol*> ( constrMember ) ) {
            if ( auto constructorDecl = resolve_field( this, constructorSymbol, this->appliedFuncArgs ) ) {
                ASSERT( constructorDecl->get_decl_flags() & ( TXD_CONSTRUCTOR | TXD_INITIALIZER ),
                        "field named " CONSTR_IDENT " is not flagged as TXD_CONSTRUCTOR or TXD_INITIALIZER: " << constructorDecl->str() );
                this->declaration = constructorDecl;
                auto constructorField = constructorDecl->get_definer()->resolve_field();
                return constructorField->get_type();
            }
        }
        if ( this->appliedFuncArgs->size() == 0 ) {
            // TODO: support default value constructor
        }
        else if ( this->appliedFuncArgs->size() == 1 ) {
            // TODO: support default assignment constructor
        }
        CERR_THROWRES( this, "No matching constructor in type " << allocType
                       << " for args (" << join( resolve_typevec( this->appliedFuncArgs ), ", ") << ")" );
    }
    return nullptr;
}

TxFunctionCallNode::TxFunctionCallNode( const TxLocation& ploc, TxExpressionNode* callee,
                                        const std::vector<TxExpressionNode*>* argsExprList, bool doesNotReturn )
        : TxExpressionNode( ploc ), doesNotReturn( doesNotReturn ), callee( callee ), origArgsExprList( argsExprList ),
          argsExprList( make_args_vec( argsExprList ) ) {
    if ( auto fieldValueNode = dynamic_cast<TxFieldValueNode*>( this->callee ) ) {
        // handle direct constructor invocation - self() and super()
        auto identifier = fieldValueNode->get_full_identifier();
        if ( identifier == "self" || identifier == "super" ) {
            auto objectDeref = new TxReferenceDerefNode( this->ploc, this->callee );
            this->callee = new TxConstructorCalleeExprNode( this->ploc, objectDeref );
            this->isSelfSuperConstructorInvocation = true;
        }
    }
}

void TxFunctionCallNode::declaration_pass() {
    if ( this->isSelfSuperConstructorInvocation ) {
        if ( !( this->context().enclosing_lambda() && this->context().enclosing_lambda()->get_constructed() ) )
            CERROR( this, "self() / super() constructor may only be invoked from within the type's other constructors" );
        // TODO: shall only be legal as first statement within constructor body
    }
}

const TxQualType* TxFunctionCallNode::define_type() {
    // The resolution here shall resolve to the function signature that *closest* matches the argument types,
    // but also takes automatic (implicit) type conversions into account (if needed).
    // The automatic type conversions thus considered shall then be applied upon function invocation.
    // Prepare for resolving possible function overloading by registering actual function signature with
    // the callee node, BEFORE the callee node type is resolved:
    ASSERT( !this->callee->get_applied_func_args(), "callee already has applied func arg types: " << this->callee );
    this->callee->set_applied_func_args( this->origArgsExprList );
    this->calleeType = this->callee->resolve_type();
    if ( this->calleeType->get_type_class() != TXTC_FUNCTION ) {
        CERR_THROWRES( this, "Callee of function call expression is not of function type: " << this->calleeType );
    }
    else if ( /*auto constructorType =*/ dynamic_cast<const TxConstructorType*>( this->calleeType->type()->acttype() ) ) {
        // constructor functions return void but the constructor invocation expression yields the constructed type:
        if ( auto calleeConstructor = dynamic_cast<TxConstructorCalleeExprNode*>( this->callee ) ) {
            ASSERT( calleeConstructor->get_constructed_type(), "Expected callee field get_constructed_type() to be non-null: " << this->callee );
            return calleeConstructor->get_constructed_type();
        }
        else {
            ASSERT( dynamic_cast<TxFieldValueNode*>( this->callee ), "Expected callee to be a TxFieldValueNode but was: " << this->callee );
            auto calleeField = static_cast<TxFieldValueNode*>( this->callee );
            ASSERT( calleeField->get_constructed_type(), "Expected callee field get_constructed_type() to be non-null: " << this->callee );
            return calleeField->get_constructed_type();
        }
//        auto objectDefiner = constructorType->get_constructed_type_decl()->get_definer();
//        return objectDefiner->resolve_type();
    }
    else
        return new TxQualType( this->calleeType->type()->return_type() );
}

void TxFunctionCallNode::symbol_resolution_pass() {
    TxExpressionNode::symbol_resolution_pass();

    const TxActualType* actualCalleeType = ( this->calleeType ? this->calleeType->type()->acttype() : nullptr );
    auto constructorType = dynamic_cast<const TxConstructorType*>( actualCalleeType );
    if ( constructorType ) {
        // Stack construction syntactically looks like a function call, e.g. Int(42)
        // If the callee is a constructor, we substitute this function call with a stack construction expression:
        if ( !dynamic_cast<TxConstructorCalleeExprNode*>( this->callee ) ) {  // (prevents infinite recursion)
            auto calleeField = static_cast<TxFieldValueNode*>( this->callee );
            auto typeDeclNode = new TxTypeDeclWrapperNode( this->ploc, calleeField->get_constructed_type()->type()->get_declaration() );
//            auto typeDeclNode = new TxTypeDeclWrapperNode( this->ploc, constructorType->get_constructed_type_decl() );

            // Implementation note: Declaration pass is already run on the args, but we need to run it on the new construction node
            // and its new children, and we need to run resolution pass on the whole sub-tree.
            auto wrappedArgs = make_expr_wrapper_vec( this->origArgsExprList );
            this->inlinedExpression = new TxStackConstructionNode( this->ploc, typeDeclNode, wrappedArgs );
            run_declaration_pass( this->inlinedExpression, this, "inlinedexpr" );
            for ( auto argExpr : *this->origArgsExprList )
                argExpr->symbol_resolution_pass();
            this->inlinedExpression->symbol_resolution_pass();
            return;
        }
    }

    // Verify arguments and apply implicit conversions if needed:
    if ( this->calleeType && this->calleeType->get_type_class() == TXTC_FUNCTION ) {
        auto calleeArgTypes = this->calleeType->type()->argument_types();
        auto arrayArgElemType = this->calleeType->type()->vararg_elem_type();
        if ( !arrayArgElemType ) {
            if ( auto fixedArrayArgType = this->calleeType->type()->fixed_array_arg_type() ) {
                arrayArgElemType = fixedArrayArgType->element_type()->type();
            }
        }

// this check has already been done in callee resolution
//        if (calleeArgTypes.size() != this->argsExprList->size()
//                && !( varArgElemType && this->argsExprList->size() >= calleeArgTypes.size() - 1 )) {
//            CERROR(this, "Callee of function call expression has mismatching argument count: " << this->calleeType);
//        }
//        else
        {
            if ( arrayArgElemType
                 && !( calleeArgTypes.size() == this->origArgsExprList->size()
                       && get_reinterpretation_degree( this->origArgsExprList->back(), calleeArgTypes.back() ) >= 0 ) ) {
                // Calling a var-args function, and last provided arg does not directly match the var-arg tail arg.
                // transform the passed var-args into an array which is passed as the last argument
                unsigned lastCalleeArgIx = calleeArgTypes.size() - 1;
                auto arrayArgs = new std::vector<TxMaybeConversionNode*>();
                for ( unsigned i = lastCalleeArgIx; i < this->argsExprList->size(); i++ ) {
                    arrayArgs->push_back( this->argsExprList->at( i ) );
                }
                this->argsExprList->resize( lastCalleeArgIx );
                const TxLocation& varArgLoc = ( arrayArgs->empty() ? this->ploc : arrayArgs->front()->ploc );
                auto elemTypeExpr = new TxTypeExprWrapperNode( arrayArgElemType->get_definer() );
                auto arrayArgNode = new TxMaybeConversionNode( new TxFilledArrayLitNode( varArgLoc, elemTypeExpr, arrayArgs ) );
                run_declaration_pass( arrayArgNode, this, "arg" );
                this->argsExprList->push_back( arrayArgNode );
            }
            ASSERT( calleeArgTypes.size() == this->argsExprList->size(), "Mismatching argument count for callee " << this->calleeType );

            for ( unsigned i = 0; i < this->argsExprList->size(); i++ ) {
                auto argExpr = this->argsExprList->at( i );
                auto argDefType = calleeArgTypes.at( i );
                // note: similar rules to assignment
                // TODO: check dataspace rules if function arg is a reference
                argExpr->insert_conversion( argDefType );  // generates compilation error upon mismatch
            }
        }

        auto funcType = static_cast<const TxFunctionType*>( actualCalleeType );
        if ( !constructorType && funcType->modifiable_closure() ) {
            ASSERT( this->callee->get_data_graph_origin_expr(), "Callee with modifiable closere didn't have origin expression: " << this->callee );
            if ( !this->callee->get_data_graph_origin_expr()->check_chain_mutable() ) {
                // error message already generated
                //CERROR( this, "Can't invoke modifying method on immutable closure: " << funcType );
            }
        }
    }

    if ( auto inlineCalleeType = dynamic_cast<const TxInlineFunctionType*>( actualCalleeType ) ) {
        this->inlinedExpression = inlineCalleeType->make_inline_expr( this->callee, this->argsExprList );
    }

    if ( this->inlinedExpression ) {
        this->inlinedExpression->symbol_resolution_pass();
        return;
    }

    callee->symbol_resolution_pass();
    for ( auto argExpr : *this->argsExprList )
        argExpr->symbol_resolution_pass();
}
