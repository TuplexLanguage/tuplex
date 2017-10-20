#include "ast_exprs.hpp"
#include "ast_lambda_node.hpp"
#include "ast_array.hpp"
#include "ast_ref.hpp"
#include "ast/ast_util.hpp"
#include "ast_field.hpp"
#include "symbol/symbol_lookup.hpp"


TxQualType TxConstructorCalleeExprNode::define_type( TxPassInfo passInfo ) {
    ASSERT( this->appliedFuncArgs, "appliedFuncArgTypes of TxConstructorCalleeExprNode not initialized" );
    {
        auto allocType = this->objectExpr->resolve_type( passInfo );
        // find the constructor (note, constructors aren't inherited):
        if ( auto constructorDecl = resolve_constructor( this, allocType.type(), this->appliedFuncArgs ) ) {
            this->declaration = constructorDecl;
            auto constructorField = constructorDecl->get_definer()->resolve_field();
            return constructorField->qtype();
        }
        if ( this->appliedFuncArgs->size() == 0 ) {
            // TODO: support default value constructor
        }
        else if ( this->appliedFuncArgs->size() == 1 ) {
            // TODO: support default assignment constructor
        }
        CERR_THROWRES( this, "No matching constructor in type " << allocType
                       << " for args (" << join( attempt_typevec( this->appliedFuncArgs ), ", ") << ")" );
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
    this->callee->set_applied_func_args( this->origArgsExprList );
}

void TxFunctionCallNode::declaration_pass() {
    if ( this->isSelfSuperConstructorInvocation ) {
        if ( !( this->context().enclosing_lambda() && this->context().enclosing_lambda()->get_constructed() ) )
            CERROR( this, "self() / super() constructor may only be invoked from within the type's other constructors" );
        // TODO: shall only be legal as first statement within constructor body
    }
}

TxQualType TxFunctionCallNode::define_type( TxPassInfo passInfo ) {
    // The resolution here shall resolve to the function signature that *closest* matches the argument types,
    // but also takes automatic (implicit) type conversions into account (if needed).
    // The automatic type conversions thus considered shall then be applied upon function invocation.
    // Prepare for resolving possible function overloading by registering actual function signature with
    // the callee node, BEFORE the callee node type is resolved:
    auto ctype = this->callee->resolve_type( passInfo ).type();
    if ( ctype->get_type_class() != TXTC_FUNCTION ) {
        CERR_THROWRES( this, "Callee of function call expression is not of function type: " << ctype );
    }
    this->calleeType = ctype;

    auto constructorType = dynamic_cast<const TxConstructorType*>( this->calleeType );
    if ( constructorType ) {
        // Stack construction syntactically looks like a function call, e.g. Int(42)
        // If the callee is a constructor, we substitute this function call with a stack construction expression:
        if ( !dynamic_cast<TxConstructorCalleeExprNode*>( this->callee ) ) {  // (prevents infinite recursion)
            auto calleeField = static_cast<TxFieldValueNode*>( this->callee );
            auto constructedType = calleeField->get_constructed_type( passInfo );
            auto typeDeclNode = new TxQualTypeExprNode( new TxTypeDeclWrapperNode( this->ploc, constructedType->get_declaration() ) );

            // Implementation note: Declaration pass is already run on the args, but we need to run it on the new construction node
            // and its new children, and we need to run resolution pass on the whole sub-tree.
            auto wrappedArgs = make_expr_wrapper_vec( this->origArgsExprList );
            this->inlinedExpression = new TxStackConstructionNode( this->ploc, typeDeclNode, wrappedArgs );
            run_declaration_pass( this->inlinedExpression, this, "inlinedexpr" );
//            for ( auto argExpr : *this->origArgsExprList )
//                argExpr->resolution_pass();
//            this->inlinedExpression->resolution_pass();
            run_resolution_pass( this->inlinedExpression, "inlinedexpr" );
            return constructedType;
        }
    }

    // Verify arguments and apply implicit conversions if needed:
    {
        auto calleeArgTypes = this->calleeType->argument_types();
        auto arrayArgElemType = this->calleeType->vararg_elem_type();
        if ( !arrayArgElemType ) {
            if ( auto fixedArrayArgType = this->calleeType->fixed_array_arg_type() ) {
                arrayArgElemType = fixedArrayArgType->element_type().type();
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
                auto elemTypeExpr = new TxQualTypeExprNode( new TxTypeDeclWrapperNode( varArgLoc, arrayArgElemType->get_declaration() ) );
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
                argExpr->insert_conversion( passInfo, argDefType );  // generates compilation error upon mismatch
            }
        }

//        auto funcType = static_cast<const TxFunctionType*>( actualCalleeType );
        if ( this->calleeType->modifiable_closure() && !constructorType ) {
            ASSERT( this->callee->get_data_graph_origin_expr(), "Callee with modifiable closere didn't have origin expression: " << this->callee );
            if ( !this->callee->get_data_graph_origin_expr()->check_chain_mutable() ) {
                // error message already generated
                //CERROR( this, "Can't invoke modifying method on immutable closure: " << funcType );
            }
        }
    }

    if ( auto inlineCalleeType = dynamic_cast<const TxInlineFunctionType*>( this->calleeType ) ) {
        this->inlinedExpression = inlineCalleeType->make_inline_expr( this->callee, this->argsExprList );
    }

    if ( auto constructedType = this->callee->get_constructed_type( passInfo ) ) {
        // constructor functions return void but the constructor invocation expression yields the constructed type
        return constructedType;
    }
    else
        return this->calleeType->return_type();
}
