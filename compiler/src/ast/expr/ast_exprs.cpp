#include "ast_exprs.hpp"
#include "ast_lambda_node.hpp"
#include "ast_array.hpp"
#include "ast_ref.hpp"
#include "ast/ast_util.hpp"
#include "ast_field.hpp"
#include "symbol/symbol_lookup.hpp"


TxQualType TxConstructorCalleeExprNode::define_type( TxPassInfo passInfo ) {
    ASSERT( this->appliedFuncArgs, "appliedFuncArgTypes of TxConstructorCalleeExprNode not initialized" );
    auto allocType = this->objectExpr->resolve_type( passInfo );
    // find the constructor (note, constructors aren't inherited):
    this->declaration = resolve_constructor( this, allocType.type(), this->appliedFuncArgs );
    return this->declaration->get_definer()->resolve_field()->qtype();
}

TxFunctionCallNode::TxFunctionCallNode( const TxLocation& ploc, TxExpressionNode* callee,
                                        const std::vector<TxExpressionNode*>* argsExprList, bool doesNotReturn )
        : TxExpressionNode( ploc ), doesNotReturn( doesNotReturn ), //isInitInvokation( isInitInvokation ),
          callee( callee ), origArgsExprList( argsExprList ), argsExprList( make_args_vec( argsExprList ) ) {
    this->callee->set_applied_func_args( this->origArgsExprList );
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
            ASSERT( dynamic_cast<TxNamedFieldNode*>( this->callee ), "not a TxNamedFieldNode: " << this->callee );
            auto calleeField = static_cast<TxNamedFieldNode*>( this->callee );
            auto constructedType = calleeField->get_constructed_type( passInfo );
            auto typeDeclNode = new TxQualTypeExprNode( new TxTypeDeclWrapperNode( this->ploc, constructedType->get_declaration() ) );

            // Implementation note: Declaration pass is already run on the args, but we need to run it on the new construction node
            // and its new children, and we need to run resolution pass on the whole sub-tree.
            auto wrappedArgs = make_expr_wrapper_vec( this->origArgsExprList );
            this->inlinedExpression = new TxStackConstructionNode( this->ploc, typeDeclNode, wrappedArgs );
            run_declaration_pass( this->inlinedExpression, this, "inlinedexpr" );
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

                TxLocation varArgLoc = this->ploc;
                if ( !arrayArgs->empty() )
                    varArgLoc = TxLocation( arrayArgs->front()->ploc.begin, arrayArgs->back()->ploc.end, arrayArgs->front()->ploc.parserCtx );
                auto typeDeclWrNode = new TxTypeDeclWrapperNode( varArgLoc, arrayArgElemType->get_declaration() );
                auto elemTypeExpr = new TxQualTypeExprNode( typeDeclWrNode );
                auto filledArrayNode = new TxFilledArrayLitNode( varArgLoc, elemTypeExpr, arrayArgs );
                auto arrayArgNode = new TxMaybeConversionNode( filledArrayNode );
                run_declaration_pass( arrayArgNode, this, "arg" );
                // NOTE: The TxFilledArrayLitNode does not consider itself the OWNER of the arg nodes!
                // Therefore we maintain a special reference to these args so our visitor can traverse them:
                this->varargsList = arrayArgs;

                this->argsExprList->push_back( arrayArgNode );
            }
            ASSERT( calleeArgTypes.size() == this->argsExprList->size(), "Mismatching argument count for callee " << this->calleeType );

            for ( unsigned i = 0; i < this->argsExprList->size(); i++ ) {
                auto argExpr = this->argsExprList->at( i );
                auto argDefType = calleeArgTypes.at( i );
                // note: similar rules to assignment
                // no qualifiers since this only copies value
                // TODO: check dataspace rules if function arg is a reference
                argExpr->insert_conversion( passInfo, argDefType );  // generates compilation error upon mismatch
            }
        }

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



static const TxActualType* make_mutable_specialization( TxPassInfo passInfo, TxNode* origin, const TxActualType* origType,
                                                        const TxActualType* newSemBase=nullptr ) {
    const TxLocation& loc = origin->ploc;
    auto newbindings = new std::vector<TxTypeArgumentNode*>();
    for ( auto bdecl : origType->get_bindings() ) {
        if ( auto btypedecl = dynamic_cast<const TxTypeDeclaration*>( bdecl ) ) {
            if ( origType->get_type_class() == TXTC_ARRAY ) {
                // special case to facilitate implicit element type mutability in array type expressions
                auto newbind = new TxModifiableTypeNode( loc, new TxTypeDeclWrapperNode( loc, btypedecl ) );
                newbindings->push_back( new TxTypeArgumentNode( newbind ) );

            }
            else {
                auto newbind = new TxTypeDeclWrapperNode( loc, btypedecl );
                newbindings->push_back( new TxTypeArgumentNode( newbind ) );
            }
        }
        else {
            auto bvaluedecl = dynamic_cast<const TxFieldDeclaration*>( bdecl );
            auto previnitexpr = bvaluedecl->get_definer()->get_init_expression();
            if ( auto convinitexpr = dynamic_cast<TxMaybeConversionNode*>( previnitexpr ) )
                previnitexpr = convinitexpr->originalExpr;
            auto newbind = new TxExprWrapperNode( previnitexpr );
            newbindings->push_back( new TxTypeArgumentNode( newbind ) );
        }
    }
    auto genBaseTypeDecl = ( newSemBase ? newSemBase->get_declaration() : origType->get_semantic_base_type()->get_declaration() );
    auto genBaseTypeNode = new TxTypeDeclWrapperNode( loc, genBaseTypeDecl );
    auto mutTypeDef = new TxGenSpecTypeNode( loc, genBaseTypeNode, newbindings );
    mutTypeDef->set_requires_mutable( true );
    run_declaration_pass( mutTypeDef, origin, "mut-type" );
    auto type = mutTypeDef->resolve_type( passInfo ).type();
    LOG_DEBUG( origin->LOGGER(), "Created mutable specialization for " << origin << ": " << type );
    return type;
}

static const TxActualType* get_mutable_specialization( TxPassInfo passInfo, TxNode* origin, const TxActualType* type ) {
    if ( !type->is_generic_specialization() )
        CERR_THROWRES( origin, "Can't specialize mutable type from base type: " << type );;
    if ( type->get_semantic_base_type()->is_mutable() )
        return make_mutable_specialization( passInfo, origin, type );
    else {
        auto newSemBase = get_mutable_specialization( passInfo, origin, type->get_semantic_base_type() );
        return make_mutable_specialization( passInfo, origin, type, newSemBase );
    }
}

TxQualType TxModifiableValueNode::define_type( TxPassInfo passInfo ) {
    TxQualType qtype = this->exprNode->resolve_type( passInfo );
    if ( !qtype.is_modifiable() ) {
        auto typeEnt = qtype.type();
        if ( !typeEnt->is_mutable() ) {
            if ( typeEnt->is_generic_specialization() && typeEnt->get_source_base_type()->is_mutable() ) {
                // copying an immutable type to a modifiable field is ok if we can obtain the mutable specialization
                // corresponding to the source's immutable specialization
                qtype = TxQualType( get_mutable_specialization( passInfo, this->exprNode, typeEnt ), true );
                //std::cerr << "Made mutable specialization from " << typeEnt << "  to " << qtype->type() << std::endl;
            }
            else
                CERR_THROWRES( this, "Can't use immutable type as modifiable: " << qtype );
        }
        else
            qtype = TxQualType( typeEnt, true );
    }
    return qtype;
}

void TxModifiableValueNode::verification_pass() const {
    if ( dynamic_cast<TxModifiableValueNode*>( this->exprNode ) )
        CWARNING( this, "Multiple mutability modifiers specified (has no effect)" );
}
