#pragma once


class TxActualType;
class TxBoolType;
class TxScalarType;
class TxReferenceType;
class TxArrayType;
class TxFunctionType;
class TxTupleType;
class TxInterfaceType;

class TxTypeVisitor {
public:
    virtual ~TxTypeVisitor() { }
    virtual void visit(const TxActualType& txType) = 0;
    virtual void visit(const TxBoolType& txType) = 0;
    virtual void visit(const TxScalarType& txType) = 0;
    virtual void visit(const TxReferenceType& txType) = 0;
    virtual void visit(const TxArrayType& txType) = 0;
    virtual void visit(const TxFunctionType& txType) = 0;
    virtual void visit(const TxTupleType& txType) = 0;
    virtual void visit(const TxInterfaceType& txType) = 0;
};
