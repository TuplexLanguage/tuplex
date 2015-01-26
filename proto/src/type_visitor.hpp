#pragma once


class TxType;
class TxReferenceType;
class TxArrayType;
class TxScalarType;
class TxFunctionType;
class TxTupleType;

class TxTypeVisitor {
public:
    virtual ~TxTypeVisitor() { }
    virtual void visit(const TxType& txType) = 0;
    virtual void visit(const TxReferenceType& txType) = 0;
    virtual void visit(const TxArrayType& txType) = 0;
    virtual void visit(const TxScalarType& txType) = 0;
    virtual void visit(const TxFunctionType& txType) = 0;
    virtual void visit(const TxTupleType& txType) = 0;
};
