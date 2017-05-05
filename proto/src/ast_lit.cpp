#include <cstdint>
#include <cerrno>

#include "ast_lit.hpp"
#include "ast_types.hpp"


bool is_signed_out_of_range(const int64_t i64, const BuiltinTypeId typeId) {
    switch (typeId) {
    case TXBT_BYTE:
        return (i64 < INT8_MIN  || i64 > INT8_MAX);
    case TXBT_SHORT:
        return (i64 < INT16_MIN || i64 > INT16_MAX);
    case TXBT_INT:
        return (i64 < INT32_MIN || i64 > INT32_MAX);
    case TXBT_LONG:
        return false;  // (i64 < INT64_MIN || i64 > INT64_MAX);
    default:
        ASSERT(false, "Unhandled type id " << typeId);
        return true;
    }
}

bool is_unsigned_out_of_range(const uint64_t u64, const BuiltinTypeId typeId) {
    switch (typeId) {
    case TXBT_UBYTE:
        return (u64 > UINT8_MAX);
    case TXBT_USHORT:
        return (u64 > UINT16_MAX);
    case TXBT_UINT:
        return (u64 > UINT32_MAX);
    case TXBT_ULONG:
        return false;  // (u64 > UINT64_MAX);
    default:
        ASSERT(false, "Unhandled type id " << typeId);
        return true;
    }
}

/** removes underscores, spaces and tabs */
inline std::string strip_ignored_chars(const std::string input) {
    std::string tmp = input;
    tmp.erase(std::remove_if(tmp.begin(), tmp.end(), [](char c){ return c=='_' || c==' ' || c=='\t'; }), tmp.end());
    return tmp;
}


static const BuiltinTypeId inttypeids[] = { TXBT_BYTE, TXBT_SHORT, TXBT_INT, TXBT_LONG, TXBT_UBYTE, TXBT_USHORT, TXBT_UINT, TXBT_ULONG };
static const BuiltinTypeId uinttypeids[] = { TXBT_UBYTE, TXBT_USHORT, TXBT_UINT, TXBT_ULONG };
static const BuiltinTypeId sinttypeids[] = { TXBT_BYTE, TXBT_SHORT, TXBT_INT, TXBT_LONG };


IntValue::IntValue(const std::string& sourceLiteral, bool hasRadix, BuiltinTypeId typeId) {
    char* pEnd;

    // pre-parse value string:
    std::string valueStr;
    std::string typeStr;
    if (hasRadix) {
        auto valuePos = sourceLiteral.find('#');
        ASSERT(valuePos!=std::string::npos, "Expected '#' radix separator in string: '" << sourceLiteral << "'");
        std::string radixStr(sourceLiteral, 0, valuePos);
        auto typePos = sourceLiteral.find('#', ++valuePos);
        valueStr = strip_ignored_chars(sourceLiteral.substr(valuePos, typePos-valuePos));
        if (typePos != std::string::npos)
            typeStr = sourceLiteral.substr(typePos+1);

        this->radix = strtoul(radixStr.c_str(), &pEnd, 10);
        if (errno == ERANGE || *pEnd || this->radix < 2 || this->radix > 36) {
            errno = 0;
            this->outOfRange = true;
            return;
        }
    }
    else {
        this->radix = 10;
        valueStr = strip_ignored_chars(sourceLiteral);
        if (std::isalpha(valueStr.back())) {
            int suffixLen = (valueStr.length() > 2 && std::isalpha(valueStr.at(valueStr.length()-2)) ? 2 : 1);
            typeStr = sourceLiteral.substr(sourceLiteral.length()-suffixLen);
            valueStr.erase(valueStr.length()-suffixLen);
        }
    }

    // (if typeId arg is provided, type-indicating suffix in the value string is ignored)
    if (!typeId && !typeStr.empty()) {  // literal has type-specifying suffix
        this->_signed = ( typeStr.front() != 'U' );
        switch (typeStr.back()) {
        case 'B':  typeId = (this->_signed ? TXBT_BYTE  : TXBT_UBYTE);   break;
        case 'S':  typeId = (this->_signed ? TXBT_SHORT : TXBT_USHORT);  break;
        case 'I':  typeId = (this->_signed ? TXBT_INT   : TXBT_UINT);    break;
        case 'L':  typeId = (this->_signed ? TXBT_LONG  : TXBT_ULONG);   break;
        default:
            ASSERT(false, "Invalid integer literal type suffix: ''" << typeStr.back() << "'");
        }
    }

    if (typeId) {  // specified type
        ASSERT(std::count(inttypeids, inttypeids+(sizeof(inttypeids)/sizeof(inttypeids[0])), typeId),
               "Invalid concrete integer type id: " << typeId);
        this->typeId = typeId;
        if (std::count(uinttypeids, uinttypeids+(sizeof(uinttypeids)/sizeof(uinttypeids[0])), typeId)) {
            this->_signed = false;
            this->value.u64 = strtoull(valueStr.c_str(), &pEnd, radix);
            if (errno == ERANGE || *pEnd || valueStr.front() == '-')
                this->outOfRange = true;
            else
                this->outOfRange = is_unsigned_out_of_range(this->value.u64, this->typeId);
            errno = 0;
        }
        else {
            this->_signed = true;
            this->value.i64 = strtoll(valueStr.c_str(), &pEnd, radix);
            if (errno == ERANGE || *pEnd)
                this->outOfRange = true;
            else
                this->outOfRange = is_signed_out_of_range(this->value.i64, this->typeId);
            errno = 0;
        }
    }
    else {  // unspecified type
        // this will set all zero / positive integers to the smallest possible unsigned type,
        // and negative integers to the smallest possible signed type
        this->value.u64 = strtoull(valueStr.c_str(), &pEnd, radix);
        if (errno == ERANGE || *pEnd || valueStr.front() == '-') {
            errno = 0;
            this->value.i64 = strtoll(valueStr.c_str(), &pEnd, radix);
            if (errno == ERANGE || *pEnd)
                this->outOfRange = true;
            errno = 0;
            this->_signed = true;
            if      (this->value.i64 >= INT8_MIN)   this->typeId = TXBT_BYTE;
            else if (this->value.i64 >= INT16_MIN)  this->typeId = TXBT_SHORT;
            else if (this->value.i64 >= INT32_MIN)  this->typeId = TXBT_INT;
            else                                    this->typeId = TXBT_LONG;
        }
        else {
            this->_signed = false;
            if      (this->value.u64 <= UINT8_MAX)   this->typeId = TXBT_UBYTE;
            else if (this->value.u64 <= UINT16_MAX)  this->typeId = TXBT_USHORT;
            else if (this->value.u64 <= UINT32_MAX)  this->typeId = TXBT_UINT;
            else                                     this->typeId = TXBT_ULONG;
        }
    }
}

IntValue::IntValue(int64_t i64value, BuiltinTypeId typeId, bool _signed) {
    this->_signed = _signed;
    if (_signed) {
        this->value.i64 = i64value;
        if (typeId) {
            ASSERT(std::count(sinttypeids, sinttypeids+(sizeof(sinttypeids)/sizeof(sinttypeids[0])), typeId),
                   "Invalid concrete signed integer type id: " << typeId);
            this->typeId = typeId;
            this->outOfRange = is_signed_out_of_range(this->value.u64, this->typeId);
        }
        else {
            if      (this->value.i64 >= INT8_MIN  && this->value.i64 <= INT8_MAX)   this->typeId = TXBT_BYTE;
            else if (this->value.i64 >= INT16_MIN && this->value.i64 <= INT16_MAX)  this->typeId = TXBT_SHORT;
            else if (this->value.i64 >= INT32_MIN && this->value.i64 <= INT32_MAX)  this->typeId = TXBT_INT;
            else                                                                    this->typeId = TXBT_LONG;
        }
    }
    else {
        if (i64value < 0)
            this->outOfRange = true;
        this->init_unsigned(static_cast<uint64_t>(i64value), typeId);
    }
}

void IntValue::init_unsigned(uint64_t u64value, BuiltinTypeId typeId) {
    value.u64 = u64value;
    _signed = false;
    if (typeId) {
        ASSERT(std::count(uinttypeids, uinttypeids+(sizeof(uinttypeids)/sizeof(uinttypeids[0])), typeId),
               "Invalid concrete unsigned integer type id: " << typeId);
        this->typeId = typeId;
        this->outOfRange = is_unsigned_out_of_range(this->value.u64, this->typeId);
    }
    else {
        if      (this->value.u64 <= UINT8_MAX)   this->typeId = TXBT_UBYTE;
        else if (this->value.u64 <= UINT16_MAX)  this->typeId = TXBT_USHORT;
        else if (this->value.u64 <= UINT32_MAX)  this->typeId = TXBT_UINT;
        else                                     this->typeId = TXBT_ULONG;
    }
}

uint32_t IntValue::get_value_UInt() const {
    if (this->_signed) {
        if (this->value.i64 >= 0 && this->value.i64 <= UINT32_MAX)
            return static_cast<uint32_t>(this->value.i64);
    }
    else {
        if (this->value.u64 <= UINT32_MAX)
            return static_cast<uint32_t>(this->value.u64);
    }
    throw std::range_error("Value of constant integer is not within range of type UInt");
}



void TxCStringLitNode::symbol_declaration_pass( LexicalContext& lexContext) {
    this->set_context( lexContext );

    // (for now) Create AST to declare the implicit type of this c-string literal:
    TxTypeExpressionNode* elemTypeExpr = new TxIdentifiedTypeNode(this->parseLocation, "tx.UByte");
    TxExpressionNode* lengthExpr = new TxIntegerLitNode(this->parseLocation, literal.length()-2, false, TXBT_UINT);
    TxTypeExpressionNode* typeExpr = new TxArrayTypeNode(this->parseLocation, elemTypeExpr, lengthExpr);
    this->cstringTypeNode = typeExpr;
    this->cstringTypeNode->symbol_declaration_pass( lexContext, nullptr );
}



