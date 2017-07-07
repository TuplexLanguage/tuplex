#include "ast_lit.hpp"

#include <cerrno>
#include <cstdint>
#include <float.h>

#include "ast_string.hpp"
#include "ast/type/ast_types.hpp"

static bool is_signed_out_of_range( const int64_t i64, const BuiltinTypeId typeId ) {
    switch ( typeId ) {
    case TXBT_BYTE:
        return ( i64 < INT8_MIN || i64 > INT8_MAX );
    case TXBT_SHORT:
        return ( i64 < INT16_MIN || i64 > INT16_MAX );
    case TXBT_INT:
        return ( i64 < INT32_MIN || i64 > INT32_MAX );
    case TXBT_LONG:
        return false;  // (i64 < INT64_MIN || i64 > INT64_MAX);
    default:
        ASSERT( false, "Unhandled type id " << typeId );
        return true;
    }
}

static bool is_unsigned_out_of_range( const uint64_t u64, const BuiltinTypeId typeId ) {
    switch ( typeId ) {
    case TXBT_UBYTE:
        return ( u64 > UINT8_MAX );
    case TXBT_USHORT:
        return ( u64 > UINT16_MAX );
    case TXBT_UINT:
        return ( u64 > UINT32_MAX );
    case TXBT_ULONG:
        return false;  // (u64 > UINT64_MAX);
    default:
        ASSERT( false, "Unhandled type id " << typeId );
        return true;
    }
}

/** removes underscores, spaces and tabs */
inline std::string strip_ignored_chars( const std::string input ) {
    std::string tmp = input;
    tmp.erase( std::remove_if( tmp.begin(), tmp.end(), [](char c) {return c=='_' || c==' ' || c=='\t';} ), tmp.end() );
    return tmp;
}

static const BuiltinTypeId inttypeids[] = { TXBT_BYTE, TXBT_SHORT, TXBT_INT, TXBT_LONG, TXBT_UBYTE, TXBT_USHORT, TXBT_UINT, TXBT_ULONG };
static const BuiltinTypeId sinttypeids[] = { TXBT_BYTE, TXBT_SHORT, TXBT_INT, TXBT_LONG };
static const BuiltinTypeId uinttypeids[] = { TXBT_UBYTE, TXBT_USHORT, TXBT_UINT, TXBT_ULONG };

void TxIntegerLitNode::IntConstant::initialize( const std::string& sourceLiteral, bool hasRadix, bool negative, BuiltinTypeId typeId ) {
    char* pEnd;

    // pre-parse source string:
    std::string valueStr;
    if ( negative ) {
        valueStr = '-';
    }
    const std::string trimmedLiteral( strip_ignored_chars( sourceLiteral ) );

    // parse radix prefix:
    if ( hasRadix ) {
        if ( trimmedLiteral.at(0) == '0' && ( trimmedLiteral.at(1) == 'x' || trimmedLiteral.at(1) == 'X' ) ) {
            valueStr.append( trimmedLiteral.substr( 2 ) );
            this->radix = 16;
        }
        else {
            auto valuePos = trimmedLiteral.find( '#' );
            ASSERT( valuePos != std::string::npos, "Expected '#' radix separator in string: '" << sourceLiteral << "'" );
            std::string radixStr( trimmedLiteral, 0, valuePos );
            valueStr.append( trimmedLiteral.substr( valuePos + 1) );

            this->radix = strtoul( radixStr.c_str(), &pEnd, 10 );
            if ( errno == ERANGE || *pEnd || this->radix < 2 || this->radix > 36 ) {
                errno = 0;
                this->outOfRange = true;
                return;
            }
        }
    }
    else {
        valueStr.append ( trimmedLiteral );
        this->radix = 10;
    }

    // parse type signifier suffix:
    std::string typeStr;
    auto typePos = valueStr.find( '#' );
    if ( typePos != std::string::npos ) {
        typeStr = valueStr.substr( typePos + 1 );
        valueStr.erase( typePos );
    }
    else if ( this->radix <= 10 ) {
        // if radix <= 10, the # before the type signifier may be skipped (e.g. 8#731UI instead of 8#731#UI)
        if ( std::isalpha( valueStr.back() ) ) {
            int suffixLen = ( valueStr.length() > 2 && std::isalpha( valueStr.at( valueStr.length() - 2 ) ) ? 2 : 1 );
            typeStr = valueStr.substr( valueStr.length() - suffixLen );
            valueStr.erase( valueStr.length() - suffixLen );
        }
    }
    if ( !typeStr.empty() ) {  // literal has type-specifying suffix
        BuiltinTypeId signifierTypeId = (BuiltinTypeId)0;
        this->_signed = ( typeStr.front() != 'U' );
        switch ( typeStr.back() ) {
        case 'B':
            signifierTypeId = ( this->_signed ? TXBT_BYTE : TXBT_UBYTE );
            break;
        case 'S':
            signifierTypeId = ( this->_signed ? TXBT_SHORT : TXBT_USHORT );
            break;
        case 'I':
            signifierTypeId = ( this->_signed ? TXBT_INT : TXBT_UINT );
            break;
        case 'L':
            signifierTypeId = ( this->_signed ? TXBT_LONG : TXBT_ULONG );
            break;
        default:
            THROW_LOGIC( "Invalid integer literal type suffix: ''" << typeStr.back() << "'" );
        }
        if ( typeId && typeId != signifierTypeId )
            THROW_LOGIC( "Both typeId and type signifier suffix specified and they are not equal: " << typeId << " != '" << typeStr.back() << "'" );
        typeId = signifierTypeId;
    }

    // parse value:
    if ( typeId ) {  // specified type
        ASSERT( std::count( inttypeids, inttypeids + ( sizeof( inttypeids ) / sizeof( inttypeids[0] ) ), typeId ),
                "Invalid concrete integer type id: " << typeId );
        this->typeId = typeId;
        if ( std::count( uinttypeids, uinttypeids + ( sizeof( uinttypeids ) / sizeof( uinttypeids[0] ) ), typeId ) ) {
            this->_signed = false;
            this->value.u64 = strtoull( valueStr.c_str(), &pEnd, radix );
            if ( errno == ERANGE || *pEnd || valueStr.front() == '-' )
                this->outOfRange = true;
            else
                this->outOfRange = is_unsigned_out_of_range( this->value.u64, this->typeId );
            errno = 0;
        }
        else {
            this->_signed = true;
            this->value.i64 = strtoll( valueStr.c_str(), &pEnd, radix );
            if ( errno == ERANGE || *pEnd )
                this->outOfRange = true;
            else
                this->outOfRange = is_signed_out_of_range( this->value.i64, this->typeId );
            errno = 0;
        }
    }
    else {  // unspecified type
        // this will set all zero / positive integers to the smallest possible unsigned type,
        // and negative integers to the smallest possible signed type
        this->value.u64 = strtoull( valueStr.c_str(), &pEnd, radix );
        if ( errno == ERANGE || *pEnd || valueStr.front() == '-' ) {
            errno = 0;
            this->value.i64 = strtoll( valueStr.c_str(), &pEnd, radix );
            if ( errno == ERANGE || *pEnd )
                this->outOfRange = true;
            errno = 0;
            this->_signed = true;
            if ( this->value.i64 >= INT8_MIN )
                this->typeId = TXBT_BYTE;
            else if ( this->value.i64 >= INT16_MIN )
                this->typeId = TXBT_SHORT;
            else if ( this->value.i64 >= INT32_MIN )
                this->typeId = TXBT_INT;
            else
                this->typeId = TXBT_LONG;
        }
        else {
            this->_signed = false;
            if ( this->value.u64 <= UINT8_MAX )
                this->typeId = TXBT_UBYTE;
            else if ( this->value.u64 <= UINT16_MAX )
                this->typeId = TXBT_USHORT;
            else if ( this->value.u64 <= UINT32_MAX )
                this->typeId = TXBT_UINT;
            else
                this->typeId = TXBT_ULONG;
        }
    }
}

void TxIntegerLitNode::IntConstant::initialize( int64_t i64value, BuiltinTypeId typeId, bool _signed ) {
    this->_signed = _signed;
    if ( _signed ) {
        this->value.i64 = i64value;
        if ( typeId ) {
            ASSERT( std::count( sinttypeids, sinttypeids + ( sizeof( sinttypeids ) / sizeof( sinttypeids[0] ) ), typeId ),
                    "Invalid concrete signed integer type id: " << typeId );
            this->typeId = typeId;
            this->outOfRange = is_signed_out_of_range( this->value.u64, this->typeId );
        }
        else {
            if ( this->value.i64 >= INT8_MIN && this->value.i64 <= INT8_MAX )
                this->typeId = TXBT_BYTE;
            else if ( this->value.i64 >= INT16_MIN && this->value.i64 <= INT16_MAX )
                this->typeId = TXBT_SHORT;
            else if ( this->value.i64 >= INT32_MIN && this->value.i64 <= INT32_MAX )
                this->typeId = TXBT_INT;
            else
                this->typeId = TXBT_LONG;
        }
    }
    else {
        if ( i64value < 0 )
            this->outOfRange = true;
        this->init_unsigned( static_cast<uint64_t>( i64value ), typeId );
    }
}

void TxIntegerLitNode::IntConstant::init_unsigned( uint64_t u64value, BuiltinTypeId typeId ) {
    value.u64 = u64value;
    _signed = false;
    if ( typeId ) {
        ASSERT( std::count( uinttypeids, uinttypeids + ( sizeof( uinttypeids ) / sizeof( uinttypeids[0] ) ), typeId ),
                "Invalid concrete unsigned integer type id: " << typeId );
        this->typeId = typeId;
        this->outOfRange = is_unsigned_out_of_range( this->value.u64, this->typeId );
    }
    else {
        if ( this->value.u64 <= UINT8_MAX )
            this->typeId = TXBT_UBYTE;
        else if ( this->value.u64 <= UINT16_MAX )
            this->typeId = TXBT_USHORT;
        else if ( this->value.u64 <= UINT32_MAX )
            this->typeId = TXBT_UINT;
        else
            this->typeId = TXBT_ULONG;
    }
}

void TxIntegerLitNode::declaration_pass() {
    if ( !this->sourceLiteral.empty() ) {
        this->constValue.initialize( this->sourceLiteral, this->hasRadix, this->negative );
    }
    else {
        this->constValue.initialize( this->i64value, this->typeId, this->_signed );
    }

    if ( this->constValue.radix < 2 || this->constValue.radix > 36 )
        CERROR( this, "Radix outside valid range [2,36]: " << this->constValue.radix );
    else if ( this->constValue.outOfRange )
        CERROR( this, "Integer literal '" << sourceLiteral << "' badly formatted or outside value range of type " << this->constValue.typeId );
}


TxFloatingLitNode::FloatConstant::FloatConstant( const std::string& literal ) {
    this->outOfRange = false;
    char* tailptr = nullptr;
    this->value = strtod( literal.c_str(), &tailptr );
    ASSERT( tailptr, "expected non-null tailptr, literal='" << literal << "'" );
    if ( char fs = tailptr[0] ) {
        if ( fs == '#' )
            fs = tailptr[1];
        switch ( fs ) {
        case 'D':
            this->typeId = TXBT_DOUBLE;
            break;
        case 'F':
            this->typeId = TXBT_FLOAT;
            if ( fabs( this->value ) > FLT_MAX )
                this->outOfRange = true;
            break;
        case 'H':
            this->typeId = TXBT_HALF;
            if ( fabs( this->value ) > 65503.0 )
                this->outOfRange = true;
            break;
        default:
            THROW_LOGIC( "Invalid floating point type signifier '" << fs << "' / malformed floating point literal '" << literal << "'" );
        }
    }
    else {
        // Note: We don't implicitly deduce Half type since it has poor precision; default is Float.
        if ( fabs( this->value ) <= FLT_MAX )
            this->typeId = TXBT_FLOAT;
        else
            this->typeId = TXBT_DOUBLE;
    }
}

TxFloatingLitNode::FloatConstant::FloatConstant( double value, BuiltinTypeId typeId )
        : typeId( typeId ), value( value ) {
    this->outOfRange = false;
    switch ( typeId ) {
    case TXBT_HALF:
        if ( fabs( value ) > 65503.0 )
            this->outOfRange = true;
        break;
    case TXBT_FLOAT:
        if ( fabs( value ) > FLT_MAX )
            this->outOfRange = true;
        break;
    case TXBT_DOUBLE:
        break;
    default:
        THROW_LOGIC( "Type id not a concrete floating point type: " << typeId );
    }
}


const std::string TxBoolLitNode::TRUE = "TRUE";
const std::string TxBoolLitNode::FALSE = "FALSE";


TxCharacterLitNode::TxCharacterLitNode( const TxLocation& ploc, const std::string& literal )
        : TxLiteralElementaryValueNode( ploc ), literal( literal ), value( parse_string_literal( literal, 1, 1 ).front() ) {
}
