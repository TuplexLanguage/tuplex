// A Bison parser, made by GNU Bison 3.0.4.

// Locations for Bison parsers in C++

// Copyright (C) 2002-2015 Free Software Foundation, Inc.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// As a special exception, you may create a larger work that contains
// part or all of the Bison parser skeleton and distribute that work
// under terms of your choice, so long as that work isn't itself a
// parser generator using the skeleton or a modified version thereof
// as a parser skeleton.  Alternatively, if you modify or redistribute
// the parser skeleton itself, you may (at your option) remove this
// special exception, which will cause the skeleton and the resulting
// Bison output files to be licensed under the GNU General Public
// License without this special exception.

// This special exception was added by the Free Software Foundation in
// version 2.2 of Bison.

/**
 ** \file /home/christer/proj/workspace/proto/build/location.hh
 ** Define the TxLocation class.
 */

/********* Copied Bison default and modified to add parse context info. *********/

#ifndef YY_YY_HOME_CHRISTER_PROJ_WORKSPACE_PROTO_SRC_LOCATION_HPP_INCLUDED
# define YY_YY_HOME_CHRISTER_PROJ_WORKSPACE_PROTO_SRC_LOCATION_HPP_INCLUDED

#include "parser/position.hh"
#include "util/assert.hpp"

using yy::position;
class TxParserContext;

//namespace yy {
/// Abstract a location.
class TxLocation
{
public:

//    /// Construct a location from \a b to \a e.
//    TxLocation (const position& b, const position& e)
//      : begin (b)
//      , end (e)
//    {
//    }
//
    /// Construct a 0-width location in \a p.
    explicit TxLocation( const position& p = position() )
            : begin( p ),
              end( p )
    {
    }

    /// Construct a 0-width location in \a f, \a l, \a c.
    explicit TxLocation( unsigned int l,
                         unsigned int c,
                         TxParserContext* parserCtx )
            : begin( l, c ),
              end( l, c ),
              parserCtx( parserCtx )
    {
        ASSERT( parserCtx, "NULL parserCtx" );
    }

    TxLocation( const TxLocation& loc ) = default;

//    /// Initialization.
//    void initialize (std::string* f = YY_NULLPTR,
//                     unsigned int l = 1u,
//                     unsigned int c = 1u)
//    {
//      begin.initialize (f, l, c);
//      end = begin;
//    }

    TxLocation& operator= ( const TxLocation& ) = default;
//    void operator =( const TxLocation& loc )
//                     {
//        begin = loc.begin;
//        end = loc.end;
//        parserCtx = loc.parserCtx;
//    }

    /** \name Line and Column related manipulators
     ** \{ */
public:
    /// Reset initial location to final location.
    void step()
    {
        this->begin = this->end;
    }

    /// Extend the current location to the COUNT next columns.
    void columns( int count = 1 )
                  {
        end += count;
    }

    /// Extend the current location to the COUNT next lines.
    void lines( int count = 1 )
                {
        end.lines( count );
    }
    /** \} */

public:
    /// Beginning of the located region.
    position begin;
    /// End of the located region.
    position end;
    TxParserContext* parserCtx = nullptr;
};

/// Join two locations, in place.
inline TxLocation& operator+=( TxLocation& res, const TxLocation& end )
                               {
    res.end = end.end;
    return res;
}

/// Join two locations.
inline TxLocation operator+( TxLocation res, const TxLocation& end )
                             {
    return res += end;
}

/// Add \a width columns to the end position, in place.
inline TxLocation& operator+=( TxLocation& res, int width )
                               {
    res.columns( width );
    return res;
}

/// Add \a width columns to the end position.
inline TxLocation operator+( TxLocation res, int width )
                             {
    return res += width;
}

/// Subtract \a width columns to the end position, in place.
inline TxLocation& operator-=( TxLocation& res, int width )
                               {
    return res += -width;
}

/// Subtract \a width columns to the end position.
inline TxLocation operator-( TxLocation res, int width )
                             {
    return res -= width;
}

/// Compare two location objects.
inline bool
operator==( const TxLocation& loc1, const TxLocation& loc2 )
            {
    return loc1.begin == loc2.begin && loc1.end == loc2.end;
}

/// Compare two location objects.
inline bool
operator!=( const TxLocation& loc1, const TxLocation& loc2 )
            {
    return !( loc1 == loc2 );
}

/// From parsercontext.hpp
std::string format_location( const TxLocation& ploc );

/** \brief Intercept output stream redirection.
 ** \param ostr the destination output stream
 ** \param loc a reference to the location to redirect
 **
 ** Avoid duplicate information.
 */
template<typename YYChar>
inline std::basic_ostream<YYChar>&
operator<<( std::basic_ostream<YYChar>& ostr, const TxLocation& loc )
            {
    ostr << format_location( loc );
    return ostr;
//    unsigned int end_col = 0 < loc.end.column ? loc.end.column - 1 : 0;
//    ostr << loc.begin;
//    if ( loc.end.filename
//         && ( !loc.begin.filename
//              || *loc.begin.filename != *loc.end.filename ) )
//        ostr << '-' << loc.end.filename << ':' << loc.end.line << '.' << end_col;
//    else if ( loc.begin.line < loc.end.line )
//        ostr << '-' << loc.end.line << '.' << end_col;
//    else if ( loc.begin.column < end_col )
//        ostr << '-' << end_col;
//    return ostr;
}

//} // yy
#endif // !YY_YY_HOME_CHRISTER_PROJ_WORKSPACE_PROTO_SRC_LOCATION_HPP_INCLUDED
