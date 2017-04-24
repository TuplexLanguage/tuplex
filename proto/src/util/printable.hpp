#pragma once


class Printable {
public:
    virtual ~Printable() = default;
    virtual std::string str() const = 0;
};


template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<< (std::basic_ostream<charT, traits> &lhs, Printable const &printable_rhs) {
    return lhs << printable_rhs.str();
}

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<< (std::basic_ostream<charT, traits> &lhs, Printable const *printable_rhs) {
    if (printable_rhs)
        return lhs << printable_rhs->str();
    else
        return lhs << "NULL";
}



#include <memory> // for std::allocator


template <template <typename, typename> class Container,
          typename Value,
          typename Allocator=std::allocator<Value> >
std::string join( const Container<Value, Allocator>& cont, const std::string& infix )
{
    std::stringstream str;
    if (! cont.empty()) {
        auto ai = cont.cbegin();
        str << (*ai);
        for (ai++; ai != cont.cend(); ai++)
            str << infix << (*ai);
    }
    return str.str();
}

template <template <typename, typename> class Container,
          typename Value,
          typename Allocator=std::allocator<Value> >
inline std::string join( const Container<Value, Allocator>* cont, const std::string& infix )
{
    return join( *cont, infix );
}
