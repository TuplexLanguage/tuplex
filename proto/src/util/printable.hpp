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
