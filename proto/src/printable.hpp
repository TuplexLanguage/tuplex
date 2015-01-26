#pragma once


class Printable {
public:
    virtual ~Printable() = default;
    virtual std::string to_string() const = 0;
};


template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<< (std::basic_ostream<charT, traits> &lhs, Printable const &printable_rhs) {
    return lhs << printable_rhs.to_string();
}

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<< (std::basic_ostream<charT, traits> &lhs, Printable const *printable_rhs) {
    return lhs << printable_rhs->to_string();
}
