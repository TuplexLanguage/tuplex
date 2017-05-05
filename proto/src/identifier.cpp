#include <iostream>

#include "identifier.hpp"

const std::string TxIdentifier::pop() {
    const std::string last_segment( this->segments.back() );
    this->segments.pop_back();
    if ( this->segments.size() )
        this->ns.erase( this->ns.length() - last_segment.length() - 1 );  // (including period)
    else
        this->ns.clear();
    return last_segment;
}
