#pragma once

#include <string>

inline bool begins_with( const std::string& str, const std::string& head ) {
    return str.length() >= head.length() && !str.compare( 0, head.length(), head );
}

inline bool ends_with( const std::string& str, const std::string& tail ) {
    return str.length() >= tail.length() && !str.compare( str.length() - tail.length(), tail.length(), tail );
}
