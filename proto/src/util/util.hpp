#pragma once

inline bool begins_with( const std::string& str, const std::string& tail ) {
    return str.length() >= tail.length() && !str.compare( 0, tail.length(), tail );
}

inline bool ends_with( const std::string& str, const std::string& tail ) {
    return str.length() >= tail.length() && !str.compare( str.length() - tail.length(), tail.length(), tail );
}
