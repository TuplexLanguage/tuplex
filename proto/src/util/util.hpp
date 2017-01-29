#pragma once


inline bool ends_with(std::string const &fullString, std::string const &ending) {
    if (fullString.length() >= ending.length())
        return (0 == fullString.compare( fullString.length() - ending.length(), ending.length(), ending ) );
    return false;
}
