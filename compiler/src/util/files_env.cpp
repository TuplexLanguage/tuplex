#include "files_env.hpp"

#include <sys/types.h>
#include <sys/stat.h>

//#include "tinydir/tinydir.h"

#if _WIN32
static const char PATH_VAR_DELIMITER = ';';
static const char PATH_SEPARATOR = '\\';
#else
static const char PATH_VAR_DELIMITER = ':';
static const char PATH_SEPARATOR = '/';
#endif

bool is_path_separator( char s ) {
#if _WIN32
    return s == '\\' || s == ':';
#else
    return s == '/';
#endif
}

char get_path_separator() {
    return PATH_SEPARATOR;
}

int file_status( const std::string& pathname ) {
    // stat is faster than fopen()
    struct stat buffer;
    if ( stat( pathname.c_str(), &buffer ) != 0 )
        return 0;  // doesn't seem to exist (rather: be accessible)
    else if ( buffer.st_mode & S_IFDIR )  // S_ISDIR() doesn't seem to exist on windows
        return 2;  // directory
    else
        return 1;  // file
}

std::string get_environment_variable( const std::string& varname ) {
    //printf("Getting env var '%s'\n", varname.c_str());
    const char* var = getenv( varname.c_str() );
    return var ? std::string( var ) : std::string();
}

std::vector<std::string> get_path_list( const std::string& paths ) {
    std::vector<std::string> result;
    if ( !result.empty() )
        return result;

    if ( !paths.empty() ) {
        size_t previous = 0;
        size_t index = paths.find( PATH_VAR_DELIMITER );
        while ( index != std::string::npos ) {
            result.push_back( paths.substr( previous, index - previous ) );
            previous = index + 1;
            index = paths.find( PATH_VAR_DELIMITER, previous );
        }
        result.push_back( paths.substr( previous ) );
    }
    return result;
}

std::string get_file_name( const std::string& path ) {
    size_t index = path.find_last_of( PATH_SEPARATOR );
    if ( index == std::string::npos )
        return path;
    return path.substr( index + 1 );
}
