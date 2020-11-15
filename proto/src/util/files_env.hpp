#pragma once

#include <string>
#include <vector>

extern bool is_path_separator( char s );

extern char get_path_separator();

extern int file_status( const std::string& pathname );

extern std::string get_environment_variable( const std::string& varname );

extern std::vector<std::string> get_path_list( const std::string& paths );

/** Returns the file name component of the provided path. */
extern std::string get_file_name( const std::string& path );
