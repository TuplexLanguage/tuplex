# Root CMakeLists.txt for Tuplex prototype

cmake_minimum_required (VERSION 3.8)

# these options need to be set before PROJECT keyword (note, undocumented, but widely used)
set(CMAKE_DISABLE_SOURCE_CHANGES ON)
set(CMAKE_DISABLE_IN_SOURCE_BUILD ON)

# project name
project(Tuplex)

# the version number
set (Tuplex_VERSION_MAJOR 0)
set (Tuplex_VERSION_MINOR 5)


#set(CMAKE_VERBOSE_MAKEFILE on)


# add general compile flags

SET (CMAKE_C_FLAGS                "-Wall -std=c99")
SET (CMAKE_C_FLAGS_DEBUG          "-g -DDEVMODE")
SET (CMAKE_C_FLAGS_MINSIZEREL     "-Os")
SET (CMAKE_C_FLAGS_RELEASE        "-O3")
SET (CMAKE_C_FLAGS_RELWITHDEBINFO "-O2 -g")

#SET (CMAKE_CXX_COMPILER             "/usr/bin/clang++")
#SET (CMAKE_CXX_COMPILER             "/usr/bin/gcc")
SET (CMAKE_CXX_FLAGS                "-Wall -std=c++17")
SET (CMAKE_CXX_FLAGS_DEBUG          "-g -DDEVMODE")
SET (CMAKE_CXX_FLAGS_MINSIZEREL     "-Os")
SET (CMAKE_CXX_FLAGS_RELEASE        "-O3")
SET (CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g")


add_subdirectory (src)
add_subdirectory (autotest/bench)
