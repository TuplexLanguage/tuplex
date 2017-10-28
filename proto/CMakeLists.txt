cmake_minimum_required (VERSION 3.5)

# project name
project(Tuplex)

# the version number
set (Tuplex_VERSION_MAJOR 0)
set (Tuplex_VERSION_MINOR 2)


set(CMAKE_VERBOSE_MAKEFILE off)


add_subdirectory (src)
add_subdirectory (autotest/bench)
