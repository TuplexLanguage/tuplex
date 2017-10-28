# Tuplex source directory cmakelist

set(CMAKE_BUILD_TYPE "Release")  # Debug Release RelWithDebInfo MinSizeRel


set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)


set(GENERATED_OUTPUT_PATH ${CMAKE_BINARY_DIR}/generated)


# add to the search path for include files so that we will find our own includes
include_directories("${CMAKE_CURRENT_SOURCE_DIR}")


# add the targets

add_executable(cbench
    primes_cbench.c
    timing.c
)

add_executable(txbench
    primes_txbench.o
    timing.c
)

add_custom_command(OUTPUT primes_txbench.o
    COMMAND txc -tx ../.. -nojit -bc -o - primes_txbench.tx | opt -O3 | llc -filetype obj -o primes_txbench.o
    DEPENDS primes_txbench.tx
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Compiling Tuplex source"
    VERBATIM
)

#target_link_libraries(cbench timing)
#target_link_libraries(txbench timing)


# add compile flags
# -std=c++11
# -std=c++1y
# -fexceptions -fcxx-exceptions 
# -fprofile-arcs -ftest-coverage
# -Wno-deprecated-register
SET (CMAKE_C_COMPILER             "/usr/bin/clang")
#SET (CMAKE_C_COMPILER             "/usr/bin/gcc")
SET (CMAKE_C_FLAGS                "-Wall -std=c99")
SET (CMAKE_C_FLAGS_DEBUG          "-g -DDEVMODE")
SET (CMAKE_C_FLAGS_MINSIZEREL     "-Os")
SET (CMAKE_C_FLAGS_RELEASE        "-O3")
SET (CMAKE_C_FLAGS_RELWITHDEBINFO "-O2 -g")

SET (CMAKE_CXX_COMPILER             "/usr/bin/clang++")
#SET (CMAKE_CXX_COMPILER             "/usr/bin/gcc")
SET (CMAKE_CXX_FLAGS                "-Wall -std=c++11 -pthread")
SET (CMAKE_CXX_FLAGS_DEBUG          "-g -DDEVMODE")
SET (CMAKE_CXX_FLAGS_MINSIZEREL     "-Os")
SET (CMAKE_CXX_FLAGS_RELEASE        "-O3")
SET (CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g")
