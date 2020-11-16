# Tuplex benchmarks source directory cmakelist

set(CMAKE_BUILD_TYPE "Release")  # Debug Release RelWithDebInfo MinSizeRel


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

set( TX_BENCH_SOURCE_FILE "${CMAKE_CURRENT_SOURCE_DIR}/primes_txbench.tx" )
add_custom_command(OUTPUT primes_txbench.o
    COMMAND ${CMAKE_BINARY_DIR}/bin/txc -tx ${CMAKE_SOURCE_DIR} -quiet -nojit -bc -o - ${TX_BENCH_SOURCE_FILE} | opt -O3 | llc --relocation-model=pic --filetype obj -o primes_txbench.o
    DEPENDS ${TX_BENCH_SOURCE_FILE} ${CMAKE_BINARY_DIR}/bin/txc
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Compiling Tuplex source"
    VERBATIM
)

#target_link_libraries(cbench timing)
#target_link_libraries(txbench timing)

SET (CMAKE_C_FLAGS "-Wall -std=c99")
