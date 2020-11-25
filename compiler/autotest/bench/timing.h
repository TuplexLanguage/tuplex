#ifndef AUTOTEST_BENCH_TIMING_H_
#define AUTOTEST_BENCH_TIMING_H_

#include <stdint.h>


void* alloc_time_point();

void free_time_point( void* timePoint );


uint64_t get_time_diff_nanos( void* startPoint );


#endif /* AUTOTEST_BENCH_TIMING_H_ */
