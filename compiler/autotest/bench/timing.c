#include "timing.h"

#define __USE_POSIX199309

#include <stdlib.h>
#include <unistd.h>
#include <time.h>

typedef struct timespec timespec;
//struct timespec {
//    time_t tv_sec; /* seconds */
//    long tv_nsec; /* nanoseconds */
//};




static timespec calc_diff(timespec start, timespec end)
{
    timespec temp;
    if ((end.tv_nsec-start.tv_nsec)<0) {
        temp.tv_sec = end.tv_sec-start.tv_sec-1;
        temp.tv_nsec = 1000000000+end.tv_nsec-start.tv_nsec;
    } else {
        temp.tv_sec = end.tv_sec-start.tv_sec;
        temp.tv_nsec = end.tv_nsec-start.tv_nsec;
    }
    return temp;
}


void* alloc_time_point() {
    timespec* timePoint = malloc( sizeof( timespec ) );
    clock_gettime( CLOCK_PROCESS_CPUTIME_ID, timePoint );
    return timePoint;
}

void free_time_point( void* timePoint ) {
    free( timePoint );
}

uint64_t get_time_diff_nanos( void* startPoint ) {
    timespec endPoint;
    clock_gettime( CLOCK_PROCESS_CPUTIME_ID, &endPoint );
    timespec diff = calc_diff( *(timespec*)startPoint, endPoint );
    return diff.tv_nsec;
}
