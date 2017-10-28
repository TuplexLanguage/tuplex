#include <stdlib.h>
#include <stdio.h>

#include "timing.h"


unsigned long prime_sieve( unsigned long limit ) {
    unsigned long length = limit / 8 + 1;
    unsigned char* sieve = malloc( length );
    if ( !sieve )
        abort();

    // manually set sieve to all ones:
    for ( unsigned long i = 0; i < length; i++ )
        sieve[i] = 0xFF;

    // mark all non-primes:
    for ( unsigned long x = 2; x <= limit; x++ ) {
        for ( unsigned long m = x * 2; m <= limit; m += x ) {
            unsigned long i = m >> 3;
            unsigned b = m & 7;
            sieve[i] &= ~( ((unsigned char)1) << b );
        }
    }

    // count number of primes:
    unsigned long count = 0;
    for ( unsigned long x = 2; x <= limit; x++ ) {
        unsigned long i = x >> 3;
        unsigned b = x & 7;
        if ( sieve[i] & ( ((unsigned char)1) << b ) ) {
            //printf( "Prime: %lu\n", x );
            ++count;
        }
    }

    free( sieve );
    return count;
}

int main( int argc, const char** argv ) {
    printf( "Limit\tCount\tMicros\n" );
    if ( argc > 1 ) {
        long limit = atol( argv[1] );
        if ( limit > 0 ) {
            void* start = alloc_time_point();
            unsigned long count = prime_sieve( limit );
            uint64_t nanos = get_time_diff_nanos( start );
            free_time_point( start );
            printf( "%lu\t%lu\t%lu\n", limit, count, nanos/1000 );
        }
    }
    else {
        long limit = 100;
        for ( int l = 2; l <= 7; l++ ) {
            void* start = alloc_time_point();
            unsigned long count = prime_sieve( limit );
            uint64_t nanos = get_time_diff_nanos( start );
            free_time_point( start );
            printf( "%lu\t%lu\t%lu\n", limit, count, nanos/1000 );
            limit *= 10;
        }
    }

    return 0;
}
