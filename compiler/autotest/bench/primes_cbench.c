#include <stdlib.h>
#include <stdio.h>

#include "timing.h"


unsigned long prime_sieve( unsigned long limit ) {
    unsigned long length = limit / 8 + 1;
    unsigned char* sieve = malloc( length );
    if ( !sieve )
        abort();

    void* start = alloc_time_point();

    // manually set sieve to all ones:
    for ( unsigned long i = 0; i < length; i++ )
        sieve[i] = 0xFF;

    uint64_t micros1 = get_time_diff_nanos( start )/1000;

    // mark all non-primes:
    for ( unsigned long x = 2; x <= limit; x++ ) {
        for ( unsigned long m = x * 2; m <= limit; m += x ) {
            unsigned long i = m >> 3;
            unsigned b = m & 7;
            sieve[i] &= ~( ((unsigned char)1) << b );
        }
    }

    uint64_t micros2 = get_time_diff_nanos( start )/1000 - micros1;

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

    uint64_t micros3 = get_time_diff_nanos( start )/1000 - micros2;
    free_time_point( start );
    printf( "%lu\t%lu\t%lu\t%lu\t%lu\n", limit, count, micros1, micros2, micros3 );
    return count;
}

int main( int argc, const char** argv ) {
    printf( "Limit\tCount\tMicros\n" );
    if ( argc > 1 ) {
        long limit = atol( argv[1] );
        if ( limit > 0 ) {
            prime_sieve( limit );
        }
    }
    else {
        long limit = 100;
        for ( int l = 2; l <= 7; l++ ) {
            prime_sieve( limit );
            limit *= 10;
        }
    }

    return 0;
}
