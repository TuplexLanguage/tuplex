---
layout: default
---
## Ranges

Ranges are first class citizens, enabling them to be used and manipulated as values themselves, in for loop syntax, in array/sequence element selection, and in sequence generation.

    myrange := 1..10;   ## default step is 1
    for i in myrange,
        j in 9..-2..0:  ## here step is -2
    {
        print( %s i %% ", " %s j );
    }

    ## Note: The support of the following is not yet complete.

    even := c"0123456789"[ 0..2..10 ];  ## selects every second character
    for c in even:
    {
        print( %s c );
    }

    myarray := [ myrange... ];  ## expands the range in-place
