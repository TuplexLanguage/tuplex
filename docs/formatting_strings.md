---
layout: default
---
## Formatting Strings

The string concatenation `%%` and formatting `%s` operators are first-class expression operators. This allows them to be used in arbitrary expressions, and printing to the console is just one use-case. The `%s` operators accepts all the conventional C printf formatting codes.

`%s operand` will create a string representation of the operand (similar to .to_string() in some other languages).

`%???s operand` `%???d operand` etc will create a formatted string representation using the specified formatting codes.

If a formatting operator is preceded by another string, it concatenates the left string with the formatted result. This way simple concatenations as well as advanced formatting can be strung together seamlessly.

    print( "Hello" %% ", world!" );
    mystring := "number-to-formatted-string: " %-05d -4444;
    print( mystring %% " and lets append this" %40s " and this with min-width" );
    stream.write( "write formatted values to stream: " %-20s mystring %8d 32 );

