---
layout: default
overview: true
---
What is Tuplex?
---------------

<p>
Tuplex is a statically compiled, strongly typed, imperative programming language.
</p>
<p>
It features a sophisticated unified generic type system. It strives to be easy to write, easy to read, easy to avoid memory and concurrency bugs, and fast to execute.
</p>
Tuplex originated as a research project with a two-fold purpose:
<ul>
  <li>
    Combining a uniform, easy to write and read syntax with powerful and efficient array, tuple, and custom container handling
  </li>
  <li>
    A language test bed for developing the concept of <em>dataspaces</em> which guarantee data race safety in concurrent programs
  </li>
</ul>


Hello World Example
-------------------

Not beating around the bush, this is the <em>Hello, World!</em> program in Tuplex:

    main() {
        print( "Hello, world!" );
    }

The syntax is a relative of C, Java, and Python, with some influences from Rust, Go, and Ada.

About Easiness
--------------

What does the Tuplex design consider "easy to write and read" to mean?
<ul>
  <li>
    Uniformity of syntax - the same construct having the same syntax regardless of context
  </li>
  <li>
    Orthogonality - basic constructs being semantically independent and intuitive to combine
  </li>
  <li>
    Little or no boilerplate
  </li>
  <li>
    Simple / straight-forward logic should be intuitive to write and read
  </li>
  <li>
    The syntax should be visually light-weight and aid (not obfuscate) the programmer's understanding
  </li>
</ul>

Some Highlights
---------------

### Formatting Strings

The string concatenation `%%` and formatting `%s` operators are first-class expression operators. This allows them to be used in arbitrary expressions, and printing to the console is just one use-case. The `%s` operators accepts all the conventional C printf formatting codes.

`%s operand` will create a string representation of the operand (similar to .to_string() in some other languages).

`%???s operand` `%???d operand` etc will create a formatted string representation using the specified formatting codes.

If a formatting operator is preceded by another string, it concatenates the left string with the formatted result. This way simple concatenations as well as advanced formatting can be strung together seamlessly.

    print( "Hello" %% ", world!" );
    mystring := "number-to-formatted-string: " %-05d -4444;
    print( mystring %% " and lets append this" %40s " and this with min-width" );
    stream.write( "write formatted values to stream: " %-20s mystring %8f 3.14 );

### Ranges

Ranges are first class citizens, enabling them to be used and manipulated as values themselves, in for loop syntax, in array/sequence element selection, and in sequence generation.

    myrange := 1..10;    ## default step is 1
    for i in myrange,
        j in 9..-2..0  ## here step is -2
    {
        print( %s i %% ", " %s j );
    }

    ## Note: The support of the following is not yet complete.

    even := c"0123456789"[ 0..2..10 ];  ## selects every second character
    for c in even
    {
        print( %s c );
    }

    myarray := [ myrange... ];  ## expands the range in-place
