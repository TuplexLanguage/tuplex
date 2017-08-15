---
layout: default
overview: true
---
What is Tuplex?
---------------

Tuplex is a statically compiled, strongly typed, imperative programming language with some semantic, syntactic, and data representation innovations.

It features a sophisticated unified generic type system.

It strives to be easy to write, easy to read, easy to avoid memory and concurrency bugs, and fast to execute.

Tuplex originated as a research project with a two-fold purpose:

* Combining a uniform, easy to write and read syntax, with powerful and efficient array, tuple, and custom container handling

* A language test bed for developing a proof-of-concept of <em>dataspaces</em> which guarantee data race safety in concurrent programs


Hello World Example
-------------------

Not beating around the bush, this is the <em>Hello, World!</em> program in Tuplex:

    main() {
        print( "Hello, world!" );
    }

The syntax is a relative of C, Java, and Python, with some influences from Rust, Go, and Ada.


Notes
-----

### Status

The compiler is in a working state, and quite extensive programs can be written in Tuplex. The foundation library is already quite extensive and makes use of the language's most advanced features. There is also a very easy-to-use foreign function interface to C.

BUT, a large test suite notwithstanding it is not yet fully stable, and there are some core features to be completed:
- Conditional type casting
- Equality comparison of complex arrays
- Safe initialization of complex arrays (arrays-of-arrays and arrays-of-tuples)
- Complete the foundation library tie-up of the Collection, Sequence, etc interfaces with the Array and String types

These are interdepenent under the hood and will hopefully be done quite soon.

### About Easiness

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

Tuplex has a working, expressive and unambiguous grammar. However, once the core features which are syntactic first class citizens are completed, an overhaul is planned. As constructs have been implemented the syntax has grown somewhat in complexity and is not quite as "easy" as has been aimed for. Another objective is to make semicolons optional and support an indentation-based block structure like e.g. Python (while retaining the option to use braces if desired, e.g. Haskell provides a choice like this).