---
layout: default
overview: true
title: Tuplex Home
---
What is Tuplex?
---------------

Tuplex is a statically compiled, imperative, strongly typed programming language with innovations in semantics, data representation, and syntax.

It features a sophisticated and unified generic type system.

It strives to be easy to write, easy to read, easy to avoid memory and concurrency bugs, while being fast to execute.

Tuplex originated as a research project with a two-fold purpose:

* Designing a uniform, easy to write and read syntax, with powerful and efficient array, tuple, and custom container handling

* A language test bed for developing a proof-of-concept of <em>dataspaces</em> which guarantee data race safety in concurrent programs

The compiler is developed using LLVM, enabling it to use LLVM's extensive suite of optimizations and hardware targets.


Hello World Example
-------------------

Not beating around the bush, this is the <em>Hello, World!</em> program in Tuplex:

    main():
        print( "Hello, world!" );

The syntax is a relative of C, Java, and Python, with some influences from Rust, Go, and Ada.


Notes
-----

### Status

The language and compiler is in a working state, and quite extensive programs can be written in Tuplex. The foundation library is already quite extensive and makes use of the language's most advanced features. There is also a very easy-to-use foreign function interface to C.

Tasks completed in Aug-Oct:

- Debugging information has been added. A Tuplex program can now be stepped and inspected in your favorite debugger
- Basic type reflection and safe runtime type casting and conditionals
- Completed handling of dynamic-length and nested arrays
- Completed uniform handling of equality and assignment, including of nested array
- The type resolution system has been refactored, both simplyfying the codebase and boosting performace >10x

Remaining major tasks until milestone 1, fully working and consistent core language:

- Safe object initialization, including automatic constructor generation


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

Tuplex has a working, expressive and unambiguous grammar. However, once the core features that have syntactic constructs are completed, an overhaul is planned. As constructs have been implemented the syntax has grown a bit in complexity and is not quite as "easy" as aimed for. One of the objectives is to make semicolons optional and support an indentation-based block structure like e.g. Python (while retaining the option to use braces if desired, e.g. Haskell provides a choice like this).