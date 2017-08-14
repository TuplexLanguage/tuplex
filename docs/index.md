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

About Simplicity
----------------

What does Tuplex design consider "simple to write and read" to mean?
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
