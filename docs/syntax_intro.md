---
layout: default
---
## Syntax Introduction

(This is a quick syntax introduction which presumes the reader is familiar with programming in imperative languages.)

Functions (and methods and lambdas) are defined using `()->`.

This example also shows how the 'main' function - the entry point of the program - can be written. (The return type of main() can be omitted if desired.)

    main()->Int : {
        return 0;
    }

Compound statements that consist of a header and a body use a colon to separate the two. If the body contains more than a single statement it is enclosed in `{`braces`}`. Since the simple main function above contains a single statement, it could also be written as:

    main()->Int :
        return 0;

Or as a one-liner: `main()->Int : return 0;`

The same applies to `if`, `for`, and `while`.

#### Fields

Fields / variables are defined using `: =`, and both implicit and explicit type declaration is supported. Note that literal values are given the smallest type needed to hold the value, unless the literal type signifier is supplied (more details on this later).

    ubyteValue := 32;
    intValue := 32I;
    intValue : Int = 32;


Type definitions require the `type` keyword. The following examples also show how simple array and reference types are defined.

     type MyIntAlias : Int;
     type MyIntArray : [4]Int;
     type MyIntReference : &Int;


> Tuplex naming convention: Start type names with a capital letter; and functions, fields / variables with a lower case letter. Constants should be all-caps.

The syntax of scalar and boolean expressions, as well as function calls, is straight-forward and similar to other languages like C++ and Java. If and while statements can be single-statement using colon, or multi-statement using brackets.

    result := (2.0 + square( someLen )) / 3.14;
    if result > 20.0:
        puts( c"big!" );     ## C-string i.e. one character per byte
    tmp := ~ Int( result );  ## explicit cast from floating-point to Int
    while tmp > 0: {
        puts( c"iteration!" );
        tmp = tmp - 1;
    }


> Mutability: Tuplex variables / fields are immutable by default. The 'mut' keyword or the '~' tilde token are used to indicate the value may be modified after initialization.


#### Other Basic Syntax Elements

- Line comments begin with ##.
- Multi-line comments are enclosed between /* and */. Comments can be nested.
- Statements are terminated with ';' semicolon.
- Bodies of types, functions, and suites of statements are enclosed in '{}' braces.


#### Imports

In single-file programs it isn't necessary to declare a module name. In order to use types and functions of other modules however they must be imported. Import statements must appear at the beginning of a file or module.

    import tx.c.*;  ## imports all names in the tx.c module

> Note: The tx module is imported by default (equivalent to the statement 'import tx.*;'). This contains all the built-in types. The sub-modules of tx aren't, however.


#### A Working Program

Now let's put it all together into a working program:

```
import tx.c.puts;  ## imports the puts name from the tx.c module

/** converts arg to Float and returns its square */
square( intVal : Int ) -> Float : {
    return Float(intVal) * Float(intVal);
}

/** a global constant */
INT_VALUE := 9I;

main()->Int : {
    someLen := INT_VALUE;
    result := (2.0 + square( someLen )) / 3.14;
    if result > 20.0:
        puts( c"big!" );     ## C-string i.e. one character per byte
    tmp := ~ Int( result );  ## explicit cast from floating-point to Int
    while tmp > 0 : {
        puts( c"iteration!" );
        tmp = tmp - 1;
    }
    return 0;
}
```
