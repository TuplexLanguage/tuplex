---
layout: default
---
## Essential Language Features

This is an overview of the essential features of the Tuplex programming language.

### Consistency of syntax

* Simple, unified and universal name and namespace handling
* Context-free, consistent, left-to-right type expression syntax
* Syntactically unified function/lambda construct

Work in progress:
* Intuitive and unified syntax for arrays, collection types, ranges, and sequencors / iterators

### Strong yet easy to use type system

* Strong, compile-time type checking
* All values are syntactically treated as objects, but without "boxing" overhead
* Generic types, parameterizable with both types and values
* Consistent polymorphism (all values are objects) with single inheritance plus interfaces
* Automatic type inference
* Safe and intuitive type checking and type conversion
* Values and types are immutable by default, must be explicitly declared as modifiable

Work in progress:
* Type-safe references; no null reference vulnerability
* Value/object initialization guarantees

### Modern source file, module, and compilation handling

* Order of declaration doesn't matter
* No redundancy, no forward declarations
* All parts of a source code entity in one place
  * interface
  * annotations
  * implementation
  * documentation
* Easy-to-use C foreign function interface

Work in progress:
* Language support for defining the external/public API of distribution packages

### Guaranteed thread-safe data for high-performance concurrency

This work requires many of the other essential features to be ready and will commence in the second project phase.
* Dataspaces
* Compiler statically guarantees no memory data race vulnerability
* Modifiable data never visible to more than one thread at a time
* Dataspaces are a light-weight, static syntax with no run-time overhead
* Data can be shared by passing memory references - no copying required

### Possible features on vision-list

These capabilities aren't "essential", but would complement the rest of the language well.

* Comprehensive run-time type information
* Built-in efficient serialization and de-serialization
* Safe unions (so-called sum types) as a built-in language feature
* Vectors (SIMD types) as first-class citizens
* Zero-boiler-plate object proxying (type delegation)

### What Tuplex does not aim to provide

* Direct access to memory addresses, registers etc
* Abstraction from data representation details such as sizes of elementary data types
* Dynamic code loading (except for OS-level dynamic link libraries)
