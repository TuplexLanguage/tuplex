---
layout: default
title: A Consistent Type System Foundation
---
{{ page.date | date: "%Y-%m-%d" }}
## {{ page.title }}

Labels: Type Classes, Types

> **Updated 2017-08-31:**
> Added type hierarchy diagram

The type system is the cornerstone of a programming language. It is the essential enabler for how a program's sought behavior can be expressed and it is the chief controller preventing behavior that is unintended.

Different languages emphasize these two roles to varying degrees. Sometimes one is given clear priority over the other. Some dynamically types languages take the extreme approach of not having the type system control anything at all, which can make for a very easy and low-friction programming experience for smaller and simpler tasks. But until the code is actually executed you never know exactly what is going to happen.

Haskell, the pure functional language, has the strictest type system I've experienced and it takes a bit of getting used to. It is mathematically and semantically consistent and it really does go a long way in helping ensure the program will be correct once it passes the type checker and compiles. However I'm not sure one would say it is easy to work with...

Tuplex is an imperative language, facilitating object-oriented, data-oriented, and functional programming styles. While a goal is to be as easy as possible to learn, read, and write, it should also facilitate high-performance implementation, as well as memory safety in multi-threaded programs.

*Easiness* means the type system shall be consistent in syntax and semantics. The basic type syntax shall be easy to learn for reasonably experienced programmers. The manners in how basic types are recombined into advanced ones shall appear straight-forward and predictable.

*Performance* means low-level data structure concepts shall be reflected among the basic types.

*Memory-safety* means there shall be strict rules governing references, uninitialized memory, and sharing of data between threads.

### Common Type Hierarchy

The type system is akin to a pure OO type system, with some distinctions. Types are not referred to as "classes". The Tuple types and their instances are analogous to what is called classes and objects in other OO-languages.

All values in Tuplex are referred to as objects. Some objects are elementary which means they are not usually subdivided into smaller data parts - like integers, booleans, and references. Other objects are composite which means they are composed of a number of smaller objects.

> Note: Tuplex objects are not "boxed". Neither the elementary types nor the composite ones have a "header" or other construct with a memory footprint adjacent to the object value.

Like all OO-type systems, a type derives the characteristics of its parent, and usually adds some of its own (e.g. new fields and methods). This way all the types are organized into a hierarchy and the strengths of OO within polymorphism and behavior reusability can be utilized.

### Any

Being part of the common type hierarchy, all types ultimately derive from the global root type at the top, named Any. All types thus derive the characteristics of Any. They aren't very many. Currently considered are key() and equals().

Any is never directly derived from in user code. It serves as the super type for the built-in base type of each type class.

### Type Classes

![Type hierarchy diagram]({{ site.baseurl }}/assets/BuiltinTypes.png)
<p align="center"><i>The blue types are the type class base types.<br>Some subtypes included for illustration.</i></p>

`Any` is a special type class with only one member, the Any type.

`Elementary` - scalar numbers like Int and Float, and Bool.

`Ref` - reference to an object of a certain type somewhere in memory.

`Array` - zero or more objects of the same type serially arranged in memory.

`Tuple` - zero or more objects of varying types grouped together in memory.

`Interface` - zero object memory footprint, but defines methods that the object will support.

`Function` - a callable code block with a defined function signature (argument and return types).

> Note: Function is often short-hand for any callable including lambdas, static methods, instance methods, and free functions. The share a uniform syntax and handling.

The purpose of each type class is to represent and efficiently implement a specific low-level behavior. Elementary types represent the conventional single-register values for numeric and boolean arithmetic.  Arrays represent - arrays, the mainstay of efficient computing. Functions represent executable machine code and interfaces represent a set of methods that shall be provided by all implementors of that interface.

All the type classes are basic building blocks in the sense that none of them could be represented in terms of the other ones. As such the type classes constitute a fully reduced set of type building blocks. (To be picky, there is one - conceptual - exception if performance were ignored: An array is a special case of a tuple whose elements are all of the same type.)

Additional type classes may be added in the future: Union as a type-safe sum type, and Vector to provide effective built-in support for SIMD computing.

### Object Types

Interfaces do not define any data content for their objects. Functions define code, not data. Interfaces and functions are thus not considered "object" types.

The other type classes - Elementary, Ref, Array, and Tuple - are the object type classes.

### Single Inheritance

Tuplex is single-inheritance in that each type derives from a single base type. In addition to the base type a type may implement a number of interfaces.
User-defined interfaces have Interface as their main base type.
This means that each type belongs to a specific type class.

### Generic Types

Generics, a.k.a. parameterized types, or templates in C++, are a core part of the type system. Ref and Array themselves are actually generic types. These are their built-in declarations:

`Ref{T} derives Any : ...`
    
`Array{E, L : UInt} derives Any : ...`

Generics is a complex subject and their implementation has been the most difficult part of the compiler. They will be covered in more depth in a post of their own.
