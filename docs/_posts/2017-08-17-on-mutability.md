---
layout: default
title: On Mutability
---
{{ page.date | date: "%Y-%m-%d" }}
## {{ page.title }}

Labels: Mutability, Modifiability, Type System, Syntax

> **Updated 2017-08-24:**
> - Revised syntax for mutable field declarations with implicit type.
> - Added section on mutable methods, lambdas, and closures

### Immutability is the default for fields and types

All fields (including variables) are immutable unless explicitly declared mutable.

All types are immutable unless explicitly declared mutable.

If a type is immutable then fields of that type may never be declared mutable.

> Thus all instances of an immutable type are guaranteed to be immutable.

The tilde `~` symbol is used to denote mutability in type and field declarations.

    imm0 := 42;         ## immutable field (implicit type)
    imm1 : Int = 42;    ## immutable field (explicit type)

    mut0 : ~Int = 42;   ## mutable field (explicit type)
    mut1 := ~ 42;       ## mutable field (implicit type)
    mut2 := ~ imm0;     ## creating a mutable copy of imm0

    err : ~Int = ~42;   ## error, can't add qualifier to both field and type


    ## An immutable type - the default - is declared like so:
    type MyImmType : {
         immfield : Int;
    }  

    ## A mutable type is declared like so:
    type ~ MyMutType : {
         mutfield : ~Int;
    }

The type definition of `Int` and all the other built-in elementary types are mutable.<br>(See `tx/elementary.tx`.)

    builtin type ~ Int derives Signed ...


#### Initialization

Immutable fields must be initialized upon declaration. The exception is member fields which are initialized in the constructor.


### Immutability is transitive

Immutability is part of the memory safety mechanisms and the goal is to control access on graphs of data objects. The immutability characteristic is therefore transitive when traversing the object graph via field members and references, referred to as the "access path":

    obj.someref.somearray[2].somefield

Note that while traversing the the object graph access path, the container that holds a reference needs to be considered mutable, and the reference target type needs to be mutable, but the reference itself (its pointer value) does **not** need to be mutable. In other words one can have a generally mutable object graph where one or more links between the objects will not change what they point to.

Consider these examples of L-value expressions that are being assigned to. In order for the assignee to be assignable via the stated access path, all the fields that are underlined must be declared mutable (which of course requires that their types allow mutability).

<pre><code><u>someReferenceField</u> = someRefValue;
someRef . <u>member</u> = someValue;
<u>someObj</u> . <u>member</u> . reference . <u>array</u>[42] . <u>member</u> . <u>reference</u> = aRefValue;
</code></pre>

As shown here, the tail field of the access path (the actual assignee) must always be mutable regardless of whether it is a reference or not, since it is being written to by the assignment. Except for the tail field, references in the access path need not be mutable.


### Immutability and generic types

Immutability for generic types gets more complicated since they effectively are a recombination of multiple type definitions, each with its own mutability/immutability: The generic base type; the individual specialization of that type; and that specialization's type parameters.

These are the requirements:

* The programmer should be able to define both immutable and mutable generic types, just like for non-generic types.

* A generic type may be specialized with in some cases mutable type parameters, other cases with immutable type parameters.

* A generic type that has multiple type parameters may be specialized with both mutable and immutable ones simultaneously!

This leads to the following rules:

* In order to declare a mutable specialized type of a generic type, the generic type must have been declared mutable. (This is consistent with conventional inheritance in Tuplex, where a mutable subtype must have a supertype that is mutable.)

* An immutable specialization of a mutable generic type will suppress (make inaccessible) methods that modify the instance (like non-const methods in C++).

  This makes it straight-forward to instantiate both mutable and immutable versions of containers, for instance.

* A mutable specialization can still be declared with one or more immutable type parameters. Errors will be generated if the provided type parameters conflict with expected mutability. In practice a mutable specialization will typically (but not always) require one or more of its type arguments to be mutable.

#### Example: A generic Map

Consider the use case of a Map, a generic container type that is parameterized on the types the keys and values in its entries. Some usages might be constant (thus prohibiting any modifying operations to it), while other usages might need it to be modifiable.

In order to allow for mutable instances, the generic type must be declared mutable:

    type ~ Map{ K, V } : { ... }

Both mutable and immutable specializations of Map may be declared. If the map specialization is declared mutable, it will in practice require both type parameters to be mutable since the map's modifying methods will modify keys and values in it.

For immutable map instances, only non-modifying methods may be invoked.

    type ConstMap derives Map{ Int, String };   ## immutable specific map type
    type ConstMap derives Map{ ~Int, ~String };
         ## ~ on the type parameters has no effect if the type is immutable 

    type ~ MutMap derives Map{ ~Int, ~String }; ## mutable specific map type
    type ~ MutMap derives Map{ Int, String };   ## will cause compilation error

    mymap :=  ConstMap( ... ); ## creates a constant instance
    mymap :=  MutMap( ... );   ## also creates constant instance, but of MutMap
    mymap := ~ConstMap( ... ); ## error
    mymap := ~MutMap( ... );   ## creates a mutable instance

It's analogous for the built-in arrays - they must have a mutable element type in order to be mutable.


### Mutable methods, lambdas, and closures

In order to preserve immutability on objects of types with methods, we must distinguish between the methods that modify the object and those that don't. In order to be permitted to modify the object's contents, the method must be declared as mutating. (For readers familiar with C++ this is analogous to non-const and const methods.)

    type ~ Stack{ E } : {
        count() -> UInt :      ## non-mutating (read-only) method
        { ... }

        push( value : E ) ~ :  ## mutating method
        { ... }

        pop() ~ -> E :         ## mutating method with return value
        { ... }
    }

In mutating methods, the `self` reference will refer to a mutable object; in the non-mutating method it will refer to an immutable object.

(As described in the previous section, any mutating methods in generic types are suppressed in the non-mutable type specializations of that type.)

#### Applies to all lambda types

This concept is actually not specific to methods. Methods are a special case of functions with a closure, where the closure is simply the `self` reference. In Tuplex any function / lambda (and function / lambda types) can be declared mutable, which means they may modify their closure.

A non-mutating function / lambda with no mutable arguments is thus a pure function.
