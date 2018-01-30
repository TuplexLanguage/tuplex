---
layout: default
---

## Type System Basics

*This text presumes the reader is generally familiar with types in programming languages.*

For a general text on the type system approach and goals, see the post [A Consistent Type System Foundation](2017/04/27/consistent_type_system.html).

### Type Declaration Syntax

Object types and interface types have similar declaration syntax.

<pre><code>type  ~?  NAME ( <: BASETYPE  ( , INTERFACE )* )? ( ; | BODY )
interface NAME ( <: INTERFACE ( , INTERFACE )* )? ( ; | BODY )
</code></pre>

* A tilde `~` declares an object type to be mutable, which means instances of it are allowed to be made mutable.

* The `<:` token denotes type derivation (like in the Julia language). If unspecified, object types will by default derive from `Tuple` and interfaces from `Interface`.

(`~` and `mut` are interchangeable, as are `<:` and `derives`. Here we use the short form.)

Declaring a plain old data type (a tuple of fields), mutable or not, is very simple:

    ## An immutable plain old data type:
    type MyImmType : {
         immfield : Int;
    }  

    ## A mutable plain old data type:
    type ~ MyMutType : {
         mutfield : ~Int;
    }

> The Tuplex naming convention is to start type names with a capital letter.


### Common Type Hierarchy

The type system is akin to a pure OO type system, with some distinctions. Types are not referred to as “classes”. The Tuple types and their instances are analogous to what is called classes and objects in other OO-languages.

All values in Tuplex are referred to as objects. Some objects are elementary which means they are not usually subdivided into smaller data parts - like integers, booleans, and references. Other objects are composite which means they are composed of a number of smaller objects.

> Tuplex objects are not “boxed”. Neither the elementary types nor the composite ones have a “header” or other construct with a memory footprint adjacent to the object value.

Like all OO-type systems, a type derives the characteristics of its parent, and usually adds some of its own (e.g. new fields and methods). This way all the types are organized into a hierarchy and the strengths of OO within polymorphism and behavior reusability can be utilized.

Tuplex is single inheritance with addition of interfaces (like Java and C#).

* The **basetype** of a type must be a proper object supertype (type classes Tuple or Array).

* Any additional supertypes must be interfaces.


### Inheritance

The fields and methods of a type are its *members*.

A type inherits all the members of its supertypes, and may access them by name.

A type member is either an instance member or a non-instance (virtual/static) member.

If a subtype declares a member with the same name as a member of a supertype, the supertype's member is overridden. That means that for instances of the subtype, the *overriding* member will be accessed by that name instead of the overridden original. This redirection is transparent to users of the type and is [a mechanism for polymorphism](https://en.wikipedia.org/wiki/Subtyping).

Non-instance members are always virtual in Tuplex except for some special cases (e.g. constructors). Therefore the typical 'static' keyword used in many other OO languages is not used here, instead Tuplex uses only 'virtual'. *(This may be revised in the future.)*


### Interfaces

Interfaces can have no instance data, i.e. no instance fields. They may however have non-instance fields and methods, including method implementations.


### Constructors

Object types whose members are initialized in non-trivial ways need constructors.

Constructors are denoted with the method name `self`. They are responsible for initializing instance data.

Constructors are special methods with special rules. Using 'self' to denote them instead of the type name as in most other OO languages makes this grammar context-free and simplifies renaming and copy-pasting.

Interfaces have no instance data and thus no constructors.

*Note: If no constructor is defined, there shall be an implicit one with the same arguments as the instance fields. This is as of this writing however not implemented yet.*


### Type Hierarchy Example

Using methods, constructors, and interfaces we can create the more sophisticated type hierarchies familiar from other OO languages:

```
interface IntfA : {
    abstract get_value()->Int;

    ## "mixin" or default-implementation interface methods:
    mixin_method_1()->Int : return 1;
    mixin_method_2()->Int : return 2;
}

interface IntfB <: IntfA : {         ## extends the interface IntfA
    abstract set_value( v : Int );
}

type ~ MyType <: Tuple, IntfB : {    ## a Tuple type that implements IntfB
    field : ~Int;

    self(f : Int) : {
        self.field = f;
    }

    override get_value()->Int : {
        return self.field;
    }

    override set_value( v : Int ) ~ : {
        self.field = v;
    }

    override mixin_method_2()->Int : {
        return 3;
    }
}
```


### "Empty" Derivations

An empty derivation is a derivation that that doesn't specify any new members or interfaces. It creates a unique new type that is structurally identical to an existing one:

    type MyImmType2 <: MyImmType;
    type ~ MyMutType2 <: MyMutType;

Note: An empty derivation has the same constructors as its basetype (which is not the case for non-empty derivations).


### Type Attributes Abstract and Final

An object type declaration may also be preceded with one of the declaration flags 'abstract' or 'final'.

#### Abstract

An abstract type may not be instantiated directly; only concrete subtypes thereof.

If a type is declared abstract it is permitted to contain abstract members. An abstract member is a member declaration without a definition/body. A subtype that is concrete must then override that member with a matching definition/body.

Interfaces are implicitly abstract.

#### Final

Final implies the type may never be derived from by a subtype.


### Generic Types

Generic types are covered in [Type System: Generics](type_system_generics.html).