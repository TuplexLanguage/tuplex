---
layout: default
---

## Type System: Generics

This text is quite technical and presumes the reader is familiar with generic programming concepts.

> *On generic programming, from Wikipedia: [Generic programming is a style of computer programming in which algorithms are written in terms of types to-be-specified-later that are then instantiated when needed for specific types provided as parameters. This approach permits writing common functions or types that differ only in the set of types on which they operate when used, thus reducing duplication.](https://en.wikipedia.org/wiki/Generic_programming)*

### Terminology

A **concrete** type is a type that is adequately known to the compiler, so that instances of it may be created (instantiable).

A **type parameter** is a parameter of the type itself.

Type parameters may be of the **TYPE** kind or the **VALUE** kind.

A **type parameter binding** is the binding of a type / value to a TYPE / VALUE type parameter.

A **generic type** is a type that has one or more unbound type parameters.

A **specialized type** is a type that binds one or more type parameters of a generic base type.

A **type parameter constraint** defines constraints on what types / values may be bound to it.

A **Ref-constrained** type parameter may only be bound to reference types. This has special significance:
* A generic type with only ref-constrained unbound type parameters **is concrete**.
* A single copy of code suffices for purely ref-constrained generic types.

A **generic-dependent** type is dependent on any unbound non-reference generic type parameters.

* If a local non-generic type is declared within the lexical scope of an outer generic type, this makes the local type generic-dependent.
* If a type specialization's type parameter is being bound to a type that is itself a type parameter *at the binding site*, the type specialization is generic-dependent.

VALUE type parameters can be static (known at compile time) or dynamic (known at runtime). The prime example is Array, whose size may be defined dynamically upon allocation.

TYPE type parameter bindings are static only, i.e. known at compile time.

An **abstract type** is a type that has been declared abstract. Note that this is orthogonal to genericity and may be specified independently of whether a type is generic or not.

#### When is a type concrete

A concrete type is not declared abstract, nor is it *dependent* on any non-reference-constrained unbound type parameters.

A VALUE type parameter that is bound to a dynamic value (e.g. a dynamically computed array size), is still a bound parameter and in this context considered "concrete". (There are LLVM-dependent limitations on what the compiler can support in this regard. Without going into detail here, the typical usages work.)

> Reference types, as a special case, are always concrete, whether specialized or not.


### Generic Type Declaration Syntax

The built-in types Ref and Array are prime examples of generic types and their application.

    module tx;
    
    /** A Ref instance contains a pointer to an object in memory.
     * Ref is the base type for the REF type class.
     * @param {T} The type of the pointer target object
     */
    builtin type ~ Ref{T} <: Any { ... }

    /** An Array instance contains zero or more elements of the same type
     * serially arranged in memory.
     * Array is the base type for the ARRAY type class.
     * @param {E} The element type
     * @param {C} The array capacity, a UInt value
     */
    builtin type ~ Array{E, C:UInt} <: Any, Collection{E},
                                       Updatable{UInt, E} { ... }

The above don't specify constraints on their type parameters. The following is an example of a type that declares a ref-constrained type parameter:

    type ~ MyHandle{R <: Ref} { ... }


### Type Specialization Syntax

To define a reference specialization:

    intref : Ref{Int};

To define an array specialization:

    intarray : Array{Int, 4};

Note that `Array{Int}` would still be a generic type, and we couldn't declare function arguments or fields of such a type. We can however use references to such a type. To define a reference to an array of any length:

    arrayref : Ref{Array{Int}};

As we see, specializations don't have to bind all the type parameters at once. We can define *partial specializations* like the array above.

Specializations can also be made by binding to what is generic type parameters in an outer context. Array with its interface implementations and its local type ArrayIterator illustrates this:

```
builtin type ~ Array{E, C:UInt} <: Any, Collection{E}, Updatable{UInt, E}
{
    type ~ ArrayIterator <: Tuple, Iterator{E} {
        array     : Ref{Array{E}};
        nextIndex : ~UInt;
        ...
    }
    ....
}
```

Note that while ArrayIterator is not itself a *generic* type, it is *generic-dependent* due to it using E which is a generic parameter of its outer lexical context.

#### Syntactic Sugar

Since references and arrays are so common there is syntactic sugar for declaring them, which is quite similar to many other languages.

`&Int` is shorthand for `Ref{Int}`.

`[4]Int` is shorthand for `Array{Int,4}`, and `[]Int` for `Array{Int}`.

The may of course be combined, for example: `&[]&Int` is a reference to an array (of arbitrary length) of references to Int.


### Redeclaring and Recombining Type Parameters

One of the things that makes Tuplex generics powerful is that the parameters can be freely redeclared and recombined within a type hierarchy.

Consider a use case case where a data type is to be defined, which shall combine a generic base type with a generic interface defined in another module or API. Since the base type and the interface were defined independently of each other, it is up to the new data type to bring them together and bind their respective type parameters together:

```
type Pair{A,B} {
    a : A;
    b : B;
}

interface Attribute{K,V} {
    abstract get_key()   -> K;
    abstract get_value() -> V;
}

/** uses the Pair implementation to represent a Map entry
 *  and also supports the Attribute interface
 */
type MapEntry{K,V} <: Pair{K,V}, Attribute{K,V} {
    override get_key()   -> K  { return self.a; }
    override get_value() -> V  { return self.b; }
}
```

A generic type may be extended by adding additional type parameters. Consider a use case where the Pair above was to be extended with some 'label' of a generic type, and for some reason the new type wants to use other names for Pair's type parameters:

```
type LabelPair{X, Y, L} <: Pair{X, Y} {
    label : L;
}
```

### Accessing the Type Parameter Bindings

Type parameters are implicit member symbols of their generic type.

`tx.Array.E` is the fully qualified name of the type declaration that is the generic TYPE type parameter of `tx.Array`. It has the constraint type as its type, in this case `Any`.

`tx.Array.C` is the fully qualified name of the field declaration that is the generic VALUE type parameter of `tx.Array`. It is of the type `UInt`.

Within the generic type's own namespace the name can of course be referred to directly without full qualifier, in this case as `E` and `C`.

Accessing a type parameter binding of a specialized type is analogous:

    myarray : Array{Int,4};
    ## myarray.E is Int
    ## myarray.C equals 4

> If you use the -ds or -dsx features of the txc command to dump the symbol table you can see the type parameters / bindings as members in the symbol table.

#### Accessing Shadowed Bindings

In a type hierarchy the parameters / bindings of ancestor types may be shadowed. Consider this example:

```
module my;

type Type1{E} <: Tuple {
    var : E;
}

type Type2{E} <: Type1{Ref{E}};

obj : Type2{Float};
```

How do we refer to the type E which obj.var is declared with?

`obj.E` will resolve to `Type2.E` which is `Float`.

`Type1.E` will resolve to the generic type parameter, which is the default constraint type `Any`, not the binding employed for `obj`.

obj.super.super.E would be a candidate but may not work in case there are generic interfaces with a parameter 'E' implemented as well. Also the reader would have to parse the type hierarchy in order to understand what E this expression resolves to. This makes this solution unattractive.

Instead Tuplex supports an explicit way to access specific members of a object's type hierarchy. There is a "hashed name" syntax where the dots of a fully qualified name is replaced with '#' characters and this hashed name can be used as a member name referral, like so:

```
obj.E is Float
obj.my#Type2#E is Float
obj.my#Type1#E is &Float
```
