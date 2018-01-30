---
layout: default
title: On Uniformity of Syntax
---
{{ page.date | date: "%Y-%m-%d" }}
## {{ page.title }}

Labels: Syntax Functions Uniformity

One of the philosophies behind Tuplex is to have a syntax that is as uniform and consistent as possible. This means that any semantic production should have the same syntax regardless of where it is placed. This has the benefits of:
* Regular grammar - easier to learn, read, and write
* Orthogonal semantics - helps keep semantic concepts independent and possible to recombine without "exceptions to the rule"
* Making copy-paste easier - reduces need for editing copied/moved code and the associated risk of bugs

Consider the core language elements of function, type, and value expressions. If these are written the same way regardless of the form of statement they are part of, we've come a long way towards uniformity.

Functions, being one of the most complex constructs both syntactically and semantically, are a good illustration of how well a language achieves uniformity. An interesting exercise is to examine one's favorite language and see how many distinct function, method, lambda, and function pointer syntaxes it has...

The following Tuplex program shows how function type declaration, global and local functions, instance methods and lambdas, all have identical syntax, and also are fully interchangeable in assignment, function argument passing, and invocation.

My favorite aspect of this is how instance methods, free functions and lambdas all are handled equivalently and for example can be passed as argument to another function without that function caring about the difference. Closure capturing, when needed, is fully transparent.

```
type ~ FuncType derives ( a : Int )->Int;

square( a : Int )->Int : { return a * a; }

## these are equivalent:
aFunction : ( a : Int )->Int = square;
bFunction : FuncType         = square;
cFunction                   := square;

## function returning function:
type FuncProvider derives ()-> ( a : Int )->Int;

type Multiplier : {
    b : Int;

    mul( a : Int )->Int : { return a * self.b; }

    higher( f : FuncType )->Int : {
        return f( self.b );
    }
}

main()->Int : {
    localFunc := ( a : Int )->Int : return a * 2; ;

    value : ~Int = 2;

    ## global function invocation:
    value = square( value ); ## 4

    ## local function assignment and invocation:
    tmpFn : ~FuncType = localFunc;
    value = tmpFn( value );  ## 8

    ## global function assignment and invocation:
    tmpFn = aFunction;
    value = tmpFn( value );  ## 64

    ## instance method assignment and invocation:
    mulObj := Multiplier(2);
    tmpFn = mulObj.mul;
    value = tmpFn( value );  ## 128

    ## higher order function and inline lambda:
    value = value + mulObj.higher( ( a : Int )->Int : { return a + 1; } );

    return value;  ## process returns 131
}
```
