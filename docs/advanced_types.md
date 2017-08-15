---
layout: default
---
## Advanced Type Declarations

*This doc is todo, some examples from the test suite are provided in the mean time.*

```
#########################################
## Interfaces and (Tuple) object types

interface IntfA {
    abstract get_value()->Int;

    ## "mixin" or default-implementation interface methods:
    mixin_method_1()->Int { return 1; }
    mixin_method_2()->Int { return 2; }
}

interface IntfB {
    abstract set_value( v : Int );
}

type ~ Type : Tuple : IntfA, IntfB {
    fld : ~Int;

    self(f : Int) { self.fld = f; }

    override get_value()->Int {
        return self.fld;
    }

    override set_value( v : Int ) ~ {
        self.fld = v;
    }

    override mixin_method_2()->Int {
        return 3;
    }
}


#########################################
## Multiple levels of TYPE parameters, immutable and mutable:

type ImmFirst< A, B, C > {
    amemb : A;
    bmemb : B;
    cmemb : C;
}

type ImmSecnd<B,C> : ImmFirst<Int,B,C> {
}

type ImmThird<C> : ImmSecnd<UInt,C> {
}

type ImmFourth : ImmThird<Float> {
}

test_immfourth() {
    inst : ImmFourth;
    av : inst.A = -14000000I;
    bv : inst.B = 3000000000UI;
    cv : inst.C = 3.14;
}


type ~ First< A, B, C > {
    amemb : A;
    bmemb : B;
    cmemb : C;

    method()->Int { return 2; }
}

type ~ Secnd<B,C> : First<~Int,B,C> {
    foo : Float;
}

type ~ Third<C> : Secnd<~UInt,C> {
    foo : Float;
}

type ~ Fourth : Third<~Float> {
    foo : Float;
}

test_fourth() {
    inst : ~Fourth;
    inst.amemb = -14000000I;
    inst.bmemb = 3000000000UI;
    inst.cmemb = 3.14;
}


#########################################
## Redeclaring and recombining TYPE parameters:

type ~ Duo<A,B> {
    amemb : A;
    bmemb : B;
}

interface AnIf<C,D> {
    abstract get_c() -> C;
    abstract get_d() -> D;
}

type ~ Combo<X,Y> : Duo<X,Y> implements AnIf<X,Y> {
    xmemb : X;
    ymemb : Y;

    override get_c() -> X  { return self.xmemb; }
    override get_d() -> Y  { return self.ymemb; }
}

test_combo( v : Int ) {
    mycombo : ~Combo<~Float,~Long>;
    mycombo.xmemb = 3.14 * Float(v);
    mycombo.ymemb = 42 * v;
    assert mycombo.get_c() == 3.14;
    assert mycombo.get_d() == 42;
}
```