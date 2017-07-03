#pragma once


/** The type classes of Tuplex. Each type class is handled specially by the compiler. */
enum TxTypeClass {
    /** Represents the Any root type. */
    TXTC_ANY,
    /** The built-in, non-aggregate types (e.g. Bool, Scalar). */
    TXTC_ELEMENTARY,
    /** The Ref types. */
    TXTC_REFERENCE,
    /** The Array types. */
    TXTC_ARRAY,
    /** The Tuple types. */
    TXTC_TUPLE,
    /** The Union types. */
    TXTC_UNION,
    /** The function types, including methods and lambdas. */
    TXTC_FUNCTION,
    /** The interface types. */
    TXTC_INTERFACE,
    /** The internal, implicit interface adapter types. */
    TXTC_INTERFACEADAPTER,
    /** The internal Void type (represents the "return type" of functions that do not return a value). */
    TXTC_VOID,
};
