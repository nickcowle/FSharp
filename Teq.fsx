/// Teq, short for 'type equality' is a type that carries
/// around a proof that two types are equal
type Teq<'a, 'b> = private Refl of ('a -> 'b) * ('b -> 'a)

[<RequireQualifiedAccess>]
module Teq =

    let refl<'a> : Teq<'a, 'a> = Refl (id, id)

    // Teq<'a , 'b> -> 'a -> 'b
    let castFrom (Refl (castFrom, castTo)) = castFrom

    // Teq<'a, 'b> -> 'b -> 'a
    let castTo (Refl (castFrom, castTo)) = castTo

    // For places where the type system isn't powerful enough
    // to prove equality, we can fall back on trustMe. Note that
    // an invalid use of trustMe will blow up earlier, i.e. at the
    // point at which the Teq is constructed rather than when we
    // use it to cast
    let internal trustMe<'a, 'b> : Teq<'a, 'b> =
        unbox refl<'a>

    // An example where we can't safely construct a Teq from another,
    // although we know this will always hold
    let domain (_ : Teq<'a -> 'b, 'c -> 'd>) : Teq<'a, 'c> =
        trustMe

    // Ditto
    let range (_ : Teq<'a -> 'b, 'c -> 'd>) : Teq<'b, 'd> =
        trustMe
