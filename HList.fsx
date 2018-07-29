#load "Teq.fsx"

open Teq

type 'a HList =
| Nil of Teq<'a, unit>
| Cons of 'a HListConsCrate

and 'a HListConsCrate = abstract member Apply : HListConsEvaluator<'a, 'ret> -> 'ret
and HListConsEvaluator<'a, 'ret> = abstract member Eval : 'b -> 'c HList -> Teq<'a, 'b -> 'c> -> 'ret

[<RequireQualifiedAccess>]
module HList =

    // Strictly speaking not type-safe, but we know this will always hold
    let private congHList (_ : Teq<'a, 'b>) : Teq<'a HList, 'b HList> =
        Teq.trustMe

    // Not that the type of empty is unit HList - the Teq enforces this
    let empty = HList.Nil Teq.refl

    // Again, the Teq here enforces a return type of ('a -> 'b) HList
    let cons (x : 'a) (xs : 'b HList) =
        HList.Cons
            { new HListConsCrate<'a -> 'b> with
                member __.Apply e = e.Eval x xs Teq.refl<'a -> 'b>
            }

    let head<'a, 'b> (xs : ('a -> 'b) HList) : 'a =
        match xs with
        | Nil _ ->
            // Note that it is impossible to hit this case, since the generic
            // type parameter of our HList implies that the list is non-empty
            failwith "Impossible - we have a proof that unit = 'a -> 'b"
        | Cons b ->
            b.Apply
                { new HListConsEvaluator<_,_> with
                    member __.Eval x _ teq =
                        let teq = teq |> Teq.domain
                        x |> Teq.castTo teq
                }

    let tail<'a, 'b> (xs : ('a -> 'b) HList) : 'b HList =
       match xs with
       | Nil _ ->
            // Note that it is impossible to hit this case, since the generic
            // type parameter of our HList implies that the list is non-empty
            failwith "Impossible - we have a proof that unit = 'a -> 'b"
        | Cons b ->
            b.Apply
                { new HListConsEvaluator<_,_> with
                    member __.Eval _ xs teq =
                        let teq = teq |> Teq.range |> congHList
                        xs |> Teq.castTo teq
                }

(* Computation expression builder *)

type HListBuilder () =

    member __.Yield    a     = HList.cons a
    member __.Combine (f, g) = g >> f
    member __.Delay    f     = f ()
    member __.Run      f     = f HList.empty

let hList = HListBuilder ()

(* Example usage *)

let rec toStringContents<'a> (xs : 'a HList) : string list =
    match xs with
    | Nil _ -> []
    | Cons b ->
        b.Apply
            { new HListConsEvaluator<_,_> with
                member __.Eval x xs _ =
                    sprintf "%A" x :: toStringContents xs
            }

let example : (string -> int -> bool -> unit) HList =
    HList.empty
    |> HList.cons true
    |> HList.cons 1234
    |> HList.cons "Hello"

printfn
    "The contents of your HList is: [ %s ]"
    (example |> toStringContents |> String.concat " ; ")

let example2 : (bool -> int -> string -> unit) HList =
    hList {
        yield true
        yield 1234
        yield "Hello"
    }
