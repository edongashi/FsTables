namespace FsTables.Core

[<AutoOpen>]
module Operators =
  open System.Collections.Generic

  let inline (>>=) x f = Option.bind f x
  let inline (>=>) f1 f2 arg = f1 arg >>= f2

  let at<'T> index (sequence : seq<'T>) =
    match sequence with
    | :? ('T[]) as arr -> arr |> Array.tryItem index
    | :? (IList<'T>) as list ->
        if index > 0 && index < list.Count
        then Some list.[index]
        else None
    | _ -> sequence |> Seq.tryItem index

  let inline (@?) seq index = at index seq

  type OptionBuilder() =
    member x.Bind(v,f) = Option.bind f v
    member x.Return v = Some v
    member x.ReturnFrom o = o
    member x.Zero () = None

  let option = OptionBuilder()
