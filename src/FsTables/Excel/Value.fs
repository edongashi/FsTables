namespace FsTables.Common

type Value<'T> =
  | Inherit
  | Literal of 'T
  | Computed of Lazy<'T>
  | Fallback of 'T
  | FallbackComputed of Lazy<'T>

type Value = Value<obj>

module Value =
  type private ResolvedValue<'T> =
    | ResolvedLiteral of 'T
    | ResolvedLazy of Lazy<'T>

  let private coalesceSecond r l =
    match l with
    | Some _, _ -> l
    | None, _ -> (None, Some r)

  let resolve defval (ctx : seq<_>) =
    use enumerator = ctx.GetEnumerator()

    let rec walk() =
      if enumerator.MoveNext() then
        match enumerator.Current with
        | Inherit -> walk()
        | Literal v -> (Some v, None)
        | Computed v -> (Some(v.Force()), None)
        | Fallback v -> walk() |> coalesceSecond (ResolvedLiteral v)
        | FallbackComputed v -> walk() |> coalesceSecond (ResolvedLazy v)
      else (None, None)

    let result = walk()
    match result with
    | Some v, _ -> v
    | _, Some(ResolvedLiteral v) -> v
    | _, Some(ResolvedLazy v) -> v.Force()
    | _ -> defval
