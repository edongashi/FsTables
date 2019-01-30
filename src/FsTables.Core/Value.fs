namespace FsTables.Core

type Value<'T> =
  | Inherit
  | Literal of 'T
  | Computed of Lazy<'T>
  | Fallback of 'T
  | FallbackComputed of Lazy<'T>

type Value = Value<obj>

type IValueContext<'T> =
  abstract Value : Value<'T>
  abstract Parent : IValueContext<'T> option

module Value =
  type private ResolvedValue<'T> =
    | ResolvedLiteral of 'T
    | ResolvedLazy of Lazy<'T>
  
  let private coalesceSecond r l =
    match l with
    | Some _, _ -> l
    | None, _ -> (None, Some r)
  
  let private maybeMap f = Option.map f >> Option.defaultValue (None, None)

  let resolve defval (ctx : IValueContext<_>) =
    let rec walk (ctx : IValueContext<_>) =
      match ctx.Value with
      | Inherit -> ctx.Parent |> maybeMap walk
      | Literal v -> (Some v, None)
      | Computed v -> (Some(v.Force()), None)
      | Fallback v -> 
          ctx.Parent
          |> maybeMap walk
          |> coalesceSecond (ResolvedLiteral v)
      | FallbackComputed v -> 
          ctx.Parent
          |> maybeMap walk
          |> coalesceSecond (ResolvedLazy v)
    
    let result = walk ctx
    match result with
    | Some v, _ -> v
    | _, Some(ResolvedLiteral v) -> v
    | _, Some(ResolvedLazy v) -> v.Force()
    | _ -> defval
