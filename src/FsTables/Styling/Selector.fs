namespace FsTables.Styling
open FsTables.Common

type ISelectable =
  abstract MatchTag : Tag -> bool
  abstract CanContainChild : Tag -> bool
  abstract Children : unit -> seq<ISelectable>

module Selectable =
  let createSimple tags getChildren =
    let tags = Set.ofList<string> tags
    { new ISelectable with
        member __.MatchTag (Tag tag) = Set.contains tag tags
        member __.CanContainChild _ = true
        member __.Children() = getChildren() }

type Node =
  { Element : ISelectable
    Index : int
    Parents : Node list }

module Node =
  let createRoot element =
    { Element = element
      Index = 0
      Parents = [] }

  let isRoot node = List.isEmpty node.Parents

  let children node =
    let children = node.Element.Children()
    let newParents = node :: node.Parents
    children |> Seq.mapi (fun i child -> {
      Element = child
      Index = i
      Parents = newParents
    })

type IndexComparison =
  | LessThan
  | LessThanOrEqual
  | Equal
  | NotEqual
  | GreaterThan
  | GreaterThanOrEqual

type Selector =
  | OrSelector of Selector * Selector
  | AndSelector of Selector * Selector
  | NotSelector of Selector
  | DescendantSelector of Selector * Selector
  | ChildSelector of Selector * Selector
  | SubquerySelector of Selector * Selector
  | TagSelector of Tag
  | IndexSelector of IndexComparison * int
  | EvenSelector
  | OddSelector
  | AnySelector
  | RootSelector

module rec Selector =
  open Node

  let rec check selector node =
    match selector with
    | OrSelector(a, b) -> node |> check a || node |> check b
    | AndSelector(a, b) -> node |> check a && node |> check b
    | NotSelector a -> node |> check a |> not
    | DescendantSelector(a, b) ->
        node |> isRoot |> not
        && node |> check b
        && node |> anyParent a
    | ChildSelector(a, b) ->
        match node.Parents with
        | parent :: _ -> node |> check b && parent |> check a
        | [] -> false
    | SubquerySelector(a, b) ->
        node |> check a && node |> anyChild b
    | TagSelector tag -> node.Element.MatchTag tag
    | IndexSelector(comparison, index) ->
        match comparison with
        | LessThan -> index < node.Index
        | LessThanOrEqual -> index <= node.Index
        | Equal -> index = node.Index
        | NotEqual -> index <> node.Index
        | GreaterThan -> index > node.Index
        | GreaterThanOrEqual -> index >= node.Index
    | EvenSelector -> node.Index % 2 = 1
    | OddSelector -> node.Index % 2 = 0
    | AnySelector -> true
    | RootSelector -> node |> isRoot

  let shouldDescend s n =
    true // TODO

  let any selector node =
    node |> find selector |> Seq.isEmpty |> not

  let anyChild selector node =
    node |> children |> Seq.exists (any selector)

  let anyParent selector node =
    node.Parents |> List.exists (check selector)

  let rec find selector node =
    let inline findChildren s = node |> children |> Seq.collect (find s)
    seq {
      match selector with
      | OrSelector(DescendantSelector(a1, b1), DescendantSelector(a2, b2)) ->
          let left = if node |> check a1 then b1 else DescendantSelector(a1, b1)
          let right = if node |> check a2 then b2 else DescendantSelector(a2, b2)
          let descendLeft = node |> shouldDescend left
          let descendRight = node |> shouldDescend right
          let simplifiedSelector =
            match (descendLeft, descendRight) with
            | true, true -> OrSelector(left, right)
            | true, false -> left
            | false, true -> right
            | _ -> selector
          if descendLeft || descendRight then
            yield! findChildren simplifiedSelector
      | OrSelector(DescendantSelector(a, b), right) ->
          let left = if node |> check a then b else DescendantSelector(a, b)
          let descendLeft = node |> shouldDescend left
          let descendRight = node |> shouldDescend right
          let simplifiedSelector =
            match (descendLeft, descendRight) with
            | true, true -> OrSelector(left, right)
            | true, false -> left
            | false, true -> right
            | _ -> selector
          if node |> check right then
            yield node
          if descendLeft || descendRight then
            yield! findChildren simplifiedSelector
      | OrSelector(left, DescendantSelector(a, b)) ->
          let right = if node |> check a then b else DescendantSelector(a, b)
          let descendLeft = node |> shouldDescend left
          let descendRight = node |> shouldDescend right
          let simplifiedSelector =
            match (descendLeft, descendRight) with
            | true, true -> OrSelector(left, right)
            | true, false -> left
            | false, true -> right
            | _ -> selector
          if node |> check left then
            yield node
          if descendLeft || descendRight then
            yield! findChildren simplifiedSelector
      | RootSelector ->
          if node |> isRoot then
            yield node
      | DescendantSelector(a, b) ->
          if node |> check a then
            if node |> shouldDescend b then
              yield! findChildren b
          elif node |> shouldDescend selector then
            yield! findChildren selector
      | ChildSelector(a, b) ->
          if node |> check a then
            if node |> shouldDescend b then
              for child in (node |> children) do
                if child |> check b then
                  yield child
                if child |> shouldDescend selector then
                  yield! child |> find selector
          elif node |> shouldDescend selector then
            yield! findChildren selector
      | SubquerySelector(a, b) ->
          if node |> check a then
            if Seq.isEmpty (findChildren b) |> not then
              yield node
          if node |> shouldDescend selector then
            yield! findChildren selector
      | _ ->
          if node |> check selector then
            yield node
          if node |> shouldDescend selector then
            yield! findChildren selector
    }
