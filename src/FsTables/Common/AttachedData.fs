namespace FsTables.Common

type Tag = Tag of string

type Value =
  | String of string
  | Integer of int
  | Float of float
  | Boolean of bool
  | Null

type AttachedData =
  { Props : Map<string, Value>
    Tags : Tag list }

module AttachedData =
  let empty =
    { Props = Map.empty
      Tags = List.empty }
