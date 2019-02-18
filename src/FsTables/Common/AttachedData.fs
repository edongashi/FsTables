namespace FsTables.Common

type Tag = Tag of string

type AttachedData =
  { Props : Map<string, Value>
    Tags : Tag list }

module AttachedData =
  let empty =
    { Props = Map.empty
      Tags = List.empty }
