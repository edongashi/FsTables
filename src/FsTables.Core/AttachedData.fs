namespace FsTables.Core

type Tag = Tag of string

type AttachedData =
  { Props : Map<string, Value>
    Tags : Tag list }
