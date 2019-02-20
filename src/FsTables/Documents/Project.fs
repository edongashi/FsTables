namespace FsTables.Documents
open FsTables.Common

type Page =
  { Tags : Tag list }

type Document =
  { Title : string
    Tags : Tag list }

type Project =
  { Documents : Document list
    Tags : Tag list }
