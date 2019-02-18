namespace FsTables.Excel
open FsTables.Common

type CellContent =
  | EmptyContent
  | TextContent of string
  | DoubleContent of double
  | IntContent of int

type CellStyle =
  { Content : Value<CellContent> }

type Cell =
  { Style : CellStyle
    Data : AttachedData }

module CellStyle =
  let empty = { Content = Inherit }
