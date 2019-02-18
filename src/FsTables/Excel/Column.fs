namespace FsTables.Excel
open FsTables.Common

type ColumnWidth = ColumnWidth of double

type ColumnStyle =
  { Width : Value<ColumnWidth> }

type Column =
  { ColumnStyle : ColumnStyle
    CellStyle : CellStyle
    Data : AttachedData }

module ColumnStyle =
  let empty = { Width = Fallback(ColumnWidth 8.0) }
