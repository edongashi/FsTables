namespace FsTables.Core

type ColumnWidth = ColumnWidth of double

type ColumnStyle =
  { Width : Value<ColumnWidth> }

type Column =
  { ColumnStyle : ColumnStyle
    CellStyle : CellStyle
    Data : AttachedData }
