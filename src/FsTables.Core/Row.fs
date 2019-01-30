namespace FsTables.Core

type RowHeight = RowHeight of double

type RowStyle =
  { Height : Value<RowHeight> }

type Row =
  { Cells : seq<Cell>
    RowStyle : RowStyle
    CellStyle : CellStyle
    Data : AttachedData }

type CellIndex = CellIndex of int

module Row =

  // Accessors

  let cell (CellIndex c) row = row.Cells @? c
