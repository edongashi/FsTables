namespace FsTables.Excel
open FsTables.Common

type RowHeight = RowHeight of double

type RowStyle =
  { Height : Value<RowHeight> }

type Row =
  { Cells : seq<Cell>
    RowStyle : RowStyle
    CellStyle : CellStyle
    Data : AttachedData }

type CellIndex = CellIndex of int

module RowStyle =
  let empty = { Height = Fallback(RowHeight 15.0) }

module Row =

  // Accessors

  let cell (CellIndex c) row = row.Cells @? c

  // Mutators

  let inline withCellsF f row =
    { row with Cells = f row.Cells }

  let inline withCells cells row =
    { row with Cells = cells }

  // Functions

  let map f = withCellsF (Seq.map f)

  let mapi f = withCellsF (Seq.indexed >> f >> Seq.map snd)

  let filter f = withCellsF (Seq.filter f)

  let filteri f = withCellsF (Seq.indexed >> Seq.filter f >> Seq.map snd)

  let without cell = filter ((=) cell >> not)

  let withouti (CellIndex c) = filteri (fst >> (=) c >> not)

  let append cell = withCellsF (Seq.append cell)

  let force = withCellsF Seq.toArray
