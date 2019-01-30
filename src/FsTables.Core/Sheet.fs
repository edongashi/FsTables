namespace FsTables.Core

type SheetName = SheetName of string

type Sheet =
  { Name : SheetName
    Rows : seq<Row>
    Columns : seq<Column>
    CellStyle : CellStyle
    RowStyle : RowStyle
    ColumnStyle : ColumnStyle
    Data : AttachedData }

type RowIndex = RowIndex of int

type ColumnIndex = ColumnIndex of int

module Sheet =

  // Accessors

  let row (RowIndex r) sheet = sheet.Rows @? r

  let column (ColumnIndex c) sheet = sheet.Columns @? c

  let cell r c = row r >=> Row.cell c

  // Mutators

  let inline withRowsF f sheet =
    { sheet with Rows = f sheet.Rows }

  let inline withRows rows sheet =
    { sheet with Rows = rows }

  let inline withCols cols sheet =
    { sheet with Columns = cols }

  let inline withColsF f sheet =
    { sheet with Columns = f sheet.Columns }

  // Row functions

  let map f = withRowsF (Seq.map f)

  let mapi f = withRowsF (Seq.indexed >> f >> Seq.map snd)

  let filter f = withRowsF (Seq.filter f)

  let filteri f = withRowsF (Seq.indexed >> Seq.filter f >> Seq.map snd)

  let without row = filter ((=) row >> not)

  let withouti (RowIndex r) = filteri (fst >> (=) r >> not)

  let append row = withRowsF (Seq.append row)

  // Column functions

  let mapc f = withColsF (Seq.map f)

  let mapci f = withColsF (Seq.indexed >> f >> Seq.map snd)

  let filterc f = withColsF (Seq.filter f)

  let filterci f = withColsF (Seq.indexed >> Seq.filter f >> Seq.map snd)

  let withoutc col = filter ((=) col >> not)

  let withoutci (ColumnIndex c) = filteri (fst >> (=) c >> not)

  let appendc col = withColsF (Seq.append col)

  // Infrastructure

  let force = withRowsF Seq.toArray >> withColsF Seq.toArray

  let forceRec = force // TODO
