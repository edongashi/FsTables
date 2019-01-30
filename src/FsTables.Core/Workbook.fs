namespace FsTables.Core

type WorkbookName = WorkbookName of string

type Workbook =
  { Name : WorkbookName
    Sheets : seq<Sheet>
    CellStyle : CellStyle
    RowStyle : RowStyle
    ColumnStyle : ColumnStyle
    Data : AttachedData }

type SheetIndex = SheetIndex of int

module Workbook =

  // Accessors

  let sheet (SheetIndex s) workbook = workbook.Sheets @? s

  let row s r = sheet s >=> Sheet.row r

  let column s c = sheet s >=> Sheet.column c

  let cell s r c = row s r >=> Row.cell c

  // Mutators

  let inline withSheetsF f workbook =
    { workbook with Sheets = f workbook.Sheets }

  let inline withSheets sheets workbook =
    { workbook with Sheets = sheets }

  // Functions

  let map f = withSheetsF (Seq.map f)

  let mapi f = withSheetsF (Seq.indexed >> f >> Seq.map snd)

  let filter f = withSheetsF (Seq.filter f)

  let filteri f = withSheetsF (Seq.indexed >> Seq.filter f >> Seq.map snd)

  let without sheet = filter ((=) sheet >> not)

  let withouti (SheetIndex s) = filteri (fst >> (=) s >> not)

  let append sheet = withSheetsF (Seq.append sheet)

  // Infrastructure

  let force = withSheetsF Seq.toArray

  let forceRec = withSheetsF (Seq.map Sheet.forceRec >> Seq.toArray)
