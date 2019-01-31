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

  let sheet (SheetIndex s) wb = wb.Sheets @? s

  let row s r = sheet s >=> Sheet.row r

  let column s c = sheet s >=> Sheet.column c

  let cell s r c = sheet s >=> Sheet.row r >=> Row.cell c

  // Mutators

  let inline withSheetsF f wb =
    { wb with Sheets = f wb.Sheets }

  let inline withSheets sheets wb =
    { wb with Sheets = sheets }

  // Functions

  let map f = withSheetsF (Seq.map f)

  let mapi f = withSheetsF (Seq.indexed >> f >> Seq.map snd)

  let filter f = withSheetsF (Seq.filter f)

  let filteri f = withSheetsF (Seq.indexed >> Seq.filter f >> Seq.map snd)

  let without sheet = filter ((=) sheet >> not)

  let withouti (SheetIndex s) = filteri (fst >> (=) s >> not)

  let append sheet = withSheetsF (Seq.append sheet)

  let cellStyleChain s r (CellIndex c) wb =
    option {
      let! sheet = wb |> sheet s
      let! row = sheet |> Sheet.row r
      let! cell = row |> Row.cell (CellIndex c)
      let col = sheet |> Sheet.column (ColumnIndex c)

      return (fun getter -> seq {
        yield getter cell.Style
        if col.IsSome then yield getter col.Value.CellStyle
        yield getter row.CellStyle
        yield getter sheet.CellStyle
        yield getter wb.CellStyle
      })
    }

  let rowStyleChain s r wb =
    option {
      let! sheet = wb |> sheet s
      let! row = sheet |> Sheet.row r

      return (fun getter -> seq {
        yield getter row.RowStyle
        yield getter sheet.RowStyle
        yield getter wb.RowStyle
      })
    }

  let columnStyleChain s c wb =
    option {
      let! sheet = wb |> sheet s
      let! col = sheet |> Sheet.column c

      return (fun getter -> seq {
        yield getter col.ColumnStyle
        yield getter sheet.ColumnStyle
        yield getter wb.ColumnStyle
      })
    }

  let force = withSheetsF Seq.toArray

  let forceRec = map Sheet.forceRec >> force
