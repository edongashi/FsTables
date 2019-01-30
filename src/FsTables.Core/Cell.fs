namespace FsTables.Core

type CellStyle =
  { Content : Value }

type Cell =
  { Style : CellStyle
    Data : AttachedData }
