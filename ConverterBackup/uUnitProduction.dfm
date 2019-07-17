object UnitProductionForm: TUnitProductionForm
  Left = 201
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Unit Production'
  ClientHeight = 384
  ClientWidth = 246
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Grid: TPowerGrid
    Left = 0
    Top = 0
    Width = 246
    Height = 384
    Align = alClient
    ColCount = 4
    DefaultRowColor = clBlack
    Editing = False
    FixedRows = 1
    ImageCol = 0
    Options = [pgoLines, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
    RowCount = 1
    Sorted = False
    SortBy = 0
    StickySelect = False
    TopRow = 1
    OnDrawCell = GridDrawCell
    ColWidths = (
      119
      36
      36
      51)
  end
end
