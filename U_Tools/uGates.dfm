object GatesForm: TGatesForm
  Left = 206
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Gates'
  ClientHeight = 392
  ClientWidth = 296
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
    Width = 296
    Height = 392
    Align = alClient
    ColCount = 2
    DefaultRowColor = clBlack
    Editing = False
    FixedRows = 1
    ImageCol = 0
    Options = [pgoLines, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
    RowCount = 2
    Sorted = True
    SortBy = 0
    StickySelect = False
    TopRow = 1
    OnDblClick = GridDblClick
    ColWidths = (
      45
      247)
  end
end
