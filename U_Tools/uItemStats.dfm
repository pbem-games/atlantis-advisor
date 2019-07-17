object ItemStatsForm: TItemStatsForm
  Left = 206
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Item Stats'
  ClientHeight = 408
  ClientWidth = 291
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Grid: TPowerGrid
    Left = 0
    Top = 0
    Width = 291
    Height = 408
    Align = alClient
    ColCount = 3
    DefaultRowColor = clBlack
    Editing = False
    FixedRows = 1
    Images = ResForm.IconList
    ImageCol = 0
    Options = [pgoLines, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
    RowCount = 1
    Sorted = True
    SortBy = 0
    StickySelect = False
    TopRow = 1
    OnDrawIcon = GridDrawIcon
    ColWidths = (
      147
      64
      64)
  end
end
