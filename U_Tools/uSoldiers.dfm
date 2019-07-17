object SoldiersForm: TSoldiersForm
  Left = 201
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Soldiers'
  ClientHeight = 430
  ClientWidth = 441
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
    Width = 441
    Height = 430
    Align = alClient
    ColCount = 1
    DefaultRowColor = clBlack
    DefaultRowHeight = 53
    Editing = False
    FixedRows = 0
    ImageCol = 0
    Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoNoSelect, pgoStretchLastCol]
    RowCount = 0
    Sorted = False
    SortBy = 0
    StickySelect = False
    TopRow = 0
    OnDrawCell = GridDrawCell
    ColWidths = (
      437)
  end
end
