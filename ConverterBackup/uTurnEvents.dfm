object TurnEventsForm: TTurnEventsForm
  Left = 201
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Turn Events'
  ClientHeight = 441
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Grid: TPowerGrid
    Left = 0
    Top = 0
    Width = 361
    Height = 441
    Align = alClient
    ColCount = 2
    DefaultRowColor = clBlack
    DefaultRowHeight = 23
    Editing = False
    FixedRows = 0
    ImageCol = 0
    Options = [pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoNoSelect, pgoStretchLastCol, pgoMultilineCells]
    RowCount = 0
    Sorted = False
    SortBy = 0
    StickySelect = False
    TopRow = 0
    OnDrawCell = GridDrawCell
    OnMouseDown = GridMouseDown
    OnMouseMove = GridMouseMove
    ColWidths = (
      22
      335)
  end
end
