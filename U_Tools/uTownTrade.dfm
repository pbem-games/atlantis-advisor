object TownTradeForm: TTownTradeForm
  Left = 201
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Town Trade'
  ClientHeight = 331
  ClientWidth = 381
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
  DesignSize = (
    381
    331)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 12
    Width = 45
    Height = 13
    Caption = 'Wanted'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 16
    Top = 156
    Width = 48
    Height = 13
    Caption = 'For Sale'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 297
    Top = 303
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 0
  end
  object gWanted: TPowerGrid
    Left = 8
    Top = 28
    Width = 365
    Height = 121
    ColCount = 4
    DefaultRowColor = clBlack
    Editing = False
    FixedRows = 0
    ImageCol = 0
    Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol, pgoMultilineCells]
    RowCount = 0
    Sorted = False
    SortBy = 1
    ShowHint = True
    StickySelect = False
    TopRow = 0
    OnDrawCell = DrawCell
    ColWidths = (
      35
      96
      39
      191)
  end
  object gForSale: TPowerGrid
    Tag = 1
    Left = 8
    Top = 172
    Width = 365
    Height = 121
    ColCount = 4
    DefaultRowColor = clBlack
    Editing = False
    FixedRows = 0
    ImageCol = 0
    Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol, pgoMultilineCells]
    RowCount = 0
    Sorted = False
    SortBy = 1
    ShowHint = True
    StickySelect = False
    TopRow = 0
    OnDrawCell = DrawCell
    ColWidths = (
      35
      96
      39
      191)
  end
  object cmTowns: TComboBox
    Left = 8
    Top = 303
    Width = 145
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemHeight = 13
    Sorted = True
    TabOrder = 3
  end
  object bAdd: TButton
    Left = 160
    Top = 303
    Width = 57
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Add'
    TabOrder = 4
    OnClick = bAddClick
  end
end
