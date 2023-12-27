object TradeStructForm: TTradeStructForm
  Left = 201
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Production Chart'
  ClientHeight = 310
  ClientWidth = 425
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
  object Label1: TLabel
    Left = 20
    Top = 12
    Width = 51
    Height = 13
    Caption = 'Products'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 208
    Top = 12
    Width = 108
    Height = 13
    Caption = 'Output progression'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 24
    Top = 283
    Width = 107
    Height = 13
    Caption = 'Show workers at level:'
  end
  object gProducts: TPowerGrid
    Left = 12
    Top = 32
    Width = 177
    Height = 241
    ColCount = 2
    DefaultRowColor = clBlack
    Editing = False
    FixedRows = 0
    ImageCol = 0
    Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
    RowCount = 0
    Sorted = False
    SortBy = 0
    StickySelect = False
    TopRow = 0
    OnDrawCell = gProductsDrawCell
    OnSelectCell = gProductsSelectCell
    ColWidths = (
      48
      125)
  end
  object gStructs: TPowerGrid
    Left = 196
    Top = 32
    Width = 217
    Height = 241
    ColCount = 4
    DefaultRowColor = clBlack
    Editing = False
    FixedRows = 1
    ImageCol = 0
    Options = [pgoLines, pgoRowSelect, pgoStretchLastCol]
    RowCount = 1
    Sorted = False
    SortBy = 0
    StickySelect = False
    TopRow = 1
    OnDrawCell = gStructsDrawCell
    ColWidths = (
      29
      109
      41
      34)
  end
  object Button1: TButton
    Left = 340
    Top = 284
    Width = 75
    Height = 21
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object eLevel: TSpinEdit
    Left = 136
    Top = 280
    Width = 37
    Height = 22
    MaxValue = 5
    MinValue = 1
    TabOrder = 3
    Value = 3
    OnChange = eLevelChange
  end
end
