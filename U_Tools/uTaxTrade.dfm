object TaxTradeForm: TTaxTradeForm
  Left = 201
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Tax / Trade Regions'
  ClientHeight = 381
  ClientWidth = 311
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
  object PageControl1: TPageControl
    Left = 4
    Top = 4
    Width = 301
    Height = 345
    ActivePage = TabSheet1
    TabIndex = 0
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Tax Regions'
      object lTax: TLabel
        Left = 12
        Top = 8
        Width = 108
        Height = 13
        Caption = 'Used: 12 of 21 (War 2)'
      end
      object gTax: TPowerGrid
        Left = 0
        Top = 32
        Width = 289
        Height = 281
        ColCount = 4
        DefaultRowColor = clBlack
        Editing = False
        FixedRows = 1
        ImageCol = 0
        Options = [pgoLines, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
        RowCount = 1
        Sorted = True
        SortBy = 0
        StickySelect = False
        TopRow = 1
        OnDblClick = GridDblClick
        OnDrawCell = gTaxDrawCell
        ColWidths = (
          157
          39
          40
          49)
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Trade Regions'
      ImageIndex = 1
      object lTrade: TLabel
        Left = 12
        Top = 8
        Width = 116
        Height = 13
        Caption = 'Used: 12 of 21 (Trade 2)'
      end
      object gTrade: TPowerGrid
        Left = 0
        Top = 32
        Width = 289
        Height = 281
        ColCount = 1
        DefaultRowColor = clBlack
        Editing = False
        FixedRows = 1
        ImageCol = 0
        Options = [pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
        RowCount = 1
        Sorted = True
        SortBy = 0
        StickySelect = False
        TopRow = 1
        OnDblClick = GridDblClick
        ColWidths = (
          285)
      end
    end
    object tsFishing: TTabSheet
      Caption = 'Fishing'
      ImageIndex = 2
      object lFishing: TLabel
        Left = 12
        Top = 8
        Width = 116
        Height = 13
        Caption = 'Used: 12 of 21 (Trade 2)'
      end
      object gFishing: TPowerGrid
        Left = 0
        Top = 32
        Width = 289
        Height = 281
        ColCount = 1
        DefaultRowColor = clBlack
        Editing = False
        FixedRows = 1
        ImageCol = 0
        Options = [pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
        RowCount = 1
        Sorted = True
        SortBy = 0
        StickySelect = False
        TopRow = 1
        OnDblClick = GridDblClick
        ColWidths = (
          285)
      end
    end
    object tsRoadBuild: TTabSheet
      Caption = 'Road Build'
      ImageIndex = 3
      object lRoadBuild: TLabel
        Left = 12
        Top = 8
        Width = 116
        Height = 13
        Caption = 'Used: 12 of 21 (Trade 2)'
      end
      object gRoadBuild: TPowerGrid
        Left = 0
        Top = 32
        Width = 289
        Height = 281
        ColCount = 1
        DefaultRowColor = clBlack
        Editing = False
        FixedRows = 1
        ImageCol = 0
        Options = [pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
        RowCount = 1
        Sorted = True
        SortBy = 0
        StickySelect = False
        TopRow = 1
        OnDblClick = GridDblClick
        ColWidths = (
          285)
      end
    end
  end
  object Button1: TButton
    Left = 224
    Top = 356
    Width = 75
    Height = 21
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
