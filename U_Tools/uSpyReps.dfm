object SpyRepForm: TSpyRepForm
  Left = 201
  Top = 103
  Width = 577
  Height = 421
  Caption = 'Spy Reports'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object gChanges: TPowerGrid
    Left = 0
    Top = 29
    Width = 569
    Height = 365
    Align = alClient
    Color = clBlack
    ColCount = 9
    DefaultRowColor = clBlack
    Editing = False
    FixedRows = 1
    ImageCol = 0
    RowCount = 1
    Sorted = True
    SortBy = 0
    StickySelect = False
    TopRow = 1
    OnDblClick = gChangesDblClick
    OnDrawCell = gChangesDrawCell
    ColWidths = (
      21
      108
      43
      111
      36
      89
      132
      32
      64)
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 569
    Height = 29
    Caption = 'ToolBar1'
    EdgeBorders = []
    Images = ResForm.BtnImages
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object ToolButton2: TToolButton
      Left = 0
      Top = 2
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 58
      Style = tbsSeparator
    end
    object btnAppeared: TToolButton
      Left = 8
      Top = 2
      Hint = 'Appeared'
      AllowAllUp = True
      Caption = 'btnAppeared'
      ImageIndex = 54
      Style = tbsCheck
      OnClick = FilterChange
    end
    object btnDisappeared: TToolButton
      Left = 31
      Top = 2
      Hint = 'Disappeared'
      AllowAllUp = True
      Caption = 'btnDisappeared'
      ImageIndex = 55
      Style = tbsCheck
      OnClick = FilterChange
    end
    object btnChanges: TToolButton
      Left = 54
      Top = 2
      Hint = 'Changed'
      AllowAllUp = True
      Caption = 'btnChanges'
      ImageIndex = 0
      Style = tbsCheck
      OnClick = FilterChange
    end
    object btnGuard: TToolButton
      Left = 77
      Top = 2
      Hint = 'Guards Events'
      AllowAllUp = True
      Caption = 'btnGuard'
      ImageIndex = 28
      Style = tbsCheck
      OnClick = FilterChange
    end
    object btnItems: TToolButton
      Left = 100
      Top = 2
      Hint = 'Inventory changes'
      AllowAllUp = True
      Caption = 'btnItems'
      ImageIndex = 43
      Style = tbsCheck
      OnClick = FilterChange
    end
    object btnEconomy: TToolButton
      Left = 123
      Top = 2
      Hint = 'Region economy'
      Caption = 'btnEconomy'
      ImageIndex = 26
      Style = tbsCheck
      OnClick = FilterChange
    end
    object ToolButton6: TToolButton
      Left = 146
      Top = 2
      Width = 8
      Caption = 'ToolButton6'
      ImageIndex = 44
      Style = tbsSeparator
    end
    object btnRegion: TToolButton
      Left = 154
      Top = 2
      Hint = 'Selected Region'
      Caption = 'btnRegion'
      ImageIndex = 17
      Style = tbsCheck
      OnClick = FilterChange
    end
    object btnUnit: TToolButton
      Left = 177
      Top = 2
      Hint = 'Selected Unit'
      Caption = 'btnUnit'
      ImageIndex = 0
      Style = tbsCheck
      OnClick = FilterChange
    end
    object btnAllTurns: TToolButton
      Left = 200
      Top = 2
      Hint = 'All loaded turns'
      Caption = 'btnAllTurns'
      ImageIndex = 56
      Style = tbsCheck
      OnClick = FilterChange
    end
    object ToolButton1: TToolButton
      Left = 223
      Top = 2
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 57
      Style = tbsSeparator
    end
    object cmFactions: TComboBox
      Left = 231
      Top = 2
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnChange = FilterChange
    end
  end
end
