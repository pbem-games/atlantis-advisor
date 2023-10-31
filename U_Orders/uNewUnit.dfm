object NewUnitForm: TNewUnitForm
  Left = 967
  Top = 303
  BorderStyle = bsDialog
  Caption = 'Form'
  ClientHeight = 473
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    361
    473)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 16
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label8: TLabel
    Left = 268
    Top = 312
    Width = 47
    Height = 13
    Caption = 'Consume:'
  end
  object Label9: TLabel
    Left = 268
    Top = 352
    Width = 37
    Height = 13
    Caption = 'Reveal:'
  end
  object Label10: TLabel
    Left = 268
    Top = 392
    Width = 31
    Height = 13
    Caption = 'Spoils:'
  end
  object Label4: TLabel
    Left = 268
    Top = 224
    Width = 42
    Height = 13
    Caption = 'Delayed:'
  end
  object Label5: TLabel
    Left = 84
    Top = 448
    Width = 31
    Height = 13
    Caption = 'copies'
  end
  object Label7: TLabel
    Left = 12
    Top = 448
    Width = 23
    Height = 13
    Caption = 'Form'
  end
  object Button1: TButton
    Left = 193
    Top = 445
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 14
  end
  object Button2: TButton
    Left = 277
    Top = 445
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 15
  end
  object eName: TEdit
    Left = 52
    Top = 12
    Width = 265
    Height = 21
    TabOrder = 0
    Text = 'Unit'
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 44
    Width = 253
    Height = 57
    Caption = 'Men'
    TabOrder = 1
    object cbBuy: TCheckBox
      Left = 16
      Top = 22
      Width = 45
      Height = 17
      Caption = 'Buy'
      TabOrder = 0
      OnClick = cbBuyClick
    end
    object cmForSale: TComboBox
      Left = 116
      Top = 20
      Width = 125
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 2
      OnChange = cmForSaleChange
      OnDrawItem = cmItemsDrawItem
    end
    object eForSale: TIntEdit
      Left = 64
      Top = 20
      Width = 49
      Height = 22
      MaxValue = 9999
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = PaymentChange
    end
  end
  object GroupBox2: TGroupBox
    Left = 4
    Top = 220
    Width = 253
    Height = 213
    Caption = 'Equipment'
    TabOrder = 3
    object Image2: TImage
      Left = 176
      Top = 187
      Width = 16
      Height = 16
      Picture.Data = {
        07544269746D617036040000424D360400000000000036000000280000001000
        0000100000000100200000000000000400000000000000000000000000000000
        0000FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0FF00FFC0000000C00000000000000000FF00FF00FF00FFC0FF00FFC0FF00
        FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0000000C000FFFF0000FFFFFF00FFFFFF000000FFFF00FF00FF00FFC0FF00
        FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0000000C000FFFF0000FFFFFF00FFFFFF000000FFFF00FF00FF00FFC0FF00
        FFC0FF00FFC0FF00FFC0FF00FFC0000000C00000000000000000FF00FF00FF00
        FFC0FF00FFC0000000C00000000000000000FF00FF00000000C0000000000000
        0000FF00FF00FF00FFC0000000C000FFFF0000FFFFFF00FFFFFF000000FFFF00
        FF00FF00FFC0FF00FFC0FF00FFC0FF00FFC0000000C0FFFFFF00FFFFFFFFFFFF
        FFFF000000FFFF00FF00000000C000000000000000000000000000000000FF00
        FF00FF00FFC0FF00FFC0FF00FFC0000000C000000000FFFFFF00FFFFFFFFFFFF
        FFFF000000FFFF00FF00000000C0FFFFFF00FFFFFFFFFFFFFFFF000000FFFF00
        FF00FF00FFC0FF00FFC0000000C000FFFF0000FFFFFF000000FF000000000000
        0000FF00FF00FF00FFC0000000C000000000000000000000000000000000FF00
        FF00FF00FFC0FF00FFC0000000C000FFFF0000FFFFFF00FFFFFF000000FFFF00
        FF00FF00FFC0FF00FFC0000000C000FFFF0000FFFFFF00FFFFFF000000FFFF00
        FF00FF00FFC0FF00FFC0FF00FFC0000000C00000000000000000FF00FF00FF00
        FFC0FF00FFC0FF00FFC0000000C000000000000000000000000000000000FF00
        FF00FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0000000C0000000000000
        0000FF00FF00FF00FFC0000000C000FFFF0000FFFFFF00FFFFFF000000FFFF00
        FF00FF00FFC0FF00FFC0FF00FFC0FF00FFC0000000C000FFFF0000FFFFFF00FF
        FFFF000000FFFF00FF00FF00FFC0000000C00000000000000000FF00FF00FF00
        FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0000000C000000000000000000000
        000000000000FF00FF00FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0000000C0FFFFFF00FFFFFFFFFFFF
        FFFF000000FFFF00FF00FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0000000C0000000000000
        0000FF00FF00FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0FF00FF00FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0}
      Transparent = True
    end
    object lSilver: TLabel
      Left = 196
      Top = 188
      Width = 6
      Height = 13
      Caption = '0'
    end
    object eClaim: TIntEdit
      Left = 68
      Top = 184
      Width = 69
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = SilverChange
    end
    object cbClaim: TCheckBox
      Left = 16
      Top = 186
      Width = 49
      Height = 17
      Caption = 'Claim'
      TabOrder = 0
      OnClick = cbClaimClick
    end
    object gFormer: TPowerGrid
      Left = 16
      Top = 20
      Width = 221
      Height = 157
      ColCount = 3
      DefaultRowColor = clBlack
      Editing = False
      FixedRows = 1
      Images = ResForm.IconList
      ImageCol = 2
      Options = [pgoLines, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
      RowCount = 1
      Sorted = True
      SortBy = 2
      StickySelect = False
      TopRow = 1
      OnDrawIcon = gFormerDrawIcon
      OnSetEditText = gFormerSetEditText
      ColWidths = (
        48
        47
        122)
    end
  end
  object GroupBox3: TGroupBox
    Left = 4
    Top = 104
    Width = 253
    Height = 113
    Caption = 'Payment Required'
    TabOrder = 2
    object Label3: TLabel
      Left = 176
      Top = 80
      Width = 34
      Height = 13
      Caption = 'months'
    end
    object lNeeds: TLabel
      Left = 196
      Top = 16
      Width = 49
      Height = 13
      AutoSize = False
      Caption = '0'
    end
    object Image1: TImage
      Left = 176
      Top = 16
      Width = 16
      Height = 16
      Picture.Data = {
        07544269746D617036040000424D360400000000000036000000280000001000
        0000100000000100200000000000000400000000000000000000000000000000
        0000FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0FF00FFC0000000C00000000000000000FF00FF00FF00FFC0FF00FFC0FF00
        FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0000000C000FFFF0000FFFFFF00FFFFFF000000FFFF00FF00FF00FFC0FF00
        FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0000000C000FFFF0000FFFFFF00FFFFFF000000FFFF00FF00FF00FFC0FF00
        FFC0FF00FFC0FF00FFC0FF00FFC0000000C00000000000000000FF00FF00FF00
        FFC0FF00FFC0000000C00000000000000000FF00FF00000000C0000000000000
        0000FF00FF00FF00FFC0000000C000FFFF0000FFFFFF00FFFFFF000000FFFF00
        FF00FF00FFC0FF00FFC0FF00FFC0FF00FFC0000000C0FFFFFF00FFFFFFFFFFFF
        FFFF000000FFFF00FF00000000C000000000000000000000000000000000FF00
        FF00FF00FFC0FF00FFC0FF00FFC0000000C000000000FFFFFF00FFFFFFFFFFFF
        FFFF000000FFFF00FF00000000C0FFFFFF00FFFFFFFFFFFFFFFF000000FFFF00
        FF00FF00FFC0FF00FFC0000000C000FFFF0000FFFFFF000000FF000000000000
        0000FF00FF00FF00FFC0000000C000000000000000000000000000000000FF00
        FF00FF00FFC0FF00FFC0000000C000FFFF0000FFFFFF00FFFFFF000000FFFF00
        FF00FF00FFC0FF00FFC0000000C000FFFF0000FFFFFF00FFFFFF000000FFFF00
        FF00FF00FFC0FF00FFC0FF00FFC0000000C00000000000000000FF00FF00FF00
        FFC0FF00FFC0FF00FFC0000000C000000000000000000000000000000000FF00
        FF00FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0000000C0000000000000
        0000FF00FF00FF00FFC0000000C000FFFF0000FFFFFF00FFFFFF000000FFFF00
        FF00FF00FFC0FF00FFC0FF00FFC0FF00FFC0000000C000FFFF0000FFFFFF00FF
        FFFF000000FFFF00FF00FF00FFC0000000C00000000000000000FF00FF00FF00
        FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0000000C000000000000000000000
        000000000000FF00FF00FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0000000C0FFFFFF00FFFFFFFFFFFF
        FFFF000000FFFF00FF00FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0000000C0000000000000
        0000FF00FF00FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0FF00FF00FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00FFC0FF00
        FFC0}
      Transparent = True
    end
    object btnRepStudy: TSpeedButton
      Left = 30
      Top = 50
      Width = 16
      Height = 16
      AllowAllUp = True
      GroupIndex = -1
      Down = True
      Flat = True
      Glyph.Data = {
        76010000424D760100000000000036000000280000000A0000000A0000000100
        18000000000040010000C40E0000C40E00000000000000000000FFFFFFFFFFFF
        FFFFFF000000000000000000000000000000FFFFFFFFFFFF0000FFFFFF000000
        000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFF000000
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000FFFFFF
        FFFFFFFFFFFF000000000000FFFFFF0000000000000000000000000000FFFFFF
        FFFFFF000000FFFFFFFFFFFF000000FFFFFFFFFFFF0000000000000000FFFFFF
        FFFFFF000000FFFFFFFFFFFF000000FFFFFFFFFFFF0000000000000000FFFFFF
        FFFFFFFFFFFF000000000000000000FFFFFFFFFFFF0000000000FFFFFF000000
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFF0000FFFFFF000000
        000000FFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFF0000FFFFFFFFFFFF
        FFFFFF000000000000000000000000FFFFFFFFFFFFFFFFFF0000}
      Margin = 2
    end
    object Label6: TLabel
      Left = 48
      Top = 52
      Width = 27
      Height = 13
      Caption = 'Study'
    end
    object cbMainentance: TCheckBox
      Left = 16
      Top = 78
      Width = 105
      Height = 17
      Caption = 'Maintenance for'
      TabOrder = 4
      OnClick = PaymentChange
    end
    object cbRecruit: TCheckBox
      Left = 16
      Top = 24
      Width = 101
      Height = 17
      Caption = 'Recruit fee'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = PaymentChange
    end
    object eMonths: TIntEdit
      Left = 124
      Top = 76
      Width = 41
      Height = 22
      MaxValue = 9999
      MinValue = 1
      TabOrder = 5
      Value = 1
      OnChange = PaymentChange
    end
    object cbStudy: TCheckBox
      Left = 16
      Top = 50
      Width = 13
      Height = 17
      TabOrder = 1
      OnClick = PaymentChange
    end
    object cmSkill: TComboBox
      Left = 84
      Top = 48
      Width = 121
      Height = 22
      Style = csOwnerDrawFixed
      DropDownCount = 12
      ItemHeight = 16
      TabOrder = 2
      OnChange = PaymentChange
      OnDrawItem = cmSkillDrawItem
    end
    object eSkillLv: TIntEdit
      Left = 208
      Top = 48
      Width = 33
      Height = 22
      MaxValue = 5
      MinValue = 1
      TabOrder = 3
      Value = 1
      OnChange = eSkillLvChange
    end
  end
  object cbOnguard: TCheckBox
    Left = 268
    Top = 52
    Width = 69
    Height = 17
    Caption = 'On guard'
    TabOrder = 4
    OnClick = cbOnguardClick
  end
  object cbAutotax: TCheckBox
    Left = 268
    Top = 72
    Width = 69
    Height = 17
    Caption = 'Autotax'
    TabOrder = 5
    OnClick = cbAutotaxClick
  end
  object cbAvoid: TCheckBox
    Left = 268
    Top = 92
    Width = 69
    Height = 17
    Caption = 'Avoid'
    TabOrder = 6
    OnClick = cbAvoidClick
  end
  object cbBehind: TCheckBox
    Left = 268
    Top = 112
    Width = 69
    Height = 17
    Caption = 'Behind'
    TabOrder = 7
  end
  object cbHold: TCheckBox
    Left = 268
    Top = 132
    Width = 69
    Height = 17
    Caption = 'Hold'
    TabOrder = 8
  end
  object cbNoaid: TCheckBox
    Left = 268
    Top = 152
    Width = 69
    Height = 17
    Caption = 'No aid'
    TabOrder = 9
  end
  object cmConsume: TComboBox
    Left = 272
    Top = 328
    Width = 77
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 11
    Items.Strings = (
      'silver'
      'unit'
      'faction')
  end
  object cmReveal: TComboBox
    Left = 272
    Top = 368
    Width = 77
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 12
    Items.Strings = (
      'hide'
      'unit'
      'faction')
  end
  object cmSpoils: TComboBox
    Left = 272
    Top = 408
    Width = 77
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 13
    Items.Strings = (
      'all'
      'none'
      'walk'
      'ride'
      'fly')
  end
  object cbNocross: TCheckBox
    Left = 268
    Top = 172
    Width = 69
    Height = 17
    Caption = 'No cross'
    TabOrder = 10
  end
  object cbDelayTax: TCheckBox
    Left = 268
    Top = 260
    Width = 73
    Height = 17
    Caption = 'Autotax'
    TabOrder = 16
    OnClick = cbDelayTaxClick
  end
  object cbDelayGuard: TCheckBox
    Left = 268
    Top = 240
    Width = 73
    Height = 17
    Caption = 'On guard'
    TabOrder = 17
    OnClick = cbDelayGuardClick
  end
  object eCopy: TIntEdit
    Left = 40
    Top = 444
    Width = 37
    Height = 22
    MaxValue = 999
    MinValue = 1
    TabOrder = 18
    Value = 1
  end
  object ToolBar1: TToolBar
    Left = 324
    Top = 10
    Width = 37
    Height = 29
    Align = alNone
    Caption = 'ToolBar1'
    EdgeBorders = []
    Images = ResForm.BtnImages
    TabOrder = 19
    object btnTemplate: TToolButton
      Left = 0
      Top = 2
      Hint = 'Save as Template'
      Caption = 'btnTemplate'
      ImageIndex = 61
      ParentShowHint = False
      ShowHint = True
      OnClick = btnTemplateClick
    end
  end
  object cbShare: TCheckBox
    Left = 268
    Top = 192
    Width = 69
    Height = 17
    Caption = 'Share'
    TabOrder = 20
  end
end
