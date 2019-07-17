object SkillEditForm: TSkillEditForm
  Left = 206
  Top = 101
  HelpContext = 11
  BorderStyle = bsDialog
  Caption = 'Skill Editor'
  ClientHeight = 428
  ClientWidth = 548
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 456
    Top = 404
    Width = 75
    Height = 21
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object SkillTree: TTreeView
    Left = 4
    Top = 32
    Width = 201
    Height = 389
    DragMode = dmAutomatic
    Images = ResForm.IconList
    Indent = 19
    ReadOnly = True
    TabOrder = 1
    OnChange = SkillTreeChange
    OnExpanding = SkillTreeExpanding
  end
  object GroupBox1: TGroupBox
    Left = 212
    Top = 4
    Width = 329
    Height = 393
    Caption = 'Skill'
    TabOrder = 2
    object Label1: TLabel
      Left = 12
      Top = 24
      Width = 28
      Height = 13
      Caption = 'Code:'
    end
    object CodeLabel: TLabel
      Left = 48
      Top = 24
      Width = 37
      Height = 13
      Caption = 'WEAP'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 12
      Top = 88
      Width = 48
      Height = 13
      Caption = 'Based on:'
    end
    object Label4: TLabel
      Left = 104
      Top = 24
      Width = 24
      Height = 13
      Caption = 'Cost:'
    end
    object Label2: TLabel
      Left = 208
      Top = 24
      Width = 53
      Height = 13
      Caption = 'Specialists:'
    end
    object Label5: TLabel
      Left = 208
      Top = 160
      Width = 30
      Height = 13
      Caption = 'Ability:'
    end
    object mDesc: TRichEdit
      Left = 8
      Top = 208
      Width = 313
      Height = 177
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object cbCombat: TCheckBox
      Left = 116
      Top = 72
      Width = 85
      Height = 17
      Caption = 'Combat spell'
      TabOrder = 1
      OnClick = DataChange
    end
    object cbCast: TCheckBox
      Left = 116
      Top = 52
      Width = 85
      Height = 17
      Caption = 'Can be cast'
      TabOrder = 2
      OnClick = DataChange
    end
    object cmType: TComboBox
      Left = 8
      Top = 56
      Width = 93
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 3
      OnChange = DataChange
      OnDrawItem = cmTypeDrawItem
      Items.Strings = (
        'Simple'
        'Magic'
        'Foundation')
    end
    object cmBased: TComboBox
      Left = 8
      Top = 176
      Width = 141
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      ParentShowHint = False
      ShowHint = True
      Sorted = True
      TabOrder = 4
      OnDrawItem = cmBasedDrawItem
    end
    object eCost: TIntEdit
      Left = 136
      Top = 20
      Width = 57
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 5
      Value = 0
      OnChange = DataChange
    end
    object ToolBar2: TToolBar
      Left = 154
      Top = 179
      Width = 47
      Height = 18
      Align = alNone
      ButtonHeight = 18
      ButtonWidth = 19
      Caption = 'ToolBar2'
      EdgeBorders = []
      Flat = True
      Images = ResForm.SmallBtnList
      TabOrder = 6
      object btnAddBased: TToolButton
        Left = 0
        Top = 0
        Caption = 'btnAddBased'
        ImageIndex = 0
        OnClick = btnAddBasedClick
      end
      object btnDelBased: TToolButton
        Left = 19
        Top = 0
        Caption = 'btnDelBased'
        ImageIndex = 1
        OnClick = btnDelBasedClick
      end
    end
    object gSpecs: TPowerGrid
      Left = 204
      Top = 40
      Width = 117
      Height = 113
      ColCount = 1
      DefaultRowColor = clBlack
      Editing = False
      FixedRows = 0
      ImageCol = 0
      Options = [pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
      RowCount = 0
      Sorted = False
      SortBy = 0
      StickySelect = False
      TopRow = 0
      OnDrawCell = gSpecsDrawCell
      ColWidths = (
        113)
    end
    object cmAbility: TComboBox
      Left = 204
      Top = 176
      Width = 117
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      Sorted = True
      TabOrder = 8
      OnChange = DataChange
      OnDrawItem = cmAbilityDrawItem
    end
    object gBased: TPowerGrid
      Left = 8
      Top = 104
      Width = 185
      Height = 69
      ColCount = 2
      DefaultRowColor = clBlack
      Editing = False
      FixedRows = 0
      Images = ResForm.IconList
      ImageCol = 0
      Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
      RowCount = 0
      Sorted = False
      SortBy = 0
      StickySelect = False
      TopRow = 0
      OnSetEditText = gBasedSetEditText
      ColWidths = (
        132
        49)
    end
  end
  object ToolBar1: TToolBar
    Left = 12
    Top = 4
    Width = 177
    Height = 29
    Align = alNone
    Caption = 'ToolBar1'
    EdgeBorders = []
    Images = ResForm.BtnImages
    TabOrder = 3
    object btnSort: TToolButton
      Left = 0
      Top = 2
      Hint = 'Sort skills'
      Caption = 'btnSort'
      ImageIndex = 34
      ParentShowHint = False
      ShowHint = True
      OnClick = btnSortClick
    end
    object ToolButton1: TToolButton
      Left = 23
      Top = 2
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 35
      Style = tbsSeparator
    end
    object btnRequest: TToolButton
      Left = 31
      Top = 2
      Hint = 'Request Info'
      Caption = 'btnRequest'
      ImageIndex = 69
      OnClick = btnRequestClick
    end
    object btnRequestAll: TToolButton
      Left = 54
      Top = 2
      Hint = 'Request All'
      Caption = 'btnRequestAll'
      ImageIndex = 70
      OnClick = btnRequestAllClick
    end
  end
end
