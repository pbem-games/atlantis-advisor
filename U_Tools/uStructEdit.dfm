object StructEditForm: TStructEditForm
  Left = 745
  Top = 284
  HelpContext = 12
  BorderStyle = bsDialog
  Caption = 'Object Editor'
  ClientHeight = 418
  ClientWidth = 545
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    545
    418)
  PixelsPerInch = 96
  TextHeight = 13
  object CloseBtn: TButton
    Left = 464
    Top = 392
    Width = 75
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Grid: TPowerGrid
    Left = 4
    Top = 32
    Width = 193
    Height = 381
    Anchors = [akLeft, akTop, akBottom]
    ColCount = 4
    DefaultRowColor = clBlack
    Editing = False
    FixedRows = 1
    Images = ResForm.IconList
    ImageCol = 1
    Options = [pgoLines, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
    RowCount = 1
    Sorted = True
    SortBy = 0
    StickySelect = True
    TopRow = 1
    OnDragOver = GridDragOver
    OnEndDrag = GridEndDrag
    OnMouseDown = GridMouseDown
    OnSelectCell = GridSelectCell
    ColWidths = (
      27
      19
      95
      48)
  end
  object ToolBar1: TToolBar
    Left = 4
    Top = 4
    Width = 177
    Height = 25
    Align = alNone
    Caption = 'ToolBar1'
    EdgeBorders = []
    Images = ResForm.BtnImages
    TabOrder = 2
    object btnNoFilter: TToolButton
      Left = 0
      Top = 2
      Hint = 'No filter'
      Caption = 'btnNoFilter'
      Down = True
      Grouped = True
      ImageIndex = 9
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
      OnClick = ToolButtonClick
    end
    object btnFilter: TToolButton
      Tag = 1
      Left = 23
      Top = 2
      Hint = 'Filter by current item'
      Caption = 'btnFilter'
      Grouped = True
      ImageIndex = 25
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
      OnClick = ToolButtonClick
    end
    object ToolButton1: TToolButton
      Left = 46
      Top = 2
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 26
      Style = tbsSeparator
    end
    object btnArrange: TToolButton
      Left = 54
      Top = 2
      Hint = 'Arrange object indexes'
      Caption = 'btnArrange'
      ImageIndex = 34
      ParentShowHint = False
      ShowHint = True
      OnClick = btnArrangeClick
    end
    object ToolButton2: TToolButton
      Left = 77
      Top = 2
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 35
      Style = tbsSeparator
    end
    object btnRequest: TToolButton
      Left = 85
      Top = 2
      Hint = 'Request Info'
      Caption = 'btnRequest'
      ImageIndex = 69
      ParentShowHint = False
      ShowHint = True
      OnClick = btnRequestClick
    end
    object btnRequestAll: TToolButton
      Left = 108
      Top = 2
      Hint = 'Request All'
      Caption = 'btnRequestAll'
      ImageIndex = 70
      ParentShowHint = False
      ShowHint = True
      OnClick = btnRequestAllClick
    end
  end
  object GroupBox1: TGroupBox
    Left = 204
    Top = 4
    Width = 173
    Height = 293
    Caption = 'Object'
    TabOrder = 3
    object Label1: TLabel
      Left = 12
      Top = 24
      Width = 32
      Height = 13
      Caption = 'Group:'
    end
    object GroupLabel: TLabel
      Left = 48
      Top = 24
      Width = 91
      Height = 13
      Caption = 'Magical fortress'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 12
      Top = 80
      Width = 88
      Height = 13
      Caption = 'Material(s) to build:'
    end
    object Label7: TLabel
      Left = 12
      Top = 152
      Width = 59
      Height = 13
      Caption = 'Skill to build:'
    end
    object lTool: TLabel
      Left = 12
      Top = 200
      Width = 107
      Height = 13
      Caption = 'Building bonus for tool:'
    end
    object cmMaterial1: TComboBox
      Left = 16
      Top = 96
      Width = 93
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 0
      OnChange = DataChange
      OnDrawItem = cmIDataDrawItem
    end
    object cmMaterial2: TComboBox
      Left = 16
      Top = 120
      Width = 93
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 1
      OnCloseUp = DataChange
      OnDrawItem = cmIDataDrawItem
    end
    object cmSkill: TComboBox
      Left = 16
      Top = 168
      Width = 105
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 2
      OnCloseUp = DataChange
      OnDrawItem = cmSkillDrawItem
    end
    object SizeEdit: TIntEdit
      Left = 112
      Top = 96
      Width = 45
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 0
      OnChange = DataChange
    end
    object SkillLvEdit: TIntEdit
      Left = 124
      Top = 168
      Width = 33
      Height = 22
      MaxValue = 5
      MinValue = 1
      TabOrder = 4
      Value = 1
      OnChange = DataChange
    end
    object cmType: TComboBox
      Left = 16
      Top = 48
      Width = 141
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 5
      OnChange = DataChange
      OnDrawItem = cmTypeDrawItem
      Items.Strings = (
        'Trade structure'
        'Defence structure'
        'Transport'
        'Flying transport'
        'Closed lair'
        'Shaft'
        'Road'
        'Fleet')
    end
    object cmTool: TComboBox
      Left = 16
      Top = 216
      Width = 105
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 6
      OnChange = DataChange
      OnDrawItem = cmIDataDrawItem
    end
    object eToolBonus: TIntEdit
      Left = 124
      Top = 216
      Width = 33
      Height = 22
      MaxValue = 99
      MinValue = 0
      TabOrder = 7
      Value = 0
      OnChange = DataChange
    end
  end
  object GroupBox2: TGroupBox
    Left = 380
    Top = 4
    Width = 161
    Height = 105
    Caption = 'Transport'
    TabOrder = 4
    object Label4: TLabel
      Left = 12
      Top = 28
      Width = 44
      Height = 13
      Caption = 'Capacity:'
    end
    object Label5: TLabel
      Left = 12
      Top = 56
      Width = 34
      Height = 13
      Caption = 'Sailors:'
    end
    object Label8: TLabel
      Left = 12
      Top = 80
      Width = 34
      Height = 13
      Caption = 'Speed:'
    end
    object CapacityEdit: TIntEdit
      Left = 68
      Top = 24
      Width = 65
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = DataChange
    end
    object SailorEdit: TIntEdit
      Left = 68
      Top = 52
      Width = 65
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = DataChange
    end
    object SpeedEdit: TIntEdit
      Left = 68
      Top = 76
      Width = 65
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
      OnChange = DataChange
    end
  end
  object GroupBox3: TGroupBox
    Left = 380
    Top = 112
    Width = 161
    Height = 125
    Caption = 'Defence structure, warship'
    TabOrder = 5
    object Label2: TLabel
      Left = 12
      Top = 28
      Width = 53
      Height = 13
      Caption = 'Defence to'
    end
    object Label6: TLabel
      Left = 128
      Top = 28
      Width = 20
      Height = 13
      Caption = 'men'
    end
    object ProtectionEdit: TIntEdit
      Left = 72
      Top = 24
      Width = 49
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = DataChange
    end
    object gDefence: TPowerGrid
      Left = 8
      Top = 56
      Width = 145
      Height = 57
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
      OnDrawCell = gDefenceDrawCell
      OnSetEditText = gDefenceSetEditText
      ColWidths = (
        96
        45)
    end
  end
  object GroupBox4: TGroupBox
    Left = 380
    Top = 240
    Width = 161
    Height = 57
    Caption = 'Increases production'
    TabOrder = 6
    object cmProduction: TComboBox
      Left = 20
      Top = 20
      Width = 129
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 0
      OnCloseUp = DataChange
      OnDrawItem = cmIDataDrawItem
    end
  end
  object mDescription: TRichEdit
    Left = 208
    Top = 304
    Width = 333
    Height = 81
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 7
  end
end
