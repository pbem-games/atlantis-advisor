object TerrEditForm: TTerrEditForm
  Left = 201
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Terrain Editor'
  ClientHeight = 334
  ClientWidth = 315
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
  object Label2: TLabel
    Left = 149
    Top = 12
    Width = 72
    Height = 13
    Caption = 'Mini Map color:'
  end
  object Grid: TPowerGrid
    Left = 8
    Top = 8
    Width = 125
    Height = 317
    ColCount = 1
    DefaultRowColor = clBlack
    FixedRows = 0
    ImageCol = 0
    Options = [pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
    RowCount = 0
    Sorted = False
    SortBy = 0
    StickySelect = True
    TopRow = 0
    OnDrawCell = GridDrawCell
    OnSelectCell = GridSelectCell
    ColWidths = (
      121)
  end
  object Button1: TButton
    Left = 232
    Top = 308
    Width = 75
    Height = 21
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object gbProps: TGroupBox
    Left = 144
    Top = 32
    Width = 161
    Height = 269
    Caption = 'Properties'
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 76
      Height = 13
      Caption = 'Movement cost:'
    end
    object Label3: TLabel
      Left = 16
      Top = 76
      Width = 71
      Height = 13
      Caption = 'Use in combat:'
    end
    object eMoveCost: TIntEdit
      Left = 100
      Top = 20
      Width = 45
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = DataChange
    end
    object cbWater: TCheckBox
      Left = 16
      Top = 52
      Width = 57
      Height = 17
      Caption = 'Water'
      TabOrder = 1
      OnClick = DataChange
    end
    object cbRiding: TCheckBox
      Left = 24
      Top = 92
      Width = 97
      Height = 17
      Caption = 'Riding mounts'
      TabOrder = 2
      OnClick = DataChange
    end
    object cbFlying: TCheckBox
      Left = 24
      Top = 108
      Width = 97
      Height = 17
      Caption = 'Flying mounts'
      TabOrder = 3
      OnClick = DataChange
    end
    object AdvGrid: TPowerGrid
      Left = 8
      Top = 136
      Width = 145
      Height = 101
      ColCount = 1
      DefaultRowColor = clBlack
      FixedRows = 0
      ImageCol = 0
      Options = [pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
      RowCount = 0
      Sorted = False
      SortBy = 0
      StickySelect = False
      TopRow = 0
      OnDrawCell = AdvGridDrawCell
      ColWidths = (
        141)
    end
    object cmAdv: TComboBox
      Left = 8
      Top = 240
      Width = 105
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 5
      OnDrawItem = cmAdvDrawItem
    end
    object ToolBar1: TToolBar
      Left = 118
      Top = 243
      Width = 39
      Height = 22
      Align = alNone
      ButtonHeight = 18
      ButtonWidth = 19
      Caption = 'ToolBar1'
      EdgeBorders = []
      Flat = True
      Images = ResForm.SmallBtnList
      TabOrder = 6
      object btnAddAdv: TToolButton
        Left = 0
        Top = 0
        Caption = 'btnAddAdv'
        ImageIndex = 0
        OnClick = btnAddAdvClick
      end
      object btnDelAdv: TToolButton
        Left = 19
        Top = 0
        Caption = 'btnDelAdv'
        ImageIndex = 1
        OnClick = btnDelAdvClick
      end
    end
  end
  object ColorBtn: TColorBtn
    Left = 231
    Top = 10
    Width = 34
    Height = 17
    Color = clWhite
    ColorDialog = ColorDialog
    Transparency = False
    OnClick = ColorBtnClick
  end
  object ColorDialog: TColorDialog
    Ctl3D = True
    Left = 16
    Top = 16
  end
end
