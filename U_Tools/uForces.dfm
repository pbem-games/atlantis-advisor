object ForceForm: TForceForm
  Left = 201
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Armies'
  ClientHeight = 348
  ClientWidth = 449
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
  object Grid: TPowerGrid
    Left = 8
    Top = 28
    Width = 197
    Height = 313
    ColCount = 1
    DefaultRowColor = clBlack
    FixedRows = 0
    Options = [pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
    RowCount = 0
    Sorted = True
    SortBy = 0
    StickySelect = True
    TopRow = 0
    OnDrawCell = GridDrawCell
    OnSelectCell = GridSelectCell
    ColWidths = (
      193)
  end
  object Button1: TButton
    Left = 364
    Top = 320
    Width = 75
    Height = 21
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object ToolBar1: TToolBar
    Left = 12
    Top = 4
    Width = 53
    Height = 21
    Align = alNone
    ButtonHeight = 18
    ButtonWidth = 19
    Caption = 'ToolBar1'
    EdgeBorders = []
    Flat = True
    Images = ResForm.SmallBtnList
    TabOrder = 2
    object btnAddForce: TToolButton
      Left = 0
      Top = 0
      Caption = 'btnAddForce'
      ImageIndex = 0
      OnClick = btnAddForceClick
    end
    object btnDelForce: TToolButton
      Left = 19
      Top = 0
      Caption = 'btnDelForce'
      ImageIndex = 1
    end
  end
  object GroupBox1: TGroupBox
    Left = 212
    Top = 4
    Width = 229
    Height = 309
    Caption = 'Properties'
    TabOrder = 3
    object Label1: TLabel
      Left = 16
      Top = 36
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object Label2: TLabel
      Left = 16
      Top = 64
      Width = 27
      Height = 13
      Caption = 'Color:'
    end
    object eName: TEdit
      Left = 60
      Top = 32
      Width = 157
      Height = 21
      TabOrder = 0
      OnExit = DataChange
      OnKeyPress = eNameKeyPress
    end
    object btnColor: TColorBtn
      Left = 60
      Top = 62
      Width = 34
      Height = 17
      Color = clWhite
      ColorDialog = ColorDialog
      Transparency = True
      OnClick = DataChange
    end
  end
  object ColorDialog: TColorDialog
    Ctl3D = True
    Left = 408
    Top = 8
  end
end
