object TeachForm: TTeachForm
  Left = 203
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Teach'
  ClientHeight = 322
  ClientWidth = 493
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lStudents: TLabel
    Left = 372
    Top = 8
    Width = 87
    Height = 13
    Caption = 'Students: 10 of 10'
  end
  object TeachGrid: TPowerGrid
    Left = 4
    Top = 32
    Width = 485
    Height = 257
    Color = clBlack
    ColCount = 7
    DefaultRowColor = clBlack
    Editing = False
    FixedRows = 1
    ImageCol = 0
    Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
    RowCount = 2
    Sorted = True
    SortBy = 1
    StickySelect = False
    TopRow = 1
    OnClick = TeachGridClick
    OnDrawCell = TeachGridDrawCell
    ColWidths = (
      18
      108
      37
      112
      44
      72
      90)
  end
  object Button2: TButton
    Left = 408
    Top = 296
    Width = 75
    Height = 21
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 328
    Top = 296
    Width = 75
    Height = 21
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object ToolBar1: TToolBar
    Left = 12
    Top = 4
    Width = 145
    Height = 27
    Align = alNone
    Caption = 'ToolBar1'
    EdgeBorders = []
    Images = ResForm.BtnImages
    TabOrder = 3
    object btnMyFaction: TToolButton
      Left = 0
      Top = 2
      Hint = 'Player'#39's faction'
      Caption = 'btnMyFaction'
      ImageIndex = 11
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
      OnClick = btnMyFactionClick
    end
    object btnStudents: TToolButton
      Left = 23
      Top = 2
      Hint = 'Students'
      Caption = 'btnStudents'
      ImageIndex = 40
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
      OnClick = btnStudentsClick
    end
  end
end
