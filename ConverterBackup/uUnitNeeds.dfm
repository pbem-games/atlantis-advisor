object UnitNeedsForm: TUnitNeedsForm
  Left = 201
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Needs'
  ClientHeight = 360
  ClientWidth = 447
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
    447
    360)
  PixelsPerInch = 96
  TextHeight = 13
  object lMen: TLabel
    Left = 240
    Top = 12
    Width = 35
    Height = 13
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = '10 men'
  end
  object Label1: TLabel
    Left = 12
    Top = 336
    Width = 165
    Height = 13
    Caption = 'Drag items to add or remove needs'
  end
  object btnOk: TButton
    Left = 278
    Top = 332
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOkClick
  end
  object Button2: TButton
    Left = 358
    Top = 332
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object gAvai: TPowerGrid
    Left = 284
    Top = 8
    Width = 157
    Height = 313
    ColCount = 1
    DefaultRowColor = clBlack
    Editing = False
    FixedRows = 0
    ImageCol = 0
    Options = [pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
    RowCount = 0
    Sorted = True
    SortBy = 0
    StickySelect = False
    TopRow = 0
    OnDrawCell = gAvaiDrawCell
    OnDragDrop = gAvaiDragDrop
    OnDragOver = gAvaiDragOver
    OnMouseDown = gAvaiMouseDown
    ColWidths = (
      153)
  end
  object cbEquipment: TCheckBox
    Left = 12
    Top = 12
    Width = 113
    Height = 17
    Caption = 'Equipment, priority '
    TabOrder = 3
    OnClick = cbEquipmentClick
  end
  object cbNoGive: TCheckBox
    Left = 12
    Top = 32
    Width = 121
    Height = 17
    Caption = 'Do not give anything'
    TabOrder = 4
    OnClick = cbNoGiveClick
  end
  object Grid: TPowerGrid
    Left = 8
    Top = 64
    Width = 269
    Height = 257
    ColCount = 4
    DefaultRowColor = clBlack
    Editing = False
    FixedRows = 1
    ImageCol = 0
    Options = [pgoLines, pgoRowSelect, pgoStretchLastCol]
    RowCount = 2
    Sorted = False
    SortBy = 0
    StickySelect = False
    TopRow = 1
    OnDrawCell = GridDrawCell
    OnDragDrop = GridDragDrop
    OnDragOver = GridDragOver
    OnSetEditText = GridSetEditText
    OnMouseDown = GridMouseDown
    ColWidths = (
      58
      32
      115
      60)
  end
  object eEquipPriority: TIntEdit
    Left = 124
    Top = 10
    Width = 37
    Height = 22
    MaxValue = 99
    MinValue = 0
    TabOrder = 6
    Value = 0
    OnChange = eEquipPriorityChange
  end
end
