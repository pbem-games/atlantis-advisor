object WantedItemsForm: TWantedItemsForm
  Left = 206
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Wanted Items'
  ClientHeight = 378
  ClientWidth = 234
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
  object cmItem: TComboBox
    Left = 8
    Top = 8
    Width = 217
    Height = 22
    Style = csOwnerDrawFixed
    ItemHeight = 16
    TabOrder = 0
    OnChange = cmItemChange
    OnDrawItem = cmItemDrawItem
  end
  object Grid: TPowerGrid
    Left = 8
    Top = 40
    Width = 217
    Height = 269
    ColCount = 2
    DefaultRowColor = clBlack
    Editing = False
    FixedRows = 1
    ImageCol = 0
    Options = [pgoLines, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
    RowCount = 2
    Sorted = True
    SortBy = 1
    SortOrder = soDescending
    StickySelect = False
    TopRow = 1
    OnDblClick = GridDblClick
    ColWidths = (
      139
      74)
  end
  object cmTowns: TComboBox
    Left = 8
    Top = 320
    Width = 141
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Sorted = True
    TabOrder = 2
  end
  object btnAddTown: TButton
    Left = 152
    Top = 320
    Width = 73
    Height = 21
    Caption = 'Add'
    TabOrder = 3
    OnClick = btnAddTownClick
  end
  object Button2: TButton
    Left = 152
    Top = 352
    Width = 73
    Height = 21
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
end
