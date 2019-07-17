object RegStatForm: TRegStatForm
  Left = 203
  Top = 101
  BorderStyle = bsDialog
  Caption = 'Region Stats'
  ClientHeight = 290
  ClientWidth = 422
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
    Top = 8
    Width = 405
    Height = 245
    ColCount = 1
    DefaultRowColor = clBlack
    DefaultRowHeight = 30
    FixedRows = 1
    ImageCol = 0
    Options = [pgoLines, pgoNoSelect]
    RowCount = 1
    Sorted = False
    SortBy = 0
    StickySelect = False
    TopRow = 1
    OnDrawCell = GridDrawCell
    ColWidths = (
      90)
  end
  object Button1: TButton
    Left = 332
    Top = 264
    Width = 75
    Height = 21
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object cbNumbers: TCheckBox
    Left = 16
    Top = 264
    Width = 97
    Height = 17
    Caption = 'Show Numbers'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = cbNumbersClick
  end
end
