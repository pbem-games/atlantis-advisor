object ListFilterForm: TListFilterForm
  Left = 252
  Top = 151
  BorderStyle = bsToolWindow
  Caption = 'List Filter'
  ClientHeight = 281
  ClientWidth = 154
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 4
    Top = 256
    Width = 69
    Height = 21
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 80
    Top = 256
    Width = 69
    Height = 21
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ItemBox: TCheckListBox
    Left = 4
    Top = 4
    Width = 145
    Height = 213
    ItemHeight = 17
    Items.Strings = (
      'Other Items'
      'Men'
      'Monsters'
      'Magic Items'
      'Weapon'
      'Armor'
      'Food'
      'Mounts'
      'Resources'
      'Advanced Items'
      'Tools'
      'Trade goods')
    Style = lbOwnerDrawFixed
    TabOrder = 2
    OnDrawItem = ItemBoxDrawItem
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 224
    Width = 45
    Height = 17
    Caption = 'All'
    TabOrder = 3
    OnClick = CheckBox1Click
  end
end
