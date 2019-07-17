object RegForm: TRegForm
  Left = 189
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Registration'
  ClientHeight = 111
  ClientWidth = 287
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 20
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label2: TLabel
    Left = 16
    Top = 48
    Width = 28
    Height = 13
    Caption = 'Code:'
  end
  object NameEdit: TEdit
    Left = 52
    Top = 16
    Width = 217
    Height = 21
    TabOrder = 0
  end
  object CodeEdit: TEdit
    Left = 52
    Top = 44
    Width = 217
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 60
    Top = 80
    Width = 75
    Height = 21
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 156
    Top = 80
    Width = 75
    Height = 21
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = Button2Click
  end
end
