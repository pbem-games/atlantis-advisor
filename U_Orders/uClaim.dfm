object ClaimForm: TClaimForm
  Left = 203
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Claim'
  ClientHeight = 118
  ClientWidth = 186
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
  object Label1: TLabel
    Left = 24
    Top = 52
    Width = 28
    Height = 13
    Caption = 'Claim:'
  end
  object Label2: TLabel
    Left = 24
    Top = 20
    Width = 53
    Height = 13
    Caption = 'Unclaimed:'
  end
  object UnclaimedLabel: TLabel
    Left = 108
    Top = 20
    Width = 24
    Height = 13
    Caption = '2000'
  end
  object Image1: TImage
    Left = 88
    Top = 18
    Width = 16
    Height = 16
    Transparent = True
  end
  object Button1: TButton
    Left = 12
    Top = 88
    Width = 75
    Height = 21
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 100
    Top = 88
    Width = 75
    Height = 21
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object SpinEdit: TIntEdit
    Left = 88
    Top = 48
    Width = 73
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 0
    OnKeyPress = SpinEditKeyPress
  end
end
