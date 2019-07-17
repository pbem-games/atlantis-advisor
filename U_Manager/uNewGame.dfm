object NewGameForm: TNewGameForm
  Left = 203
  Top = 103
  BorderStyle = bsDialog
  Caption = 'New Game'
  ClientHeight = 162
  ClientWidth = 248
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    248
    162)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 167
    Top = 135
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Button2: TButton
    Left = 87
    Top = 135
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 8
    Width = 237
    Height = 117
    Caption = 'Properties'
    TabOrder = 2
    object Label1: TLabel
      Left = 24
      Top = 28
      Width = 60
      Height = 13
      Caption = 'Game name:'
    end
    object Label2: TLabel
      Left = 24
      Top = 56
      Width = 39
      Height = 13
      Caption = 'Ruleset:'
    end
    object GameNameEdit: TEdit
      Left = 92
      Top = 24
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'new game'
    end
    object cmRuleset: TComboBox
      Left = 92
      Top = 52
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 1
    end
    object cbLocal: TCheckBox
      Left = 92
      Top = 84
      Width = 81
      Height = 17
      Caption = 'Local game'
      TabOrder = 2
    end
  end
end
